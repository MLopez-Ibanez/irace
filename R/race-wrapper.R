###############################################################################
# FIXME: This file needs a description
###############################################################################
buildCommandLine <- function(values, switches) {
  stopifnot(length(values) == length(switches))
  command <- ""
  # FIXME: This probably can be implemented faster with apply() and
  # paste(collapse=" "). But how to get the index i in that case?
  for (i in seq_along(values)) {
    value <- values[i]
    if (!is.na(value)) {
      command <- paste(command, " ", switches[i], value, sep="")
    }
  }
  return(command)
}


## FIXME: function needs a description
race.init <- function(candidates, parameters, config)
{
  # FIXME: This is quite a waste of time. It will be better to pass the index
  # around.
  race.instances <- seq(.irace$next.instance, nrow(config$instancesList))
  # Return init list
  return (list (no.candidates = nrow(candidates), 
                no.tasks = length(race.instances),
                race.instances = race.instances,
                candidates = candidates, 
                parameters = parameters,
                config = config
                ))
}

## FIXME: function needs a description
race.info <- function(data)
  return(list(race.name = "irace",
              no.candidates = data$no.candidates, 
              no.tasks = data$no.tasks))

# FIXME: This should be called in race-wrapper for cases where hook.run.default
# is overriden
check.output <- function(output, command, config, hook.run.call = NULL, outputRaw = NULL)
{
  # We check the output here to provide better error messages.
  err.msg <- NULL
  if (length(output) < 1 || length(output) > 2 || any (is.na (output)) || any (!is.numeric(output))) {
    err.msg <- paste("The output of '", command, "' is not numeric!", sep = "")
  } else if (any(is.infinite(output))) {
    err.msg <- paste("The output of '", command, "' is not finite!", sep = "")
  }

  if (config$timeBudget > 0 && length(output) < 2)
    err.msg <- paste("When timeBudget > 0, the output of `", command,
                     "' must be two numbers 'cost time'!", sep = "")

  if (config$timeBudget == 0 && length(output) > 1)
    err.msg <- paste("When timeBudget == 0, the output of `", command,
                     "' must be one number 'cost'!", sep = "")

  if (!is.null(err.msg)) {
    if (!is.null(hook.run.call)) {
      err.msg <- paste(err.msg, "\n", .irace.prefix,
                       "The call to hookRun was:\n", hook.run.call, sep="")
    }

    if (is.null(outputRaw)) {
      # Message for a function call.
      outputRaw <- output
      advice.txt <- paste(
        "This is not a bug in irace, but means that something failed in",
        "a call to the hookRun or hookEvaluate functions provided by the user.",
        "Please check those functions carefully.")
    } else {
      # Message for an external script.
      advice.txt <- paste(
        sep="",
        "This is not a bug in irace, but means that something failed when",
        " running the command above or it was terminated before completion.",
        " Try to run the command above from the execution directory '",
        config$execDir, "' to investigate the issue.")
    }
    tunerError(err.msg, "\n", .irace.prefix,
               "The output was:\n", paste(outputRaw, collapse = "\n"),
               "\n", .irace.prefix, advice.txt)
  }
}

# This function is used by the hook.run.default and hook.evaluate.default. If
# overridden, check.output will be called again later.
parse.output <- function(outputRaw, command, config, hook.run.call = NULL)
{
  if (config$debugLevel >= 1) { cat (outputRaw, sep = "\n") }

  output <- NULL
  # strsplit crashes if outputRaw == character(0)
  if (length(outputRaw) > 0) {
    output <- strsplit(trim(outputRaw), "[[:space:]]+")[[1]]
    # suppressWarnings to avoid messages about NAs introduced by coercion
    output <- suppressWarnings (as.numeric (output))
  }
  # We check the output here to provide better error messages.
  check.output(output, command, config, hook.run.call, outputRaw = outputRaw)
  
  return(output)
}

hook.evaluate.default <- function(experiment, num.candidates, config, hook.run.call)
{
  execDir <- config$execDir
  debugLevel <- config$debugLevel
  hookEvaluate <- config$hookEvaluate
  if (as.logical(file.access(hookEvaluate, mode = 1))) {
    tunerError ("hookEvaluate", shQuote(hookEvaluate), "cannot be found or is not executable!\n")
  }

  ## Redirects STDERR so outputRaw captures the whole output.
  command <- paste (hookEvaluate, experiment$instance, experiment$id, num.candidates, "2>&1")
  if (debugLevel >= 1) {
    irace.note (command, "\n")
  }
  cwd <- setwd (execDir)
  # FIXME: This should use runcommand like hook.run.default
  outputRaw <- system (command, intern = TRUE)
  setwd (cwd)

  p.output <- parse.output (outputRaw, command, config, hook.run.call = hook.run.call)
   
  # FIXME: Pass the call to hook.run as hook.run.call if hook.evaluate was used.
  # FIXME: check.output is also called in parse.output and race.wrapper, that
  # is, three times!
  check.output(p.output, command = "hookEvaluate", config = config)

  return(p.output)
}

hook.run.default <- function(experiment, config)
{
  debugLevel    <- config$debugLevel
  candidate.id  <- experiment$id
  candidate     <- experiment$candidate
  instance      <- experiment$instance
  # FIXME: Not used for now
  instance.seed <- experiment$seed
  extra.params  <- experiment$extra.params 
  switches      <- experiment$switches
  
  runcommand <- function(command, args) {
    if (debugLevel >= 1) {
      irace.note (command, " ", args, "\n")
      elapsed <- proc.time()["elapsed"]
    }
    err <- NULL
    output <-  withCallingHandlers(
      tryCatch(system2(command, args, stdout = TRUE, stderr = TRUE),
               error=function(e) {
                 err <<- c(err, paste(conditionMessage(e), collapse="\n"))
                 NULL
               }), warning=function(w) {
                 err <<- c(err, paste(conditionMessage(w), collapse="\n"))
                 invokeRestart("muffleWarning")
               })
    # If e is a warning, the command failed.
    if (!is.null(err)) {
      err <- paste(err, collapse ="\n")
      if (debugLevel >= 1)
        irace.note ("ERROR (", candidate.id, ") :", err, "\n")
      return(list(output = output, error = err))
    }
    if (debugLevel >= 1) {
      irace.note ("DONE (", candidate.id, ") Elapsed: ",
                  proc.time()["elapsed"] - elapsed, "\n")
    }
    return(list(output = output, error = NULL))
  }

  hookRun <- config$hookRun
  if (as.logical(file.access(hookRun, mode = 1))) {
    tunerError ("hookRun `", hookRun, "' cannot be found or is not executable!\n")
  }

  args <- paste(instance, candidate.id, extra.params, buildCommandLine(candidate, switches))
  output <- runcommand(hookRun, args)

  if (!is.null(output$error)) {
    tunerError(output$error, "\n", .irace.prefix,
               "The call to hookRun was:\n", hookRun, " ", args, "\n", .irace.prefix,
               "The output was:\n", paste(output$output, collapse = "\n"),
               "\n", .irace.prefix,
               "This is not a bug in irace, but means that something failed when",
               " running the command above or it was terminated before completion.",
               " Try to run the command above from the execution directory '",
               config$execDir, "' to investigate the issue.")
  }

  if (!is.null(config$hookEvaluate)) {
    # FIXME: We should also check that the output is empty.
    return(paste(hookRun, args))
  }

  # Parse output
  p.output <- parse.output(output$output, paste(hookRun, args), config)

  # Check if the output is correct
  # FIXME: check.output is also called in parse.output and race.wrapper, that
  # is, three times!
  check.output(p.output, command = "hookRun", config = config)

  # hookEvaluate is NULL, so parse the output just here.
  return(p.output)
}


# FIXME: This is needed because race.R is not divided in two-stages
# run/evaluate like irace is, so there is no way to communicate data
# from the first stage to the second.
#
# FIXME: In fact, we should use this trick also in irace.R to avoid
# pass-by-copy-on-write of huge matrices and data.frames and instead
# pass-by-reference an environment containing those.
.irace <- new.env()

## FIXME: This function needs a description, what is candidate, task and data?
## Executes a list of candidates in a particular instance
## candidates: description having the id of the candidate
## instance.id: index of the instance that can be found in data$instances
## which.alive: index of the candidates that are still alive
## data: other info? tunerConfig + parameters
# LESLIE: should we replace data for the direct things? using enviroments would be better
race.wrapper <- function(candidates, instance.id, which.alive, data)
{
  debugLevel <- data$config$debugLevel

  if (debugLevel >= 4) { print(candidates) }
  stopifnot (data$parameters$nbParameters > 0)
  stopifnot (length(data$parameters$names) == data$parameters$nbParameters)
  
  instance.seed <- data$config$instancesList[instance.id, "seed"]
  real.id  <- data$config$instancesList[instance.id, "instance"]
  instance <- data$config$instances[[real.id]]
  extra.params <- NULL
  if (!is.null (data$config$instances.extra.params)
      && !is.na (data$config$instances.extra.params[[real.id]]))
    extra.params <- data$config$instances.extra.params[[real.id]]

  values <- removeCandidatesMetaData(candidates)
  parameters.names <- names(data$parameters$names)
  values <- values[parameters.names]
  switches <- data$parameters$switches[parameters.names]
  
  # Experiment list to execute 
  experiments <- list()
  ntest <- 1
  for (current.candidate in which.alive) {
    experiments[[ntest]] <- list (id = candidates[current.candidate, ".ID."],
                                  candidate = values[current.candidate,],
                                  instance = instance, extra.params = extra.params,
                                  switches = switches, seed = instance.seed)
    ntest <- ntest + 1
  }

  # Execute commands
  return (execute.experiments (experiments, length (experiments), data$config))
}

race.describe <- function(candidates, data)
{
  return (data$candidates[candidates, , drop = FALSE])
}

execute.experiments <- function(experiments, numcandidates, configuration)
{
  sgeCluster <- configuration$sgeCluster
  parallel <- configuration$parallel
  mpi <- configuration$mpi

  execDir <- configuration$execDir
  if (!isTRUE (file.info(execDir)$isdir)) {
    tunerError ("Execution directory '", execDir, "' is not found or not a directory\n")
  }

  hook.output <- vector("list", length(experiments))
  cwd <- setwd (execDir)
  on.exit(setwd(cwd), add = TRUE)

  if (!is.null(configuration$hookRunParallel)) {
    # User-defined parallelization
    hook.output <-
      configuration$hookRunParallel(experiments, .irace$hook.run, config = configuration)
  } else if (parallel > 1) {
    if (mpi) {
      if (configuration$loadBalancing) {
        hook.output <- Rmpi::mpi.applyLB(experiments, .irace$hook.run, config = configuration)
      } else {
        # Without load-balancing, we need to split the experiments into chunks
        # of size parallel.
        hook.output <- unlist(use.names=FALSE,
                              tapply(experiments,
                                     ceiling(1:length(experiments) / parallel),
                                     Rmpi::mpi.apply, .irace$hook.run,
                                     config = configuration)) 
      }
      # FIXME: if stop() is called from mpi.applyLB, it does not
      # terminate the execution of the parent process, so it will
      # continue and give more errors later. We have to terminate
      # here, but is there a nicer way to detect this and terminate?
      if (any(sapply(hook.output, inherits, "try-error"))) {
        # FIXME: mclapply has some bugs in case of error. In that
        # case, each element of the list does not keep the output of
        # each candidate and repetitions may occur.
        cat(unique(unlist( hook.output[sapply(
            hook.output, inherits, "try-error")])), file = stderr(), sep="")
        tunerError("A slave process terminated with a fatal error")
      }
    } else {
      if (.Platform$OS.type == 'windows') {
        irace.assert(!is.null(.irace$cluster))
        if (configuration$loadBalancing) {
          hook.output <-
            parallel::parLapplyLB(.irace$cluster, experiments, .irace$hook.run,
                                  config = configuration)
        } else {
          hook.output <-
            parallel::parLapply(.irace$cluster, experiments, .irace$hook.run,
                                config = configuration)
        }
        # FIXME: if stop() is called from parLapply, then the parent
        # process also terminates, and we cannot give further errors.
      } else {
        hook.output <-
          parallel::mclapply(experiments, .irace$hook.run,
                             # FALSE means load-balancing.
                             mc.preschedule = !configuration$loadBalancing,
                             mc.cores = parallel,
                             config = configuration)
        # FIXME: if stop() is called from mclapply, it does not
        # terminate the execution of the parent process, so it will
        # continue and give more errors later. We have to terminate
        # here, but is there a nicer way to detect this and terminate?
        if (any(sapply( hook.output, inherits, "try-error"))) {
          # FIXME: mclapply has some bugs in case of error. In that
          # case, each element of the list does not keep the output of
          # each candidate and repetitions may occur.
          cat(unique(unlist(
            hook.output[sapply(
              hook.output, inherits, "try-error")])), file = stderr())
          tunerError("A child process triggered a fatal error")
        }
      }
    }
  } else if (sgeCluster) {
    hook.output <- cluster.lapply (experiments, config = configuration)
  } else {
    # One process, all sequential
    for (k in seq_along(experiments)) {
      hook.output[[k]] <-.irace$hook.run(experiments[[k]], config = configuration)
    }
  }
  
  # hook.evaluate may be NULL. If so, hook.output must
  # contain the right output already.
  if (!is.null(.irace$hook.evaluate)) {
    ## Evaluate candidates sequentially
    for (k in seq_along(experiments)) {
      hook.output[[k]] <-
        .irace$hook.evaluate(experiment = experiments[[k]], numcandidates,
                             config = configuration, hook.run.call = hook.output[[k]])
    }
  }
  return(hook.output)
}
