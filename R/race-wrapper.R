# FIXME: This is needed because race.R is not divided in two-stages
# run/evaluate like irace is, so there is no way to communicate data
# from the first stage to the second.
#
# FIXME: In fact, we should use this trick also in irace.R to avoid
# pass-by-copy-on-write of huge matrices and data.frames and instead
# pass-by-reference an environment containing those.
.irace <- new.env()

buildCommandLine <- function(values, switches)
{
  irace.assert(length(values) == length(switches))
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


# FIXME: This should be called in race-wrapper for cases where target.runner.default
# is overriden
check.output <- function(output, command, scenario, target.runner.call = NULL, outputRaw = NULL)
{
  # We check the output here to provide better error messages.
  err.msg <- NULL
  if (length(output) < 1 || length(output) > 1 || any (is.na (output)) || any (!is.numeric(output))) {
    err.msg <- paste("The output of '", command, "' is not numeric!", sep = "")
  } else if (any(is.infinite(output))) {
    err.msg <- paste("The output of '", command, "' is not finite!", sep = "")
  }

  if (length(output) > 1)
    err.msg <- paste("The output of `", command, "' must be one number 'cost'!", sep = "")

  if (!is.null(err.msg)) {
    if (!is.null(target.runner.call)) {
      err.msg <- paste(err.msg, "\n", .irace.prefix,
                       "The call to targetRunner was:\n", target.runner.call, sep="")
    }

    if (is.null(outputRaw)) {
      # Message for a function call.
      outputRaw <- output
      advice.txt <- paste(
        "This is not a bug in irace, but means that something failed in",
        "a call to the targetRunner or targetEvaluator functions provided by the user.",
        "Please check those functions carefully.")
    } else {
      # Message for an external script.
      advice.txt <- paste0(
        "This is not a bug in irace, but means that something failed when",
        " running the command above or it was terminated before completion.",
        " Try to run the command above from the execution directory '",
        scenario$execDir, "' to investigate the issue.")
    }
    irace.error(err.msg, "\n", .irace.prefix,
                "The output was:\n", paste(outputRaw, collapse = "\n"),
                "\n", .irace.prefix, advice.txt)
  }
}

# This function is used by the target.runner.default and target.evaluator.default. If
# overridden, check.output will be called again later.
parse.output <- function(outputRaw, command, scenario, target.runner.call = NULL)
{
  if (scenario$debugLevel >= 2) { cat (outputRaw, sep = "\n") }

  output <- NULL
  # strsplit crashes if outputRaw == character(0)
  if (length(outputRaw) > 0) {
    output <- strsplit(trim(outputRaw), "[[:space:]]+")[[1]]
    # suppressWarnings to avoid messages about NAs introduced by coercion
    output <- suppressWarnings (as.numeric (output))
  }
  # We check the output here to provide better error messages.
  check.output(output, command, scenario, target.runner.call, outputRaw = outputRaw)
  
  return(output)
}

target.evaluator.default <- function(experiment, num.configurations, all.conf.id="", scenario, target.runner.call)
{
  configuration.id <- experiment$id.configuration
  instance.id      <- experiment$id.instance
  seed             <- experiment$seed
  instance         <- experiment$instance

  execDir <- scenario$execDir
  debugLevel <- scenario$debugLevel
  targetEvaluator <- scenario$targetEvaluator
  if (as.logical(file.access(targetEvaluator, mode = 1))) {
    irace.error ("targetEvaluator", shQuote(targetEvaluator), "cannot be found or is not executable!\n")
  }

  ## Redirects STDERR so outputRaw captures the whole output.
  command <- paste (targetEvaluator, configuration.id, instance.id, seed, instance, num.configurations, all.conf.id ,"2>&1")
  if (debugLevel >= 2) {
    irace.note (command, "\n")
  }
  cwd <- setwd (execDir)
  # FIXME: This should use runcommand like target.runner.default
  outputRaw <- system (command, intern = TRUE)
  setwd (cwd)

  p.output <- parse.output (outputRaw, command, scenario, target.runner.call = target.runner.call)
   
  # FIXME: Pass the call to target.runner as target.runner.call if target.evaluator was used.
  # FIXME: check.output is also called in parse.output and race.wrapper, that
  # is, three times!
  check.output(p.output, command = "targetEvaluator", scenario = scenario)

  return(p.output)
}

target.runner.default <- function(experiment, scenario)
{
  debugLevel       <- scenario$debugLevel
  configuration.id <- experiment$id.configuration
  instance.id      <- experiment$id.instance
  seed             <- experiment$seed
  configuration    <- experiment$configuration
  instance         <- experiment$instance
  extra.params     <- experiment$extra.params
  switches         <- experiment$switches
  
  runcommand <- function(command, args) {
    if (debugLevel >= 2) {
      irace.note (command, " ", args, "\n")
      elapsed <- proc.time()["elapsed"]
    }
    err <- NULL
    output <-  withCallingHandlers(
      tryCatch(system2(command, args, stdout = TRUE, stderr = TRUE),
               error = function(e) {
                 err <<- c(err, paste(conditionMessage(e), collapse="\n"))
                 NULL
               }), warning = function(w) {
                 err <<- c(err, paste(conditionMessage(w), collapse="\n"))
                 invokeRestart("muffleWarning")
               })
    # If the command could not be run an R error is generated.  If ‘command’
    # runs but gives a non-zero exit status this will be reported with a
    # warning and in the attribute ‘"status"’ of the result: an attribute
    # ‘"errmsg"’ may also be available.
    if (!is.null(err)) {
      err <- paste(err, collapse ="\n")
      if (!is.null(attr(output, "errmsg")))
        output <- paste(sep = "\n", attr(output, "errmsg"))
      if (debugLevel >= 2)
        irace.note ("ERROR (", configuration.id, "): ", err, "\n")
      return(list(output = output, error = err))
    }
    if (debugLevel >= 2) {
      irace.note ("DONE (", configuration.id, ") Elapsed: ",
                  formatC(proc.time()["elapsed"] - elapsed,
                          format = "f", digits = 2), "\n")
    }
    return(list(output = output, error = NULL))
  }

  targetRunner <- scenario$targetRunner
  if (as.logical(file.access(targetRunner, mode = 1))) {
    irace.error ("targetRunner '", targetRunner, "' cannot be found or is not executable!\n")
  }

  args <- paste(configuration.id, instance.id, seed, instance, extra.params,
                buildCommandLine(configuration, switches))
  output <- runcommand(targetRunner, args)

  retries <- scenario$targetRunnerRetries
  # Retry!
  while (!is.null(output$error) && retries > 0) {
    output <- runcommand(targetRunner, args)
    retries <- retries - 1
  }
  
  if (!is.null(output$error)) {
    irace.error(output$error, "\n", .irace.prefix,
               "The call to target.runner.default was:\n", targetRunner, " ", args, "\n", .irace.prefix,
               "The output was:\n", paste(output$output, collapse = "\n"),
               "\n", .irace.prefix,
               "This is not a bug in irace, but means that something failed when",
               " running the command above or it was terminated before completion.",
               " Try to run the command above from the execution directory '",
               scenario$execDir, "' to investigate the issue.")
  }

  if (!is.null(scenario$targetEvaluator)) {
    # FIXME: We should also check that the output is empty.
    return(paste(targetRunner, args))
  }

  # Parse output
  p.output <- parse.output(output$output, paste(targetRunner, args), scenario)

  # targetEvaluator is NULL, so parse the output just here.
  return(p.output)
}

execute.experiments <- function(experiments, scenario)
{
  sgeCluster <- scenario$sgeCluster
  parallel <- scenario$parallel
  mpi <- scenario$mpi

  execDir <- scenario$execDir
  if (!isTRUE (file.info(execDir)$isdir)) {
    irace.error ("Execution directory '", execDir, "' is not found or not a directory\n")
  }
  target.output <- vector("list", length(experiments))
  cwd <- setwd (execDir)
  on.exit(setwd(cwd), add = TRUE)
   
  if (!is.null(scenario$targetRunnerParallel)) {
    # User-defined parallelization
    target.output <-
      scenario$targetRunnerParallel(experiments, .irace$target.runner, scenario = scenario)
  } else if (parallel > 1) {
    if (mpi) {
      if (scenario$loadBalancing) {
        target.output <- Rmpi::mpi.applyLB(experiments, .irace$target.runner,
                                         scenario = scenario)
      } else {
        # Without load-balancing, we need to split the experiments into chunks
        # of size parallel.
        target.output <- unlist(use.names=FALSE,
                              tapply(experiments,
                                     ceiling(1:length(experiments) / parallel),
                                     Rmpi::mpi.apply, .irace$target.runner,
                                     scenario = scenario))
      }
      # FIXME: if stop() is called from mpi.applyLB, it does not
      # terminate the execution of the parent process, so it will
      # continue and give more errors later. We have to terminate
      # here, but is there a nicer way to detect this and terminate?
      if (any(sapply(target.output, inherits, "try-error"))) {
        # FIXME: mclapply has some bugs in case of error. In that
        # case, each element of the list does not keep the output of
        # each configuration and repetitions may occur.
        cat(unique(unlist(target.output[sapply(
            target.output, inherits, "try-error")])), file = stderr(), sep="")
        irace.error("A slave process terminated with a fatal error")
      }
    } else {
      if (.Platform$OS.type == 'windows') {
        irace.assert(!is.null(.irace$cluster))
        if (scenario$loadBalancing) {
          target.output <-
            parallel::parLapplyLB(.irace$cluster, experiments, .irace$target.runner,
                                  scenario = scenario)
        } else {
          target.output <-
            parallel::parLapply(.irace$cluster, experiments, .irace$target.runner,
                                scenario = scenario)
        }
        # FIXME: if stop() is called from parLapply, then the parent
        # process also terminates, and we cannot give further errors.
      } else {
        target.output <-
          parallel::mclapply(experiments, .irace$target.runner,
                             # FALSE means load-balancing.
                             mc.preschedule = !scenario$loadBalancing,
                             mc.cores = parallel,
                             scenario = scenario)
        # FIXME: if stop() is called from mclapply, it does not
        # terminate the execution of the parent process, so it will
        # continue and give more errors later. We have to terminate
        # here, but is there a nicer way to detect this and terminate?
        if (any(sapply(target.output, inherits, "try-error"))) {
          # FIXME: mclapply has some bugs in case of error. In that
          # case, each element of the list does not keep the output of
          # each configuration and repetitions may occur.
          cat(unique(unlist(
            target.output[sapply(
              target.output, inherits, "try-error")])), file = stderr())
          irace.error("A child process triggered a fatal error")
        }
      }
    }
  } else if (sgeCluster) {
    target.output <- cluster.lapply (experiments, scenario = scenario)
  } else {
    # One process, all sequential
    for (k in seq_along(experiments)) {
      target.output[[k]] <- .irace$target.runner(experiments[[k]], scenario = scenario)
    }
  }
 
  # FIXME: We are missing a check.output() here.
  return(target.output)
}
