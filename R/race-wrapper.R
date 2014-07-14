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
  # FIXME: Ideally, we wouldn't do this here but dynamically as we
  # need more instances to avoid the artificial limit of 1000
  # instances per race.
  if (config$sampleInstances) {
    # ??? This sould be sample.int(length(config$instances)) but R 2.9
    # has a bug in sample.int.
    race.instances <- rep(sample.int(length(config$instances),
                                     size = length(config$instances)),
                          length.out = 1000)
  } else {
    race.instances <- rep(1:length(config$instances), length.out = 1000)
  }
  # Return init list
  return (list (no.candidates = nrow(candidates), 
                no.tasks = 1000, # the same artificial limit as above.
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
  if (config$debugLevel >= 1) { cat (outputRaw, sep="\n") }

  output <- NULL
  # strsplit crashes if outputRaw == character(0)
  if (length(outputRaw) > 0) {
    output <- strsplit(trim(outputRaw), "[[:space:]]+")[[1]]
    # suppressWarnings to avoid NAs introduced by coercion
    output <- suppressWarnings (as.numeric (output))
  }
  # We check the output here to provide better error messages.
  check.output(output, command, config, hook.run.call, outputRaw = outputRaw)
  
  return(output)
}

hook.evaluate.default <- function(instance, candidate, num.candidates, extra.params, config, hook.run.call)
{
  execDir <- config$execDir
  debugLevel <- config$debugLevel
  hookEvaluate <- config$hookEvaluate
  if (as.logical(file.access(hookEvaluate, mode = 1))) {
    tunerError ("hookEvaluate", shQuote(hookEvaluate), "cannot be found or is not executable!\n")
  }

  ## Redirects STDERR so outputRaw captures the whole output.
  command <- paste (hookEvaluate, instance, candidate$index, num.candidates, "2>&1")
  if (debugLevel >= 1) {
    cat (format(Sys.time(), usetz=TRUE), ":", command, "\n")
  }
  cwd <- setwd (execDir)
  # FIXME: This should use runcommand like hook.run.default
  outputRaw <- system (command, intern = TRUE)
  setwd (cwd)
  return(parse.output (outputRaw, command, config, hook.run.call = hook.run.call))
}

hook.run.default <- function(instance, candidate, extra.params, config)
{
  debugLevel <- config$debugLevel
  
  runcommand <- function(command, args) {
    if (debugLevel >= 1) {
      cat (format(Sys.time(), usetz=TRUE), ":", command, args, "\n")
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
        cat (format(Sys.time(), usetz=TRUE), ": ERROR (", candidate$index,
             ") :", err, "\n")
      return(list(output = output, error = err))
    }
    if (debugLevel >= 1) {
      cat (format(Sys.time(), usetz=TRUE), ": DONE (", candidate$index,
           ") Elapsed: ", proc.time()["elapsed"] - elapsed, "\n")
    }
    return(list(output = output, error = NULL))
  }

  hookRun <- config$hookRun
  if (as.logical(file.access(hookRun, mode = 1))) {
    tunerError ("hookRun `", hookRun, "' cannot be found or is not executable!\n")
  }
  args <- paste(instance, candidate$index, extra.params,
                buildCommandLine(candidate$values, candidate$labels))
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
  # hookEvaluate is NULL, so parse the output just here.
  return(parse.output (output$output, paste(hookRun, args), config))
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
race.wrapper <- function(candidate, task, which.alive, data)
{
  debugLevel <- data$config$debugLevel
  execDir <- data$config$execDir
  sgeCluster <- data$config$sgeCluster
  parallel <- data$config$parallel
  mpi <- data$config$mpi
  timeBudget <- data$config$timeBudget

  if (!isTRUE (file.info(execDir)$isdir)) {
    tunerError ("Execution directory '", execDir, "' is not found or not a directory\n")
  }

  if (debugLevel >= 4) { print(data$candidates) }
  stopifnot (data$parameters$nbParameters > 0)
  stopifnot (length(data$parameters$names) == data$parameters$nbParameters)
  
  instance.idx <- data$race.instances[task]
  instance <- data$config$instances[[instance.idx]]
  extra.params <- NULL
  if (!is.null (data$config$instances.extra.params)
      && !is.na (data$config$instances.extra.params[[instance.idx]]))
    extra.params <- data$config$instances.extra.params[[instance.idx]]

  ## FIXME: This is testing if this is the first candidate. If so, run
  ## and evaluate all candidates. Otherwise, just print the output for
  ## the corresponding candidate.  This is an awful historical
  ## artifact because of the way the first irace was developed on top
  ## of race.
  if (candidate == which.alive[1]) {
    candidates <- vector("list", length(which.alive))
    for (k in seq_along(which.alive)) {
      candi <- which.alive[k]
      values <- data$parameters$names
      cnd <- data$candidates[candi, , drop = FALSE]
      for (i in seq_len (data$parameters$nbParameters)) {
        name <- data$parameters$names[[i]]
        values[[name]] <- cnd[[name]]
      }
      candidates[[k]] <- list(index = candi, values = values,
                              # Command-line switches
                              labels = data$parameters$switches)
    }

    # Execute commands
    .irace$hook.output <- vector("list", length(which.alive))
    cwd <- setwd (execDir)
    on.exit(setwd(cwd), add = TRUE)

    if (!is.null(data$config$hookRunParallel)) {
      # User-defined parallelization
      .irace$hook.output <-
        data$config$hookRunParallel(candidates, .irace$hook.run,
                                    instance = instance, extra.params = extra.params,
                                    config = data$config)
    } else if (parallel > 1) {
      if (mpi) {
        .irace$hook.output <-
          Rmpi::mpi.applyLB(candidates, .irace$hook.run,
                            instance = instance, extra.params = extra.params,
                            config = data$config)
        # FIXME: if stop() is called from mpi.applyLB, it does not
        # terminate the execution of the parent process, so it will
        # continue and give more errors later. We have to terminate
        # here, but is there a nicer way to detect this and terminate?
        if (any(sapply(.irace$hook.output, inherits, "try-error"))) {
          # FIXME: mclapply has some bugs in case of error. In that
          # case, each element of the list does not keep the output of
          # each candidate and repetitions may occur.
          cat(unique(unlist(
            .irace$hook.output[sapply(
              .irace$hook.output, inherits, "try-error")])), file = stderr(), sep="")
          tunerError("A slave process terminated with a fatal error")
        }
      } else {
        if (.Platform$OS.type == 'windows') {
          irace.assert(!is.null(.irace$cluster))
          # FIXME: How to do load-balancing?
          .irace$hook.output <-
            parallel::parLapply(.irace$cluster, candidates, .irace$hook.run,
                                instance = instance,
                                extra.params = extra.params,
                                config = data$config)
          # FIXME: if stop() is called from parLapply, then the parent
          # process also terminates, and we cannot give further errors.
        } else {
          .irace$hook.output <-
            parallel::mclapply(candidates, .irace$hook.run,
                               # FALSE means load-balancing.
                               mc.preschedule = FALSE, mc.cores = parallel,
                               instance = instance, extra.params = extra.params,
                               config = data$config)
          # FIXME: if stop() is called from mclapply, it does not
          # terminate the execution of the parent process, so it will
          # continue and give more errors later. We have to terminate
          # here, but is there a nicer way to detect this and terminate?
          if (any(sapply(.irace$hook.output, inherits, "try-error"))) {
            # FIXME: mclapply has some bugs in case of error. In that
            # case, each element of the list does not keep the output of
            # each candidate and repetitions may occur.
            cat(unique(unlist(
              .irace$hook.output[sapply(
                .irace$hook.output, inherits, "try-error")])), file = stderr())
            tunerError("A child process triggered a fatal error")
          }
        }
      }
    } else if (sgeCluster) {
      .irace$hook.output <-
        cluster.lapply (candidates,
                        instance = instance, extra.params = extra.params,
                        config = data$config)
    } else {
      # One process, all sequential
      for (k in seq_along(candidates)) {
        .irace$hook.output[[k]] <-
          .irace$hook.run(candidates[[k]],
                          instance = instance, extra.params = extra.params,
                          config = data$config)
      }
    }
    # hook.evaluate may be NULL. If so, .irace$hook.output must
    # contain the right output already.
    if (!is.null(.irace$hook.evaluate)) {
      ## Evaluate candidates sequentially
      for (k in seq_along(candidates)) {
        .irace$hook.output[[k]] <-
          .irace$hook.evaluate(candidate = candidates[[k]], length(candidates),
                               instance = instance, extra.params = extra.params,
                               config = data$config, hook.run.call = .irace$hook.output[[k]])
      }
    }
  }

  output <- .irace$hook.output[[match(candidate, which.alive)]]
  # FIXME: Pass the call to hook.run as hook.run.call if hook.evaluate was used.
  check.output(output, command = if (is.null(.irace$hook.evaluate)) "hookRun" else "hookEvaluate",
               config = data$config)
  return(output)
}

race.describe <- function(candidates, data)
{
  return (data$candidates[candidates, , drop = FALSE])
}
