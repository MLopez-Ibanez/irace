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


## ??? This function needs a description
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

## ??? This function needs a description
race.info <- function(data)
  return(list(race.name = data$config$expName, 
              no.candidates = data$no.candidates, 
              no.tasks = data$no.tasks, 
              extra = data$config$expDescription))

check.output <- function(output, command = "", config = stop("config needed"))
{
  # We check the output here to provide better error messages.
  err.msg <- NULL
  if (length(output) < 1 || length(output) > 2 || any (is.na (output)))
    err.msg <- paste("The output of `", command, "' is not numeric!\n", sep = "")

  if (config$timeBudget > 0 && length(output) < 2)
    err.msg <- paste("When timeBudget > 0, the output of `",
                     command,
                     "' must be two numbers 'cost time'!\n", sep = "")

  if (!is.null(err.msg)) {
    tunerError(err.msg,
               "The output was:\n", paste(output, sep="\n"),
               "\nThis is not a bug in irace, but means that something failed in",
               " a call to the hookRun or hookEvaluate functions provide by the user.",
               " Please check those functions carefully.")
  }
}

parse.output <- function(outputRaw, command, config, hook.run.command = NULL)
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
  err.msg <- NULL
  if (length(output) < 1 || length(output) > 2 || any (is.na (output)))
    err.msg <- paste("The output of `", command, "' is not numeric!\n", sep = "")
  
  if (config$timeBudget > 0 && length(output) < 2)
    err.msg <- paste("When timeBudget > 0, the output of `", command,
                     "' must be two numbers 'cost time'!\n", sep = "")
  if (!is.null(err.msg)) {
    if (!is.null(hook.run.command)) {
      err.msg <- paste(err.msg, "The call to hookRun was:\n", hook.run.command, "\n",sep="")
    }
    tunerError(err.msg,
               "The output was:\n", paste(outputRaw, sep="\n"),
               "\nThis is not a bug in irace, but means that something failed when",
               " running the command above or it was terminated before completion.",
               " Try to run the commands above from the execution directory '",
               config$execDir, "' to investigate the issue.")
  }
  return(output)
}

hook.evaluate.default <- function(instance, candidate, num.candidates, extra.params, config, hook.run.command)
{
  execDir <- config$execDir
  debugLevel <- config$debugLevel
  hookEvaluate <- config$hookEvaluate
  if (as.logical(file.access(hookEvaluate, mode = 1))) {
    stop ("hookEvaluate `", hookEvaluate,
          "' cannot be found or is not executable!\n")
  }

  ## Redirects STDERR so outputRaw captures the whole output.
  command <- paste (hookEvaluate, instance, candidate$index, num.candidates, "2>&1")
  if (debugLevel >= 1) { cat(command, "\n") }
  cwd <- setwd (execDir)
  outputRaw <- system (command, intern = TRUE)
  setwd (cwd)

  output <- parse.output (outputRaw, command, config, hook.run.command = hook.run.command)
  return (output)
}

hook.run.default <- function(instance, candidate, extra.params, config)
{
  debugLevel <- config$debugLevel
  
  runcommand <- function(command) {
    if (debugLevel >= 1) {
      cat (format(Sys.time(), usetz=TRUE), ":", command, "\n")
    }
    e <- tryCatch(output <- system(command, intern=TRUE), warning=function(w){w})
    # If e is a warning, the command failed.
    if (is.null(attributes(e))) {
      if (debugLevel >= 1) cat (format(Sys.time(), usetz=TRUE), ": DONE\n")
      e <- NULL
    }
    return(list(output = output, error = e))
  }

  hookRun <- config$hookRun
  if (as.logical(file.access(hookRun, mode = 1))) {
    stop ("hookRun `", hookRun, "' cannot be found or is not executable!\n")
  }
  command <- paste (hookRun, instance, candidate$index, extra.params,
                    buildCommandLine(candidate$values, candidate$labels))
  output <- runcommand(command)

  if (!is.null(output$error)) {
    tunerError(output$error, "!\n",
               "The output was:\n", paste(output$output, sep="\n"),
               "\nThis is not a bug in irace, but means that something failed when",
               " running the command above or it was terminated before completion.",
               " Try to run the commands above from the execution directory '",
               config$execDir, "' to investigate the issue.")
  }

  if (!is.null(config$hookEvaluate)) {
    # FIXME: We should also check that the output is empty.
    return(command)
  }
  # hookEvalute is NULL, so parse the output just here.
  return(parse.output (output$output, command, config))
}


# FIXME: This is needed because race.R is not divided in two-stages
# run/evaluate like irace is, so there is no way to communicate data
# from the first stage to the second.
.irace <- new.env()

## ??? This function needs a description, what is candidate, task and data?
race.wrapper <- function(candidate, task, which.alive, data)
{
  debugLevel <- data$config$debugLevel
  execDir <- data$config$execDir
  digits <- data$config$digits
  sgeCluster <- data$config$sgeCluster
  parallel <- data$config$parallel
  mpi <- data$config$mpi
  timeBudget <- data$config$timeBudget

  if (!isTRUE (file.info(execDir)$isdir)) {
    stop("Execution directory '", execDir, "' is not found or not a directory\n")
  }

  if (debugLevel >= 4) { print(data$candidates) }
  stopifnot (data$parameters$nbParameters > 0)
  stopifnot (length(data$parameters$names) == data$parameters$nbParameters)
  
  instance.idx <- data$race.instances[task];
  instance <- data$config$instances[[instance.idx]]
  extra.params <- NULL
  if (!is.null (data$config$instances.extra.params)
      && !is.na (data$config$instances.extra.params[[instance.idx]]))
    extra.params <- data$config$instances.extra.params[[instance.idx]]

  ## FIXME: This is testing if this is the first candidate. If so, run
  ## and evaluate all candidates. Otherwise, just print the output for
  ## the corresponding candidate.  This is an awful historical
  ## artifact because of the way the first ifrace was developed on top
  ## of race.
  if (candidate == which.alive[1]) {
    candidates <- vector("list", length(which.alive))
    for (k in seq_along(which.alive)) {
      candi <- which.alive[k]
      values <- data$parameters$names
      cnd <- data$candidates[candi, , drop = FALSE]
      for (i in seq_len (data$parameters$nbParameters)) {
        name <- data$parameters$names[[i]]
        value <- cnd[[name]]
        type <- data$parameters$types[[name]]
        # FIXME: This should not be necessary if data$candidates
        # always has correctly rounded values.
        if (!is.na(value) && type == "r") {
          value <- round(value, digits)
        }
        values[[name]] <- cnd[[name]]
      }
      candidates[[k]] <- list(index = candi, values = values,
                              # Command-line switches
                              labels = data$parameters$switches)
    }

    # Execute commands
    .irace$hook.output <- vector("list", length(which.alive))
    cwd <- setwd (execDir)
    if (parallel > 1) {
      if (mpi) {
        mpiInit(parallel)
        .irace$hook.output <-
          Rmpi::mpi.applyLB(candidates, .irace$hook.run,
                            instance = instance, extra.params = extra.params,
                            config = data$config)
      } else {
        library(multicore, quietly = TRUE)
        .irace$hook.output <-
          mclapply(candidates, .irace$hook.run, mc.cores = parallel,
                   instance = instance, extra.params = extra.params,
                   config = data$config)
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
    setwd (cwd)

    # hook.evaluate may be NULL. If so, .irace$hook.output must
    # contain the right output already.
    if (!is.null(.irace$hook.evaluate)) {
      ## Evaluate candidates sequentially
      for (k in seq_along(candidates)) {
        .irace$hook.output[[k]] <-
          .irace$hook.evaluate(candidate = candidates[[k]], length(candidates),
                               instance = instance, extra.params = extra.params,
                               config = data$config, .irace$hook.output[[k]])
      }
    }
  }

  output <- .irace$hook.output[[match(candidate, which.alive)]]
  check.output(output, command = if (is.null(.irace$hook.evaluate)) "hookRun" else "hookEvaluate",
               config = data$config)
  return(output)
}

race.describe <- function(candidate, data)
{
  return (data$candidates[candidate, ])
}
