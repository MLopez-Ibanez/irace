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
      command <- paste0(command, " ", switches[i], value)
    }
  }
  return(command)
}

# This function tries to convert a, possibly empty, character vector into a
# numeric vector.
parse.output <- function(outputRaw, verbose)
{
  if (verbose) { cat (outputRaw, sep = "\n") }
  
  # Initialize output as raw. If it is empty stays like this.
  output <- outputRaw
  # strsplit crashes if outputRaw == character(0)
  if (length(outputRaw) > 0) {
    output <- strsplit(trim(outputRaw), "[[:space:]]+")[[1]]
  }
  # suppressWarnings to avoid messages about NAs introduced by coercion
  output <- suppressWarnings (as.numeric (output))
  return (output)
}

target.error <- function(err.msg, output, scenario, target.runner.call,
                         target.evaluator.call = NULL)
{
  if (!is.null(target.evaluator.call)) {
    err.msg <- paste0(err.msg, "\n", .irace.prefix,
                      "The call to targetEvaluator was:\n", target.evaluator.call)
  }
  if (!is.null(target.runner.call)) {
    err.msg <- paste0(err.msg, "\n", .irace.prefix,
                      "The call to targetRunner was:\n", target.runner.call)
  }
  if (is.null(output$outputRaw)) {
    # Message for a function call.
    # FIXME: Ideally, we should print the list as R would print it.
    output$outputRaw <- toString(output)
    advice.txt <- paste0(
      "This is not a bug in irace, but means that something failed in ",
      "a call to the targetRunner or targetEvaluator functions provided by the user.",
      " Please check those functions carefully.")
  } else {
    # Message for an external script.
    advice.txt <- paste0(
      "This is not a bug in irace, but means that something failed when",
      " running the command(s) above or they were terminated before completion.",
      " Try to run the command(s) above from the execution directory '",
      scenario$execDir, "' to investigate the issue.")
  }
  irace.error(err.msg, "\n", .irace.prefix,
              "The output was:\n", paste(output$outputRaw, collapse = "\n"),
              "\n", .irace.prefix, advice.txt)
}

check.output.target.evaluator <- function (output, scenario, target.runner.call = NULL)
{
  if (!is.list(output)) {
    target.error ("The output of targetEvaluator must be a list",
                  list(), scenario, target.runner.call = target.runner.call)
    return(NULL)
  }

  err.msg <- output$error
  if (is.null(err.msg)) {
    if (is.null(output$cost)) {
      err.msg <- "The output of targetEvaluator must be one number 'cost'!"
    } else if (is.na (output$cost)) {
      err.msg <- "The output of targetEvaluator is not numeric!"
    } else if (is.infinite(output$cost)) {
      err.msg <- "The output of targetEvaluator is not finite!"
    }
  }

  if (!is.null(err.msg)) {
    target.error (err.msg, output, scenario,
                  target.runner.call = target.runner.call,
                  target.evaluator.call = output$call)
  }
}

exec.target.evaluator <- function (experiment, num.configurations, all.conf.id,
                                   scenario, target.runner.call)
{
  output <- .irace$target.evaluator(experiment, num.configurations, all.conf.id,
                                    scenario, target.runner.call)
  check.output.target.evaluator (output, scenario, target.runner.call = target.runner.call)
  return (output)
}

target.evaluator.default <- function(experiment, num.configurations, all.conf.id,
                                     scenario, target.runner.call)
{
  configuration.id <- experiment$id.configuration
  instance.id      <- experiment$id.instance
  seed             <- experiment$seed
  instance         <- experiment$instance

  execDir <- scenario$execDir
  debugLevel <- scenario$debugLevel
  targetEvaluator <- scenario$targetEvaluator
  if (as.logical(file.access(targetEvaluator, mode = 1))) {
    irace.error ("targetEvaluator", shQuote(targetEvaluator),
                 "cannot be found or is not executable!\n")
  }

  cwd <- setwd (execDir)
  # FIXME: I think we don't even need to paste the args, since system2 handles this by itself.'
  args <- paste(configuration.id, instance.id, seed, instance, num.configurations, all.conf.id)
  output <- runcommand(targetEvaluator, args, configuration.id, debugLevel)
  setwd (cwd)

  cost <- time <- NULL
  err.msg <- output$error
  
  if (is.null(err.msg)) {
    v.output <- parse.output(output$output, verbose = (scenario$debugLevel >= 2))
    if (length(v.output) != 1) {
      err.msg <- paste0("The output of targetEvaluator must be one number 'cost'!")
    } else {
      cost <- v.output[1]
    }
  }
  return(list(cost = cost,
              error = err.msg, outputRaw = output$output,
              call = paste(targetEvaluator, args)))
}

check.output.target.runner <- function (output, scenario)
{
  if (!is.list(output)) {
    output <- list()
    err.msg <- paste0("The output of targetRunner must be a list")
    target.error (err.msg, output, scenario, target.runner.call = NULL)
    return(output)
  }
  
  err.msg <- output$error
  if (is.null(err.msg)) {
    if (is.null.or.na(output$cost))
      output$cost <- NULL
    if (is.null.or.na(output$time))
      output$time <- NULL
    # When targetEvaluator is provided targetRunner must return only the time.
    if (!is.null(.irace$target.evaluator)) {
      if (scenario$maxTime > 0 && is.null(output$time)) {
        err.msg <- paste0("The output of targetRunner must be one number 'time'!")
      } else if (!is.null(output$cost)) {
        err.msg <- paste0("The output of targetRunner must be empty or just one number 'time'!")
      }
    } else if (scenario$maxTime > 0 && (is.null (output$cost) || is.null(output$time))) {
      err.msg <- paste0("The output of targetRunner must be two numbers 'cost time'!")
    } else if (scenario$maxExperiments > 0 && is.null (output$cost)) {
      err.msg <- paste0("The output of targetRunner must be one number 'cost'!")
    } else if (!is.null(output$time) && output$time < 0) {
      err.msg <- paste0("The value of time returned by targetRunner cannot be negative (", output$time, ")!")
    } 

    if (!is.null (output$cost)) {
      if (is.na(output$cost)) {
        err.msg <- paste0("The cost returned by targetRunner is not numeric!")
      } else if (is.infinite(output$cost)) {
        err.msg <- paste0("The cost returned by targetRunner is not finite!")
      }
    }

    if (!is.null (output$time)) {
      if (is.na(output$time)) {
        err.msg <- paste0("The time returned by targetRunner is not numeric!")
      } else if (is.infinite(output$time)) {
        err.msg <- paste0("The time returned by targetRunner is not finite!")
      }
    }

    # Fix too small time.
    output$time <- if (is.null(output$time)) NA else max(output$time, 0.01)
    
  }
  if (!is.null(err.msg)) {
    target.error (err.msg, output, scenario, target.runner.call = output$call)
  }
  return (output)
}

# This function invokes target.runner.  When used on a remote node by Rmpi,
# environments do not seem to be shared and the default value is evaluated too
# late, thus we have to pass .irace$target.runner explicitly.
exec.target.runner <- function(experiment, scenario,
                               target.runner = .irace$target.runner)
{
  doit <- function(experiment, scenario)
  {
    x <- target.runner(experiment, scenario)
    return (check.output.target.runner (x, scenario))
  }
  
  retries <- scenario$targetRunnerRetries
  while (retries > 0) {
    output <- try (doit(experiment, scenario))
    if (!inherits(output, "try-error") && is.null(output$error)) {
      return (output)
    }
    irace.note("Retrying (", retries, " left).\n")
    retries <- retries - 1
  }
  output <- doit(experiment, scenario)
  return (output)
}

target.runner.default <- function(experiment, scenario)
{
  debugLevel       <- scenario$debugLevel
  configuration.id <- experiment$id.configuration
  instance.id      <- experiment$id.instance
  seed             <- experiment$seed
  configuration    <- experiment$configuration
  instance         <- experiment$instance
  switches         <- experiment$switches
  
  targetRunner <- scenario$targetRunner
  if (as.logical(file.access(targetRunner, mode = 1))) {
    irace.error ("targetRunner ", shQuote(targetRunner), " cannot be found or is not executable!\n")
  }

  args <- paste(configuration.id, instance.id, seed, instance,
                buildCommandLine(configuration, switches))
  output <- runcommand(targetRunner, args, configuration.id, debugLevel)

  cost <- time <- NULL
  err.msg <- output$error
  if (is.null(err.msg)) {
    v.output <- parse.output(output$output, verbose = (scenario$debugLevel >= 2))
    if (length(v.output) > 2) {
      err.msg <- paste0("The output of targetRunner should not be more than two numbers!")
    } else if (length(v.output) == 1) {
      if (!is.null(scenario$targetEvaluator)) {
        time <- v.output[1]
      } else {
        cost <- v.output[1]
      }
    } else if (length(v.output) == 2) {
      cost <- v.output[1]
      time <- v.output[2]
    }
  }
  return(list(cost = cost, time = time,
              error = err.msg, outputRaw = output$output,
              call = paste(targetRunner, args)))
}

execute.experiments <- function(experiments, scenario)
{
  parallel <- scenario$parallel
  mpi <- scenario$mpi

  execDir <- scenario$execDir
  if (!isTRUE (file.info(execDir)$isdir)) {
    irace.error ("Execution directory '", execDir, "' is not found or not a directory\n")
  }
  cwd <- setwd (execDir)
  on.exit(setwd(cwd), add = TRUE)
   
  target.output <- vector("list", length(experiments))
  if (!is.null(scenario$targetRunnerParallel)) {
    # User-defined parallelization
    target.output <-
      scenario$targetRunnerParallel(experiments, exec.target.runner, scenario = scenario)
  } else if (scenario$batchmode != 0) {
    target.output <- cluster.lapply (experiments, scenario = scenario)
  } else if (parallel > 1) {
    if (mpi) {
      if (scenario$loadBalancing) {
        target.output <- Rmpi::mpi.applyLB(experiments, exec.target.runner,
                                           scenario = scenario,
                                           target.runner = .irace$target.runner)
      } else {
        # Without load-balancing, we need to split the experiments into chunks
        # of size parallel.
        target.output <- unlist(use.names = FALSE,
                                tapply(experiments,
                                       ceiling(1:length(experiments) / parallel),
                                       Rmpi::mpi.apply, exec.target.runner,
                                       scenario = scenario,
                                       target.runner = .irace$target.runner))
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
            target.output, inherits, "try-error")])), file = stderr(), sep = "")
        irace.error("A slave process terminated with a fatal error")
      }
    } else {
      if (.Platform$OS.type == 'windows') {
        irace.assert(!is.null(.irace$cluster))
        if (scenario$loadBalancing) {
          target.output <-
            parallel::parLapplyLB(.irace$cluster, experiments, exec.target.runner,
                                  scenario = scenario)
        } else {
          target.output <-
            parallel::parLapply(.irace$cluster, experiments, exec.target.runner,
                                scenario = scenario)
        }
        # FIXME: if stop() is called from parLapply, then the parent
        # process also terminates, and we cannot give further errors.
      } else {
        target.output <-
          parallel::mclapply(experiments, exec.target.runner,
                             # FALSE means load-balancing.
                             mc.preschedule = !scenario$loadBalancing,
                             mc.cores = parallel,
                             scenario = scenario)
        # FIXME: if stop() is called from mclapply, it does not
        # terminate the execution of the parent process, so it will
        # continue and give more errors later. We have to terminate
        # here, but is there a nicer way to detect this and terminate?
        if (any(sapply(target.output, inherits, "try-error"))
            || any(sapply(target.output, is.null))) {
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
  } else {
    # One process, all sequential
    for (k in seq_along(experiments)) {
      target.output[[k]] <- exec.target.runner(experiments[[k]], scenario = scenario)
    }
  }
 
  return(target.output)
}

execute.evaluator <- function(experiments, scenario, target.output, configurations.id)
{
  ## FIXME: We do not need the configurations.id argument:
  # configurations.id <- sapply(experiments, function(x) x[["id.configuration"]])
  all.conf.id <- paste(configurations.id, collapse = " ")
  
  ## Evaluate configurations sequentially
  for (k in seq_along(experiments)) {
    output <- exec.target.evaluator(experiment = experiments[[k]],
                                    num.configurations = length(configurations.id),
                                    all.conf.id, scenario = scenario,
                                    target.runner.call = target.output[[k]]$call)
    target.output[[k]]$cost <- output$cost
    if (is.null(target.output[[k]]$call))
      target.output[[k]]$call <- output$call
    if (is.null(target.output[[k]]$time))
      target.output[[k]]$time <- output$time
  }
  return(target.output)
}
