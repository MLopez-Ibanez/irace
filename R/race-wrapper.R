# FIXME: This is needed because race.R is not divided in two-stages
# run/evaluate like irace is, so there is no way to communicate data
# from the first stage to the second.
#
# FIXME: In fact, we should use this trick also in irace.R to avoid
# pass-by-copy-on-write of huge matrices and data.frames and instead
# pass-by-reference an environment containing those.
.irace <- new.env(parent = emptyenv())

#' Generate a command-line representation of a configuration
#'
#' @description `buildCommandLine` receives two vectors, one containing
#'   the values of the parameters, the other containing the switches of the
#'   parameters. It builds a string with the switches and the values that can
#'   be used as a command line to call the program to be tuned, thus generating
#'   one candidate configuration.
#'   
#' 
#' @param values A vector containing the value of each parameter for the
#' candidate configuration.
#' @param switches A vector containing the switches of each paramter (in an
#'  order that corresponds to the values vector).
#' 
#' @return A string concatenating each element of `switches` and
#'   `values` for all parameters with a space between each pair of
#'   parameters (but none between the switches and the corresponding values).
#'
#' @examples
#' switches <- c("--switch1 ", "--switch2-", "--switch3=")
#' values <- list("value_1", 1L, sqrt(2))
#' buildCommandLine (values, switches)
#' ## Build a command-line from the results produced by a previous run of irace.
#' # First, load the data produced by irace.
#' irace.logfile <- file.path(system.file(package="irace"),
#'                            "exdata", "irace-acotsp.Rdata")
#' load(irace.logfile)
#' allConfigurations <- iraceResults$allConfigurations
#' parameters <- iraceResults$parameters
#' apply(allConfigurations[1:10, unlist(parameters$names)], 1, buildCommandLine,
#'       unlist(parameters$switches))
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
buildCommandLine <- function(values, switches)
{
  irace.assert(length(values) == length(switches))
  values <- as.list(values)
  switches <- switches[!is.na(values)]
  values <- values[!is.na(values)]
  values <- format.default(values, digits=15, scientific=FALSE)
  paste0(switches, values, collapse=" ")
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
    outputRaw <- paste0(outputRaw, collapse = "\n")
    output <- strsplit(trim(outputRaw), "[[:space:]]+")[[1]]
  }
  # suppressWarnings to avoid messages about NAs introduced by coercion
  suppressWarnings (as.numeric (output))
}

target.error <- function(err.msg, output, scenario, target.runner.call,
                         target.evaluator.call = NULL)
{
  if (!is.null(target.evaluator.call)) {
    err.msg <- paste0(err.msg, "\n", .irace_msg_prefix,
                      "The call to targetEvaluator was:\n", target.evaluator.call)
  }
  if (!is.null(target.runner.call)) {
    err.msg <- paste0(err.msg, "\n", .irace_msg_prefix,
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
      scenario$execDir, "' to investigate the issue. See also Appendix B (targetRunner troubleshooting checklist) of the User Guide (https://cran.r-project.org/package=irace/vignettes/irace-package.pdf).")
  }
  irace.error(err.msg, "\n", .irace_msg_prefix,
              "The output was:\n", paste(output$outputRaw, collapse = "\n"),
              "\n", .irace_msg_prefix, advice.txt)
}

check_output_target_evaluator <- function (output, scenario, target.runner.call = NULL, bound = NULL)
{
  if (!is.list(output)) {
    output <- list()
    target.error ("The output of targetEvaluator must be a list",
      output, scenario, target.runner.call = target.runner.call)
    return(output)
  }

  err.msg <- output$error
  if (is.null(err.msg)) {
    if (is.null(output$cost)) {
      err.msg <- "The output of targetEvaluator must contain 'cost'!"
    } else if (is.na.nowarn(output$cost)) {
      err.msg <- "The output of targetEvaluator is not numeric!"
    }
    if (scenario$batchmode != 0 && scenario$maxTime > 0) {
      if (is.null (output$time)) {
        err.msg <- "When batchmode != 0 and maxTime > 0, the output of targetEvaluator must be two numbers 'cost time'!"
      }
    }
    if (is.null(output$time)) {
      output$time <- NA
    } else {
      if (is.na.nowarn(output$time)) {
        err.msg <- "The time returned by targetEvaluator is not numeric!"
      } else if (is.infinite(output$time)) {
        err.msg <- "The time returned by targetEvaluator is not finite!"
      } else if (output$time <= 0) {
        err.msg <- paste0("The value of time (", output$time, ") returned by targetEvaluator must be strictly positive!")
      } else {
        # Fix time.
        output$time <- max(output$time, scenario$minMeasurableTime)
        if (!is.null(bound) && !is.na(bound) && bound > 0 && bound + scenario$minMeasurableTime < output$time) {
          err.msg <- paste0("The time returned by targetEvaluator (", output$time, ") does not respect the given bound of ", bound, "!")
        }
      }
    }
  }

  if (!is.null(err.msg)) {
    target.error (err.msg, output, scenario,
                  target.runner.call = target.runner.call,
                  target.evaluator.call = output$call)
  }
  output
}

exec.target.evaluator <- function (experiment, num.configurations, all.conf.id,
                                   scenario, target_evaluator, target.runner.call)
{
  output <- target_evaluator(experiment, num.configurations, all.conf.id,
                             scenario, target.runner.call)
  check_output_target_evaluator(output, scenario, target.runner.call = target.runner.call, bound = experiment$bound)
}

#' target.evaluator.default
#'
#' `target.evaluator.default` is the default `targetEvaluator` function that is
#'  invoked if `targetEvaluator` is a string (by default
#'  `targetEvaluator` is `NULL` and this function is not invoked). You can use it as
#'  an advanced example of how to create your own `targetEvaluator` function.
#'   
#' @param experiment A list describing the experiment. It contains at least:
#'    \describe{
#'     \item{`id.configuration`}{An alphanumeric string that uniquely identifies a configuration;}
#'     \item{`id.instance`}{An alphanumeric string that uniquely identifies an instance;}
#'      \item{`seed`}{Seed for the random number generator to be used for
#'        this evaluation, ignore the seed for deterministic algorithms;}
#'      \item{`instance`}{String giving the instance to be used for this evaluation;}
#'      \item{`bound`}{(only when `capping` is enabled) Time bound for the execution;}
#'      \item{`configuration`}{1-row data frame with a column per parameter
#'        name;}
#'      \item{`switches`}{Vector of parameter switches (labels) in the order
#'        of parameters used in `configuration`.}
#'    }
#' @param num.configurations Number of  configurations alive in the race.
#' @param all.conf.id Vector of configuration IDs of the alive configurations.
#' @template arg_scenario
#' @param target.runner.call String describing the call to `targetRunner` that
#'    corresponds to this call to `targetEvaluator`. This is used for
#'    providing extra information to the user, for example, in case
#'    `targetEvaluator` fails.
#' 
#' @return The function `targetEvaluator` must return a list with one element
#'  `"cost"`, the numerical value corresponding to the cost measure of the
#'  given configuration on the given instance.
#'    
#'  The return list may also contain the following optional elements that are used
#'  by \pkg{irace} for reporting errors in `targetEvaluator`:
#'  \describe{
#'    \item{`error`}{is a string used to report an error;}
#'    \item{`outputRaw`}{is a string used to report the raw output of calls to
#'      an external program or function;}
#'    \item{`call`}{is a string used to report how `targetRunner` called 
#'      an external program or function.}
#'  }
#'
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
target.evaluator.default <- function(experiment, num.configurations, all.conf.id,
                                     scenario, target.runner.call)
{
  configuration.id <- experiment$id.configuration
  instance.id      <- experiment$id.instance
  seed             <- experiment$seed
  instance         <- experiment$instance

  debugLevel <- scenario$debugLevel
  targetEvaluator <- scenario$targetEvaluator
  if (as.logical(file.access(targetEvaluator, mode = 1))) {
    irace.error ("targetEvaluator", shQuote(targetEvaluator),
                 "cannot be found or is not executable!\n")
  }
  all.conf.id <- paste0(all.conf.id, collapse = " ")
  args <- c(configuration.id, instance.id, seed, instance, num.configurations, all.conf.id)
  withr::with_dir(scenario$execDir, {
    output <- runcommand(targetEvaluator, args, configuration.id, debugLevel, timeout = scenario$targetRunnerTimeout)
  })

  cost <- time <- NULL
  err.msg <- output$error
  if (is.null(err.msg)) {
    v.output <- parse.output(output$output, verbose = (scenario$debugLevel >= 2))
    if (length(v.output) > 2) {
      err.msg <- paste0("The output of targetEvaluator should not be more than two numbers!")
    } else if (length(v.output) == 0) {
      err.msg <- paste0("The output of targetEvaluator must be at least one number 'cost'!")
    } else if (length(v.output) == 1) {
      cost <- v.output[1]
    } else if (length(v.output) == 2) {
      cost <- v.output[1]
      time <- v.output[2]
    }
  }
  list(cost = cost, time = time,
    error = err.msg, outputRaw = output$output,
    call = paste(targetEvaluator, args, collapse=" "))
}

#' Check the output of the target runner and repair it if possible. If the 
#' output is incorrect, this function will throw an error.
#' 
#' @param output The output from target runner.
#' @template arg_scenario
#' @param bound Optional time bound that the target runner should have respected.
#' 
#' @return The output with its contents repaired.
#' 
#' @export
check_output_target_runner <- function(output, scenario, bound = NULL)
{
  if (!is.list(output)) {
    output <- list()
    target.error ("The output of targetRunner must be a list", output, scenario, target.runner.call = NULL)
    return(output)
  }
  
  err.msg <- output$error
  if (is.null(err.msg)) {
    if (!is.null (output$cost)) {
      if (is.na.or.empty(output$cost)) {
        err.msg <- "The cost returned by targetRunner is not numeric!"
      }
    }

    if (is.null(output$time)) {
      output$time <- NA
    } else {
      if (is.na(output$time)) {
        err.msg <- paste0("The time returned by targetRunner is not numeric!")
      } else if (is.infinite(output$time)) {
        err.msg <- paste0("The time returned by targetRunner is not finite!")
      } else if (output$time <= 0) {
        err.msg <- paste0("The value of time (", output$time, ") returned by targetRunner must be strictly positive!")
      } else {
        # Fix time.
        output$time <- max(output$time, scenario$minMeasurableTime)
        if (!is.null(bound) && !is.na(bound) && bound > 0 && bound + scenario$minMeasurableTime < output$time) {
          err.msg <- paste0("The time returned by targetRunner (", output$time, ") does not respect the given bound of ", bound, "!")
        }
      }
    }
    if (is.null(err.msg)) {
      # When targetEvaluator is provided, targetRunner must return only the time.
      if (!is.null(scenario$targetEvaluator)) {
        # unless using batchmode, in that case targetRunner returns neither the
        # time nor the cost.
        if (scenario$batchmode != 0) {
          if (!is.na(output$time) || !is.null(output$cost)) {
            err.msg <- "When batchmode != 0, the output of targetRunner must not contain a cost nor a time!"
          }
        } else if (scenario$maxTime > 0 && is.na(output$time)) {
          err.msg <- "The output of targetRunner must be one number 'time'!"
        } else if (!is.null(output$cost)) {
          err.msg <- "The output of targetRunner must be empty or just one number 'time'!"
        }
      } else if (scenario$maxTime > 0 && (is.null(output$cost) || is.na(output$time))) {
        err.msg <- "The output of targetRunner must be two numbers 'cost time'!"
      } else if (scenario$maxExperiments > 0 && is.null(output$cost)) {
        err.msg <- "The output of targetRunner must be one number 'cost'!"
      } 
    }
  }

  if (!is.null(err.msg)) {
    target.error (err.msg, output, scenario, target.runner.call = output$call)
  }
  output
}

# This function invokes target.runner.  When used on a remote node by Rmpi,
# environments do not seem to be shared and the default value is evaluated too
# late, thus we have to pass .irace$target.runner explicitly.
exec.target.runner <- function(experiment, scenario, target.runner)
{
  doit <- function(experiment, scenario) {
    x <- target.runner(experiment, scenario)
    check_output_target_runner(x, scenario, bound = experiment$bound)
  }
  
  retries <- scenario$targetRunnerRetries
  while (retries > 0L) {
    output <- try (doit(experiment, scenario))
    if (!inherits(output, "try-error") && is.null(output$error))
      return (output)
    irace.note("Retrying (", retries, " left).\n")
    retries <- retries - 1L
  }
  doit(experiment, scenario)
}

parse.aclib.output <- function(outputRaw)
{
  outputRaw <- paste0(outputRaw, collapse = "\n")
  text <- regmatches(outputRaw,
                     regexec("Result of this algorithm run:\\s*\\{(.+)\\}\\s*\n",
                             outputRaw))[[1]][2]
  aclib.match <- function(text, key, value) {
    pattern <- paste0('"', key, '":\\s*', value)
    return(regmatches(text, regexec(pattern, text))[[1]][2])
  }
  cost <- runtime <- error <- NULL
  # AClib wrappers print:
  # Result of this algorithm run:  {"status": "SUCCESS", "cost": cost, "runtime": time }
  # FIXME: This is not very robust. If we are going to be using jsonlite, then we can simply do:
  # jsonlite::fromJSON('{\"misc\": \"\", \"runtime\": 164.14, \"status\": \"SUCCESS\", \"cost\": \"0.121340\"}')
  status <- aclib.match(text, "status", '"([^"]+)"')
  if (!is.character(status)) {
    error <- paste0("Not valid AClib output")
  } else if (status %in% c("SUCCESS", "TIMEOUT")) {
    cost <- aclib.match(text, "cost", "([^[:space:],}]+)")
    cost <- suppressWarnings(as.numeric(cost))
    runtime <- aclib.match(text, "runtime", "([^[:space:],}]+)")
    runtime <- suppressWarnings(as.numeric(runtime))
    if (is.null.or.na(cost) && is.null.or.na(runtime))
      error <- paste0("Not valid cost or runtime in AClib output")
  } else if (status %in% c("CRASHED", "ABORT")) {
    # FIXME: Implement ABORT semantics of fatal error
    error <- paste0("targetRunner returned status (", status, ")")
  } else {
    error <- paste0("Not valid AClib output status (", status, ")")
  }
  return(list(status = status, cost = cost, time = runtime, error = error))
}

target.runner.aclib <- function(experiment, scenario)
{
  debugLevel   <- scenario$debugLevel
  res <- run_target_runner(experiment, scenario)
  cmd <- res$cmd
  output <- res$output
  args <- res$args
  
  err.msg <- output$error
  if (is.null(err.msg)) {
    return(c(parse.aclib.output(output$output),
             list(outputRaw = output$output, call = paste(cmd, args))))
  }
  
  list(cost = NULL, time = NULL, error = err.msg,
       outputRaw = output$output, call = paste(cmd, args))
}

check_target_cmdline <- function(target_cmdline, launcher, capping)
{
  required <- c("seed", "instance", "targetRunnerArgs")
  if (capping) required <- c(required, "bound")
  for (x in required) {
    if (!grepl(paste0("{", x, "}"), target_cmdline, fixed=TRUE))
      irace.error("targetCmdline '", target_cmdline, "' must contain '{", x, "}'")
  }
}

expand_target_cmdline <- function(target_cmdline, experiment, targetRunner, targetRunnerArgs)
{
  vars <- list(configurationID = experiment$id.configuration,
               instanceID      = experiment$id.instance,
               seed            = experiment$seed,
               instance        = experiment$instance,
               bound           = experiment$bound,
               targetRunner = targetRunner,
               targetRunnerArgs = targetRunnerArgs)
  for (x in names(vars)) {
    value <- vars[[x]]
    if (is.null(value)) value <- ""
    if (x == "targetRunner") value <- shQuote(value)
    target_cmdline <- gsub(paste0("{", x, "}"), value, target_cmdline, fixed=TRUE)
  }
  target_cmdline
}
  
run_target_runner <- function(experiment, scenario)
{
  configuration.id <- experiment$id.configuration
  instance.id      <- experiment$id.instance
  seed             <- experiment$seed
  configuration    <- experiment$configuration
  instance         <- experiment$instance
  switches         <- experiment$switches
  bound            <- experiment$bound

  targetRunner <- scenario[["targetRunner"]]
  debugLevel <- scenario$debugLevel
  
  if (scenario$aclib) {
    # FIXME: Use targetCmdline for this
    has_value <- !is.na(configuration)
    # <executable> [<arg>] [<arg>] ... [--cutoff <cutoff time>] [--instance <instance name>] 
    # [--seed <seed>] --config [-param_name_1 value_1] [-param_name_2 value_2] ...
    args <- paste("--instance", instance, "--seed", seed, "--config",
                  paste0("-", switches[has_value], " ", configuration[has_value],
                         collapse = " "))
    if (!is.null.or.na(bound))
      args <- paste("--cutoff", bound, args)
  } else {
    args <- expand_target_cmdline(scenario$targetCmdline, experiment, targetRunner,
                                  targetRunnerArgs=buildCommandLine(configuration, switches))
  }
  targetRunnerLauncher <- scenario$targetRunnerLauncher
  if (!is.null.or.empty(targetRunnerLauncher))
    targetRunner <- targetRunnerLauncher
  
  output <- runcommand(targetRunner, args, configuration.id, debugLevel, timeout = scenario$targetRunnerTimeout)
  list(cmd=targetRunner, output=output, args=args)
}

#' Default `targetRunner` function.
#'
#' Use it as an advanced example of how to create your own `targetRunner` function.
#' 
#' @param experiment A list describing the experiment. It contains at least:
#'    \describe{
#'     \item{`id.configuration`}{An alphanumeric string that uniquely identifies a configuration;}
#'     \item{`id.instance`}{An alphanumeric string that uniquely identifies an instance;}
#'      \item{`seed`}{Seed for the random number generator to be used for
#'        this evaluation, ignore the seed for deterministic algorithms;}
#'      \item{`instance`}{String giving the instance to be used for this evaluation;}
#'      \item{`bound`}{(only when `capping` is enabled) Time bound for the execution;}
#'      \item{`configuration`}{1-row data frame with a column per parameter
#'        name;}
#'      \item{`switches`}{Vector of parameter switches (labels) in the order
#'        of parameters used in `configuration`.}
#'    }
#' @template arg_scenario
#' 
#' @return If `targetEvaluator` is `NULL`, then the `targetRunner`
#'  function must return a list with at least one element `"cost"`,
#'  the numerical value corresponding to the evaluation of the given
#'  configuration on the given instance.
#'    
#'  If the scenario option `maxTime` is non-zero or if `capping` is enabled 
#'  then the list must contain at least another element `"time"` that reports the
#'  execution time for this call to `targetRunner`.
#'  The return list may also contain the following optional elements that are used
#'  by \pkg{irace} for reporting errors in `targetRunner`:
#'  \describe{
#'    \item{`error`}{is a string used to report an error;}
#'    \item{`outputRaw`}{is a string used to report the raw output of calls to
#'      an external program or function;}
#'    \item{`call`}{is a string used to report how `targetRunner` called 
#'      an external program or function.}
#'  }
#'
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
target.runner.default <- function(experiment, scenario)
{
  res <- run_target_runner(experiment, scenario)
  cmd <- res$cmd
  output <- res$output
  args <- res$args
  
  debugLevel <- scenario$debugLevel
  cost <- time <- NULL
  err.msg <- output$error
  if (is.null(err.msg)) {
    v.output <- parse.output(output$output, verbose = (debugLevel >= 2))
    if (length(v.output) > 2) {
      err.msg <- "The output of targetRunner should not be more than two numbers!"
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
  list(cost = cost, time = time,
       error = err.msg, outputRaw = output$output,
       call = paste(cmd, args, collapse = " "))
}

execute.experiments <- function(experiments, scenario)
{
  parallel <- scenario$parallel
  mpi <- scenario$mpi
  target_runner <- .irace$target.runner
  execDir <- scenario$execDir
  if (!fs::dir_exists(execDir))
    irace.error ("Execution directory '", execDir, "' is not found or not a directory\n")
  withr::local_dir(execDir)
   
  if (!is.null(scenario$targetRunnerParallel)) {
    # FIXME: We should remove the exec.target.runner parameter from
    # targetRunnerParallel and do lapply(target.output,
    # check_output_target_runner, scenario=scenario) after it to make sure the output is valid.    
    # User-defined parallelization
    target.output <-
      scenario$targetRunnerParallel(experiments, exec.target.runner,
                                    scenario = scenario,
                                    target.runner = target_runner)
    if (length(target.output) != length(experiments)) {
      irace.error("Stopping because the output of targetRunnerParallel is missing elements. The output was:\n",
        paste0(capture.output(str(target.output)), collapse="\n"))
    }
  } else if (scenario$batchmode != 0L) {
    target.output <- cluster.lapply (experiments, scenario = scenario)
  } else if (parallel > 1) {
    if (mpi) {
      if (scenario$loadBalancing) {
        target.output <- Rmpi::mpi.applyLB(experiments, exec.target.runner,
                                           scenario = scenario,
                                           target.runner = target_runner)
      } else {
        # Without load-balancing, we need to split the experiments into chunks
        # of size parallel.
        target.output <- unlist(use.names = FALSE,
                                tapply(experiments,
                                       ceiling(seq_along(experiments) / parallel),
                                       Rmpi::mpi.apply, exec.target.runner,
                                       scenario = scenario,
                                       target.runner = target_runner))
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
                                  scenario = scenario,
                                  target.runner = target_runner)
        } else {
          target.output <-
            parallel::parLapply(.irace$cluster, experiments, exec.target.runner,
                                scenario = scenario,
                                target.runner = target_runner)
        }
        # FIXME: if stop() is called from parLapply, then the parent
        # process also terminates, and we cannot give further errors.
      } else {
        target.output <-
          parallel::mclapply(experiments, exec.target.runner,
                             # FALSE means load-balancing.
                             mc.preschedule = !scenario$loadBalancing,
                             mc.cores = parallel,
                             scenario = scenario,
                             target.runner = target_runner)
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
    target.output <- lapply(experiments, exec.target.runner,
                            scenario = scenario,
                            target.runner = target_runner)
  }
  target.output
}

execute.evaluator <- function(experiments, scenario, target.output, configurations.id)
{
  ## FIXME: We do not need the configurations.id argument:
  irace.assert(isTRUE(all.equal(configurations.id,
                                sapply(experiments, getElement, "id.configuration"))))
  nconfs <- length(configurations.id)
  ## Evaluate configurations sequentially
  for (k in seq_along(experiments)) {
    output <- exec.target.evaluator(experiment = experiments[[k]],
                                    num.configurations = nconfs,
                                    configurations.id, scenario = scenario,
                                    target_evaluator = .irace$target.evaluator,
                                    target.runner.call = target.output[[k]]$call)
    target.output[[k]]$cost <- output$cost
    if (is.null(target.output[[k]]$call))
      target.output[[k]]$call <- output$call
    if (is.null(target.output[[k]]$time) || !is.null.or.na(output$time))
      target.output[[k]]$time <- output$time
  }
  target.output
}
