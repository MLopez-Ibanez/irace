#' Execute [irace()] multiple times with the same or different scenarios and parameter space definitions.
#'
#' There are three modes of operation:
#' \itemize{
#'  \item One `scenarios` and `k` `parameters`: `k` runs with the same scenario and each parameter space definition.
#'  \item One `parameters` and `k` `scenarios`: `k` runs with the same parameter space definition and each scenario.
#'  \item `k` `parameters` and `k` scenarios: `k` runs with each scenario and parameter space definition.
#' }
#' Each of the `k` runs can be repeated `n` times by supplying a value for `n`.
#'
#' @param scenarios (`list()`) \cr A list of scenarios.
#' If only a single scenario is supplied, it is used for all parameters.
#' @param parameters (`list()`) \cr A list of parameter space definitions.
#' If only a single definition is supplied, it is used for all scenarios.
#' @param n (`integer(1)`) \cr The number of repetitions.
#' @param parallel (`integer(1)`) \cr The number of workers to use.
#' A value of `1` means sequential execution. Note that `parallel > 1` is not supported on Windows.
#' @param split_output (`logical(1)`) \cr If `TRUE`, the output of [irace()] is written to `{execDir}/run_{i}/irace.out`
#'  instead of the standard output.
#' @param global_seed (`integer(1)`) \cr The global seed used to seed the individual runs.
#'
#' @return A list of the outputs of [irace()].
#'
#' @seealso
#' \describe{
#'  \item{[irace()]}{the main interface for single irace runs.}
#' }
#'
#' @concept running
#' @export
multi_irace <- function(scenarios, parameters, n = 1L, parallel = 1, split_output = parallel > 1, global_seed = NULL)
{
  # Parallel execution is not available on Windows.
  if (.Platform$OS.type == 'windows') {
    irace.assert(parallel == 1L)
  }

  # Allow either the same number of scenarios and parameters, or a single scenario or parameter space definition.
  if (length(scenarios) != length(parameters)) {
    if (length(scenarios) == 1L) {
      scenarios <- rep(scenarios, each = length(parameters))
    } else if (length(parameters) == 1L) {
      parameters <- rep(parameters, each = length(scenarios))
    } else {
      irace.error("Invalid arguments: ",
                  "Cannot execute 'irace' with", length(scenarios),
                  "scenarios and", length(parameters), "parameters.",
                  "Either supply the same number of scenarios and parameters,
                  or one scenario or one parameter space definition.")
    }
  }

  # Repeat the existing scenarios and parameters 'n' times.
  if (n > 1L) {
    scenarios <- rep(scenarios, each = n)
    parameters <- rep(parameters, each = n)
  }

  # Overwrite scenario seeds.
  seeds <- gen_random_seeds(length(scenarios), global_seed = global_seed)

  for (i in seq_along(scenarios)) {
    # FIXME: We should store this seed in state not in scenario. We should not modify scenario.
    scenarios[[i]]$seed <- seeds[[i]]
    scenarios[[i]]$parameters <- parameters[[i]]
    # Check each scenario.
    scenarios[[i]] <- checkScenario(scenarios[[i]])

    # Modify 'logFile' and 'execDir' with the index of the run.
    # Paths are guaranteed to be absolute because of 'checkScenario'.
    logFile_old <- scenarios[[i]]$logFile
    execDir_old <- scenarios[[i]]$execDir

    # 'path/to/execDir' -> 'path/to/execDir/run_{i}'.
    execDir <- file.path(execDir_old, sprintf("run_%02d", i))
    scenarios[[i]]$execDir <- execDir
    fs::dir_create(execDir)

    if (nzchar(logFile_old)) {
      logFile <- if (is.sub.path(logFile_old, execDir_old)) {
        # 'logFile' is located in the old 'execDir', so move it into the new 'execDir'.
        # 'path/to/execDir/logFile.rdata' -> 'path/to/execDir/run_{i}/logFile.rdata'.
        sub(execDir_old, execDir, logFile_old)
      } else {
        # 'logFile' is located outside, so adapt the file name.
        # 'path/to/logFile.rdata' -> 'path/to/logFile_{i}.rdata'.
        # pathWithoutExt <- tools::file_path_sans_ext(logFile_old)
        # pathExt <- tools::file_ext(logFile_old)
        # pathWithoutExtWithIndex <- sprintf("%s_%02d", pathWithoutExt, i)
        # paste(pathWithoutExtWithIndex, pathExt, sep = ".")
        irace.error("Invalid 'logFile' path (", logFile_old, "): ",
          "The 'logFile' must be located inside the 'execDir' (", execDir_old, ").")
      }
      scenarios[[i]]$logFile <- logFile
    }
  }


  irace_run <- function(scenario) {
    if (scenario$quiet || !split_output) {
      irace(scenario)
    } else {
      withr::with_output_sink(file.path(scenario$execDir, "irace.out"), {
        irace(scenario)
      })
    }
  }

  if (parallel > 1L) {
    runs <- parallel::mcmapply(irace_run, scenarios, mc.cores = parallel, SIMPLIFY = FALSE)
    # FIXME: if stop() is called from mcmapply, it does not
    # terminate the execution of the parent process, so it will
    # continue and give more errors later. We have to terminate
    # here, but is there a nicer way to detect this and terminate?
    if (any(sapply(runs, inherits, "try-error")) || any(sapply(runs, is.null))) {
      # FIXME: mcmapply has some bugs in case of error. In that
      # case, each element of the list does not keep the output of
      # each configuration and repetitions may occur.
      errors <- unique(unlist(runs[sapply(runs, inherits, "try-error")]))
      cat(errors, file = stderr())
      irace.error("A child process triggered a fatal error")
    }
  } else {
    runs <- mapply(irace_run, scenarios, SIMPLIFY = FALSE)
  }
  runs
}
