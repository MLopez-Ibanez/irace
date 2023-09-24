#' `multi_irace` executes [irace()] multiple times with the same or different scenarios and parameter space definitions.
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
multi_irace <- function(scenarios, parameters, n = 1L, global_seed = NULL)
{
  # Check each scenario.
  scenarios <- lapply(scenarios, checkScenario)

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

  for (i in seq_along(scenarios)) {
    # Modify 'logFile' and 'execDir' with the index of the run.
    # Paths are guaranteed to be absolute because of 'checkScenario'.
    logFile_old <- scenarios[[i]]$logFile
    execDir_old <- scenarios[[i]]$execDir

    # 'path/to/execDir' -> 'path/to/execDir/run_{i}'.
    execDir <- file.path(execDir_old, sprintf("run_%02d", i))
    scenarios[[i]]$execDir <- execDir
    if (!dir.exists(execDir)) dir.create(execDir, recursive = TRUE)

    if (logFile_old != '') {
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
        irace.error("Invalid 'logFile' path: ", "The 'logFile' must be located inside the 'execDir'.")
      }
      scenarios[[i]]$logFile <- logFile
    }
  }

  # Overwrite scenario seeds.
  seeds <- gen_random_seeds(length(scenarios), global_seed = global_seed)
  for (i in seq_along(scenarios)) {
    # FIXME: We should store this seed in state not in scenario. We should not modify scenario.
    scenarios[[i]]$seed <- abs(seeds[[i]])
  }

  mapply(irace, scenarios, parameters, SIMPLIFY = FALSE)
}
