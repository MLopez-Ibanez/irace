#' Summarise the results of a run of irace
#' 
#' @inheritParams has_testing_data
#'
#' @return `list()`
#' 
#' @examples
#' irace_results <- read_logfile(system.file("exdata/irace-acotsp.Rdata",
#'                                           package="irace", mustWork=TRUE))
#' irace_summarise(irace_results)
#' 
#' @author Manuel López-Ibáñez
#' @concept analysis
#' @export
irace_summarise <- function(iraceResults)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)

  if (is.null(iraceResults$state$elapsed)) {
    time_cpu_user <- time_cpu_sys <- time_cpu_total <- time_wallclock <- NA
  } else {
    time_cpu_user <- iraceResults$state$elapsed[["user"]]
    time_cpu_sys <- iraceResults$state$elapsed[["system"]]
    time_cpu_total <- time_cpu_user + time_cpu_sys
    time_wallclock <- iraceResults$state$elapsed[["wallclock"]]
  }

  n_iterations <- length(iraceResults$allElites)
  experiment_log <- iraceResults$state$experiment_log
  if (is.null(experiment_log)) {
    experiment_log <- iraceResults$experimentLog
  } else if (is.null(experiment_log))
    stop("Experiment log is NULL")
  
  version <- iraceResults$irace_version
  if (is.null(version))
    version <- iraceResults$irace.version

  time_targetrunner <- iraceResults$state$recovery_info$timeUsed
  if (is.null(time_targetrunner))
    time_targetrunner <- iraceResults$state$timeUsed

  rejected_ids <- iraceResults$state$rejected_ids
  if (is.null(rejected_ids))
    rejected_ids <- iraceResults$state$rejectedIDs
  
  list(
    version = version,
    n_iterations = n_iterations,
    n_configurations = nrow(iraceResults$allConfigurations),
    n_initial_configurations = if (is.null(iraceResults$scenario$initConfigurations)) 0L else nrow(iraceResults$scenario$initConfigurations),
    n_instances = nrow(iraceResults$experiments),
    n_experiments = nrow(experiment_log),
    n_elites = length(iraceResults$allElites[[n_iterations]]),
    n_soft_restarts = sum(iraceResults$softRestart),
    n_rejected = length(rejected_ids),
    time_targetrunner = time_targetrunner,
    time_cpu_user = time_cpu_user,
    time_cpu_sys = time_cpu_sys,
    time_cpu_total = time_cpu_total,
    time_wallclock = time_wallclock,
    termination_reason = if (is.null(iraceResults$state$completed)) "Missing" else iraceResults$state$completed
  )
}
