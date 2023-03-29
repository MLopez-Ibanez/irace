#' Summarise the results of a run of irace
#' 
#' @template arg_iraceresults
#'
#' @return `list()`
#' 
#' @examples
#' irace_results <- read_logfile(system.file("exdata/irace-acotsp.Rdata", package="irace",
#'                                           mustWork=TRUE))
#  irace_summarise(irace_results)
#' 
#' @author Manuel López-Ibáñez
#' @concept analysis
#' @export
irace_summarise <- function(iraceResults)
{
  niterations <- length(iraceResults$allElites)

  time_cpu_user <- time_cpu_sys <- time_cpu_total <- time_wallclock <- NA
  if (!is.null(iraceResults$state$elapsed)) {
    time_cpu_user <- iraceResults$state$elapsed["user"]
    time_cpu_sys <- iraceResults$state$elapsed["system"]
    time_cpu_total <- time_cpu_user + time_cpu_sys
    time_wallclock <- iraceResults$state$elapsed["wallclock"]
  }
  
  list(
    version = iraceResults$irace.version,
    n_iterations = niterations,
    n_configurations = nrow(iraceResults$allConfigurations),
    n_instances = nrow(iraceResults$experiments),
    n_experiments = nrow(iraceResults$experimentLog),
    n_elites = length(iraceResults$allElites[[niterations]]),
    n_soft_restarts = sum(iraceResults$softRestart),
    n_rejected = length(iraceResults$state$rejectedIDs),
    time_cpu_user = time_cpu_user,
    time_cpu_sys = time_cpu_sys,
    time_cpu_total = time_cpu_total,
    time_wallclock = time_wallclock,
    termination_reason = if (is.null(iraceResults$state$completed)) "Missing" else iraceResults$state$completed
  )
}
