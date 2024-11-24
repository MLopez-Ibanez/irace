#' Postselection race
#'
#' \code{psRace} performs a postselection race of a set of configurations.
#'
#' @inheritParams has_testing_data
#' 
#' @param max_experiments `numeric(1)`\cr Number of experiments for the
#'   post-selection race. If it is equal to or smaller than 1, then it is a
#'   fraction of the total budget given by
#'   `iraceResults$scenario$maxExperiments` or `iraceResults$scenario$maxTime /
#'   iraceResults$state$boundEstimate`.
#' @param conf_ids  IDs of the configurations in iraceResults$allConfigurations to be used for ablation.
#' If NULL, the `iteration_best` argument will be to decide.
#' @param iteration_elites  If TRUE, only select the best configuration of each iteration.
#' If FALSE, select from all elite configurations of all iterations. `max_experiments`
#'
#' @return If iraceLogFile is NULL, it returns a list with the following elements:
#'  \describe{
#'    \item{configurations}{Configurations used in the race.}
#'    \item{instances}{A matrix with the instances used in the experiments. First column has the
#'    instances ids from iraceResults$scenario$instances, second column the seed assigned to the instance.}
#'    \item{maxExperiments}{Maximum number of experiments set for the race.}
#'    \item{experiments}{A matrix with the results of the experiments (columns are configurations, rows are instances).}
#'    \item{elites}{Best configurations found in the experiments.}
#'  }
#' If `iraceLogFile` is provided this list object will be saved in `iraceResults$psrace_log`.
#'
#' @examples
#' \donttest{
#'   logfile <- system.file(package="irace", "exdata", "sann.rda")
#'   # Execute the postselection after the execution of irace. Use 10% of the total budget.
#'   psRace(logfile, max_experiments=0.1)
#' }
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @export
psRace <- function(iraceResults, max_experiments, conf_ids = NULL, iteration_elites = FALSE)
{
  irace.note("Starting post-selection:\n")
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing.")
  iraceResults <- read_logfile(iraceResults)
  scenario   <- iraceResults$scenario
  race_state <- iraceResults$state
  race_state$initialize(scenario, new = FALSE) # Restores the random seed
  if (max_experiments <= 0) stop("'max_experiments' must be positive.")
  if (max_experiments <= 1) {
    budget <- if (scenario$maxTime == 0L)
                scenario$maxExperiments else scenario$maxTime / race_state$boundEstimate
    max_experiments <- as.integer(ceiling(max_experiments * budget))
    if (scenario$maxTime == 0L)
      cat(sep="", "# scenario maxExperiments:", scenario$maxExperiments, "\n")
    else
      cat(sep="", "# scenario maxTime:", scenario$maxTime, "\n")
  }
  # Get selected configurations.
  if (is.null(conf_ids)) {
    # FIXME: Handle scenario$maxTime > 0
    irace.assert(scenario$maxTime == 0)

    which_max_last <- function(x) 1L + length(x) - which.max(rev(x))
    
    get_confs_for_psrace <- function(iraceResults, blockSize, iteration_elites, max_experiments, conf_ids, rejected_ids) {
      allElites <- iraceResults$allElites
      experiments <- iraceResults$experiments
      conf_ids <- if (iteration_elites) unlist(rev(allElites)) else allElites[[length(allElites)]]
      conf_ids <- unique(c(conf_ids, iraceResults$allConfigurations[[".ID."]]))
      # NA may be generated if we skipped iterations.
      if (anyNA(conf_ids))
        conf_ids <- conf_ids[!is.na(conf_ids)]
      # Remove rejected configurations.
      if (length(rejected_ids))
        conf_ids <- setdiff(conf_ids, rejected_ids)
      experiments <- experiments[, conf_ids, drop = FALSE]
      conf_needs <- matrixStats::colCounts(experiments, value = NA)
      # Remove any configuration that needs more than max_experiments.
      conf_needs <- conf_needs[conf_needs <= max_experiments]
      # We want to evaluate in at least scenario$blockSize instances more.
      n_confs <- floor(max_experiments / blockSize)
      # If we have n_confs that have been evaluated in all instances, select those.
      if (sum(conf_needs == 0L) >= n_confs) {
        conf_needs <- conf_needs[conf_needs == 0L][1:n_confs]
        conf_ids <- names(conf_needs)
        cat(sep="", "# Configurations selected: ", paste0(collapse=", ", conf_ids),
          ".\n# Pending instances: ", paste0(collapse=", ", conf_needs), ".\n")
        return(conf_ids)
      }
      if (length(conf_needs) > 21L) {
        conf_needs <- conf_needs[1L:21L]
      }
      # We want to race at least two configurations.
      combs <- lapply(seq(3L, as.integer(2^22) - 1L, 2L), function(z) as.logical(intToBits(z)))
      n <- sapply(combs, sum, USE.NAMES=FALSE)
      # Let's try first to evaluate on new instances.
      conf_needs_blocksize <- conf_needs +  blockSize
      left <- sapply(combs, function(x) max_experiments - sum(conf_needs_blocksize[x]), USE.NAMES=FALSE)
      if (any(left >= 0L)) { # We have enough budget to evaluate on a new instance.
        # Select the combination that will allow to evaluate the most configurations.
        combs <- combs[left >= 0]
        n <- n[left >= 0]
        winner <- which.max(n)
        conf_needs <- conf_needs[combs[[winner]]]
        conf_ids <- names(conf_needs)
        cat(sep="", "# Configurations selected: ", paste0(collapse=", ", conf_ids),
          ".\n# Pending instances: ", paste0(collapse=", ", conf_needs), ".\n")
        return(conf_ids)
      }
      # We do not have enough budget to see new instances.
      left <- sapply(combs, function(x) max_experiments - sum(conf_needs[x]), USE.NAMES=FALSE)
      irace.assert(any(left >= 0), eval_after=save(iraceResults, file="bug-conf_ids.Rdata"))
      # Select the combination that will allow to evaluate the most configurations.
      combs <- combs[left >= 0]
      n <- n[left >= 0]
      winner <- which.max(n)
      conf_needs <- conf_needs[combs[[winner]]]
      conf_ids <- names(conf_needs)
        cat(sep="", "# Configurations selected: ", paste0(collapse=", ", conf_ids),
          ".\n# Pending instances: ", paste0(collapse=", ", conf_needs), ".\n")
      return(conf_ids)
    }
    conf_ids <- get_confs_for_psrace(iraceResults, scenario$blockSize, iteration_elites,
      max_experiments, conf_ids = conf_ids, rejected_ids = race_state$rejected_ids)
    irace.assert(length(conf_ids) > 1L, eval_after = {
      # Debug what happened if the assert failed.
      rejected_ids <- race_state$rejected_ids
      cat("blockSize:", scenario$blockSize, "\niteration_elites: ", as.character(iteration_elites),
        "\nrejected: ", paste0(collapse=", ", rejected_ids), "\n")
      allElites <- iraceResults$allElites
      experiments <- iraceResults$experiments
      conf_ids <- if (iteration_elites) unlist(rev(allElites)) else allElites[[length(allElites)]]
      cat("conf_ids1:", paste0(collapse=", ", conf_ids), "\n")
      conf_ids <- unique(c(conf_ids, iraceResults$allConfigurations[[".ID."]]))
      cat("conf_ids2:", paste0(collapse=", ", conf_ids), "\n")
      # NA may be generated if we skipped iterations.
      if (anyNA(conf_ids))
        conf_ids <- conf_ids[!is.na(conf_ids)]
      cat("conf_ids3:", paste0(collapse=", ", conf_ids), "\n")
      # Remove rejected configurations.
      if (length(rejected_ids))
        conf_ids <- setdiff(conf_ids, rejected_ids)
      cat("conf_ids4:", paste0(collapse=", ", conf_ids), "\n")
      experiments <- 
      conf_needs <- matrixStats::colCounts(experiments[, conf_ids, drop = FALSE], value = NA)
      cat("conf_needs:", paste0(collapse=", ", conf_needs), "\n")
    })
    
  } else if (length(conf_ids) <= 1L) {
    irace.error ("The number configurations provided should be larger than 1.")
  } else if (length(race_state$rejected_ids) && any(conf_ids %in% race_state$rejected_ids)) {
    irace.error ("Some configuration IDs provided were rejected in the previous run: ",
      paste0(collapse=", ", intersect(conf_ids, race_state$rejected_ids)), ".")
  }
  if (!all(conf_ids %in% iraceResults$allConfigurations[[".ID."]])) {
    irace.error("Some configuration IDs provided cannot be found in the configurations: ",
      paste0(collapse=", ", setdiff(conf_ids, iraceResults$allConfigurations[[".ID."]])), ".")
  }
  elite_configurations <- iraceResults$allConfigurations[iraceResults$allConfigurations[[".ID."]] %in% conf_ids, , drop=FALSE]

  # Generate new instances.
  generateInstances(race_state, scenario, max_experiments / nrow(elite_configurations), update = TRUE)
  elite_data <- if (scenario$elitist)
                  iraceResults$experiments[, as.character(elite_configurations[[".ID."]]), drop=FALSE] else NULL
  race_state$next_instance <- nrow(iraceResults$experiments) + 1L

  cat(sep="",
    "# Seed: ", race_state$seed,
    "\n# Configurations: ", nrow(elite_configurations),
    "\n# Available experiments: ", max_experiments,
    "\n# minSurvival: 1\n")

  raceResults <- elitist_race(race_state,
    maxExp = max_experiments,
    minSurvival = 1L,
    configurations = elite_configurations,
    scenario = scenario,
    elite.data = elite_data,
    elitist_new_instances = 0L)

  elite_configurations <- extractElites(raceResults$configurations,
    nbElites = race_state$recovery_info$minSurvival,
    debugLevel = scenario$debugLevel)
  irace.note("Elite configurations (first number is the configuration ID;",
               " listed from best to worst according to the ",
               test.type.order.str(scenario$testType), "):\n")
  if (!scenario$quiet)
    configurations_print(elite_configurations, metadata = scenario$debugLevel >= 1L)

  if (!is.null(scenario$logFile)) {
    elapsed <- race_state$time_elapsed()
    if (!scenario$quiet)
      cat("# Total CPU user time: ", elapsed["user"], ", CPU sys time: ", elapsed["system"],
        ", Wall-clock time: ", elapsed["wallclock"], "\n", sep="")
    indexIteration <- 1L + max(race_state$experiment_log[["iteration"]])
    # We add indexIteration as an additional column.
    set(raceResults$experiment_log, j = "iteration", value = indexIteration)
    race_state$experiment_log <- rbind(race_state$experiment_log, raceResults$experiment_log)
    # Merge new results.
    iraceResults$experiments <- merge_matrix(iraceResults$experiments, raceResults$experiments)
    iraceResults$iterationElites[indexIteration] <- elite_configurations[[".ID."]][1L]
    iraceResults$allElites[[indexIteration]] <- elite_configurations[[".ID."]]
    race_state$elite_configurations <- elite_configurations
    iraceResults$state <- race_state
    irace_save_logfile(iraceResults, scenario)
  }

  # FIXME: This log should contain only information of what was done in the
  # psRace and avoid duplicating info from iraceResults.
  psrace_log <-  list(configurations = elite_configurations,
    instances = race_state$instances_log[seq_nrow(iraceResults$experiments), , drop = FALSE],
    max_experiments = max_experiments,
    experiments = iraceResults$experiments,
    elites = elite_configurations[[".ID."]])
  iraceResults$psrace_log <- psrace_log
  elite_configurations
}
