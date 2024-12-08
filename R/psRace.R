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

    # We want to race at least two configurations, so we generate all
    # integers between 3 and 2^max_confs - 1L that are not powers of 2, then convert
    # it to a bit-string then to a logical vector.
    generate_combs_2 <- function(max_confs) {
      max_pow <- as.integer(2^max_confs)
      pow_2 <- as.integer(2L^(0L:(max_confs-1L)))
      s <- seq.int(3L, max_pow - 1L, 2L)
      if (max_pow >= 8L)
        s <- c(s, setdiff(seq.int(6L, max_pow - 2L, 2L), pow_2[-1L:-3L]))
      lapply(s, function(z) as.logical((z %/% pow_2) %% 2L))
    }
    # A version of the above that is not limited to two configurations.
    generate_combs_1 <- function(max_confs) {
      max_pow <- as.integer(2^max_confs)
      pow_2 <- as.integer(2L^(0L:(max_confs-1L)))
      lapply(seq.int(1L, max_pow - 1L, 1L),
        function(z) as.logical((z %/% pow_2) %% 2L))
    }
    # FIXME: Is this really faster than head(x, n=max_len) ?
    truncate_conf_needs <- function(x, max_len) {
      if (length(x) <= max_len) return(x)
      x[seq_len(max_len)]
    }
    
    get_confs_for_psrace <- function(iraceResults, iteration_elites, max_experiments, conf_ids, rejected_ids) {
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
      n_done <- nrow(experiments) - min(conf_needs)
      # Remove any configuration that needs more than max_experiments.
      conf_needs <- conf_needs[conf_needs <= max_experiments]
      scenario <- iraceResults$scenario
      if (!scenario$deterministic || n_done < length(scenario$instances)) {
        # We want to evaluate in at least n_new instances more.
        n_new <- max(scenario$blockSize, scenario$eachTest)
        n_confs <- floor(max_experiments / n_new)
        # If we have n_confs that have been evaluated in all instances, select those.
        if (n_confs > 1L && sum(conf_needs == 0L) >= n_confs) {
          conf_needs <- conf_needs[conf_needs == 0L][1:n_confs]
          conf_ids <- names(conf_needs)
          cat(sep="", "# Configurations selected: ", paste0(collapse=", ", conf_ids),
            ".\n# Pending instances: ", paste0(collapse=", ", conf_needs), ".\n")
          return(conf_ids)
        }
        # Let's try first to evaluate on new instances.
        conf_needs_new <- truncate_conf_needs(conf_needs + n_new, max_len = 16L)
        combs <- generate_combs_2(length(conf_needs_new))
        left <- sapply(combs, function(x) max_experiments - sum(conf_needs_new[x]), USE.NAMES=FALSE)
        irace.assert(!is.null(left) && length(left) > 0L && !anyNA(left),
          eval_after= {
            cat("max_experiments: ", max_experiments, "\n")
            cat("length(left): ", length(left), "\n")
            cat("sum(is.na(left)): ", sum(is.na(left)), "\n")
            save(iraceResults, file="bug-conf_ids.Rdata")
          })
        
        if (any(left >= 0L)) { # We have enough budget to evaluate on a new instance.
          # Select the combination that will allow to evaluate the most configurations.
          combs <- combs[left >= 0]
          n <- sapply(combs, sum, USE.NAMES=FALSE)
          winner <- which.max(n)
          conf_needs <- conf_needs[combs[[winner]]]
          conf_ids <- names(conf_needs)
          cat(sep="", "# Configurations selected: ", paste0(collapse=", ", conf_ids),
            ".\n# Pending instances: ", paste0(collapse=", ", conf_needs), ".\n")
          return(conf_ids)
        }
      }
      # We do not have enough budget to see new instances, so we need consider
      # at least 1 configuration that has NA values, but we still include the
      # configurations that have been evaluated on all instances.
      conf_needs_zero <- conf_needs[conf_needs == 0L]
      conf_needs <- truncate_conf_needs(conf_needs[conf_needs > 0L], 16L)
      combs <- generate_combs_1(length(conf_needs))
      left <- sapply(combs, function(x) max_experiments - sum(conf_needs[x]), USE.NAMES=FALSE)
      irace.assert(any(left >= 0), eval_after=save(iraceResults, file="bug-conf_ids.Rdata"))
      # Select the combination that will allow us to evaluate the most configurations.
      combs <- combs[left >= 0]
      n <- sapply(combs, sum, USE.NAMES=FALSE)
      winner <- which.max(n)
      conf_needs <- c(conf_needs_zero, conf_needs[combs[[winner]]])
      conf_ids <- names(conf_needs)
        cat(sep="", "# Configurations selected: ", paste0(collapse=", ", conf_ids),
          ".\n# Pending instances: ", paste0(collapse=", ", conf_needs), ".\n")
      return(conf_ids)
    }
    conf_ids <- get_confs_for_psrace(iraceResults, iteration_elites, max_experiments,
      conf_ids = conf_ids, rejected_ids = race_state$rejected_ids)
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
  elite_data <- iraceResults$experiments[, as.character(elite_configurations[[".ID."]]), drop=FALSE]
  race_state$next_instance <- nrow(iraceResults$experiments) + 1L

  cat(sep="",
    "# Seed: ", race_state$seed,
    "\n# Configurations: ", nrow(elite_configurations),
    "\n# Available experiments: ", max_experiments,
    "\n# minSurvival: 1\n")

  # FIXME: elitist_race should not require setting this, but it currently does.
  scenario$elitist <- TRUE
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
