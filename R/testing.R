#' Execute the given configurations on the testing instances specified in the
#' scenario
#'
#' @template arg_configurations
#' @template arg_scenario
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{\code{experiments}}{Experiments results.}
#'     \item{\code{seeds}}{Array of the instance seeds used in the experiments.}
#'   }
#'
#' @details A test instance set must be provided through `scenario[["testInstances"]]`.
#'
#' @seealso
#'  [testing_fromlog()]
#' 
#' @author Manuel López-Ibáñez
#' @export
testConfigurations <- function(configurations, scenario)
{
  # We need to set up a default scenario (and repeat all checks) in case
  # we are called directly instead of being called after executing irace.
  scenario <- checkScenario(scenario)
  
  testInstances <- scenario[["testInstances"]]
  instances_id <- names(testInstances)
  if (length(testInstances) == 0) irace.error("No test instances given")
  if (is.null(instances_id)) irace.error("testInstances must have names")
  
  # 2147483647 is the maximum value for a 32-bit signed integer.
  # We use replace = TRUE, because replace = FALSE allocates memory for each possible number.
  ## FIXME: scenario[["testInstances"]] and scenario$instances behave differently,
  ## we should unify them so that the seeds are also saved in scenario.
  instanceSeed <- sample.int(2147483647L, size = length(testInstances), replace = TRUE)
  names(instanceSeed) <- instances_id
  
  # If there is no ID (e.g., after using readConfigurations), then add it.
  if (".ID." %not_in% colnames(configurations))
    configurations[[".ID."]] <- seq_nrow(configurations)
  
  # Create experiment list
  experiments <- createExperimentList(configurations, parameters = scenario$parameters,
                                      instances = testInstances, instances.ID = instances_id, seeds = instanceSeed,
                                      bounds = rep(scenario$boundMax, nrow(configurations)))
  race_state <- RaceState$new(scenario)
  if (scenario$debugLevel >= 3L) {
    irace.note ("Memory used before execute.experiments in testConfigurations():\n")
    race_state$print_mem_used()
  }
  race_state$start_parallel(scenario)
  on.exit(race_state$stop_parallel())
  target.output <- execute.experiments(race_state, experiments, scenario)
  # targetEvaluator may be NULL. If so, target.output must contain the right
  # output already.
  if (!is.null(scenario$targetEvaluator))
    target.output <- execute_evaluator(race_state$target_evaluator, experiments,
      scenario, target.output, configurations[[".ID."]])

  # FIXME: It would be much faster to get convert target.output$cost to a
  # vector, then initialize the matrix with the vector.
  testResults <- matrix(NA, ncol = nrow(configurations), nrow = length(testInstances),
                        # dimnames = list(rownames, colnames)
                        dimnames = list(instances_id, configurations$.ID.))

  cost <- sapply(target.output, getElement, "cost")
  if (scenario$capping)
    cost <- applyPAR(cost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
  # FIXME: Vectorize this loop
  for (i in seq_along(experiments)) {
    testResults[rownames(testResults) == experiments[[i]]$id.instance,
                colnames(testResults) == experiments[[i]]$id.configuration] <- cost[i]
  }
  if (scenario$debugLevel >= 3L) {
    irace.note ("Memory used at the end of testConfigurations():\n")
    race_state$print_mem_used()
  }

  ## FIXME: Shouldn't we record these experiments in experimentLog ?
  list(experiments = testResults, seeds = instanceSeed)
}
