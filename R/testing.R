testConfigurations <- function(configurations, scenario, parameters)
{
  # We need to set up a default scenario (and repeat all checks) in case
  # we are called directly instead of being called after executing irace.
  scenario <- checkScenario(defaultScenario(scenario))
  
  testInstances <- scenario$testInstances
  extra.params  <- scenario$testInstances.extra.params
  # 2147483647 is the maximum value for a 32-bit signed integer.
  # We use replace = TRUE, because replace = FALSE allocates memory for each possible number.
  instanceSeed <- sample.int(2147483647, size = length(testInstances), replace = TRUE)

  values <- removeConfigurationsMetaData(configurations)
  values <- values[,parameters$names, drop = FALSE]
  switches <- parameters$switches[parameters$names]

  if (scenario$debugLevel >= 3) {
    irace.note ("Memory used in testConfigurations():\n")
    irace.print.memUsed()
  }
  
  # If there is no ID (e.g., after using readConfigurations), then add it.
  if (! (".ID." %in% colnames(configurations))) {
    configurations$.ID. <- 1:nrow(configurations)
  }
  # Create experiment list
  experiments <- vector("list", nrow(configurations) * length(testInstances))
  ntest <- 1
  for (i in 1:nrow(configurations)) {
    for (j in 1:length(testInstances)) {
      experiments[[ntest]] <- list(id.configuration = configurations[i, ".ID."],
                                   id.instance  = paste0(j, "t"),
                                   seed         = instanceSeed[j],
                                   configuration = values[i, , drop = FALSE],
                                   instance = testInstances[j],
                                   extra.params = extra.params[j], 
                                   switches = switches)
      ntest <- ntest + 1
    }
  }

  startParallel(scenario)
  on.exit(stopParallel())

  if (scenario$debugLevel >= 3) {
    irace.note ("Memory used in testConfigurations():\n")
    irace.print.memUsed()
  }

  target.output <- execute.experiments (experiments, scenario)
  # target.evaluator may be NULL. If so, target.output must
  # contain the right output already.
  if (!is.null(.irace$target.evaluator))
    target.output <- execute.evaluator (experiments, scenario, target.output,
                                        configurations$.ID.)

  testResults <- matrix(NA, ncol = nrow(configurations), nrow = length(testInstances),
                        # dimnames = list(rownames, colnames)
                        dimnames = list (testInstances, configurations$.ID.))

  for (i in seq_along(experiments)) {
    testResults[rownames(testResults) == experiments[[i]]$instance,
                colnames(testResults) == experiments[[i]]$id.configuration] <- target.output[[i]]$cost
  }
  ## MANUEL: Shouldn't we record these experiments in experimentLog
  
  return(list(experiments = testResults, seeds = instanceSeed))
}
