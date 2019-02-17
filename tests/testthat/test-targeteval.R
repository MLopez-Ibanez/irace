context("targeteval")

source("common.R")

test_that("target.evaluator", {

target.runner <- function(experiment, scenario)
{
  return(list(call = toString(experiment)))
}

target.evaluator <- function(experiment, num.configurations, all.conf.id,
                             scenario, target.runner.call)
{
  result <- list(cost = runif(1), call = toString(experiment))
  return(result)
}

parameters <- readParameters(text = '
algorithm       "--"                 c          (as,mmas,eas,ras,acs)
')

   generate.set.seed()
   scenario <- checkScenario (list(targetRunner = target.runner,
                                   targetEvaluator = target.evaluator,
                                   maxExperiments = 200, instances = runif(100)))
   irace:::checkTargetFiles(scenario = scenario, parameters = parameters)
   confs <- irace(scenario = scenario, parameters = parameters)
   expect_more_than(nrow(confs), 0)
})