library(irace)
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

parameters.table <- '
algorithm       "--"                 c          (as,mmas,eas,ras,acs)
'

parameters <- readParameters(text = parameters.table)

scenario <- list(targetRunner = target.runner,
                 targetEvaluator = target.evaluator,
                 maxExperiments = 200, instances = runif(100))
scenario <- checkScenario (scenario)

irace(scenario = scenario, parameters = parameters)
