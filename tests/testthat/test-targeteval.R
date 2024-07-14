withr::with_output_sink("test-targeteval.Rout", {

test_that("target.evaluator", {

target.runner <- function(experiment, scenario)
  list(call = toString(experiment))

target.evaluator <- function(experiment, num_configurations, all_conf_id,
                             scenario, target_runner_call)
  list(cost = runif(1), call = toString(experiment))

parameters <- readParameters(text = '
algorithm       "--"                 c          (as,mmas,eas,ras,acs)
')

   generate.set.seed()
   scenario <- checkScenario(list(
     targetRunner = target.runner, targetEvaluator = target.evaluator,
     maxExperiments = 200, instances = runif(100),
     parameters = parameters))
   irace:::checkTargetFiles(scenario = scenario)
   confs <- irace(scenario = scenario)
   expect_gt(nrow(confs), 0L)
})


}) # withr::with_output_sink()
