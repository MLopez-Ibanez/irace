withr::with_output_sink("test-bug71.Rout", {

test_that("bug71.evaluator", {

  target.runner <- function(experiment, scenario)
    list(cost = runif(1), call = toString(experiment))

    parameters <- readParameters(text = '
algorithm       "--"                 c          (as,mmas,eas,ras,acs)
')
    scenario <- checkScenario(list(
      targetRunner = target.runner,
      maxExperiments = 200, instances = runif(10),
      initConfigurations = data.frame(algorithm="as"),
     parameters = parameters))
    expect_true(irace:::checkTargetFiles(scenario = scenario))
})

}) # withr::with_output_sink()
