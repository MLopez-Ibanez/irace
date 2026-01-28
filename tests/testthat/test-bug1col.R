withr::with_output_sink("test-bug1col.Rout", {

test_that("bug1col", {

  target_runner <- function(experiment, scenario)
    list(cost = runif(1))

    parameters <- readParameters(text = '
c_1 "c_1" c (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
')
    scenario <- list(targetRunner = target_runner,
      instances = 1:10,
      configurationsFile="confs1col.txt",
      maxExperiments = 200,
      testType = "F-test",
      nbIterations = 1,
      postselection = 0,
      minNbSurvival = 1,
      elitistLimit = 0,
      logFile = "",
      parameters = parameters)
    confs <- irace(scenario = scenario)
})

}) # withr::with_output_sink()
