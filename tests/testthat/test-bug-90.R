withr::with_output_sink("test-bug-90.Rout", {

  test_that("bug-90", {
    parameters <- readParameters(text='
a  "" c    (1,2,3,4)
b  "" c    (0, 1, 2, 3)
c  "" r    (0,1)
d  "" r    (0,1)
e  "" r    (0,1)
f  "" i    (1,10)
')
    logFile <- withr::local_tempfile(fileext=".Rdata")
    scenario <- defaultScenario(list(
      parameters = parameters,
      instances = 1:20,
      elitist = FALSE,
      maxExperiments = 200,
      logFile = logFile,
      targetRunner = function(experiment, scenario)
        list(cost = experiment$configuration$c * experiment$configuration$d * experiment$configuration$e)
    ))
    confs <- irace(scenario)
    best_conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
    expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]), best_conf)
  })
}) # withr::with_output_sink()
