withr::with_output_sink("test-badoutputraw.Rout", {

  test_that("outputRaw is vector", {

    target_runner <- function(experiment, scenario)
      list(cost = 100 + rnorm(1, 0, 0.1), outputRaw=c("a", "b"))

      parameters <- readParameters(text='
algorithm    "--"             c    (as,mmas,eas,ras,acs)
alpha        "--alpha "       r    (0.00, 5.00)
')
      scenario <- list(targetRunner = target_runner,
        instances=1:10,
        maxExperiments = 500, logFile = "",
        parameters = parameters)
      confs <- irace(scenario = scenario)
      expect_false(is.null(confs))
  })
})
