withr::with_output_sink("test-bug-103.Rout", {

parameters <- parametersNew(
  param_int("p", 0, 10),
  param_real("tm", 0.5, 1, condition = expression(p >= 3)),
  param_real("y", 0, expression(tm), condition = expression(p >= 5))
)

test_that("bug-103: similarConfigurations() with a dependent domain whose dependency is inactive", {
  confs <- data.frame(.ID. = 1:2, p = c(1L, 1L), tm = c(NA, NA), y = c(NA_real_, NA_real_))
  expect_identical(irace:::similarConfigurations(confs, parameters, threshold = 0.01), 1:2)
  confs$p[2L] <- 2L
  expect_null(irace:::similarConfigurations(confs, parameters, threshold = 0.01))
})

test_that("bug-103 integration: soft restart with a dependent domain whose dependency is inactive", {
  target_runner <- function(experiment, scenario) list(cost = runif(1))
  # Seeds 3, 4, 5, 6, 9 and 10 failed before the fix.
  for (seed in 1:10) {
    scenario <- checkScenario(defaultScenario(list(
      targetRunner = target_runner, parameters = parameters,
      instances = 1:6, maxExperiments = 400, seed = seed, logFile = ""
    )))
    expect_no_error(irace(scenario = scenario))
  }
})

}) # withr::with_output_sink()
