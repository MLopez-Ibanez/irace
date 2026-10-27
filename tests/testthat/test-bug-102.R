withr::with_output_sink("test-bug-102.Rout", {

test_that("bug-102: truncate_rows() with columns named like its variables", {
  for (name in c("k", "n", "nx", "i", "x")) {
    x <- data.table(1:5, runif(5))
    setnames(x, c(name, "y"))
    expect_identical(irace:::truncate_rows(x, 2L), x[1:2])
    expect_identical(irace:::truncate_rows(x, 5L), x)
    expect_identical(irace:::truncate_rows(x, 7L), x)
  }
})

test_that("bug-102 integration: parameter named n", {
  parameters <- parametersNew(
    param_int("n", 0, 10),
    param_cat("b", c("0", "1")),
    param_real("x", 0, 1, condition = expression(n >= 5))
  )
  target_runner <- function(experiment, scenario) {
    conf <- experiment$configuration
    x <- if (is.na(conf$x)) 0 else conf$x
    list(cost = (conf$n - 7)^2 + x + as.numeric(conf$b) + runif(1, 0, 0.1))
  }
  # Seeds 1, 4, 5, 6 and 8 failed before the fix.
  for (seed in 1:10) {
    scenario <- checkScenario(defaultScenario(list(
      targetRunner = target_runner, parameters = parameters,
      instances = 1:6, maxExperiments = 300, seed = seed, logFile = ""
    )))
    expect_no_error(irace(scenario = scenario))
  }
})

test_that("bug-102: parameters named param and this_model", {
  parameters <- readParameters(text = '
param      "" i (0, 10)
this_model "" r (0.5, 1)
x          "" r (0, 1)          | param >= 3
y          "" r (0, this_model) | param >= 5
c          "" c (u, v, w)
')
  target_runner <- function(experiment, scenario) {
    conf <- experiment$configuration
    v <- function(z) if (is.na(z)) 0 else z
    list(cost = (conf$param - 7)^2 + conf$this_model + v(conf$x) + v(conf$y) + runif(1, 0, 0.1))
  }
  for (seed in 1:5) {
    scenario <- checkScenario(defaultScenario(list(
      targetRunner = target_runner, parameters = parameters,
      instances = 1:6, maxExperiments = 400, seed = seed, logFile = ""
    )))
    expect_no_error(irace(scenario = scenario))
  }
})

}) # withr::with_output_sink()
