withr::with_output_sink("test-capping.Rout", {


## target runner ###########################################################
target_runner_reject <- function(experiment, scenario)
{
  if (experiment$configuration[["reject"]] == "1" && runif(1) <= 0.01)
    list(cost = -Inf, time = experiment$bound, call = toString(experiment))
  target_runner_capping_xy(experiment, scenario)
}


test_that("irace_capping_xy maxExperiments = 1000", {
  generate_set_seed()
  irace_capping_xy(maxExperiments = 1000)
})

test_that("irace_capping_xy maxTime = 1000", {
  generate_set_seed()
  expect_warning(irace_capping_xy(maxTime = 1000),
                 "boundMax = 80 is too large, using 5 instead")
})

test_that("irace_capping_xy targetRunner = target_runner_reject, maxTime = 1000", {
  skip_on_cran() 
  skip_on_coverage() # This test sometimes fails randomly
  generate_set_seed()
  irace_capping_xy(targetRunner = target_runner_reject, maxTime = 1000, boundMax = 5, debugLevel = 3)
})

test_that("capping default value", {
  parameters_table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   reject "" c (0,1)'
  
  parameters <- readParameters(text = parameters_table)

  def_scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target_runner_reject,
                   maxTime = 1200,
                   parameters = parameters)
  scenario <- checkScenario(def_scenario)
  expect_false(scenario$capping)
  def_scenario$boundMax <- 80
  scenario <- checkScenario(def_scenario)
  expect_true(scenario$capping)
})

}) # withr::with_output_sink()
