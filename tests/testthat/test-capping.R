withr::with_output_sink("test-capping.Rout", {


## target runner ###########################################################
target.runner.reject <- function(experiment, scenario)
{
  if (experiment$configuration[["reject"]] == "1" && runif(1) <= 0.01)
    list(cost = -Inf, time = experiment$bound, call = toString(experiment))
  target_runner_capping_xy(experiment, scenario)
}

cap.irace <- function(...)
{
  args <- list(...)
  parameters_table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   reject "" c (0,1)'
  
  parameters <- readParameters(text = parameters_table)

  scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target_runner_capping_xy,
                   capping = TRUE,
                   boundMax = 80,
                   testType = "t-test",
                   logFile = tempfile(fileext=".Rdata"),
                   parallel = if (system_os_is_windows()) 1L else test_irace_detectCores(),
                   parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  irace:::checkTargetFiles(scenario = scenario)
  
  confs <- irace(scenario = scenario)
  best.conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]),
                   best.conf)
}

test_that("cap.irace maxExperiments = 1000", {
  generate.set.seed()
  cap.irace(maxExperiments = 1000)
})

test_that("cap.irace maxTime = 1000", {
  generate.set.seed()
  expect_warning(cap.irace(maxTime = 1000),
                 "boundMax = 80 is too large, using 5 instead")
})

test_that("cap.irace targetRunner = target.runner.reject, maxTime = 1000", {
  skip_on_cran() 
  skip_on_coverage() # This test sometimes fails randomly
  generate.set.seed()
  cap.irace(targetRunner = target.runner.reject, maxTime = 1000, boundMax = 5, debugLevel = 3)
})

test_that("capping default value", {
  parameters_table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   reject "" c (0,1)'
  
  parameters <- readParameters(text = parameters_table)

  def_scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target.runner.reject,
                   maxTime = 1200,
                   logFile = tempfile(fileext=".Rdata"),
                   parameters = parameters)
  scenario <- checkScenario(def_scenario)
  expect_false(scenario$capping)
  def_scenario$boundMax <- 80
  scenario <- checkScenario(def_scenario)
  expect_true(scenario$capping)
})

}) # withr::with_output_sink()
