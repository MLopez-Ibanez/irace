withr::with_output_sink("test-capping.Rout", {

## target runner ###########################################################
target.runner <- function(experiment, scenario)
{
  debugLevel    <- scenario$debugLevel
  configuration.id  <- experiment$id.configuration
  instance.id   <- experiment$id.instance
  seed          <- experiment$seed
  configuration <- experiment$configuration
  instance      <- experiment$instance
  bound         <- experiment$bound

  x <- configuration[["x"]]
  y <- configuration[["y"]]
  value <- switch(instance,
                  ackley     = f_ackley(x, y),
                  goldestein = f_goldestein_price(x, y),
                  matyas     = f_matyas(x, y),
                  himmelblau  = f_himmelblau(x, y))
  
  # Simulate execution bound
  if (value > bound) value <- bound
  list(cost = value, time=value, call = toString(experiment))
}

## target runner ###########################################################
target.runner.reject <- function(experiment, scenario)
{
  if (experiment$configuration[["reject"]] == "1" && runif(1) <= 0.01)
    return(list(cost = -Inf, time = 80, call = toString(experiment)))
  target.runner(experiment, scenario)
}

cap.irace <- function(...)
{
  args <- list(...)
  parameters.table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   reject "" c (0,1)'
  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target.runner,
                   capping = TRUE,
                   boundMax = 80,
                   testType = "t-test",
                   logFile = tempfile(fileext=".Rdata"),
                   parallel = if (system_os_is_windows()) 1L else test_irace_detectCores())
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  irace:::checkTargetFiles(scenario = scenario, parameters = parameters)
  
  confs <- irace(scenario = scenario, parameters = parameters)
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
                 "boundMax = 80 is too large, using 10 instead")
})

test_that("cap.irace targetRunner = target.runner.reject, maxTime = 1000", {
  skip_on_cran() 
  skip_on_coverage() # This test sometimes fails randomly
  generate.set.seed()
  cap.irace(targetRunner = target.runner.reject, maxTime = 1000, boundMax = 5, debugLevel = 3)
})

}) # withr::with_output_sink()
