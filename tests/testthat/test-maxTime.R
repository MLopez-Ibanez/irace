context("irace")

withr::with_output_sink("test-maxTime.Rout", {

target.runner <- function(experiment, scenario)
{
  configuration     <- experiment$configuration
  tmax <-  as.numeric(configuration[["tmax"]]) 
  temp <-  as.numeric(configuration[["temp"]])
  time <- max(1, abs(rnorm(1, mean=(tmax+temp)/10)))
  return(list(cost = time, time = time, call = toString(experiment)))
}

time.irace <- function(...)
{
  args <- list(...)
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  parameters <- readParameters(text = '
   tmax "" i (1, 50)
   temp "" r (0, 10)
   ')
  scenario <- list(targetRunner = target.runner, instances = weights, seed = 1234567)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  irace:::checkTargetFiles(scenario = scenario, parameters = parameters)
  
  confs <- irace(scenario = scenario, parameters = parameters)
  expect_gt(nrow(confs), 0L)
}


test_that("maxTime 500", {
  generate.set.seed()
  time.irace(maxTime = 500)
})

test_that("maxTime 500", {
  skip_on_cran()
  generate.set.seed()
  time.irace(maxTime = 1111)
})

}) # withr::with_output_sink()
