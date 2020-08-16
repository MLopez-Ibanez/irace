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
  test_weights <- rnorm(2, mean = 0.9, sd = 0.02)
  parameters <- readParameters(text = '
   tmax "" i (1, 50)
   temp "" r (0, 10)
   ')
  scenario <- list(targetRunner = target.runner,
                   instances = weights,
                   testInstances = test_weights,
                   seed = 1234567)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  irace:::checkTargetFiles(scenario = scenario, parameters = parameters)
  
  confs <- irace(scenario = scenario, parameters = parameters)
  final_ids <- as.character(sort(confs$.ID.[1:scenario$testNbElites]))
  expect_gt(nrow(confs), 0L)
  testing_fromlog(scenario$logFile)
  load(scenario$logFile)
  if (scenario$testIterationElites) {
    # FIXME: We could test here that the correct configurations are tested.
    expect_gte(ncol(iraceResults$testing$experiments), scenario$testNbElites)
  } else {
    test_ids <- sort(colnames(iraceResults$testing$experiments))
    expect_equivalent(final_ids, test_ids)
  }
  return(confs)
}


test_that("maxTime=500 testNbElites=2 testIterationElites=FALSE", {
  generate.set.seed()
  time.irace(maxTime = 500, testNbElites=2)
})

test_that("maxTime=1111 testNbElites=3 testIterationElites=TRUE", {
  skip_on_cran()
  generate.set.seed()
  time.irace(maxTime = 1111, testNbElites=3, testIterationElites=TRUE)
})

}) # withr::with_output_sink()
