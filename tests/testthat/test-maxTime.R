withr::with_output_sink("test-maxTime.Rout", {

target.runner <- function(experiment, scenario)
{
  configuration     <- experiment$configuration
  tmax <-  configuration[["tmax"]]
  temp <-  configuration[["temp"]]
  stopifnot(is.numeric(tmax))
  stopifnot(is.numeric(temp))
  time <- max(1, abs(rnorm(1, mean=(tmax+temp)/10)))
  list(cost = time, time = time, call = toString(experiment))
}

time.irace <- function(...)
{
  args <- list(...)
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  test_weights <- rnorm(2, mean = 0.9, sd = 0.02)
  parameters <- readParameters(text = '
   tmax "" i (1, 50)
   temp "" r (0, 10)
   dummy "" c ("dummy")
   ')
  scenario <- list(targetRunner = target.runner,
                   instances = weights,
                   testInstances = test_weights,
                   seed = 1234567,
                   parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  irace:::checkTargetFiles(scenario = scenario)
  
  confs <- irace(scenario = scenario)
  final_ids <- sort(as.character(confs$.ID.[1:scenario$testNbElites]))
  expect_gt(nrow(confs), 0L)
  testing_fromlog(scenario$logFile)
  iraceResults <- read_logfile(scenario$logFile)
  if (scenario$testIterationElites) {
    # FIXME: We could test here that the correct configurations are tested.
    expect_gte(ncol(iraceResults$testing$experiments), scenario$testNbElites)
  } else {
    test_ids <- sort(colnames(iraceResults$testing$experiments))
    expect_equal(final_ids, test_ids)
  }
  confs
}


test_that("maxTime=500 testNbElites=2 testIterationElites=FALSE", {
  generate_set_seed()
  time.irace(maxTime = 500, testNbElites=2)
})

test_that("maxTime=1111 testNbElites=3 testIterationElites=TRUE", {
  skip_on_cran()
  generate_set_seed()
  time.irace(maxTime = 1111, testNbElites=3, testIterationElites=TRUE)
})

}) # withr::with_output_sink()
