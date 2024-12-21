withr::with_output_sink("test-maxTime.Rout", {

target_runner <- function(experiment, scenario)
{
  configuration     <- experiment$configuration
  tmax <-  configuration[["tmax"]]
  temp <-  configuration[["temp"]]
  time <- max(1, abs(rnorm(1, mean=(tmax+temp)/10)))
  list(cost = time, time = time)
}

time_irace <- function(...)
{
  args <- list(...)
  parameters <- readParameters(text = '
   tmax "" i (1, 50)
   temp "" r (0, 10)
   dummy "" c ("dummy")
   ')
  scenario <- list(targetRunner = target_runner,
                   instances = 1:10,
                   testInstances = 11:20,
                   seed = 1234567,
                   parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  expect_true(irace:::checkTargetFiles(scenario = scenario))
  
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
  time_irace(maxTime = 500, testNbElites=2)
})

test_that("maxTime=1111 testNbElites=3 testIterationElites=TRUE", {
  skip_on_cran()
  generate_set_seed()
  time_irace(maxTime = 1111, testNbElites=3, testIterationElites=TRUE)
})

}) # withr::with_output_sink()
