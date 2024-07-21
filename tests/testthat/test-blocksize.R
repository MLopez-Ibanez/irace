withr::with_output_sink("test-blocksize.Rout", {
cap_irace <- function(...)
{
  args <- list(...)
  parameters_table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)'
  
  parameters <- readParameters(text = parameters_table)

  scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target_runner_capping_xy,
                   capping = TRUE,
                   blockSize = 4,
                   boundMax = 80,
                   testType = "t-test",
                   parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  confs <- irace(scenario = scenario)
  best.conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]),
                   best.conf)
  invisible(read_logfile(scenario$logFile))
}

target_runner_time <- function(experiment, scenario)
{
  configuration     <- experiment$configuration
  tmax <-  configuration[["tmax"]]
  temp <-  configuration[["temp"]]
  time <- max(1, abs(rnorm(1, mean=(tmax+temp)/10)))
  list(cost = time, time = time, call = toString(experiment))
}

time_irace <- function(...)
{
  args <- list(...)
  parameters <- readParameters(text = '
   tmax "" i (-10, 10)
   temp "" r (0, 10)
   ')
  scenario <- list(targetRunner = target_runner_time,
                   instances = c("ackley", "goldestein", "matyas"),
                   blockSize=3,
                   parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)
  
  confs <- irace(scenario = scenario)
  best.conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]),
                   best.conf)
  invisible(read_logfile(scenario$logFile))
}

check_blocksize <- function(results)
{
  expect_equal(rowMeans(matrix(get_instanceID_seed_pairs(results)[["instanceID"]],nrow=results$scenario$blockSize)),
               rowMeans(matrix(seq_along(results$scenario$instances), nrow=results$scenario$blockSize)))
  expect_equal(sum(colSums(!is.na(results$experiments)) %% results$scenario$blockSize), 0)
}

test_that("blockSize error", {
  expect_error(cap_irace(maxExperiments = 1000, blockSize=3), "must be a multiple of 'blockSize")
})

test_that("blockSize cap_irace maxExperiments = 1000", {
  generate.set.seed()
  expect_warning(check_blocksize(cap_irace(maxExperiments = 1000, debugLevel = 3)),
                 "Assuming 'mu = firstTest * blockSize' because 'mu' cannot be smaller",
                 fixed = TRUE)
})

test_that("blockSize maxTime=1000", {
  generate.set.seed()
  check_blocksize(time_irace(maxTime = 1000))
})

test_that("blockSize maxTime=1000 large newInstances", {
  skip_on_cran()
  generate.set.seed()
  check_blocksize(time_irace(maxTime = 1000, instances = letters[1:9],
                             elitistNewInstances = 6, elitistLimit = 2))
})

}) # withr::with_output_sink()
