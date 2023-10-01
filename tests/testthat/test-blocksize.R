withr::with_output_sink("test-blocksize.Rout", {

## target runner ###########################################################
target.runner.cap <- function(experiment, scenario)
{
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

cap.irace <- function(...)
{
  args <- list(...)
  parameters.table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)'
  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target.runner.cap,
                   capping = TRUE,
                   blockSize = 4,
                   boundMax = 80,
                   testType = "t-test")
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  confs <- irace(scenario = scenario, parameters = parameters)
  best.conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]),
                   best.conf)
  invisible(read_logfile(scenario$logFile))
}

target.runner.time <- function(experiment, scenario)
{
  configuration     <- experiment$configuration
  tmax <-  configuration[["tmax"]]
  temp <-  configuration[["temp"]]
  time <- max(1, abs(rnorm(1, mean=(tmax+temp)/10)))
  list(cost = time, time = time, call = toString(experiment))
}

time.irace <- function(...)
{
  args <- list(...)
  parameters <- readParameters(text = '
   tmax "" i (1, 50)
   temp "" r (0, 10)
   ')
  scenario <- list(targetRunner = target.runner.time,
                   instances = c("ackley", "goldestein", "matyas"),
                   blockSize=3)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)
  
  confs <- irace(scenario = scenario, parameters = parameters)
  best.conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]),
                   best.conf)
  invisible(read_logfile(scenario$logFile))
}

check_blocksize <- function(results)
{
  expect_equal(rowMeans(matrix(get_instanceID_seed_pairs(results)[,"instanceID"],nrow=results$scenario$blockSize)),
               rowMeans(matrix(seq_along(results$scenario$instances), nrow=results$scenario$blockSize)))
  expect_equal(sum(colSums(!is.na(results$experiments)) %% results$scenario$blockSize), 0)
}

test_that("blockSize error", {
  expect_error(cap.irace(maxExperiments = 1000, blockSize=3), "must be a multiple of 'blockSize")
})

test_that("blockSize cap.irace maxExperiments = 1000", {
  generate.set.seed()
  expect_warning(check_blocksize(cap.irace(maxExperiments = 1000, debugLevel = 3)),
                 "Assuming 'mu = firstTest * blockSize' because 'mu' cannot be smaller",
                 fixed = TRUE)
})

test_that("blockSize maxTime=1000", {
  generate.set.seed()
  check_blocksize(time.irace(maxTime = 1000))
})

test_that("blockSize maxTime=1000 large newInstances", {
  skip_on_cran()
  generate.set.seed()
  check_blocksize(time.irace(maxTime = 1000, instances = letters[1:9],
                             elitistNewInstances = 6, elitistLimit = 2))
})

}) # withr::with_output_sink()
