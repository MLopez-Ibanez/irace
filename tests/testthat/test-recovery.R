withr::with_output_sink("test-recovery.Rout", {

test_that("recovery works", {

target_runner_xy <- function(experiment, scenario)
{
  configuration <- experiment$configuration
  instance      <- experiment$instance

  x <- configuration[["x"]]
  y <- configuration[["y"]]
  value <- switch(instance,
                  ackley     = f_ackley(x, y),
                  goldestein = f_goldestein_price(x, y),
                  matyas     = f_matyas(x, y),
                  himmelblau  = f_himmelblau(x, y))
  list(cost = value)
}

parameters_table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   '

  parameters <- readParameters(text = parameters_table)
  logFile <- withr::local_tempfile(pattern = "irace", fileext = ".Rdata")

  seed <- sample.int(min(2147483647L, .Machine$integer.max), size = 1, replace = TRUE)
  maxExperiments <- 500L
  limit <- 100L

  scenario <- list(
    instances = c("ackley", "goldestein", "matyas", "himmelblau"),
    parameters = parameters,
    targetRunner = target_runner_xy,
    logFile = logFile,
    seed = seed,
    quiet = TRUE,
    maxExperiments = maxExperiments)

  expect_silent(confs <- irace(scenario = scenario))

  scenario$targetRunner <- wrap_target_runner_error(target_runner_xy, limit)
  # Otherwise, the tests are too fast.
  with_mocked_bindings({
    expect_error(irace(scenario = scenario), "== irace == The cost returned by targetRunner is not numeric")
  },
  .irace_minimum_saving_time = 0
  )

  logFile_new <- withr::local_tempfile(pattern = "irace", fileext = ".Rdata")
  scenario <- modifyList(scenario, list(
    targetRunner = wrap_target_runner_counter(target_runner_xy),
    recoveryFile = logFile,
    seed = NA,
    quiet = FALSE,
    logFile = logFile_new))
  recover_confs <- irace(scenario = scenario)
  expect_identical(confs, recover_confs)
  expect_lt(environment(scenario$targetRunner)$counter, maxExperiments - 50)
})

test_that("recovery maxTime", {
  target_runner <- function(experiment, scenario) {
    configuration     <- experiment$configuration
    tmax <-  configuration[["tmax"]]
    temp <-  configuration[["temp"]]
    time <- max(1, abs(rnorm(1, mean=(tmax+temp)/10)))
    list(cost = time, time = time)
  }
  parameters <- readParameters(text = '
   tmax "" i (1, 50)
   temp "" r (0, 10)
   ')
  logFile <- withr::local_tempfile(pattern = "irace", fileext = ".Rdata")
  seed <- 1234567

  scenario <- list(targetRunner = target_runner,
    instances = 1:10,
    seed = seed,
    maxTime = 500,
    logFile = logFile,
    quiet = TRUE,
    parameters = parameters)
  expect_silent(confs <- irace(scenario = scenario))

  scenario$targetRunner <- wrap_target_runner_error(target_runner, 200L)
  # Otherwise, the tests are too fast.
  with_mocked_bindings({
    expect_error(irace(scenario = scenario), "== irace == The cost returned by targetRunner is not numeric")
  },
  .irace_minimum_saving_time = 0
  )
  
  logFile_new <- withr::local_tempfile(pattern = "irace", fileext = ".Rdata")
  scenario <- modifyList(scenario, list(
    targetRunner = wrap_target_runner_counter(target_runner),
    recoveryFile = logFile,
    seed = NA,
    quiet = FALSE,
    logFile = logFile_new))
  recover_confs <- irace(scenario = scenario)
  expect_identical(confs, recover_confs)
  expect_lt(environment(scenario$targetRunner)$counter,
    nrow(read_logfile(logFile_new)$state$experiment_log) - 150L)
})
  
}) # withr::with_output_sink()
