withr::with_output_sink("test-targeteval.Rout", {

  target_evaluator <- function(experiment, num_configurations, all_conf_id,
                               scenario, target_runner_call) {
    withr::local_seed(experiment$seed)
    list(cost = runif(1), call = deparse1(experiment))
  }

  parameters <- readParameters(text = '
algorithm       "--"                 c          (as,mmas,eas,ras,acs)
')

test_that("target_evaluator", {

  target_runner <- function(experiment, scenario) {
    list(call = deparse1(experiment))
  }
  seed <- sample.int(min(2147483647L, .Machine$integer.max), size = 1L, replace = TRUE)
  instances <- 1:10
  limit <- 58L
  maxExperiments <- 200
  logFile <- withr::local_tempfile(pattern = "irace", fileext = ".Rdata")
  
  scenario <- checkScenario(list(
    targetRunner = target_runner, targetEvaluator = target_evaluator,
    maxExperiments = maxExperiments, instances = instances,
    logFile = logFile,
    seed = seed,
    quiet = TRUE,
    parameters = parameters))

  expect_true(irace:::checkTargetFiles(scenario = scenario))

  scenario <- checkScenario(list(
    targetRunner = target_runner, targetEvaluator = target_evaluator,
    maxExperiments = maxExperiments, instances = instances,
    logFile = logFile,
    seed = seed,
    quiet = TRUE,
    parameters = parameters))
  
  expect_silent(confs <- irace(scenario = scenario))
  expect_gt(nrow(confs), 0L)
  scenario$targetRunner <- wrap_target_runner_error(target_runner, limit)
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
  expect_equal(environment(scenario$targetRunner)$counter, maxExperiments - limit + 1L)
})

test_that("target_evaluator maxTime", {

  target_runner <- function(experiment, scenario) {
    withr::local_seed(experiment$seed)
    list(time = min(experiment$bound, as.integer(1 + 10*runif(1))))
  }
  
  seed <- sample.int(min(2147483647L, .Machine$integer.max), size = 1L, replace = TRUE)
  instances <- 1:10
  limit <- 100L
  logFile <- withr::local_tempfile(pattern = "irace", fileext = ".Rdata")
  scenario <- checkScenario(list(
     targetRunner = target_runner, targetEvaluator = target_evaluator,
     maxTime = 2000, boundMax = 10, instances = instances,
     logFile = logFile, seed = seed,
     quiet = TRUE,
     parameters = parameters))
  expect_true(scenario$capping)
  expect_silent(confs <- irace(scenario = scenario))
  expect_gt(nrow(confs), 0L)

  scenario$targetRunner <- wrap_target_runner_error(target_runner, limit)
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
    sum(!is.na(read_logfile(logFile_new)$experiments)) - 10L)
})

}) # withr::with_output_sink()
