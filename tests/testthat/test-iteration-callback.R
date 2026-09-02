callback_scenario <- function(log_file)
{
  parameters <- readParameters(text = 'x "" i (1, 10)')
  list(
    targetRunner = function(experiment, scenario) {
      list(cost = abs(experiment$configuration[["x"]] - experiment$instance))
    },
    instances = seq_len(10L),
    maxExperiments = 100L,
    seed = 1234567L,
    quiet = TRUE,
    postselection = FALSE,
    logFile = log_file,
    parameters = parameters
  )
}

test_that("iterationCallback receives named arguments after every iteration", {
  log_file <- withr::local_tempfile(fileext = ".Rdata")
  snapshots <- list()
  # The arguments are deliberately reordered to verify that irace calls the
  # callback by name. The progress argument is collected through ... to ensure
  # callbacks remain compatible when irace adds named arguments.
  callback <- function(elites, iteration, ...) {
    dots <- list(...)
    expect_named(dots, "progress")
    snapshots[[iteration]] <<- list(
      elites = data.table::copy(elites),
      progress = dots$progress)
    runif(1L)
    data.table::set(elites, i = 1L, j = ".ID.", value = -1L)
  }

  scenario <- callback_scenario(log_file)
  scenario$iterationCallback <- callback
  with_callback <- irace(scenario)
  results <- read_logfile(log_file)

  expect_length(snapshots, length(results$allElites))
  for (iteration in seq_along(snapshots)) {
    progress <- snapshots[[iteration]]$progress
    expect_identical(
      snapshots[[iteration]]$elites[[".ID."]],
      results$allElites[[iteration]])
    expect_named(progress, c(
      "nbIterations", "maxExperiments", "experimentsUsed",
      "remainingBudget", "remainingBudgetEstimated", "currentBudget",
      "currentBudgetUsed", "maxTime", "timeUsed", "remainingTime",
      "boundEstimate"))
    expect_gte(progress$nbIterations, iteration)
    expect_identical(progress$maxExperiments, 100L)
    expect_identical(progress$experimentsUsed + progress$remainingBudget,
                     progress$maxExperiments)
    expect_false(progress$remainingBudgetEstimated)
    expect_gte(progress$currentBudget, progress$currentBudgetUsed)
    expect_gt(progress$currentBudgetUsed, 0L)
    expect_identical(progress$maxTime, 0L)
    expect_identical(progress$timeUsed, 0)
    expect_true(is.na(progress$remainingTime))
    expect_true(is.na(progress$boundEstimate))
  }
  expect_true(all(with_callback[[".ID."]] > 0L))

  without_callback <- irace(
    callback_scenario(withr::local_tempfile(fileext = ".Rdata")))
  expect_identical(with_callback, without_callback)
})

test_that("iterationCallback reports time-budget progress", {
  snapshots <- list()
  callback <- function(iteration, elites, progress, ...) {
    snapshots[[iteration]] <<- progress
  }
  scenario <- callback_scenario("")
  scenario$maxExperiments <- 0L
  scenario$maxTime <- 100
  scenario$targetRunner <- function(experiment, scenario) {
    list(
      cost = abs(experiment$configuration[["x"]] - experiment$instance),
      time = 1)
  }
  scenario$iterationCallback <- callback

  irace(scenario)

  expect_gt(length(snapshots), 0L)
  for (progress in snapshots) {
    expect_identical(progress$maxExperiments, 0L)
    expect_identical(progress$maxTime, 100L)
    expect_true(progress$remainingBudgetEstimated)
    expect_gt(progress$experimentsUsed, 0L)
    expect_gte(progress$currentBudget, progress$currentBudgetUsed)
    expect_equal(progress$remainingTime,
                 progress$maxTime - progress$timeUsed)
    expect_equal(progress$boundEstimate,
                 progress$timeUsed / progress$experimentsUsed)
  }
})

test_that("iterationCallback must be NULL or a function", {
  scenario <- callback_scenario("")
  scenario$iterationCallback <- "invalid"
  expect_error(
    irace(scenario),
    "'iterationCallback' must be a function",
    fixed = TRUE
  )
})
