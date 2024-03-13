withr::with_output_sink("test-multi_irace.Rout", {

make.target.runner <- function(parameters) {
  function(experiment, scenario) {
    cost <- max(1, abs(rnorm(1, mean=sum(unlist(experiment$configuration[parameters])))))
    list(cost = cost, call = toString(experiment))
  }
}

make.parameters.table <- function(parameters)
  readParameters(text = sprintf('%s "" r (0, 10)', parameters))


make.scenario <- function(targetRunner, maxExperiments = 1000) {
  list(targetRunner = targetRunner, maxExperiments = maxExperiments, instances = rnorm(200, mean = 0.9, sd = 0.02))
}

target.runner.1 <- make.target.runner(c("x1", "x2"))
target.runner.2 <- make.target.runner(c("x2", "x3"))
target.runner.3 <- make.target.runner(c("x3", "x1"))

parameters.table.1 <- make.parameters.table(c("x1", "x2"))
parameters.table.2 <- make.parameters.table(c("x2", "x3"))
parameters.table.3 <- make.parameters.table(c("x3", "x1"))

check.default.logFiles <- function(n) {
  for (i in 1:n) {
    dir <- sprintf("run_%02d", i)
    expect_true(dir.exists(dir))
    expect_true(file.exists(file.path(dir, "irace.Rdata")))
  }
}

test_that("multiple scenarios, multiple parameters", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()

  scenarios <- lapply(list(target.runner.1, target.runner.2, target.runner.3), make.scenario)
  parameters <- list(parameters.table.1, parameters.table.2, parameters.table.3)

  runs <- multi_irace(scenarios, parameters)

  expect_length(runs, 3)
  check.default.logFiles(3)
})

test_that("one scenario, multiple parameters", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()

  scenarios <- list(make.scenario(target.runner.1))
  dummy_parameters_names <- c("dummy1", "dummy2", "dummy3")
  parameter_names <- lapply(dummy_parameters_names, function(dummy) c("x1", "x2", dummy))
  parameters <- lapply(parameter_names, make.parameters.table)

  runs <- multi_irace(scenarios, parameters)
  expect_length(runs, 3)
  check.default.logFiles(3)
})

test_that("multiple scenarios, one parameters", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()

  scenarios <- lapply(list(100, 500, 1000), function(maxExperiments) make.scenario(target.runner.1, maxExperiments) )
  parameters <- list(parameters.table.1)

  runs <- multi_irace(scenarios, parameters)
  expect_length(runs, 3)
  check.default.logFiles(3)
})

test_that("one scenario, one parameters, multiple n", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()

  scenarios <- list(make.scenario(target.runner.1))
  parameters <- list(parameters.table.1)
  runs <- multi_irace(scenarios, parameters, n = 3)
  expect_length(runs, 3)
  check.default.logFiles(3)
})

test_that("multiple scenarios, multiple parameters, multiple n", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()

  scenarios <- lapply(list(target.runner.1, target.runner.2, target.runner.3), make.scenario)
  parameters <- list(parameters.table.1, parameters.table.2, parameters.table.3)

  runs <- multi_irace(scenarios, parameters, n = 3)
  expect_length(runs, 9)
  check.default.logFiles(9)
})

test_that("logFile not in execDir", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()

  execDir <- path_rel2abs("./multi_irace_execDir")
  logFileDir <- path_rel2abs("./multi_irace_logFileDir")

  dir.create(execDir, showWarnings = FALSE)
  dir.create(logFileDir, showWarnings = FALSE)

  scenarios <- lapply(list(target.runner.1, target.runner.2, target.runner.3), make.scenario)
  for (i in seq_along(scenarios)) {
    scenarios[[i]]$logFile <- file.path(logFileDir, "irace.Rdata")
    scenarios[[i]]$execDir <- execDir
  }

  parameters <- list(parameters.table.1, parameters.table.2, parameters.table.3)

  expect_error(multi_irace(scenarios, parameters))

  # expect_length(runs, 3)
  # for (i in 1:3) {
  #   logFile <- file.path(logFileDir, sprintf("irace_%02d.Rdata", i))
  #   expect_true(file.exists(logFile))
  # }
})

test_that("global seed", {
  skip_on_cran()

  scenarios <- lapply(list(target.runner.1, target.runner.2, target.runner.3), make.scenario)
  parameters <- list(parameters.table.1, parameters.table.2, parameters.table.3)

  runs.1 <- multi_irace(scenarios, parameters, global_seed = 42)
  runs.2 <- multi_irace(scenarios, parameters, global_seed = 42)

  expect_equal(runs.1, runs.2)
})

test_that("sequential and parallel identical", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if(test_irace_detectCores() <= 1L,
          message = "This test only makes sense if multiple cores are available")
  scenarios <- lapply(list(target.runner.1, target.runner.2, target.runner.3), make.scenario)
  parameters <- list(parameters.table.1, parameters.table.2, parameters.table.3)

  runs.sequential <- multi_irace(scenarios, parameters, global_seed = 42)
  runs.parallel <- multi_irace(scenarios, parameters, global_seed = 42, parallel = test_irace_detectCores())

  expect_equal(runs.sequential, runs.parallel)
})

}) # withr::with_output_sink()
