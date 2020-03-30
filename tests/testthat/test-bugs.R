context("bugs")

withr::with_output_sink("test-bugs.Rout", {

run_irace_from_rds <- function(rds_filename)
{
  # FIXME: For some unknown reason, this test doesn't work in R 3.6
  skip_if(grepl("^3.6", getRversion()))

  scenario <- readRDS(rds_filename)$scenario
  parameters <- readRDS(rds_filename)$parameters
    
  scenario$targetRunner <- function(experiment, scenario, filename = rds_filename) {
    saved <- readRDS(filename)
    row <- which(saved$instancesList[, "instance"] == experiment[["id.instance"]]
                 & saved$instancesList[, "seed"] == experiment[["seed"]])
    cost <- saved$experiments[row, experiment[["id.configuration"]] ]
    if (is.na(cost)) {
      print(row)
      print(experiment)
    }
    expect_false(is.na(cost))
    return(list(cost = cost))
  }
  confs <- irace(scenario = scenario, parameters = parameters)
  expect_gt(nrow(confs), 0L)
}

test_that("bug_large_new_instances", {
  skip_on_cran()
  # FIXME: For some unknown reason, this test doesn't work in R 3.6
  skip_if(grepl("^3.6", getRversion()))
  # FIXME: Convert this test to use run_irace_from_rds()
  load("bug_large_new_instances.Rdata", verbose = TRUE)

  scenario$targetRunner <- function(experiment, scenario) {
    saved_instances_list <- dynGet("saved_instances_list", inherits = TRUE)
    saved_experiments <- dynGet("saved_experiments", inherits = TRUE)
    row <- which(saved_instances_list[, "instance"] == experiment[["id.instance"]]
                 & saved_instances_list[, "seed"] == experiment[["seed"]])
    cost <- saved_experiments[row, experiment[["id.configuration"]] ]
    if (is.na(cost)) {
      print(row)
      print(experiment)
    }
    expect_false(is.na(cost))
    return(list(cost = cost))
  }
  confs <- irace(scenario = scenario, parameters = parameters)
  expect_gt(nrow(confs), 0L)
})

test_that("target.runner as string", {

  target.runner.local <- function(experiment, scenario) return(list(cost=1L))

  expect_true(irace:::is.function.name("target.runner.local"))

  # Test that a function can be given as a string.
  scenario <- list(targetRunner = "target.runner.local",
                   instances = 1:10, maxExperiments = 1000)
  scenario <- checkScenario (scenario)

  expect_equal(scenario$targetRunner, target.runner.local)
  expect_is(scenario$targetRunner, "function")
})

target.runner.global <- function(experiment, scenario) return(list(cost=1L))

test_that("target.runner as string (global)", {

  expect_true(irace:::is.function.name("target.runner.global"))

  # Test that a function can be given as a string.
  scenario <- list(targetRunner = "target.runner.global",
                   instances = 1:10, maxExperiments = 1000)
  scenario <- checkScenario (scenario)

  expect_equal(scenario$targetRunner, target.runner.global)
  expect_is(scenario$targetRunner, "function")
})


test_that("ordered assert", {
  library(irace)
  parameters <- readParameters(text='
x "" o (a,b,c,d)
')
  confs <- irace:::sampleUniform(parameters, 1, 0)
  confs$.ID. <- 1
  model <- irace:::initialiseModel(parameters, confs, 0)
  confs <- irace:::sampleModel(parameters, confs, model, 1, 0)
  expect_true(confs$x %in% parameters$domain$x)
})

test_that("maxim_bug", {
  skip_on_cran()
  run_irace_from_rds("saved_maxim_bug.rds")
})

test_that("maxim_bug2", {
  skip_on_cran()

  run_irace_from_rds("saved_maxim_bug2.rds")
})
}) # withr::with_output_sink()

