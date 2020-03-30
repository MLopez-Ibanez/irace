# This requires the installation of https://github.com/automl/GenericWrapper4AC
context("GenericWrapper4AC")

test_that("GenericWrapper4AC", {
  skip_on_cran()
  skip_on_ci()
  skip_on_travis()
  # FIXME: how to check that a python package is installed?
  
  parameters <- readParameters(text = '
   cost "cost" r (0.1, 1.00)
   runtime "runtime" r (0.1, 1.00)')

  configurations <- data.frame(.ID. = 1, cost = 0.5, runtime = 0.8)

  instances = c("time", "cost", "cost+time")
  names(instances) = instances
  scenario <- list(targetRunner = "./dummy_wrapper.py", instances = instances,
                   maxExperiments = 1000, aclib = TRUE)

  scenario <- checkScenario (scenario)
  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("cost", 2),
                                              seeds = 0:1, scenario)
  output <- irace:::execute.experiments(experiments, scenario)
  expect_equal(output[[1]]$status, "SUCCESS")
  expect_equal(output[[1]]$cost, 0.5)
  expect_true(is.null(output[[1]]$error))
  expect_equal(output[[2]]$status, "TIMEOUT")
  expect_equal(output[[2]]$cost, 0.5)
  expect_true(is.null(output[[2]]$error))
  
  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("cost", 2),
                                              seeds = 2, scenario)
  expect_error(irace:::execute.experiments(experiments, scenario), "CRASHED")
               
  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("cost", 2),
                                              seeds = 3, scenario)
  expect_error(irace:::execute.experiments(experiments, scenario), "ABORT")
  
  
  scenario <- modifyList(scenario,
                         list(capping = TRUE, boundMax = 1, maxTime = 10,
                              maxExperiments = NULL))
  scenario <- checkScenario (scenario)
  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("time", 2),
                                              seeds = 0:1, scenario,
                                              bounds = scenario$boundMax)
  output <- irace:::execute.experiments(experiments, scenario)

  expect_equal(output[[1]]$status, "SUCCESS")
  expect_equal(output[[1]]$cost, 0.8)
  expect_equal(output[[1]]$time, 0.8)
  expect_match(output[[1]]$call, "--cutoff")
  expect_true(is.null(output[[1]]$error))
  expect_equal(output[[2]]$status, "TIMEOUT")
  expect_equal(output[[2]]$cost, 0.8)
  expect_equal(output[[2]]$time, 0.8)
  expect_match(output[[2]]$call, "--cutoff")
  expect_true(is.null(output[[2]]$error))

  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("time", 2),
                                              seeds = 2, scenario,
                                              bounds = scenario$boundMax)
  expect_error(irace:::execute.experiments(experiments, scenario), "CRASHED")
               
  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("time", 2),
                                              seeds = 3, scenario,
                                              bounds = scenario$boundMax)
  expect_error(irace:::execute.experiments(experiments, scenario), "ABORT")

  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("cost+time", 2),
                                              seeds = 0:1, scenario,
                                              bounds = scenario$boundMax)
  output <- irace:::execute.experiments(experiments, scenario)

  expect_equal(output[[1]]$status, "SUCCESS")
  expect_equal(output[[1]]$cost, 0.5)
  expect_equal(output[[1]]$time, 0.8)
  expect_match(output[[1]]$call, "--cutoff")
  expect_true(is.null(output[[1]]$error))
  expect_equal(output[[2]]$status, "TIMEOUT")
  expect_equal(output[[2]]$cost, 0.5)
  expect_equal(output[[2]]$time, 0.8)
  expect_match(output[[2]]$call, "--cutoff")
  expect_true(is.null(output[[2]]$error))

  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("cost+time", 2),
                                              seeds = 2, scenario,
                                              bounds = scenario$boundMax)
  expect_error(irace:::execute.experiments(experiments, scenario), "CRASHED")
               
  experiments <- irace:::createExperimentList(configurations,
                                              parameters,
                                              instances = scenario$instances,
                                              instances.ID = rep("cost+time", 2),
                                              seeds = 3, scenario,
                                              bounds = scenario$boundMax)
  expect_error(irace:::execute.experiments(experiments, scenario), "ABORT")

})
