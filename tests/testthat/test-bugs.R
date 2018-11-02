library(irace)
context("bugs")
test_that("bug_large_new_instances", {
  skip_on_cran()

  load("bug_large_new_instances.Rdata", verbose = TRUE)
  scenario$targetRunner <- function(experiment, scenario) {
    instancesList <- dynGet("saved_instances_list", inherits = TRUE)
    experiments <- dynGet("saved_experiments", inherits = TRUE)
    row <- which(instancesList$instance == experiment$id.instance
                 & instancesList$seed == experiment$seed)
    return(list(cost = experiments[row, experiment$id.configuration]))
  }
  confs <- irace(scenario = scenario, parameters = parameters)
  expect_true(nrow(confs) > 0)
})
