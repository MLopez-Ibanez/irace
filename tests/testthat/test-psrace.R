withr::with_output_sink("test-psrace.Rout", {

test_that("max_experiments=0.1", {
  skip_on_cran()
  generate_set_seed()
  irace_log <- read_logfile(system.file(package="irace", "exdata", "sann.rda"))
  # Use a temporary file to not change the original "sann.rda".
  psrace_logFile <- withr::local_tempfile(fileext = ".Rdata")
  # Execute the post-selection after the execution of irace. Use 10% of the total budget.
  psRace(irace_log, max_experiments=0.1, psrace_logFile = psrace_logFile)
  irace_log <- read_logfile(psrace_logFile)
  budget <- nrow(irace_log$state$experiment_log[iteration == max(iteration)])
  expect_gt(budget, 50L)
  # It should be equal but elitist_race sometimes fails to consume all the budget.
  expect_lte(budget, irace_log$psrace_log$max_experiments)
})

test_that("max_experiments=101", {
  skip_on_cran()
  generate_set_seed()
  irace_log <- read_logfile(system.file(package="irace", "exdata", "sann.rda"))
  conf_ids <- unlist(tail(irace_log$allElites, n=1L))
  # Use a temporary file to not change the original "sann.rda".
  psrace_logFile <- withr::local_tempfile(fileext = ".Rdata")
  # Execute the post-selection after the execution of irace. 
  psRace(irace_log, max_experiments=101, conf_ids = conf_ids, psrace_logFile = psrace_logFile)
  irace_log <- read_logfile(psrace_logFile)
  budget <- nrow(irace_log$state$experiment_log[iteration == max(iteration)])
  expect_gt(budget, 10L)
  # It should be equal but elitist_race sometimes fails to consume all the budget.
  expect_lte(budget, irace_log$psrace_log$max_experiments)
})

}) # withr::with_output_sink()
