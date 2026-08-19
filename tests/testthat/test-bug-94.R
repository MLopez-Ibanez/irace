withr::with_output_sink("test-bug-94.Rout", {

test_that("bug-94", {
  param <- structure(list(domain = c("a", "b", "c")), class = "ParamOrd")
  sampled <- sample_model.ParamOrd(param, n = 200, model = list(1, NA_real_))
  expect_false(anyNA(sampled))
  expect_true(all(sampled %in% param$domain))
})

test_that("bug-94 integration: conditional ParamOrd never sampled as NA", {
  parameters <- parametersNew(
    param_cat(name = "use", values = c("0", "1")),
    param_ord(name = "level", values = c("a", "b", "c"), condition = 'use == "1"'),
    param_real(name = "x", lower = 0, upper = 1)
  )

  bad <- 0L
  target_runner <- function(experiment, scenario) {
    conf <- experiment$configuration
    if (conf[["use"]] == "1" && is.na(conf[["level"]])) bad <<- bad + 1L
    list(cost = runif(1))
  }

  scenario <- checkScenario(defaultScenario(list(
    targetRunner = target_runner, parameters = parameters,
    instances = as.character(1:6), maxExperiments = 400, seed = 1, logFile = ""
  )))
  invisible(irace(scenario = scenario))
  expect_equal(bad, 0L)
})

}) # withr::with_output_sink()
