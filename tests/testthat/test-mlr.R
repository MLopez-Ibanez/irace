context("mlr")

test_that("mlr", {
  skip_on_cran()
  suppressWarnings(skip_if_not_installed("mlr", minimum_version = "2.14.1"))

  ps = mlr::makeParamSet(
    mlr::makeNumericParam("cp", lower = 0.1, upper = 1),
    mlr::makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  lrn = mlr::makeLearner("classif.rpart")
  n = 30
  ctrl = mlr::makeTuneControlIrace(maxExperiments = n, nbIterations = 1L, minNbSurvival = 1)
  tr = mlr::tuneParams(lrn, iris.task, hout, par.set = ps, control = ctrl)
  expect_true(TRUE)
})
