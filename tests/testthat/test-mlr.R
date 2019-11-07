context("mlr")

test_that("mlr", {
  skip_on_cran()
  suppressWarnings(skip_if_not_installed("mlr", minimum_version = "2.15.0"))
  require(mlr, quietly = TRUE)

  ps = ParamHelpers::makeParamSet(
    ParamHelpers::makeNumericParam("cp", lower = 0.1, upper = 1),
    ParamHelpers::makeIntegerParam("minsplit", lower = 1, upper = 10))

  lrn = mlr::makeLearner("classif.rpart")
  
  ctrl = mlr::makeTuneControlIrace(maxExperiments = 30, nbIterations = 1L, minNbSurvival = 1)
  tr = mlr::tuneParams(lrn, mlr::iris.task, mlr::hout, par.set = ps, control = ctrl)
  expect_true(TRUE)
})
