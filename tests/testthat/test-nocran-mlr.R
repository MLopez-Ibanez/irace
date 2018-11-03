context("mlr")

test_that("mlr", {
  skip_on_cran()
  suppressWarnings(library(mlr))

  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.1, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  lrn = makeLearner("classif.rpart")
  n = 30
  ctrl = makeTuneControlIrace(maxExperiments = n, nbIterations = 1L, minNbSurvival = 1)
  tr = tuneParams(lrn, iris.task, hout, par.set = ps, control = ctrl)
  expect_true(TRUE)
})
