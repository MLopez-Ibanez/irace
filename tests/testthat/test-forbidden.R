withr::with_output_sink("test-forbidden.Rout", {

test_that("checkForbidden", {
  
test.checkForbidden <- function(param.file)
{
  params <- readParameters(param.file)
  confs <- readConfigurationsFile("configurations.txt", params)
  exp.confs <- readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        6     "x1"   3.5   "low"
NA        NA   "x3"   4.5   "low"
', parameters = params)
  confs <- irace:::checkForbidden(confs, params$forbidden)
  rownames(confs) <- rownames(exp.confs) <- NULL
  expect_equal(confs, exp.confs)
}
  test.checkForbidden(test_path("parameters.txt"))
  test.checkForbidden(test_path("logparameters.txt"))
})

test_that("filter_forbidden", {

test_filter_forbidden <- function(param_file)
{
  params <- readParameters(param_file)
  confs <- readConfigurationsFile("configurations.txt", params)
  exp_confs <- data.table::as.data.table(readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        6     "x1"   3.5   "low"
NA        NA   "x3"   4.5   "low"
', parameters = params))
  confs <- irace:::filter_forbidden(data.table::as.data.table(confs), params$forbidden)
  expect_equal(confs, exp_confs)
}
  test_filter_forbidden(test_path("parameters.txt"))
  test_filter_forbidden(test_path("logparameters.txt"))
})

test_that("retries", {
  params <- readParameters(text='
p0 "" r (0,1)
[forbidden]
p0 > 0.9
')
  confs <- irace:::sampleUniform(params, 50)
  expect_equal(nrow(confs), 50L)
})

test_that("max retries", {
  params <- readParameters(text='
p0 "" r (0,1)
[forbidden]
p0 <= 0.5
p0 > 0.5
')
  expect_error(irace:::sampleUniform(params, 1), "perhaps your constraints are too strict")
})

})
