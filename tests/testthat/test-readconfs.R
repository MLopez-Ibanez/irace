withr::with_output_sink("test-readconfs.Rout", {

params <- readParameters("parameters.txt")

expect_error_readconfs <- function(params, table, exp)
  expect_error(irace::readConfigurationsFile(text=table, parameters = params), exp)

test_that("no error", {
  x <- irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
NA       NA   "x3"   4.5   "low"', parameters=params)
  expect_equal(nrow(x), 3L)
  expect_equal(ncol(x), 6L) # It adds the fixed column
})

test_that("wrong fixed", {
  expect_error_readconfs(params, '
param1 param2 mode   real mutation fixed
5        NA    "x2"   4.0   "low"  fixed
1        NA    "x2"   4.0   "low"  fixed
NA       NA   "x3"   4.5   "low"   wrong', "is not among the valid values")
})

test_that("checkDuplicates", {
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
NA        NA   "x3"   4.5   "low"
', "Duplicated")

  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', "Duplicated")

})

test_that("parameter values", {
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.0   "low"
11      NA    "x2"   4.0   "low"
', "is not within the valid range")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5001 "low"
', "is not within the valid range")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1.1     NA    "x2"   4.5   "low"
', "is not an integer")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5   "lower"
', "is not among the valid values")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x3"   4.0   "low"
1       NA    "x2"   4.5   "low"
', "is not enabled")
})

test_that("parameter names", {
  expect_error_readconfs(params, '
param1 param0 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', "do not match")
  expect_error_readconfs(params, '
param1 mode   real mutation
5       "x2"   4.0   "low"
1       "x2"   4.0   "low"
', "are missing")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation param3
5        NA    "x2"   4.0   "low"  NA
1        NA    "x2"   4.0   "low"  NA
', "do not match")
})


test_that("conditional fixed", {

  params <- readParameters(text='
param1 "" c ("fixed")
param2 "" c ("cond_fixed") | param3 == "on"
param3 "" c ("on", "off")
')

  x <- irace::readConfigurationsFile(text='
param1 param3
fixed  off
', parameters=params)

  expect_equal(x[["param2"]], NA)

  x <- irace::readConfigurationsFile(text='
param1 param2 param3
fixed  NA     off
', parameters=params)

  x <- irace::readConfigurationsFile(text='
param1 param3
fixed  on
', parameters=params)

  expect_equal(x[["param2"]], "cond_fixed")

  expect_error_readconfs(params, '
param1 param2      param3
fixed  cond_fixed     off
', "is not enabled because of condition")
})

}) # withr::with_output_sink
