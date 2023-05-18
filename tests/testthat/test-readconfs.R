withr::with_output_sink("test-readconfs.Rout", {

params <- irace::readParameters("parameters.txt")

expect_error_readconfs <- function(table, exp)
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
    expect_error_readconfs('
param1 param2 mode   real mutation fixed
5        NA    "x2"   4.0   "low"  fixed
1        NA    "x2"   4.0   "low"  fixed
NA       NA   "x3"   4.5   "low"   wrong', "is not among the valid values")
  })
  
  test_that("checkDuplicates", {
  expect_error_readconfs('
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
NA        NA   "x3"   4.5   "low"
', "Duplicated")

  expect_error_readconfs('
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', "Duplicated")

})

test_that("parameter values", {
  expect_error_readconfs('
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.0   "low"
11      NA    "x2"   4.0   "low"
', "is not within the valid range")
  expect_error_readconfs('
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5001 "low"
', "is not within the valid range")
  expect_error_readconfs('
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1.1     NA    "x2"   4.5   "low"
', "is not an integer")
  expect_error_readconfs('
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5   "lower"
', "is not among the valid values")
  expect_error_readconfs('
param1 param2 mode   real mutation
5       NA    "x3"   4.0   "low"
1       NA    "x2"   4.5   "low"
', "is not enabled")
})

test_that("parameter names", {
  expect_error_readconfs('
param1 param0 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', "do not match")
  expect_error_readconfs('
param1 mode   real mutation
5       "x2"   4.0   "low"
1       "x2"   4.0   "low"
', "are missing")
  expect_error_readconfs('
param1 param2 mode   real mutation param3
5        NA    "x2"   4.0   "low"  NA
1        NA    "x2"   4.0   "low"  NA
', "do not match")
})

}) # withr::with_output_sink
