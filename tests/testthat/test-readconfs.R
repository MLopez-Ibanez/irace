withr::with_output_sink("test-readconfs.Rout", {

params <- irace::readParameters("parameters.txt")

test_that("checkDuplicates", {
  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
NA        NA   "x3"   4.5   "low"
', parameters = params), "Duplicated")

  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', parameters = params), "Duplicated")

})

test_that("parameter values", {
  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.0   "low"
11      NA    "x2"   4.0   "low"
', parameters = params), "is not within the valid range")
  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5001 "low"
', parameters = params), "is not within the valid range")
  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1.1     NA    "x2"   4.5   "low"
', parameters = params), "is not an integer")
  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5   "lower"
', parameters = params), "is not among the valid values")
  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5       NA    "x3"   4.0   "low"
1       NA    "x2"   4.5   "low"
', parameters = params), "is not enabled")
})

test_that("parameter names", {
  expect_error(irace::readConfigurationsFile(text='
param1 param0 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', parameters = params), "do not match")
  expect_error(irace::readConfigurationsFile(text='
param1 mode   real mutation
5       "x2"   4.0   "low"
1       "x2"   4.0   "low"
', parameters = params), "are missing")
  expect_error(irace::readConfigurationsFile(text='
param1 param2 mode   real mutation param3
5        NA    "x2"   4.0   "low"  NA
1        NA    "x2"   4.0   "low"  NA
', parameters = params), "do not match")
})

}) # withr::with_output_sink
