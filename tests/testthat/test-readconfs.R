context("Test read configurations")

withr::with_output_sink("test-readconfs.Rout", {

test_that("checkDuplicates", {
  params <- irace:::readParameters("parameters.txt")
  
  expect_error(irace:::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
NA        NA   "x3"   4.5   "low"
', parameters = params), "Duplicated")

  expect_error(irace:::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', parameters = params), "Duplicated")

})

})

