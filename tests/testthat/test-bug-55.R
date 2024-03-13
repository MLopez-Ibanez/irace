# https://github.com/MLopez-Ibanez/irace/issues/55
withr::with_output_sink("test-bug-55.Rout", {
test_that("bug-55", {
    
  parameters_txt <- '
foo "--foo " i (0, 1)
foo2 "--foo2 " c (true, false) | foo == 0
'
  params <- irace::readParameters(text=parameters_txt)
  configurations_txt <- '
  foo  foo2
1   0 false
2   1  <NA>
'
  confs <- readConfigurationsFile(text=configurations_txt, parameters = params)
  expect_equal(confs, data.frame(foo=c(0L,1L), foo2=c("false",NA), row.names=c("1", "2"), stringsAsFactors=FALSE))
})
})

