withr::with_output_sink("test-argparser.Rout", {
test_that("argparse", {
  params_def <- data.frame(name =".param", type ="s", short = "-p", long=NA, default=NA, domain=NA, description="")
  parser <- irace:::CommandArgsParser$new("-p 'something something'", params_def)
  expect_length(parser$argv, 2L)
})
})
