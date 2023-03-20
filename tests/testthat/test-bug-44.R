# https://github.com/MLopez-Ibanez/irace/issues/44
withr::with_output_sink("test-bug-44.Rout", {
test_that("bug 44", {
  readParameters(text='a "a" r (0.01, 0.99)', digits=2)
  readParameters(text='a "a" r (0, 1)', digits=1)
  expect_error(readParameters(text='a "a" r (0.01, 0.99)', digits=1), "must be representable")
  expect_error(readParameters(text='a "a" r,log (1e-8, 1)', digits=4), "must be representable")
})
})
