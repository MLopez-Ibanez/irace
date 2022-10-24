context("bug")
# https://github.com/MLopez-Ibanez/irace/issues/44
withr::with_output_sink("test-bug-44.Rout", {
  readParameters(text='a "a" r (0.01, 0.99)', digits=2)
  expect_error(readParameters(text='a "a" r (0.01, 0.99)', digits=1), "must be representable")
})
