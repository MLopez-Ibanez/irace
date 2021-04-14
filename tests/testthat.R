if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(irace)
  test_check("irace", reporter = c("summary","check"))
}


