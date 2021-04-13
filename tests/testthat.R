library(testthat)
library(irace)
options(
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE
)
test_check("irace", reporter = c("summary","check"))

