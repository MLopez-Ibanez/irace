library(irace)
context("similarConfigurations")
test_that("similarConfigurations", {

  parameters <- irace:::readParameters(text = '
n1 "" r (0,1)
n2 "" r (0,1)
n3 "" r (0,1)
c1 "" c ("a","b")
c2 "" c ("a","b")
c3 "" c ("a","b")
')
  confs <- read.table(header = TRUE, stringsAsFactors = FALSE, text = '
.ID. n1 n2 n3 c1 c2 c3
1    0.5  0.5  0.5      "a" "a" "a"
2    NA  0.5  0.5       "a" "a" "a"
3    0.5  0.5  0.5      "a" "a" "b"
4    0.5  0.501  0.5    "a" "a" "a"
5    0.5  0.5    0.499  "a" "a" "a"
6    0.5  0.5  0.5      "a" "a" "a"
7    0.5  0.5  0.5      "a"  NA "a"
8    0.5  0.1  0.5      "a" "a" "a"
9    0.5  0.49  0.5     "a" "a" "a"
10   0.5  0.5  0.5      "a" "a"  NA
11   0.5  0.5  0.5      "a" "a"  NA
12    NA  0.5  0.5      "a" "a" "a"
')
  expect_identical(
    irace:::similarConfigurations(confs[1:10,], parameters, threshold = 0.001),
    as.integer(c(1,6)))
  expect_identical(
    irace:::similarConfigurations(confs[1:10,], parameters, threshold = 0.01),
    as.integer(c(1,4,5,6)))
  expect_identical(
    irace:::similarConfigurations(confs[1:10,], parameters, threshold = 0.1),
    as.integer(c(1,4,5,6,9)))
  expect_identical(
    # FIXME: The output should be already sorted
    sort(irace:::similarConfigurations(confs, parameters, threshold = 0.001)),
    as.integer(c(1,2,6,10,11,12)))
})

