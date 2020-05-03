if (!require(covr)) {
  install.packages("covr")
  library(covr)
}
covr::codecov(type="all", quiet=FALSE)
