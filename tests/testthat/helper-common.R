# This file is loaded automatically by testthat.
generate.set.seed <- function()
{
  seed <- sample(2^30, 1)
  cat("Seed: ", seed, "\n")
  set.seed(seed)
}

test_irace_detectCores <- function()
{
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    x <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(x) && x == "TRUE") return(2L)
    return(parallel::detectCores())
  }
  return(1L)
}
