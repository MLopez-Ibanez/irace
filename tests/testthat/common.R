generate.set.seed <- function()
{
  seed <- sample(2^30, 1)
  cat("Seed: ", seed, "\n")
  set.seed(seed)
}

