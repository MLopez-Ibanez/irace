# This file is loaded automatically by testthat.
generate_set_seed <- function()
{
  seed <- sample(2^30, 1)
  cat("Seed: ", seed, "\n")
  set.seed(seed)
}

test_irace_detectCores <- function()
{
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return(1L)
  if (identical(Sys.getenv("COVR_COVERAGE"), "true")) return(1L)
  x <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(x) && x == "TRUE") return(2L)
  parallel::detectCores()
}

skip_on_coverage <- function() {
  if (identical(Sys.getenv("COVR_COVERAGE"), "true"))
    skip("Not run during coverage.")
  else
    invisible()
}

system_os_is_windows <- function() .Platform$OS.type == "windows"

## Functions ##########################################################
f_ackley <- function (x,y, nsize = 0.01) {
  # Transformation of parameter values 
  # from [0,1] to [vmin,vmax]
  vmin <- -5
  vmax <- 5
  x <- (x*(vmax-vmin)) + vmin
  y <- (y*(vmax-vmin)) + vmin
  
  a <- -20 * exp (-0.2 * sqrt(0.5 * (x^2 + y^2)))
  b <- exp(0.5 * (cos(2*pi*x) + cos(2*pi*y)))  
  f <- a - b + exp(1) + 20
  # Simulating stochasticity
  noise <- runif(1, min = (1-nsize), max = (1+nsize))
  f <- f*noise
  
  # Transform result from [fmin,fmax]
  # to [0,100]
  fmin <- 0
  fmax <- 15*(1+nsize)
  ((f - fmin) / (fmax-fmin)) * (100-0) + 0
}

f_goldestein_price <- function (x,y, nsize = 0.01) {
  # Transformation of parameter values 
  # from [0,1] to [vmin,vmax]
  vmin <- -2
  vmax <- 2
  x <- (x*(vmax-vmin)) + vmin
  y <- (y*(vmax-vmin)) + vmin
  
  a <- 1 + ((x + y + 1)^2) * (19 - 14*x + 3*x^2 - 14*y + 6*x*y + 3*y^2)
  b <- 30 + ((2*x - 3*y)^2) * (18 - 32*x + 12*x^2 + 48*y - 36*x*y + 27*y^2)
  f <- a*b
  # Simulating stochasticity
  noise <- runif(1, min = (1-nsize), max = (1+nsize))
  f <- f*noise

  # Transform result from [fmin,fmax] 
  # to [0,100]
  fmin <- 0
  fmax <- 1000000*(1+nsize)
  ((f - fmin) / (fmax-fmin)) * (100-0) + 0
}

f_matyas <- function (x,y, nsize = 0.01) {
  # Trasfomation of parameter values 
  # from [0,1] to [vmin,vmax]
  vmin <- -10
  vmax <- 10
  x <- (x*(vmax-vmin)) + vmin
  y <- (y*(vmax-vmin)) + vmin
  
  f <- 0.26 * (x^2 + y^2) - (0.48*x*y)
  # Simulating stochasticity
  noise <- runif(1, min = (1-nsize), max = (1+nsize))
  f <- f*noise

  # Transform result from [fmin,fmax] 
  # to [0,100]
  fmin <- 0
  fmax <- 100*(1+nsize)
  ((f - fmin) / (fmax-fmin)) * (100-0) + 0
}

f_himmelblau <- function (x,y, nsize = 0.01) {  
  # Trasfomation of parameter values 
  # from [0,1] to [vmin,vmax]
  vmin <- -5
  vmax <- 5
  x <- (x*(vmax-vmin)) + vmin
  y <- (y*(vmax-vmin)) + vmin
  
  f <- (x^2 + y - 11)^2 + (x + y^2 - 7)^2
  # Simulating stochasticity
  noise <- runif(1, min = (1-nsize), max = (1+nsize))
  f <- f*noise
  
  # Transform result from [fmin,fmax] 
  # to [0,100]
  fmin <- 0
  fmax <- 2000*(1+nsize)
  ((f - fmin) / (fmax-fmin)) * (100-0) + 0
}

target_runner_capping_xy <- function(experiment, scenario)
{
  configuration <- experiment$configuration
  instance      <- experiment$instance
  bound         <- experiment$bound

  x <- configuration[["x"]]
  y <- configuration[["y"]]
  value <- switch(instance,
                  ackley     = f_ackley(x, y),
                  goldestein = f_goldestein_price(x, y),
                  matyas     = f_matyas(x, y),
                  himmelblau  = f_himmelblau(x, y))
  
  # Simulate execution bound
  list(cost = value, time=min(value + 0.1, bound), call = toString(experiment))
}

irace_capping_xy <- function(...)
{
  args <- list(...)
  parameters_table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   reject "" c (0,1)'
  
  parameters <- readParameters(text = parameters_table)
  logFile <- withr::local_tempfile(fileext=".Rdata")
  scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target_runner_capping_xy,
                   capping = TRUE,
                   boundMax = 80,
                   testType = "t-test",
                   logFile = logFile,
                   parallel = if (system_os_is_windows()) 1L else test_irace_detectCores(),
                   parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  irace:::checkTargetFiles(scenario = scenario)
  
  confs <- irace(scenario = scenario)
  best_conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]),
                   best_conf)
}
