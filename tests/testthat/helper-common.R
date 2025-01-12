# This file is loaded automatically by testthat.
generate_set_seed <- function()
{
  seed <- sample.int(min(2147483647L, .Machine$integer.max), size = 1L, replace = TRUE)
  cat("Seed: ", seed, "\n")
  set.seed(seed)
}

test_irace_detectCores <- function()
{
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return(1L)
  # FIXME: covr takes a very long time on github actions otherwise.
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

expect_valid_configurations <- function(configurations, parameters)
{
  configurations <- irace:::removeConfigurationsMetaData(as.data.frame(configurations))
  output <- capture.output(print(configurations, width=5000L, row.names = FALSE))
  output <- paste0(output, "\n", collapse="")
  confs2 <- readConfigurationsFile(text=output, parameters=parameters)
  expect_equal(configurations, confs2)
}

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
  list(cost = value, time=min(value + 0.1, bound))
}

irace_capping_xy <- function(..., targetRunner = force(target_runner_capping_xy),
                             parallel = test_irace_detectCores())
{
  # Silence Error in `save(iraceResults, file = logfile, version = 3L)`: (converted from warning) 'package:irace' may not be available when loading
  # See https://github.com/r-lib/testthat/issues/2044
  if (!is.null(attr(environment(targetRunner), "name", exact=TRUE))) {
    environment(targetRunner) <- globalenv()
  }

  args <- list(...)
  parameters_table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   reject "" c (0,1)'

  parameters <- readParameters(text = parameters_table)
  logFile <- withr::local_tempfile(fileext=".Rdata")
  scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = targetRunner,
                   capping = TRUE,
                   boundMax = 80,
                   testType = "t-test",
                   logFile = logFile,
                   parallel = parallel,
                   parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  expect_true(irace:::checkTargetFiles(scenario = scenario))

  confs <- irace(scenario = scenario)
  best_conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]),
                   best_conf)
}

# Useful for testing recovery.
wrap_target_runner_error <- function(target_runner, limit)
{
  counter <- force(limit)
  target_runner <- force(target_runner)

  fun <- function(experiment, scenario) {
    counter <<- counter - 1L
    if (counter <= 0L)
      return(list(cost=NA))
    target_runner(experiment, scenario)
  }
  parent.env(environment(fun)) <- globalenv()
  fun
}

wrap_target_runner_counter <- function(target_runner)
{
  counter <- 0L
  target_runner <- force(target_runner)

  fun <- function(experiment, scenario) {
    counter <<- counter + 1L
    target_runner(experiment, scenario)
  }
  parent.env(environment(fun)) <- globalenv()
  fun
}
