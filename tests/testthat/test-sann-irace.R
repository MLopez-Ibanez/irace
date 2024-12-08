withr::with_output_sink("test-sann-irace.Rout", {

## Functions ##########################################################
f_rosenbrock <- function (x) {
  d  <- length(x)
  z  <- x + 1
  hz <- z[1:(d - 1)]
  tz <- z[2:d]
  sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
}

f_rastrigin <- function (x) sum(x * x - 10 * cos(2 * pi * x) + 10)

## target runner ###########################################################
target_runner <- function(experiment, scenario)
{
  debugLevel    <- scenario$debugLevel
  configuration_id  <- experiment$id_configuration
  instance_id   <- experiment$id_instance
  seed          <- experiment$seed
  configuration <- experiment$configuration
  instance      <- experiment$instance

  D <- 3
  par <- runif(D, min = -1, max = 1)
  fn <- function(x) (instance * f_rastrigin(x) + (1 - instance) * f_rosenbrock(x))
  
  tmax = 1 + configuration[["tmax"]]
  temp = 11.0 + configuration[["temp"]]
  stopifnot(tmax > 0)
  stopifnot(temp > 0)
  res <- withr::with_seed(seed,
                  optim(par, fn, method = "SANN", 
                        control = list(maxit = 10, tmax = tmax, temp = temp))
                )
  list(cost = res$value, call = toString(experiment))
}

## target runner ###########################################################
target_runner_reject <- function(experiment, scenario)
{
  if (runif(1) <= 0.05) return (list(cost = -Inf, call = toString(experiment)))
  target_runner(experiment, scenario)
}

## Run function ########################################################
sann_irace <- function(log.param=FALSE, ...)
{
  args <- list(...)

  # tmax and temp must be > 0
  if (log.param)
     parameters_table <- '
       tmax "" i,log (1, 5000)
       temp "" r,log (1, 100)
       '      
  else
     parameters_table <- '
       tmax "" i (1, 5000)
       temp "" r (1, 100)
     '  
  parameters <- readParameters(text = parameters_table)

  scenario <- list(targetRunner = target_runner,
    maxExperiments = 1000, seed = 1234567,
    parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  confs <- irace(scenario = scenario)
  best.conf <- getFinalElites(scenario$logFile, n = 1, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1, , drop = FALSE]),
                   best.conf)
}

test_that("parallel", {
  skip_on_cran()
  # Reproducible results
  generate_set_seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann_irace(instances = weights, parallel = test_irace_detectCores())
})

test_that("parallel reject", {
  # Reproducible results
  generate_set_seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann_irace(instances = weights, parallel = test_irace_detectCores(), targetRunner = target_runner_reject)
})

test_that("deterministic", {
  skip_on_cran()
  # Reproducible results
  generate_set_seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann_irace(deterministic = TRUE, instances = weights[1:7])
})

test_that("log", {
  skip_on_cran()
  # Reproducible results
  generate_set_seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann_irace(log.param=TRUE, instances = weights)
})

test_that("large newInstances", {
  skip_on_cran()
  # Reproducible results
  generate_set_seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann_irace(instances = weights, elitistNewInstances = 6, elitistLimit = 2)
})

}) # withr::with_output_sink()
