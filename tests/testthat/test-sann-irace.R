context("irace")

source("common.R")

## Functions ##########################################################
f_rosenbrock <- function (x) {
  d  <- length(x)
  z  <- x + 1
  hz <- z[1:(d - 1)]
  tz <- z[2:d]
  s  <- sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
  return(s)
}

f_rastrigin <- function (x) {
  sum(x * x - 10 * cos(2 * pi * x) + 10)
}

## target runner ###########################################################
target.runner <- function(experiment, scenario)
{
  debugLevel    <- scenario$debugLevel
  configuration.id  <- experiment$id.configuration
  instance.id   <- experiment$id.instance
  seed          <- experiment$seed
  configuration <- experiment$configuration
  instance      <- experiment$instance

  D <- 3
  par <- runif(D, min = -1, max = 1)
  fn <- function(x) {
    weight <- instance
    return(weight * f_rastrigin(x) + (1 - weight) * f_rosenbrock(x))
  }
  res <- optim(par, fn, method = "SANN", 
               control = list(maxit = 10,
                 tmax = as.numeric(configuration[["tmax"]]), 
                 temp = as.numeric(configuration[["temp"]])))
  result <- list(cost = res$value, call = toString(experiment))
  return(result)
}

## target runner ###########################################################
target.runner.reject <- function(experiment, scenario)
{
  if (runif(1) <= 0.05) return (list(cost = -Inf, call = toString(experiment)))
  return (target.runner(experiment, scenario))
}

## Run function ########################################################
sann.irace <- function(log.param=FALSE, ...)
{
  args <- list(...)

  if (log.param)
     parameters.table <- '
       tmax "" i (1, 5000)
       temp "" r,log (0, 100)
       '      
  else
     parameters.table <- '
       tmax "" i (1, 5000)
       temp "" r (0, 100)
     '  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(targetRunner = target.runner,
                   maxExperiments = 1000, seed = 1234567)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  confs <- irace(scenario = scenario, parameters = parameters)
  best.conf <- getFinalElites(logFile = scenario$logFile, n = 1,
                              drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1, , drop = FALSE]),
                   best.conf)
}

test_that("parallel", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann.irace(instances = weights, parallel = 2)
})

test_that("parallel reject", {

  # Reproducible results
  generate.set.seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann.irace(instances = weights, parallel = 2, targetRunner = target.runner.reject)
})

test_that("deterministic", {
  skip_on_cran()

  # Reproducible results
  generate.set.seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann.irace(deterministic = TRUE, instances = weights[1:7])
})

test_that("log", {
  skip_on_cran()
  # Reproducible results
  generate.set.seed()
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  sann.irace(log.param=TRUE, instances = weights)
})


