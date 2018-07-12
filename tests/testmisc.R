library(irace)

# Reproducible results
seed <- sample(2^30, 1)
cat("Seed: ", seed, "\n")
set.seed(seed)

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


weights <- rnorm(200, mean = 0.9, sd = 0.02)

## Run function ########################################################
sann.irace <- function(...)
{
  args <- list(...)

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
  stopifnot(identical(removeConfigurationsMetaData(confs[1, , drop = FALSE]),
                      best.conf))
}

sann.irace(instances = weights, parallel = 2, targetRunner = target.runner.reject)

sann.irace(instances = weights, parallel = 2)

sann.irace(deterministic = TRUE, instances = weights[1:7])



test.checkForbidden <- function(param.file)
{
  params <- irace:::readParameters(param.file)
  confs <- irace:::readConfigurationsFile("configurations.txt", params)
  forbidden <- irace:::readForbiddenFile("forbidden.txt")
  exp.confs <- irace:::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        6     "x1"   3.5   "low"
NA        NA   "x3"   4.5   "low"
', parameters = params)
  confs <- irace:::checkForbidden(confs, forbidden)
  rownames(confs) <- rownames(exp.confs) <- NULL
  stopifnot(identical(confs, exp.confs))
}
test.checkForbidden("parameters.txt")
test.checkForbidden("logparameters.txt")


test.instances <- function()
{
  # Test that a function can be given as a string.
  scenario <- list(targetRunner = "target.runner",
                   maxExperiments = 1000, seed = 1234567,
                   firstTest = 5,
                   deterministic = TRUE,
                   trainInstancesFile = "train-instances.txt")
  scenario <- checkScenario (scenario)
}
test.instances()

