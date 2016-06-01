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
target.runner <- function(experiment, scenario = list())
{
  debugLevel    <- scenario$debugLevel
  configuration.id  <- experiment$id.configuration
  instance.id   <- experiment$id.instance
  seed          <- experiment$seed
  configuration     <- experiment$configuration
  instance      <- experiment$instance

  prevstate <- .Random.seed
  set.seed(2)
  D <- 3
  par <- runif(D, min = -1, max = 1)
  fn <- function(x) {
    weight <- instance
    return(weight * f_rastrigin(x) + (1 - weight) * f_rosenbrock(x))
  }
  res <- optim(par,fn, method = "SANN", 
               control = list(maxit = 1000,
                 tmax = as.numeric(configuration[["tmax"]]), 
                 temp = as.numeric(configuration[["temp"]])))
  .Random.seed <- prevstate
   return(res$value)
}

## Run function ########################################################
sann.irace <- function(...)
{
  args <- list(...)
  require("irace")

  set.seed(2)
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  parameters.table <- '
   tmax "" i (1, 5000)
   temp "" r (0, 100)
   '  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(targetRunner = target.runner, instances = weights, maxExperiments = 1000, seed = 1234567)
  scenario <- c(scenario, args)

  scenario <- checkScenario (scenario)

  irace(scenario = scenario, parameters = parameters)
}

sann.irace()
