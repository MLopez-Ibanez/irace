library(irace)

# Reproducible results
seed <- sample(2^30,1)
cat("Seed: ", seed, "\n")
set.seed(seed)

nsize <- 0.1

## Functions ##########################################################
f_ackley <- function (x,y) {
  
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
  fmax <- 15*nsize
  f <- ((f - fmin) / (fmax-fmin)) * (100-0) + 0
  
  return(f)
}

f_goldestein_price <- function (x,y) {
  # Trasfomation of parameter values 
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
  fmax <- 1000000*nsize
  f <- ((f - fmin) / (fmax-fmin)) * (100-0) + 0
  
  return(f)
}

f_matyas <- function (x,y) {
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
  fmax <- 100*nsize
  f <- ((f - fmin) / (fmax-fmin)) * (100-0) + 0
  
  return(f)
}

f_himmelblau <- function (x,y) {  
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
  fmax <- 2000*nsize
  f <- ((f - fmin) / (fmax-fmin)) * (100-0) + 0

  return(f)
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
  bound         <- experiment$bound

  value <- switch(instance,
                  ackley     = f_ackley(as.numeric(configuration[["x"]]), as.numeric(configuration[["y"]])),
                  goldestein = f_goldestein_price(as.numeric(configuration[["x"]]), as.numeric(configuration[["y"]])),
                  matyas     = f_matyas(as.numeric(configuration[["x"]]), as.numeric(configuration[["y"]])),
                  himmelblau  = f_himmelblau(as.numeric(configuration[["x"]]), as.numeric(configuration[["y"]])))
  
  # Simulate execution bound
  if (value > bound) value <- bound
 
  result <- list(cost = value, time=value, call = toString(experiment))
  return(result)
}

## target runner ###########################################################
target.runner.reject <- function(experiment, scenario)
{
  if (runif(1) <= 0.05) return (list(cost = -Inf, time = 80, call = toString(experiment)))
  return (target.runner(experiment, scenario))
}

cap.irace <- function(...)
{
  args <- list(...)

  parameters.table <- '
   x "" r (0, 1.00)
   y "" r (0, 1.00)
   '  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(instances = c("ackley", "goldestein", "matyas", "himmelblau"),
                   targetRunner = target.runner,
                   capping = TRUE,
                   boundMax = 80,
                   testType = "t-test",
                   parallel = 2)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)

  confs <- irace(scenario = scenario, parameters = parameters)
  best.conf <- getFinalElites(logFile = scenario$logFile, n = 1,
                              drop.metadata = TRUE)
  stopifnot(identical(removeConfigurationsMetaData(confs[1, , drop = FALSE]),
                      best.conf))
}

cap.irace(maxExperiments = 1000)
cap.irace(maxTime = 50000)
cap.irace(targetRunner = target.runner.reject, maxTime = 10000)

