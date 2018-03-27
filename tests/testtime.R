library(irace)

# Reproducible results
seed <- sample(2^30, 1)
cat("Seed: ", seed, "\n")
set.seed(seed)

target.runner <- function(experiment, scenario)
{
  configuration     <- experiment$configuration
  tmax <-  as.numeric(configuration[["tmax"]]) 
  temp <-  as.numeric(configuration[["temp"]])
  time <- abs(rnorm(1, mean=(tmax+temp)/10))
  return(list(cost = time, time = time, call = toString(experiment)))
}
time.irace <- function(...)
{
  args <- list(...)
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  parameters.table <- '
   tmax "" i (1, 50)
   temp "" r (0, 10)
   '  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(targetRunner = target.runner, instances = weights, seed = 1234567)
  scenario <- c(scenario, args)
  scenario <- checkScenario (scenario)
  irace(scenario = scenario, parameters = parameters)
}

time.irace(maxTime = 500)
time.irace(maxTime = 1111)
