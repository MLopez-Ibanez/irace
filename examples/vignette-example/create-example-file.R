library(irace)

load("irace-acotsp.Rdata")

# Change paths
# FIXME: Use scenario.update.paths()
to.change <- c("logFile", "trainInstancesDir", "trainInstancesFile",
               "testInstancesDir", "testInstancesFile", "parameterFile",
               "targetRunner")
iraceResults$scenario[to.change] <-
  lapply(iraceResults$scenario[to.change],
         function(x) paste0("./", basename(x)))

iraceResults$scenario$execDir <- "./"
iraceResults$scenario$instances <-
  paste0(iraceResults$scenario$trainInstancesDir, "/",
         basename(iraceResults$scenario$instances))
iraceResults$scenario$testInstances <-
  paste0(iraceResults$scenario$testInstancesDir, "/",
         basename(iraceResults$scenario$testInstances))

save(iraceResults, file = "irace-acotsp.Rdata", version = 2)

###############################
### Small example file
###############################
# Experiment
# FIXME: Create this just using functions from the irace package.
experiment <- list (
  id.configuration = 1,
  id.instance      = iraceResults$state$.irace$instancesList[1,"instance"],
  seed             = iraceResults$state$.irace$instancesList[1,"seed"],
  configuration    = iraceResults$allConfigurations[1, iraceResults$parameters$names, drop=FALSE],
  instance         = iraceResults$scenario$instance[iraceResults$state$.irace$instancesList[1,"instance"]], 
  switches         = iraceResults$parameters$switches)

# Output
# FIXME: Create this just using functions from the irace package.
output <- list()
output[[1]] <- list(cost=iraceResults$experiments[1,1], time=as.numeric(iraceResults$experimentLog[1,"time"]))
output[[2]] <- list(cost=iraceResults$experiments[1,2], time=as.numeric(iraceResults$experimentLog[2,"time"]))

# save in the folder
save(experiment, output, file="examples.Rdata", version = 2)


## Create sann.rda
 
## We generate 200 instances (in this case, weights):
weights <- rnorm(200, mean = 0.9, sd = 0.02)

## On this set of instances, we are interested in optimizing two
## parameters of the SANN algorithm: tmax and temp. We setup the
## parameter space as follows:
parameters.table <- '
tmax "" i (1, 5000)
temp "" r (0, 100)
'

## We use the irace function readParameters to read this table:
parameters <- readParameters(text = parameters.table)

## Next, we define the function that will evaluate each candidate
## configuration on a single instance. For simplicity, we restrict to
## three-dimensional functions and we set the maximum number of
## iterations of SANN to 5000.
target.runner <- function(experiment, scenario)
{
  # Functions to be optimized:
  f_rosenbrock <- function (x) {
    d <- length(x)
    z <- x + 1
    hz <- z[1:(d - 1)]
    tz <- z[2:d]
    s <- sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
    return(s)
  }
  f_rastrigin <- function (x) {
    sum(x * x - 10 * cos(2 * pi * x) + 10)
  }

  instance <- experiment$instance
  configuration <- experiment$configuration

  D <- 3
  par <- runif(D, min=-1, max=1)
  fn <- function(x) {
    weight <- instance
    return(weight * f_rastrigin(x) + (1 - weight) * f_rosenbrock(x))
  }
  res <- stats::optim(par,fn, method="SANN",
               control=list(maxit=5000
                 , tmax = as.numeric(configuration[["tmax"]])
                 , temp = as.numeric(configuration[["temp"]])
                 ))
  ## New output interface in irace 2.0. This list may also contain:
  ## - 'time' if irace is called with 'maxTime'
  ## - 'error' is a string used to report an error
  ## - 'outputRaw' is a string used to report the raw output of calls to
  ##   an external program or function.
  ## - 'call' is a string used to report how target.runner called the
  ##   external program or function.
  return(list(cost = res$value))
}

## We define a configuration scenario by setting targetRunner to the
## function define above, instances to the first 100 random weights, and
## a maximum budget of 1000 calls to targetRunner.
scenario <- list(targetRunner = target.runner,
                 instances = weights[1:100],
                 maxExperiments = 1000,
                 logFile = "./sann.rda",
                 execDir = "./")

## We are now ready to launch irace. We do it by means of the irace
## function. The function will print information about its
## progress. This may require a few minutes, so it is not run by default.
irace(scenario = scenario, parameters = parameters)

load("sann.rda")
iraceResults$scenario$execDir <- "./"
iraceResults$scenario$logFile <- "./sann.rda"
save(iraceResults, file="sann.rda", version = 2)

# Create log-ablation.Rdata
ablation(iraceLogFile = "irace-acotsp.Rdata", src = 1, target = 60)

