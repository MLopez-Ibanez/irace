# This is an example of launching irace from an R script.
library(irace)
# Initial set up. Please check in the documentation of your cluster whether
# $SLURM_NTASKS_PER_NODE is the correct environment variable to get the number
# of tasks that can be run in parallel.
ncores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE", parallel::detectCores()))

# This needs to be customized by the user.  Instead of defining
# 'parameter_table' you could use a file 'parameters.txt'.
parameter_table <- '
param1 "--param1 " i (1,100)
param2 "--param2=" c (0,1)
'
parameters <- readParameters(text = parameter_table)

# This needs to be customized by the user.  Instead of defining targetRunner as
# an R function 'my_target_runner' you could use an executable file
# 'my_target_runner'.
my_target_runner <- function(experiment, scenario)
{
  instance <- experiment$instance
  configuration <- experiment$configuration
  seed <- experiment$seed
  cost <- my_algorithm(instance, configuration, seed)
  # You could also return 'time' if you are using 'maxTime' in the scenario.
  return(list(cost = cost))
}


# Some function that loads or defines the instances. You could also use
# trainInstancesFile or trainInstancesDir in the scenario.
instances <- load_instances() 
scenario <- list(targetRunner = my_target_runner,
                 instances = instances,
                 maxExperiments = 200,
                 parallel = ncores, parameters = parameters)

# check if the scenario is valid (comment out if taking too much time)
checkIraceScenario(scenario)

# run irace
irace_main(scenario = scenario)
