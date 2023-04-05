library(irace)
# initial set up
ncores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE", parallel::detectCores()))

parameter_table <- '...'
parameters <- readParameters(text = parameter_table)

my_target_runner <- function(experiment, scenario)
{
  instance <- experiment$instance
  configuration <- experiment$configuration
  seed <- experiment$seed
  return(list(cost = my_algorithm(instance, configuration, seed)))
}
instances <- load_instances() # Some function that loads or defines the instances. You could also use trainInstancesFile or trainInstancesDir in the scenario. 
scenario <- list(targetRunner = my_target_runner,
                 instances = instances,
                 maxExperiments = 200,
                 parallel = ncores)

# check if the scenario is valid (comment out if taking too much time)
checkIraceScenario(scenario, parameters = parameters)

# run irace
irace.main(scenario = scenario, parameters = parameters)
