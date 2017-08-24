load("irace-output.Rdata")

# Change paths
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

save(iraceResults, file = "irace-output.Rdata")

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

#save in the folder
save(experiment, output, parameters, file="examples.Rdata")

