load("irace-output.Rdata")

save(iraceResults, file="backup-irace-output.Rdata")

#Change paths
iraceResults$scenario$logFile <- paste("./",basename(iraceResults$scenario$logFile),sep="")
iraceResults$scenario$trainInstancesDir <- paste("./",basename(iraceResults$scenario$trainInstancesDir),sep="")
iraceResults$scenario$trainInstancesFile <- paste("./",basename(iraceResults$scenario$trainInstancesFile),sep="")
iraceResults$scenario$testInstancesDir <- paste("./",basename(iraceResults$scenario$testInstancesDir),sep="")
iraceResults$scenario$testInstancesFile <- paste("./",basename(iraceResults$scenario$testInstancesFile),sep="")
iraceResults$scenario$parameterFile <- paste("./",basename(iraceResults$scenario$parameterFile),sep="")
iraceResults$scenario$execDir <- "./"
iraceResults$scenario$instances <- paste(iraceResults$scenario$trainInstancesDir, "/", basename(iraceResults$scenario$instances), sep="")
iraceResults$scenario$targetRunner <- paste("./",basename(iraceResults$scenario$targetRunner),sep="")
iraceResults$scenario$testInstances <- paste(iraceResults$scenario$testInstancesDir, "/", basename(iraceResults$scenario$testInstances), sep="")

###############################
### Small example file
###############################
# Parameters
selected.names <- c("algorithm","ants","q0")
parameters <- list()
parameters$names        <- selected.names
parameters$types        <- iraceResults$parameters$types[selected.names]
parameters$isFixed      <- iraceResults$parameters$isFixed[selected.names]
parameters$nbFixed      <- sum(parameters$isFixed)
parameters$nbParameters <- length(selected.names)
parameters$nbVariable   <- parameters$nbParameters - parameters$nbFixed 
parameters$switches     <- iraceResults$parameters$switches[selected.names]
parameters$domain       <- iraceResults$parameters$domain[selected.names]
parameters$conditions   <- iraceResults$parameters$conditions[selected.names]

# Experiment
experiment <- list (id.configuration = 1,
                    id.instance      = iraceResults$state$.irace$instancesList[1,"instance"],
                    seed             = iraceResults$state$.irace$instancesList[1,"seed"],
                    configuration    = iraceResults$allConfigurations[1, iraceResults$parameters$names, drop=FALSE],
                    instance         = iraceResults$scenario$instance[iraceResults$state$.irace$instancesList[1,"instance"]], 
                    switches         = iraceResults$parameters$switches)
# Output                    
output <- list()
output[[1]] <- list(cost=iraceResults$experiments[1,1], time=as.numeric(iraceResults$experimentLog[1,"time"]))
output[[2]] <- list(cost=iraceResults$experiments[1,2], time=as.numeric(iraceResults$experimentLog[2,"time"]))

#save in the folder
save(experiment, output, parameters, file="examples.Rdata")
save(iraceResults, file="irace-output.Rdata")

#save in vignettes
#save(experiment, output, parameters, file="../../vignettes/examples.Rdata")
#save(iraceResults, file="../../vignettes/irace-output.Rdata")

