source("../tune-conf")
source("../readparameters.R")

parameters <- readparameters(parameters.file)
parameter.name.list <- parameters$names
parameter.type.list <- parameters$types
parameter.param.list <- parameters$switches
parameter.boundary.list <- parameters$boundary
parameter.subsidiary.list <- parameters$subsidiary

source("../race.R")
source("../hrace.R")
source("../eval.R")

hrace.wrapper(maxAllotedExperiments = maxAllotedExperiments,
              parameter.type.list = parameter.type.list,
              parameter.boundary.list = parameter.boundary.list,
              parameter.param.list = parameter.param.list,
              experiment.name = experiment.name,
              extra.description = extra.description,
              executable = executable,
              instance.dir = instance.dir, 
              test.instance.dir,
              parameter.subsidiary.list = parameter.subsidiary.list,
              parameter.name.list = parameter.name.list)

