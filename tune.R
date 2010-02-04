.tune.config.file <- "tune-conf"
args <- commandArgs(trailingOnly=TRUE)

if (!is.na (args[1]) && !is.null(args[1]) && length(args[1]) > 0) {
  .tune.config.file <- args[1]
}
cat ("Configuration file:", .tune.config.file, "\n")
## FIXME: We should not need to do this, the working directory should
## where we are invoked and not where the program is executed neither
## where F-Race files are located.
## FIXME: Whe should handle this in plattform agnostic way, check help("file.path").
if (!any(grep ("^/", path.expand(.tune.config.file)))) {
  .tune.config.file <- paste ("../", .tune.config.file, sep="")
}
source(.tune.config.file)
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

