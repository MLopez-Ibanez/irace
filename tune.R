## FIXME: Whe should handle this in plattform agnostic way, check help("file.path").
path.rel2abs <- function (path) {
  return (sub ('^(\\.)', paste (getwd(), '/\\1', sep=''), path))
}

.tune.config.file <- "tune-conf"
args <- commandArgs(trailingOnly=TRUE)

if (!is.na (args[2]) && !is.null(args[2]) && length(args[2]) > 0) {
  .tune.config.file <- args[2]
}

cat ("Configuration file:", .tune.config.file, "\n")
source(.tune.config.file)

if (!is.na (args[1]) && !is.null(args[1]) && length(args[1]) > 0) {
  .tune.execdir <- args[1]
}
.tune.tuning.instances.dir <- path.rel2abs (.tune.tuning.instances.dir)

cat ("Execution directory:", .tune.execdir, "\n")
cat ("Tuning instances directory:", .tune.tuning.instances.dir, "\n")
  
source("readparameters.R")

parameters <- readparameters(parameters.file)
parameter.name.list <- parameters$names
parameter.type.list <- parameters$types
parameter.param.list <- parameters$switches
parameter.boundary.list <- parameters$boundary
parameter.subsidiary.list <- parameters$subsidiary

source("race.R")
source("hrace.R")
source("eval.R")

hrace.wrapper(maxAllotedExperiments = maxAllotedExperiments,
              parameter.type.list = parameter.type.list,
              parameter.boundary.list = parameter.boundary.list,
              parameter.param.list = parameter.param.list,
              experiment.name = experiment.name,
              extra.description = extra.description,
              executable = executable,
              instance.dir = .tune.tuning.instances.dir, 
              test.instance.dir,
              parameter.subsidiary.list = parameter.subsidiary.list,
              parameter.name.list = parameter.name.list)

