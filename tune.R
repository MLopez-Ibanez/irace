## FIXME: Whe should handle this in plattform agnostic way, check
## help("file.path").
path.rel2abs <- function (path) {
  return (sub ('^(\\.)', paste (getwd(), '/\\1', sep=''), path))
}

# Function to read command-line arguments.
read.arg <- function(args, short="", long="", default=NULL)
{
  pos <- c()
  if (length (short) > 0) {
    pos <- grep (paste ("^", short, "$", sep=""), args)
    if (length (pos) == 0) {
      pos <- grep (paste ("^", short, "=", sep=""), args)
    }
  }
  if (length (long) > 0 && length (pos) == 0)  {
    pos <- grep (paste ("^", long, "$", sep=""), args)
    if (length (pos) == 0) {
      pos <- grep (paste ("^", long, "=", sep=""), args)
    }
  }
  if (length (pos) == 0) {
    return(default)
  }

  value <- unlist(strsplit (args[pos], '=', fixed=TRUE))[2]
  if (is.null (value) || is.na(value)) {
    value <- args[pos + 1]
  }
  return(value)
}

# FIXME: This should not have a default but check whether it is
# missing and error out properly.
.tune.installdir <- NULL
args <- commandArgs(trailingOnly=FALSE)
.tune.installdir <- dirname (read.arg(args,"-f","--file", .tune.installdir))

.tune.config.file <- "tune-conf"
args <- commandArgs(trailingOnly=TRUE)
# FIXME: Make these real commandline parameters. 
if (!is.na (args[2]) && !is.null(args[2]) && length(args[2]) > 0) {
  .tune.config.file <- args[2]
}

cat ("Configuration file:", .tune.config.file, "\n")
source(.tune.config.file)
if (!is.na (args[1]) && !is.null(args[1]) && length(args[1]) > 0) {
  .tune.execdir <- args[1]
}
cat("Iterated F-Race install directory:", .tune.installdir, "\n")

.tune.tuning.instances.dir <- path.rel2abs (.tune.tuning.instances.dir)

cat ("Execution directory:", .tune.execdir, "\n")
cat ("Tuning instances directory:", .tune.tuning.instances.dir, "\n")

# Read source files needed from the installation directory.
cwd <- setwd(.tune.installdir)
source("readparameters.R")
source("race.R")
source("hrace.R")
source("eval.R")
.tune.race.wrapper <- paste(.tune.installdir, "race-wrapper.R", sep="/")
setwd(cwd)

parameters <- readparameters(parameters.file)
parameter.name.list <- parameters$names
parameter.type.list <- parameters$types
parameter.param.list <- parameters$switches
parameter.boundary.list <- parameters$boundary
parameter.subsidiary.list <- parameters$subsidiary


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

