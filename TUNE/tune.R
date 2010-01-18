## FIXME: configuration should be passed as parameters or read from a
## configuration file.

## Configuration
experiment.name <- "F-RACE applied to Beam-ACO"
extra.description <- "Iterative F-Race for tuning Beam-ACO"
## This is not used anymore and should be deleted everywhere.
executable <- ""
## ????
executablecomp <- ""
## Folder to put instances
instance.dir <- "../Instances"
## ????
test.instance.dir <- ""
## Maximum number of experiments
maxAllotedExperiments <- 1000
parameters.file <- "../parameters.txt"
## END of configuration

myparams <- scan(parameters.file, what = list(name="", param="", type="",value=""), comment.char="#", flush=T, quiet=F)
evalparsevector <- function(x) return (eval(parse(text=paste("c(",x,")"))))
myparams$value <- lapply (myparams$value, evalparsevector)
print(myparams)
parameter.param.list <- as.list(myparams$param)
parameter.type.list <- as.list(myparams$type)
names (parameter.type.list) <- myparams$name
parameter.boundary.list <- myparams$value
names (parameter.boundary.list) <- myparams$name

source("race.R")
source("hrace.R")
#source("eval.R")

parameter.subsidiary.list<-list()
parameter.name.list<-list()

hrace.wrapper(maxAllotedExperiments = maxAllotedExperiments,
parameter.type.list = parameter.type.list,parameter.boundary.list = parameter.boundary.list, parameter.param.list = parameter.param.list,
experiment.name = experiment.name,extra.description = extra.description,executable = executable,instance.dir = instance.dir, 
test.instance.dir, parameter.subsidiary.list = parameter.subsidiary.list, parameter.name.list = parameter.name.list)

