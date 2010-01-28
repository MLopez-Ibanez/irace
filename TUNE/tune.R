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

# remove leading and trailing white space characters
trim <- function(s) {
  return(sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", s, perl=TRUE))
}

# divide the given string s by three parts seperated by left and right
divideInThree <- function(left, right, s, v) {
  stopifnot(left < right)
  s.left <- trim(substr(s, 1, left-1))
  tokens.left <- tokens(s.left, v)
  v <- c(v, tokens.left)
  v <- c(v, trim(substr(s, left+1, right-1)))
  s.right <- trim(substr(s, right+1, nchar(s)))
  tokens.right <- tokens(s.right, v)
  v <- c(v, tokens.right)  
  return(v)
}

# We divide a string s into an array of tokens by three levels: brackets, quotes and spaces. so quotes inside brackets are allowed, and spaces inside quotes are allowed too.
# we assume the parameter presentations don't contain brackets "()", and the brackets are well corresponded. 
# is there a way to print our own error messages or print stack trace after the assert error? 
tokens <- function(s, v) {
  if (is.null(s) || length(s)==0 || nchar(s)==0) {
    return(NULL)
  }
  pos.space <- as.vector(gregexpr("[ \t]", s)[[1]])
  pos.quote <- as.vector(gregexpr("\"", s)[[1]])
  pos.bracket.left <- as.vector(gregexpr("\\(", s)[[1]])
  pos.bracket.right <- as.vector(gregexpr("\\)", s)[[1]])
  stopifnot(length(pos.bracket.left) == length(pos.bracket.right))
  if (length(pos.bracket.left) > 0 && pos.bracket.left[1] >= 0) {
    # first priority in the case of brackets
    left <- pos.bracket.left[1]
    right <- pos.bracket.right[1]
    v <- divideInThree(left, right, s, v)
  } else if (length(pos.quote) > 0 && pos.quote[1] >= 0) {
    # second priority in the case of quotes
    stopifnot(! is.na(pos.quote[2]))
    left <- pos.quote[1]
    right <- pos.quote[2]
    v <- divideInThree(left, right, s, v)
  } else if (length(pos.space) > 0 && pos.space[1] >= 0) {
    # if there exists no quotes and brackets, only white space characters are delimiters
    left <- pos.space[1]
    right <- pos.space[1] + 1
    v <- divideInThree(left, right, s, v)
  } else {
    v <- s
  }
  return(v)
}

myparams <- readLines(con=parameters.file)
param.list <- list(name=c(), param=c(), type=c(), value=c())


for (i in myparams) {
  line <- gsub ("#.*$", "", i)
  if (nchar(line) > 0) {
    items <- tokens(line, c())
    param.list$name <- c(param.list$name, items[1])
    param.list$param <- c(param.list$param, items[2])
    param.list$type <- c(param.list$type, items[3])
    param.list$value <- c(param.list$value, items[4])
  }

}

evalparsevector <- function(x) return (eval(parse(text=paste("c(",x,")"))))
param.list$value <- lapply (param.list$value, evalparsevector)
parameter.param.list <- as.list(param.list$param)
parameter.type.list <- as.list(param.list$type)
names (parameter.type.list) <- param.list$name
parameter.boundary.list <- param.list$value
names (parameter.boundary.list) <- param.list$name

source("race.R")
source("hrace.R")
#source("eval.R")

parameter.subsidiary.list<-list()
parameter.name.list<-list()

hrace.wrapper(maxAllotedExperiments = maxAllotedExperiments,
parameter.type.list = parameter.type.list,parameter.boundary.list = parameter.boundary.list, parameter.param.list = parameter.param.list,
experiment.name = experiment.name,extra.description = extra.description,executable = executable,instance.dir = instance.dir, 
test.instance.dir, parameter.subsidiary.list = parameter.subsidiary.list, parameter.name.list = parameter.name.list)

