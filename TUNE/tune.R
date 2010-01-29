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
divideInThree <- function(left, right, s) {
  stopifnot(left <= right)
  v <- NULL
  s.left <- trim(substr(s, 1, left-1))
  tokens.left <- tokens(s.left)
  v <- c(v, tokens.left)
  if (left < right) {
    v <- c(v, substr(s, left+1, right-1))
  }
  s.right <- trim(substr(s, right+1, nchar(s)))
  tokens.right <- tokens(s.right)
  v <- c(v, tokens.right)  
  return(v)
}

# We divide a string s into an array of tokens by three levels:
# brackets, quotes and spaces. so quotes inside brackets are allowed,
# and spaces inside quotes are allowed too.  we assume the parameter
# presentations don't contain brackets "()", and the brackets are well
# corresponded.  is there a way to print our own error messages or
# print stack trace after the assert error?
tokens <- function(s) {
  if (is.null(s) || length(s)==0 || nchar(s)==0) {
    return(NULL)
  }
  pos.space <- as.vector(gregexpr("[[:space:]]", s)[[1]])
  pos.quote <- as.vector(gregexpr("\"", s)[[1]])
  pos.bracket.left <- as.vector(gregexpr("\\(", s)[[1]])
  pos.bracket.right <- as.vector(gregexpr("\\)", s)[[1]])
  stopifnot(length(pos.bracket.left) == length(pos.bracket.right))
  if (length(pos.bracket.left) > 0 && pos.bracket.left[1] >= 0) {
    # first priority in the case of brackets
    left <- pos.bracket.left[1]
    right <- pos.bracket.right[1]
    v <- divideInThree(left, right, s)
  } else if (length(pos.quote) > 0 && pos.quote[1] >= 0) {
    # second priority in the case of quotes
    stopifnot(! is.na(pos.quote[2]))
    left <- pos.quote[1]
    right <- pos.quote[2]
    v <- divideInThree(left, right, s)
  } else if (length(pos.space) > 0 && pos.space[1] >= 0) {
    # if there exists no quotes and brackets, only white space characters are delimiters
    left <- right <- pos.space[1]
    v <- divideInThree(left, right, s)
  } else {
    v <- s
  }
  return(v)
}

parseValues2Vector <- function(s, is.number=FALSE) {
  s <- trim(s)
  values <- strsplit(s, "[[:space:]]?,[[:space:]]?")[[1]]
  if (is.number) {
    values <- as.numeric(values)
  }
  return(values)
}

myparams <- readLines(con=parameters.file)
param.names <- slave.names <- c()
parameter.type.list <- parameter.param.list <- parameter.boundary.list <- parameter.subsidiary.list <- master.list <- list()

for (i in myparams) {
  line <- trim(sub("#.*$", "", i))
  if (nchar(line) > 0) {
    items <- tokens(line)
    name <- items[1]
    param.names <- c(param.names, name)
    count <- length(param.names)
    parameter.param.list[[count]] <- items[2]
    type <- items[3]
    parameter.type.list[[count]] <- items[3]
    parameter.boundary.list[[count]] <- parseValues2Vector(items[4], (type == "r" || type == "i"))
    if (! is.na(items[5])) {
      stopifnot(items[5] == "|")
      stopifnot(! is.na(items[6]) &&  ! is.na(items[7]))
      master.name <- items[6]
      slave.names <- c(slave.names, name)
      master.values <- parseValues2Vector(items[7])
      master.list[[1]] <- master.values
      names(master.list) <- master.name
      master.count <- length(slave.names)
      parameter.subsidiary.list[[master.count]] <- master.list
    }
  }

}

names(parameter.type.list) <- names(parameter.param.list) <- names(parameter.boundary.list) <- param.names
names(parameter.subsidiary.list) <- slave.names


source("race.R")
source("hrace.R")
#source("eval.R")

parameter.name.list<-list()

hrace.wrapper(maxAllotedExperiments = maxAllotedExperiments,
parameter.type.list = parameter.type.list,parameter.boundary.list = parameter.boundary.list, parameter.param.list = parameter.param.list,
experiment.name = experiment.name,extra.description = extra.description,executable = executable,instance.dir = instance.dir, 
test.instance.dir, parameter.subsidiary.list = parameter.subsidiary.list, parameter.name.list = parameter.name.list)

