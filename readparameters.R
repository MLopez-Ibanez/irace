######################################################################
# Functions for reading the parameter file
######################################################################

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
  return (v)
}


parseValues2Vector <- function(s, is.number=FALSE) {
  s <- trim(s)
  values <- strsplit(s, "[[:space:]]?,[[:space:]]?")[[1]]
  if (is.number) {
    values <- as.numeric(values)
  }
  return (values)
}

# Main function to read the parameter file. 
readparameters <- function (filename = stop("readparameters: argument \"filename\" is mandatory"))
{
  parameters <- list(names = list(),
                     types = list(),
                     switches = list(),
                     boundary = list(),
                     subsidiary = list())

  param.names <- slave.names <- c()
  master.list <- list()

  lines <- readLines(con=filename)
  for (line in lines) {
    line <- trim(sub("#.*$", "", line))
    if (nchar(line) > 0) {
      items <- tokens(line)
      name <- items[1]
      param.names <- c(param.names, name)
      count <- length(param.names)
      parameters[["names"]][[count]] <- items[1]
      parameters[["switches"]][[count]] <- items[2]
      type <- items[3]
      parameters[["types"]][[count]] <- items[3]
      parameters[["boundary"]][[count]] <- parseValues2Vector(items[4], (type == "r" || type == "i"))
      if (! is.na(items[5])) {
        stopifnot(items[5] == "|")
        stopifnot(! is.na(items[6]) &&  ! is.na(items[7]))
        master.name <- items[6]
        slave.names <- c(slave.names, name)
        master.values <- parseValues2Vector(items[7])
        master.list[[1]] <- master.values
        names(master.list) <- master.name
        master.count <- length(slave.names)
        parameters[["subsidiary"]][[master.count]] <- master.list
      }
    }
  }
  names(parameters[["names"]]) <- names(parameters[["types"]]) <- names(parameters[["switches"]]) <- names(parameters[["boundary"]]) <- param.names
  names(parameters[["subsidiary"]]) <- slave.names

  return (parameters)
}

