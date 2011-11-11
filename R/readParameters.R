#########################################################
## READ DEFINITION OF PARAMETERS FROM A FILE
#########################################################

#' Main function to read the parameters definition from a file
#' 
#' The datastructure used to record parameters is shown below.
#' A fixed parameter is a parameter that should not be sampled but instead should
#' be always set to the first values in the vector of boundaries. In this function we
#' set isFixed to TRUE if either the parameter is a categorical and has only one possible value,
#' or it is an integer and the two boundaries are equal, or it is a real and the two boundaries 
#' satisfy the following test: signif(boundary1, signifDigits) == signif(boundary2, signifDigits)
#' 
#' @param signifDigits The number of significant digits wished (used to know if parameters are fixed or not).
#' @param filename The name of the file containing the parameters. The fact that
#' this file should exist and be readable should be check before.
#' @return A list as follows.
#' 
#' parameters
#'        |
#'        +---- (names) --------- a list that contains the names of the parameters
#'        +---- (types) ----------- a list that contains the type of each parameter parameters$types = i|c|r
#'        +---- (switches) ------ a list that contains the switches to be used for the parameters on the command line
#'        +---- (boundary) ----- a vector that contains two values (the boundaries) for real and int parameters, and
#'        |                              possibly more for categorical parameters
#'        +---- (constraints) -+   
#'        |                            |                               Example param1 depends on param2 and param3 to be activated:
#'        |                            +-- (param1) --+
#'        |                            |                     +-- (param2) -- a vector containing two values (the allowed boundaries) if
#'        |                            +-- ....              |                      param2 is real or int, or all allowed values if param2 is categorical                 
#'        |                            |                     +-- (param3) -- ...
#'        |                           ...
#'        +---- (isFixed) -------- a list of boolean to know if a parameter should be considered as a fixed or not.
#'        +---- (nbParameters) --- a simple integer to know the number of parameters since we can need it often and
#'        |                                  we should avoid to compute it every time.
#'        +---- (nbFixed) ----- a simple integer to know the number of fixed parameters
#'        +---- (nbVariable) ------ a simple integer to know the number of no-fixed parameters (to be sampled)
#'        
readParameters <- function (filename = stop("filename is mandatory"),
                            signifDigits = stop("signifDigits is mandatory"),
                            debugLevel = 0)
{
  field.match <- function (line, pattern, delimited = FALSE, sep = "[[:space:]]")
  {
    #cat ("pattern:", pattern, "\n")
    if (regexpr (paste("^", pattern, sep, sep=""), line) == -1) {
      #cat("no match: NULL\n")
      return (list(match = NULL, line = line))
    }
    pos.matched <- regexpr (paste("^", pattern, sep=""), line)
    #cat("pos.matched:", pos.matched, "\n")
    if (pos.matched == -1) {
      #cat(line)
      return (list(match = NULL, line = line))
    }
    delimited <- as.integer(delimited)
    match <- substr(line, pos.matched[1] + delimited,
                    attr(pos.matched, "match.length") - delimited)
    #cat("match:",match, "\n")
    line <- substr (line, pos.matched[1] + attr(pos.matched, "match.length"),
                    nchar(line))
    line <- trim.leading (line)
    #cat(line)
    return (list(match = match, line = line))
  }

  string2vector <- function(str)
  {
    v <- c()
    str <- trim(str)
    #cat("string2vector:", str, "\n")
    while (nchar (str)) {
      result <- field.match (str, "\"[^\"]*\"", delimited = TRUE, sep="")
      #cat("result.match: ", result$match,"\n")
      if (is.null (result$match)) {
        result <- field.match (str, "[^,]+", sep="")
        #cat("result.match: ", result$match,"\n")
      }
      v <- c(v, result$match)
      #print(v)
      str <- sub(",[[:space:]]*", "", result$line)
      #print(str)
    }
    return (v)
  }

  # Determine if a parameter is fixed.
  isFixed <- function (type = stop("type is mandatory."),
                       boundaries = stop("boundaries is mandatory."),
                       signifDigits)
  {
    type <- as.character(type)
    if (type == "i") {
      return (boundaries[[1]] == boundaries[[2]])
    } else if ((type == "c") || (type == "o")) {
      return (length(boundaries) == 1)
    } else if (type == "r") {
      return (signif (as.numeric(boundaries[[1]]), signifDigits)
              == signif (as.numeric(boundaries[[2]]), signifDigits))
    }
  }
  # *************************************************************************
  # Conditional parameter: ordering of the parameters according to
  # constraints hierarchy
  # *  The constraints hierarchy is an acyclic directed graph.
  #    Functions treeLevel() and treeLevelAux() compute an order on vertex s.t:
  #    level(A) > level(B)  <=>  There is an arc A ---> B
  #    (A depends on B to be activated)
  # *  If a cycle is detected, execution is stopped
  # *  If a parameter depends on another one not defined, execution is stopped
  treeLevelAux <- function(paramName, constraintsTree, rootParam)
  {
    vars <- all.vars (constraintsTree[[paramName]])
    if (length(vars) == 0) {
      return (1) # This parameter does not have constraints
    } else {
      # This parameter has some constraints
      # Recursive call: level <- MAX( level(m) : m in children )
      maxChildLevel <- 0
      for (child in vars) {
        # The following line detects cycles
        if (child == rootParam)
          tunerError("a cycle detected in conditional parameters! ",
                     "Check definition of constraints.\n",
                     "One parameter of this cycle is '", rootParam, "'")
        
        # The following line detects a missing definition
        if (!child %in% names(constraintsTree))
          tunerError("A parameter definition is missing! ",
                     "Check definition of parameters.\n",
                     "Parameter '", paramName,
                     "' depends on '", child, "' which is not defined.")
        
        level <- treeLevelAux(child, constraintsTree, rootParam)
        if (level > maxChildLevel)
          maxChildLevel <- level
      }
      level <- maxChildLevel + 1
    }
    return (level)
  }

  treeLevel <- function(paramName, constraintsTree)
  {
    # The last parameter is used to record the root parameter of the
    # recursive call in order to detect the presence of cycles.
    return (treeLevelAux(paramName, constraintsTree, paramName))
  }

  errReadParameters <- function(filename, line, context, ...)
  {
    if (!is.null (context)) {
      context <- paste(sep="", " when reading: \"", context, "\"")
    }
    tunerError (paste (sep="", ...),
                " at ", filename, ", line ", line, context)
  }
  
  # *************************************************************************
  # FIXME: Only boundary needs to be a list, the rest should be
  # vectors, which will make many operations way faster.
  parameters <- list(names = list(),
                     types = list(),
                     switches = list(),
                     boundary = list(),
                     constraints = list(),
                     isFixed = list())

  param.names <- c()
  constraints <- list()
  lines <- readLines(con=filename)
  nbLines <- 0
  count <- 0

  for (line in lines) {
    nbLines <- nbLines + 1
    # Delete comments 
    line <- trim(sub("#.*$", "", line))
    if (nchar(line) == 0) {
      next
    }
    ## FIXME: real and integer must provide exactly 2 values and not a
    ## list. We should check this here rather than fail badly later.
    
    ## Match param.name (unquoted alphanumeric string)
    result <- field.match (line, "[._[:alnum:]]+")
    param.name <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (filename, nbLines, line,
                         "parameter name must be alphanumeric")
    }
    
    ## Match param.switch (quoted string)
    result <- field.match (line, "\"[^\"]*\"", delimited = TRUE)
    param.switch <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (filename, nbLines, line,
                         "parameter switch must be a double-quoted string")
    }
    
    ## Match param.type (single letter)
    result <- field.match (line, "[ciro]")
    param.type <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (filename, nbLines, line,
                         "parameter type must be a single character in {c,i,r,o}")
    }

    ## Match param.value (delimited by parenthesis)
    result <- field.match (line, "\\([^)]+\\)", delimited = TRUE, sep = "")
    param.value <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (filename, nbLines, line,
                         "Allowed values must be a list within parenthesis")
    }

    if (param.name %in% param.names) {
      errReadParameters (filename, nbLines, NULL,
                         "duplicated parameter name '", param.name, "'")
    }
 
    count <- count + 1
    param.names <- c(param.names, param.name)
    parameters$names[[count]] <- param.name
    parameters$switches[[count]] <- param.switch
    parameters$types[[count]] <- param.type
    parameters$boundary[[count]] <- string2vector(param.value)
    parameters$isFixed[[count]] <-
      isFixed (type = param.type,
               boundaries = parameters$boundary[[count]],
               signifDigits = signifDigits)
    
    ## Match start of constraints 
    result <- field.match (line, "\\|", sep="")
    line <- result$line
    if (!is.null(result$match) && nchar(result$match)) {
      result <- field.match (line, ".*$", sep="")
      if (is.null(result$match) || !nchar(result$match))
        errReadParameters (filename, nbLines, line,
                           "expected constraint after '|'")
      # FIXME: Provide a better error for invalid constraints like "a 2 0"
      constraints[[param.name]] <- NA
      try(constraints[[param.name]] <- parse(text=result$match))
      if (!is.expression (constraints[[param.name]]))
        errReadParameters (filename, nbLines, line,
                           "invalid constraint after '|'")
      line <- result$line
    } else if (!is.null(result$line) && nchar(result$line)) {
      errReadParameters (filename, nbLines, line,
                         "expected '|' before constraint")
    } else {
      constraints[[param.name]] <- expression(TRUE)
    }
    # *****************************************************************
  } # end loop on lines

  # Check that we have read at least one parameter
  if (count == 0) {
    tunerError("no parameter definition found: ",
               "check that the parameter file is not empty")
  }
  # Sort parameters in 'constraints' in the proper order according to
  # constraints
  hierarchyLevel <- c()
  for (paramName in param.names)
    hierarchyLevel <- c(hierarchyLevel, treeLevel(paramName, constraints))
  constraints <- constraints[order(hierarchyLevel)]

  # Print the hierarchy vector:
  if (debugLevel >= 1) {
    count <- 1
    cat ("--- Hierarchy vector ---\n")
    cat("Param : Level\n")
    for (paramName in param.names) {
      cat(paramName," : ", hierarchyLevel[count], "\n")
      count <- count + 1
    }
    cat ("------------------------\n")
  }

  stopifnot(length(constraints) == length(parameters$names))

  parameters$constraints <- constraints
  names(parameters$names) <-
    names(parameters$types) <- 
      names(parameters$switches) <- 
        names(parameters$boundary) <- 
          names(parameters$isFixed) <- param.names

  parameters$nbParameters <- length(parameters$names)
  parameters$nbFixed <- sum(parameters$isFixed == TRUE)
  parameters$nbVariable <- sum(parameters$isFixed == FALSE)
  if (debugLevel >= 2) print(parameters)
  return (parameters)
}