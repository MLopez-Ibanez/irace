#########################################################
## READ DEFINITION OF PARAMETERS FROM A FILE
#########################################################

#' readParameters
#'
#' \code{readParameters} reads the parameters to be tuned by 
#' \pkg{irace} from a file or directly from a character string.
#' 
#' @param file (optional) Character string: the name of the file 
#'  containing the definitions of theparameters to be tuned.
#' @param digits The number of decimal places to be considered for the
#'  real parameters.
#' @param debugLevel Integer: the debug level to increase the amount of output.
#' @param text (optional) Character string: if file is not supplied and this is,
#'  then parameters are read from the value of text via a text connection.
#' 
#' @return A list containing the definitions of the parameters read. The list is
#'  structured as follows:
#'   \describe{
#'     \item{\code{names}}{Vector that contains the names of the parameters.}
#'     \item{\code{types}}{Vector that contains the type of each parameter 'i', 'c', 'r', 'o'.
#'       Numerical parameters can be sampled in a log-scale with 'i,log' and 'r,log'
#'       (no spaces).}
#'     \item{\code{switches}}{Vector that contains the switches to be used for the
#'       parameters on the command line.}
#'     \item{\code{domain}}{List of vectors, where each vector may contain two
#'       values (minimum, maximum) for real and integer parameters, or
#'       possibly more for categorical parameters.}
#'     \item{\code{conditions}}{List of R logical expressions, with variables
#'       corresponding to parameter names.}
#'     \item{\code{isFixed}}{Logical vectors that specifies which parameter is fixed
#'       and, thus, it does not need to be tuned.}
#'     \item{\code{nbParameters}}{An integer, the total number of parameters.}
#'     \item{\code{nbFixed}}{An integer, the number of parameters with a fixed value.}
#'     \item{\code{nbVariable}}{Number of variable (to be tuned) parameters.}
#'   }
#'
#' @details Either 'file' or 'text' must be given. If 'file' is given, the
#'  parameters are read from the file 'file'. If 'text' is given instead,
#'  the parameters are read directly from the 'text' character string.
#'  In both cases, the parameters must be given (in 'text' or in the file
#'  whose name is 'file') in the expected form.  See the documentation
#'  for details.  If none of these parameters is given, \pkg{irace}
#'  will stop with an error.
#'
#'  A fixed parameter is a parameter that should not be sampled but
#'  instead should be always set to the only value of its domain.  In this
#'  function we set isFixed to TRUE only if the parameter is a categorical
#'  and has only one possible value.  If it is an integer and the minimum
#'  and maximum are equal, or it is a real and the minimum and maximum
#'  values satisfy 'round(minimum, digits) == round(maximum, digits)',
#'  then the parameter description is rejected as invalid to identify
#'  potential user errors.
#'
#' @examples
#'  ## Read the parameters directly from text
#'  parameters.table <- 'tmax "" i (2, 10)
#'  temp "" r (10, 50)
#'  '
#'  parameters <- readParameters(text=parameters.table)
#'  parameters
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
# Main function to read the parameters definition from a file
readParameters <- function (file, digits = 4, debugLevel = 0, text)
{
  if (missing(file) && !missing(text)) {
    filename <- strcat("text=", deparse(substitute(text)))
    file <- textConnection(text)
    on.exit(close(file))
  } else if (is.character(file)) {
    filename <- file
    file.check (file, readable = TRUE, text = "readParameter: parameter file")
  } else {
    irace.error("'file' must be a character string")
  }

  field.match <- function (line, pattern, delimited = FALSE, sep = "[[:space:]]")
  {
    #cat ("pattern:", pattern, "\n")
    positions <- lapply(1:length(pattern), function(x) regexpr (paste0("^", pattern[x], sep), line))
    if (all(sapply(positions, `[[`, 1) == -1)) {
      #cat("no match: NULL\n")
      return (list(match = NULL, line = line))
    }
    pos.matched.list <- lapply(1:length(pattern), function(x) regexpr (paste0("^", pattern[x]), line))
    #cat("pos.matched:", pos.matched, "\n")
    if (all(sapply(pos.matched.list, `[[`, 1) == -1)) {
      #cat(line)
      return (list(match = NULL, line = line))
    }
    position <- which(sapply(pos.matched.list, `[[`,1) != -1)
    if (length(position) > 1) {
      position <- position[1]
    }
    pos.matched <- pos.matched.list[[position]]
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

  # FIXME: Make quotes mandatory for categorical and ordered parameters.
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
  isFixed <- function (type, domain)
  {
    type <- as.character(type)
    if (type == "i" || type == "r") {
      return (domain[[1]] == domain[[2]])
    } else if (type == "c" || type == "o") {
      return (length(domain) == 1)
    }
  }
  # *************************************************************************
  # Subordinate parameter: ordering of the parameters according to
  # conditions hierarchy
  # *  The conditions hierarchy is an acyclic directed graph.
  #    Functions treeLevel() and treeLevelAux() compute an order on vertex s.t:
  #    level(A) > level(B)  <=>  There is an arc A ---> B
  #    (A depends on B to be activated)
  # *  If a cycle is detected, execution is stopped
  # *  If a parameter depends on another one not defined, execution is stopped
  treeLevelAux <- function(paramName, conditionsTree, rootParam)
  {
    ## FIXME: In R 3.2, all.vars does not work with byte-compiled expressions,
    ## thus we do not byte-compile them; but we could use
    ## all.vars(.Internal(disassemble(condition))[[3]][[1]])
    vars <- all.vars (conditionsTree[[paramName]])
    if (length(vars) == 0) {
      return (1) # This parameter does not have conditions
    } else {
      # This parameter has some conditions
      # Recursive call: level <- MAX( level(m) : m in children )
      maxChildLevel <- 0
      for (child in vars) {
        # The following line detects cycles
        if (child == rootParam)
          irace.error("A cycle detected in subordinate parameters! ",
                      "Check definition of conditions.\n",
                      "One parameter of this cycle is '", rootParam, "'")
        
        # The following line detects a missing definition
        if (child %!in% names(conditionsTree))
          irace.error("A parameter definition is missing! ",
                      "Check definition of parameters.\n",
                      "Parameter '", paramName,
                      "' depends on '", child, "' which is not defined.")
        
        level <- treeLevelAux(child, conditionsTree, rootParam)
        if (level > maxChildLevel)
          maxChildLevel <- level
      }
      level <- maxChildLevel + 1
    }
    return (level)
  }

  treeLevel <- function(paramName, conditionsTree)
  {
    # The last parameter is used to record the root parameter of the
    # recursive call in order to detect the presence of cycles.
    return (treeLevelAux(paramName, conditionsTree, paramName))
  }

  errReadParameters <- function(filename, line, context, ...)
  {
    if (!is.null (context)) {
      context <- paste0(" when reading: \"", context, "\"")
    }
    irace.error (paste0 (...),
                 " at ", filename, ", line ", line, context)
  }

  transform.domain <- function(transf, lower, upper, type)
  {
    if (transf == "") return(transf)
    if (transf == "log") {
      # Reject log if domain contains zero or negative values
      if (any(c(lower,upper) <= 0)) return(NULL)
      
      trLower <- log(lower)
      # +1 to adjust before floor()
      trUpper <- if (type == "i") log(upper + 1) else log(upper)
      
      irace.assert(is.finite(trLower))
      irace.assert(is.finite(trUpper))
      attr(transf, "lower") <- trLower
      attr(transf, "upper") <- trUpper
      return(transf)
    }
    irace.internal.error("unrecognized transformation type '", transf, "'")
  }
    
  parameters <- list(names = c(),
                     types = c(),
                     switches = c(),
                     domain = list(),
                     conditions = list(),
                     isFixed = c(),
                     transform = list())

  conditions <- list()
  lines <- readLines(con = file)
  nbLines <- 0
  count <- 0

  for (line in lines) {
    nbLines <- nbLines + 1
    # Delete comments 
    line <- trim(sub("#.*$", "", line))
    if (nchar(line) == 0) {
      next
    }
    ## Match param.name (unquoted alphanumeric string)
    result <- field.match (line, "[._[:alnum:]]+")
    param.name <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (filename, nbLines, line,
                         "parameter name must be alphanumeric")
    }

    if (param.name %in% parameters$names) {
      errReadParameters (filename, nbLines, NULL,
                         "duplicated parameter name '", param.name, "'")
    }

    ## Match param.switch (quoted string)
    result <- field.match (line, "\"[^\"]*\"", delimited = TRUE)
    param.switch <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (filename, nbLines, line,
                         "parameter switch must be a double-quoted string")
    }
    
    ## Match param.type (longer matches must precede shorter ones)
    result <- field.match (line, c("i,log", "r,log", "c","i","r","o"))
    param.type <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (
        filename, nbLines, line,
        "parameter type must be a single character in {'c','i','r','o'}, ",
        "with 'i', 'r' optionally followed by ',log' (no spaces in between) ",
        "to sample using a logarithmic scale")
    } else if (param.type == "i,log") {
      param.type <- "i"
      param.transform <- "log"
    } else if (param.type == "r,log") {
      param.type <- "r"
      param.transform <- "log"
    } else {
      param.transform <- ""
    }

    ## Match param.value (delimited by parenthesis)
    result <- field.match (line, "\\([^)]+\\)", delimited = TRUE, sep = "")
    param.value <- result$match
    line <- result$line
    if (is.null (result$match)) {
      errReadParameters (filename, nbLines, line,
                         "Allowed values must be a list within parenthesis")
    }

    param.value <- string2vector(param.value)
    if (param.type %in% c("r","i")) {
      param.value <- suppressWarnings(as.numeric(param.value))
      if (any(is.na(param.value)) || length(param.value) != 2) {
        errReadParameters (filename, nbLines, NULL,
                           "incorrect numeric range (", result$match,
                           ") for parameter '", param.name, "'")
      } else if (param.value[1] > param.value[2]) {
        errReadParameters (filename, nbLines, NULL,
                           "lower bound must be smaller than upper bound in numeric range (",
                           result$match, ") for parameter '", param.name, "'")
      }

      if (param.type == "r") {
        # FIXME: Given (0.01,0.99) and digits=1, this produces (0, 1), which is
        # probably not what the user wants.
        param.value <- round(param.value, digits = digits)
      } else if (param.type == "i" && !all(is.wholenumber(param.value))) {
        errReadParameters (filename, nbLines, NULL,
                           "for parameter type 'i' values must be integers (",
                           result$match, ") for parameter '", param.name, "'")
      }

      param.transform <- transform.domain(param.transform,
                                          param.value[1], param.value[2], param.type)
      if (is.null(param.transform)) {
        errReadParameters (filename, nbLines, NULL, "The domain of parameter '",
                           param.name, "' of type 'log' cannot contain zero")
      }
    } else {
      dups <- duplicated(param.value)
      if (any(dups)) {
        errReadParameters (filename, nbLines, NULL,
                           "duplicated values (",
                           paste0('\"', param.value[dups], "\"", collapse = ', '),
                           ") for parameter '", param.name, "'")
      }
    }

    count <- count + 1
    parameters$names[[count]] <- param.name
    parameters$switches[[count]] <- param.switch
    parameters$types[[count]] <- param.type
    parameters$domain[[count]] <- param.value
    parameters$transform[[count]] <- param.transform

    parameters$isFixed[[count]] <-
      isFixed (type = param.type,
               domain = parameters$domain[[count]])
    # Reject non-categorical fixed parameters. They are often the
    # result of a user error.
    if (parameters$isFixed[[count]]) {
      if (param.type == "i") {
        errReadParameters (filename, nbLines, NULL,
                           "lower and upper bounds are the same in numeric range (",
                           param.value[1], ", ", param.value[2],
                           ") for parameter '", param.name, "'")
      } else if (param.type == "r") {
        errReadParameters (filename, nbLines, NULL,
                           "given digits=", digits,
                           ", lower and upper bounds are the same in numeric range (",
                           param.value[1], ", ", param.value[2],
                           ") for parameter '", param.name, "'")
      }
    }

    ## Match start of conditions 
    result <- field.match (line, "\\|", sep="")
    line <- result$line
    if (!is.null(result$match) && nchar(result$match)) {
      result <- field.match (line, ".*$", sep="")
      if (is.null(result$match) || !nchar(result$match))
        errReadParameters (filename, nbLines, line,
                           "expected condition after '|'")
      # FIXME: Provide a better error for invalid conditions like "a 2 0"
      conditions[[param.name]] <- NA
      try(conditions[[param.name]] <- parse(text=result$match))
      if (!is.expression (conditions[[param.name]]))
        errReadParameters (filename, nbLines, line,
                           "invalid condition after '|'")
      line <- result$line
    } else if (!is.null(result$line) && nchar(result$line)) {
      errReadParameters (filename, nbLines, line,
                         "expected '|' before condition")
    } else {
      conditions[[param.name]] <- TRUE
    }
    # *****************************************************************
  } # end loop on lines

  # Check that we have read at least one parameter
  if (count == 0) {
    irace.error("No parameter definition found: ",
                "check that the parameter file is not empty")
  }
  # Sort parameters in 'conditions' in the proper order according to
  # conditions
  hierarchyLevel <- c()
  for (paramName in parameters$names)
    hierarchyLevel <- c(hierarchyLevel, treeLevel(paramName, conditions))

  # FIXME: Check that the parameter names that appear in the
  # conditions all appear in names to catch typos.
  parameters$conditions <- conditions[order(hierarchyLevel)]
  parameters$hierarchy <- hierarchyLevel

  names(parameters$types) <- 
    names(parameters$switches) <- 
      names(parameters$domain) <- 
        names(parameters$isFixed) <-
          names(parameters$hierarchy) <- 
            names(parameters$transform) <- parameters$names

  # Print the hierarchy vector:
  if (debugLevel >= 1) {
    cat ("# --- Hierarchy vector ---\n",
         "# Param : Level\n",
         paste(names(parameters$hierarchy), ":",
               parameters$hierarchy, collapse = "\n"),
         "\n# ------------------------\n", sep = "")
  }

  irace.assert(length(conditions) == length(parameters$names))

  parameters$nbParameters <- length(parameters$names)
  parameters$nbFixed <- sum(parameters$isFixed == TRUE)
  parameters$nbVariable <- sum(parameters$isFixed == FALSE)
  if (debugLevel >= 2) print(parameters, digits = 15)
  return (parameters)
}
