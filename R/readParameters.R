#' Reads the parameters to be tuned by \pkg{irace} from a file or from a
#' character string.
#' 
#' @param file (`character(1)`) \cr Filename containing the definitions of
#'   the parameters to be tuned.
#' @template arg_param_digits
#' @template arg_debuglevel
#' @template arg_text
#'
#' @return A list containing the definitions of the parameters read. The list is
#'  structured as follows:
#'   \describe{
#'     \item{`names`}{Vector that contains the names of the parameters.}
#'     \item{`types`}{Vector that contains the type of each parameter 'i', 'c', 'r', 'o'.
#'       Numerical parameters can be sampled in a log-scale with 'i,log' and 'r,log'
#'       (no spaces).}
#'     \item{`switches`}{Vector that contains the switches to be used for the
#'       parameters on the command line.}
#'     \item{`domain`}{List of vectors, where each vector may contain two
#'       values (minimum, maximum) for real and integer parameters, or
#'       possibly more for categorical parameters.}
#'     \item{`conditions`}{List of R logical expressions, with variables
#'       corresponding to parameter names.}
#'     \item{`isFixed`}{Logical vector that specifies which parameter is fixed
#'       and, thus, it does not need to be tuned.}
#'     \item{`nbParameters`}{An integer, the total number of parameters.}
#'     \item{`nbFixed`}{An integer, the number of parameters with a fixed value.}
#'     \item{`nbVariable`}{Number of variable (to be tuned) parameters.}
#'     \item{`depends`}{List of character vectors, each vector specifies
#'     which parameters depend on this one.}
#'     \item{`isDependent`}{Logical vector that specifies which parameter has
#'       a dependent domain.}
#'     \item{`digits`}{Integer vector that specifies the number of digits per parameter.}
#'     \item{`forbidden`}{List of expressions that define which parameter configurations are forbidden.}
#'   }
#'
#' @details Either `file` or `text` must be given. If `file` is given, the
#'  parameters are read from the file `file`. If `text` is given instead,
#'  the parameters are read directly from the `text` character string.
#'  In both cases, the parameters must be given (in `text` or in the file
#'  whose name is `file`) in the expected form.  See the documentation
#'  for details.  If none of these parameters is given, \pkg{irace}
#'  will stop with an error.
#'
#'  A fixed parameter is a parameter that should not be sampled but
#'  instead should be always set to the only value of its domain.  In this
#'  function we set `isFixed` to TRUE only if the parameter is a categorical
#'  and has only one possible value.  If it is an integer and the minimum
#'  and maximum are equal, or it is a real and the minimum and maximum
#'  values satisfy `round(minimum, digits) == round(maximum, digits)`,
#'  then the parameter description is rejected as invalid to identify
#'  potential user errors.
#'
#' The order of the parameters determines the order in which parameters are
#' given to `targetRunner`. Changing the order may also change the results
#' produced by `irace`, even with the same random seed.
#'
#' @examples
#'  ## Read the parameters directly from text
#'  parameters.table <- '
#'  # name       switch           type  values               [conditions (using R syntax)]
#'  algorithm    "--"             c     (as,mmas,eas,ras,acs)
#'  localsearch  "--localsearch " c     (0, 1, 2, 3)
#'  alpha        "--alpha "       r     (0.00, 5.00)
#'  beta         "--beta "        r     (0.00, 10.00)
#'  rho          "--rho  "        r     (0.01, 1.00)
#'  ants         "--ants "        i,log (5, 100)
#'  q0           "--q0 "          r     (0.0, 1.0)           | algorithm == "acs"
#'  q0dep       "--q0 "           r     (0.0, q0)            | algorithm != "acs" 
#'  rasrank      "--rasranks "    i     (1, "min(ants, 10)") | algorithm == "ras"
#'  elitistants  "--elitistants " i     (1, ants)            | algorithm == "eas"
#'  nnls         "--nnls "        i     (5, 50)              | localsearch %in% c(1,2,3)
#'  dlb          "--dlb "         c     (0, 1)               | localsearch %in% c(1,2,3)
#'  
#'  [forbidden]
#'  (alpha == 0.0) & (beta == 0.0)
#'  '
#'  parameters <- readParameters(text=parameters.table)
#'  str(parameters)
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
readParameters <- function (file, digits = 4L, debugLevel = 0L, text)
{
  if (missing(file) && !missing(text)) {
    filename <- NA
    file <- textConnection(text)
    on.exit(close(file))
  } else if (is.character(file)) {
    filename <- file
    file.check (file, readable = TRUE, text = "readParameter: parameter file")
  } else {
    irace.error("'file' must be a character string")
  }

  digits <- as.integer(digits)
  
  field.match <- function (line, pattern, delimited = FALSE, sep = "[[:space:]]")
  {
    #cat ("pattern:", pattern, "\n")
    positions <- lapply(seq_along(pattern), function(x) regexpr (paste0("^", pattern[x], sep), line))
    if (all(sapply(positions, "[[", 1) == -1)) {
      #cat("no match: NULL\n")
      return (list(match = NULL, line = line))
    }
    pos.matched.list <- lapply(seq_along(pattern), function(x) regexpr (paste0("^", pattern[x]), line))
    #cat("pos.matched:", pos.matched, "\n")
    if (all(sapply(pos.matched.list, "[[", 1) == -1)) {
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
    list(match = match, line = line)
  }

  string2vector <- function(str) {
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
  #    Function treeLevel() computes an order on vertex s.t:
  #    level(A) > level(B)  <=>  There is an arc A ---> B
  #    (A depends on B to be activated)
  # *  If a cycle is detected, execution is stopped
  # *  If a parameter depends on another one not defined, execution is stopped
  treeLevel <- function(paramName, varsTree, rootParam = paramName)
  {
    # The last parameter is used to record the root parameter of the
    # recursive call in order to detect the presence of cycles.
    vars <- varsTree[[paramName]]
    if (length(vars) == 0) return (1) # This parameter does not have conditions

    # This parameter has some conditions
    # Recursive call: level <- MAX( level(m) : m in children )
    maxChildLevel <- 0
    for (child in vars) {
      # The following line detects cycles
      if (child == rootParam)
        irace.error("A cycle detected in subordinate parameters! ",
                    "Check definition of conditions and/or dependent domains.\n",
                    "One parameter of this cycle is '", rootParam, "'")
      
      # The following line detects a missing definition
      if (child %not_in% names(varsTree))
        irace.error("A parameter definition is missing! ",
                    "Check definition of parameters.\n",
                    "Parameter '", paramName,
                    "' depends on '", child, "' which is not defined.")
        
      level <- treeLevel(child, varsTree, rootParam)
      if (level > maxChildLevel)
        maxChildLevel <- level
    }
    level <- maxChildLevel + 1
    return (level)
  }

  errReadParameters <- function(filename, line, context, ...)
  {
    context <- if (is.null (context)) "" else paste0(" when reading: \"", context, "\"")
    fileloc <- if (is.na(filename)) "" else paste0("'", filename, "'," ) 
    irace.error(paste0(..., collapse = ""), " at ", fileloc, "line ", line, context)
  }
  
  warnReadParameters <- function(filename, line, context, ...)
  {
    context <- if (is.null (context)) "" else paste0(" when reading: \"", context, "\"")
    fileloc <- if (is.na(filename)) "" else paste0("'", filename, "'," ) 
    irace.warning(paste0(..., collapse = ""), " at ", fileloc, "line ", line, context)
  }

  parse_condition <- function(s, filename, nbLines, line, context) {
    if (grepl("||", s, fixed=TRUE) || grepl("&&", s, fixed=TRUE)) {
      warnReadParameters (filename, nbLines, line,
                          paste0("Please use '&' and '|' instead of '&&' and '|'", context))
      s <- gsub("||", "|", fixed=TRUE, gsub("&&", "&", fixed=TRUE, s))
    }
    str2expression(s)
  }
  

  transform.domain <- function(transf, domain, type)
  {
    if (transf == "") return(transf)
    
    # We do not support transformation of dependent parameters, yet.
    # TODO: think about dependent domain transfomation
    if (is.expression(domain))
      irace.error("Parameter domain transformations are not yet available for",
                  " dependent parameter domains.")

    lower <- domain[1]
    upper <- domain[2]

    if (transf == "log") {
      # Reject log if domain contains zero or negative values
      if (any(domain <= 0)) return(NULL)

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

  # Checks that variables in the expressions are within
  # the parameters names.
  check_parameter_dependencies <- function (parameters) {
    for (p in names(Filter(length, parameters$depends))) {
      vars <- parameters$depends[[p]]
      flag <- vars %in% parameters$names
      if (!all(flag)) {
        irace.error ("Domain (", paste0(parameters$domain[[p]], collapse=", "),
                     ") of parameter '", p, "' is not valid: '",
                     paste0(vars[!flag], collapse=", "),
                     "' cannot be found in the scenario parameters: ",
                     paste0(parameters$names, collapse=" , ")," .")
      }
      flag <- parameters$types[vars] %in% c("i", "r")
      if (!all(flag)) {
        irace.error ("Domain of parameter '", p, "' depends on non-numerical", 
                     " parameters: ", paste0(vars[!flag], collapse=", "), " .")
      }

      # Supported operations for dependent domains
      allowed.fx <- c("+", "-", "*", "/", "%%", "min", "max", "round", "floor", "ceiling", "trunc")
      fx <- setdiff(all.names(parameters$domain[[p]], unique=TRUE), 
                       all.vars(parameters$domain[[p]], unique=TRUE))
      flag <- fx %in% allowed.fx
      if (!all(flag)) {
        irace.error ("Domain of parameter '", p, "' uses function(s) ", 
                     "not yet supported by irace: ",
                     paste0(fx[!flag], collapse=", "), " .")
      }
    }
    return(TRUE)
  }
  
  parameters <- list(names = character(0),
                     types = character(0),
                     switches = character(0),
                     domain = list(),
                     conditions = list(),
                     isFixed = logical(0),
                     # FIXME: This has to be a list because we assign
                     # attributes to elements.
                     transform = list(),
                     isDependent = logical(0))

  conditions <- list()
  lines <- readLines(con = file)
  # Delete comments 
  lines <- trim(sub("#.*$", "", lines))
  nbLines <- 0
  count <- 0
  forbidden <- NULL
  has_forbidden <- FALSE

  for (line in lines) {
    nbLines <- nbLines + 1
    if (nchar(line) == 0) next
    
    if (has_forbidden) {
      # FIXME: Better error reporting.
      exp <- parse_condition(line, filename, nbLines, line, " for forbidden expressions")
      forbidden <- c(forbidden, exp)
      next
    }
    if (grepl("^[[:space:]]*\\[forbidden\\]", line)) {
      has_forbidden <- TRUE
      next
    }
    ## Match param.name (unquoted alphanumeric string)
    result <- field.match (line, "[._[:alnum:]]+")
    param.name <- result$match
    line <- result$line
    if (is.null(result$match)) {
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
    # Regexp to detect dependent domains of the type ("min(p1)", 100)
    result <- field.match (line, "\\([^|]+\\)", delimited = TRUE, sep = "")
    param.value <- result$match
    line <- result$line
    if (is.null (param.value)) {
      errReadParameters (filename, nbLines, line,
                         "Allowed values must be a list within parenthesis")
    }

    # For numerical parameters domains could be dependent
    # thus, we keep the string values in a variable
    # for example (10, param1+2)
    param.value.str <- string2vector(param.value)
    if (param.type %in% c("r","i")) {
      # For dependent domains param.value will be NA (we will parse
      # it later)
      param.value <- suppressWarnings(as.numeric(param.value.str))
      if (length(param.value) != 2) {
        errReadParameters (filename, nbLines, NULL,
                           "incorrect numeric range (", result$match,
                           ") for parameter '", param.name, "'")
      }

      valid_real_bound <- function(x, digits) {
        if (is.na(x) || x == 0) return(TRUE)
        rx <- round(x, digits = digits)
        ((abs(rx - x) <= .irace_tolerance * max(1, abs(x)))
          && digits >= -log10(abs(x)))
      }

      if (param.type == "r") {
        if (digits >= 15L) { # This is almost infinite-precision so we do not complain.
          param.value <- round(param.value, digits = digits)
        } else if (!valid_real_bound(param.value[1], digits)
                   || !valid_real_bound(param.value[2], digits)) {
          for (i in seq.int(digits+1L,15L)) {
            if (valid_real_bound(param.value[1], i) &&
                valid_real_bound(param.value[2], i))
              break
          }
          errReadParameters (filename, nbLines, NULL,
                             "for parameter '", param.name, "' of type 'r' domain bounds (",
                             param.value[1], ", ", param.value[2],
                             ") must be representable within the given 'digits=",
                             digits, "'; you would need at least 'digits=", i, "' or adjust the domain")
        }
      } else if (param.type == "i" && any(!is.wholenumber(param.value[!is.na(param.value)]))) {
        errReadParameters (filename, nbLines, NULL,
                           "for parameter '", param.name, "' of type 'i' values must be integers (",
                           result$match, ")")
      }
      
      # Time to parse dependent domains or check values
      if (any(is.na(param.value))) {
        try(param.value[is.na(param.value)] <- parse(text=param.value.str[is.na(param.value)]))
      } else if (param.value[1] >= param.value[2]) {
        errReadParameters (filename, nbLines, NULL,
                           "lower bound must be smaller than upper bound in numeric range (",
                           result$match, ") for parameter '", param.name, "'")
      }
             
      param.transform <- transform.domain(param.transform, param.value, param.type)
      if (is.null(param.transform)) {
        errReadParameters (filename, nbLines, NULL, "The domain of parameter '",
                           param.name, "' of type 'log' cannot contain zero")
      }
    } else {
      param.value <- param.value.str
      if (anyDuplicated(param.value)) {
        dups <- duplicated(param.value)
        errReadParameters (filename, nbLines, NULL,
                           "duplicated values (",
                           paste0('\"', param.value[dups], "\"", collapse = ', '),
                           ") for parameter '", param.name, "'")
      }
    }

    count <- count + 1
    parameters$names[count] <- param.name
    parameters$switches[count] <- param.switch
    parameters$types[count] <- param.type
    parameters$domain[[count]] <- param.value
    parameters$transform[[count]] <- param.transform
    
    parameters$isFixed[count] <- isFixed(type = param.type,
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
      try(conditions[[param.name]] <- parse_condition(result$match, filename, nbLines, line, " for condition"))
      if (!is.expression(conditions[[param.name]]))
        errReadParameters (filename, nbLines, line, "invalid condition after '|'")
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

  # Generate dependency flag
  # FIXME: check if we really need this vector
  
  names(parameters$types) <- 
    names(parameters$switches) <- 
      names(parameters$domain) <- 
        names(parameters$isFixed) <-
            names(parameters$transform) <- parameters$names

  parameters$isDependent <- sapply(parameters$domain, is.expression)
  parameters$digits <- sapply(parameters$types[parameters$types == 'r'], function(x) digits)

  check_forbidden_params <- function(x, pnames, filename = NULL)
  {
    if (length(NULL) || all(all.vars(x) %in% pnames)) return(invisible())
    for (exp in x) {
      v <- setdiff(all.vars(exp), pnames)
      if (length(v)) {
        v <- paste0(v, collapse=", ")
        if (is.null(filename)) {
          irace.error("Expression '", deparse(exp), "' after [forbidden] contains unknown parameter(s): ", v)
        } else {
          irace.error("Expression '", deparse(exp), "' in '", filename, "' contains unknown parameter(s): ", v)
        }
      }
    }
  }

  if (length(forbidden)) {
    irace.note(length(forbidden), " expression(s) specifying forbidden configurations read.\n")
    check_forbidden_params(forbidden, parameters$names)
    # FIXME: Instead of a list, we should generate a single expression that is
    # the logical-OR of all elements of the list.
    # First we would need to handle the "is.na(x) | !(x)" case here.
    # Maybe: sapply(forbiddenExps, function(x) substitute(is.na(x) | !(x), list(x=x)))
    # x <- parse(text=paste0("(", paste0(forbiddenExps,collapse=")||("), ")"))
    parameters$forbidden <- sapply(forbidden, compile_forbidden)
  }
    
  # Obtain the variables in each condition
  parameters$depends <- lapply(parameters$domain, all.vars)
  # Check that dependencies are ok
  check_parameter_dependencies(parameters)
  # Merge dependencies and conditions
  parameters$depends <- Map(c, parameters$depends, lapply(conditions, all.vars))
  parameters$depends <- lapply(parameters$depends, unique)
  
  # Sort parameters in 'conditions' in the proper order according to
  # conditions
  hierarchyLevel <- sapply(parameters$names, treeLevel,
                           varsTree = parameters$depends)
  parameters$hierarchy <- hierarchyLevel
  parameters$conditions <- conditions[order(hierarchyLevel)]
  
  names(parameters$hierarchy) <- parameters$names

  # Print the hierarchy vector:
  if (debugLevel >= 1) {
    cat ("# --- Parameters Hierarchy ---\n")
    print(data.frame(Parameter = paste0(names(parameters$hierarchy)),
                     Level = parameters$hierarchy,
                     "Depends on" = sapply(parameters$depends, paste0, collapse=", "),
                     row.names=NULL))
    cat("# ------------------------\n")
  }

  irace.assert(length(parameters$conditions) == length(parameters$names))

  parameters$nbParameters <- length(parameters$names)
  parameters$nbFixed <- sum(parameters$isFixed == TRUE)
  parameters$nbVariable <- sum(parameters$isFixed == FALSE)
  if (debugLevel >= 2) {
    print(parameters, digits = 15)
    irace.note("Parameters have been read\n")
  }
  parameters
}

#' Read parameters in PCS (AClib) format and write them in irace format.
#' 
#' @param file (`character(1)`) \cr Filename containing the definitions of
#'   the parameters to be tuned.
#' @template arg_param_digits
#' @template arg_debuglevel
#' @template arg_text
#' 
#' @return A string representing the parameters in irace format.
#'
#' @details Either `file` or `text` must be given. If `file` is given, the
#'  parameters are read from the file `file`. If `text` is given instead,
#'  the parameters are read directly from the `text` character string.
#'  In both cases, the parameters must be given (in `text` or in the file
#'  whose name is `file`) in the expected form.  See the documentation
#'  for details.  If none of these parameters is given, \pkg{irace}
#'  will stop with an error.
#'
#' **FIXME:** Multiple conditions and default configuration are currently ignored. See <https://github.com/MLopez-Ibanez/irace/issues/31>
#'
#' @references
#' Frank Hutter, Manuel López-Ibáñez, Chris Fawcett, Marius Thomas Lindauer, Holger H. Hoos, Kevin Leyton-Brown, and Thomas Stützle. **AClib: A Benchmark Library for Algorithm Configuration**. In P. M. Pardalos, M. G. C. Resende, C. Vogiatzis, and J. L. Walteros, editors, _Learning and Intelligent Optimization, 8th International Conference, LION 8_, volume 8426 of Lecture Notes in Computer Science, pages 36–40. Springer, Heidelberg, 2014.
#' 
#' @seealso [readParameters()]
#' @examples
#'  ## Read the parameters directly from text
#'  pcs_table <- '
#'  # name       domain
#'  algorithm    {as,mmas,eas,ras,acs}[as]
#'  localsearch  {0, 1, 2, 3}[0]
#'  alpha        [0.00, 5.00][1]
#'  beta         [0.00, 10.00][1]
#'  rho          [0.01, 1.00][0.95]
#'  ants         [1, 100][10]il
#'  q0           [0.0, 1.0][0]
#'  rasrank      [1, 100][1]i
#'  elitistants  [1, 750][1]i
#'  nnls         [5, 50][5]i
#'  dlb          {0, 1}[1] 
#'  Conditionals:
#'  q0 | algorithm in {acs}
#'  rasrank | algorithm in {ras}
#'  elitistants | algorithm in {eas}
#'  nnls | localsearch in {1,2,3}
#'  dlb | localsearch in {1,2,3} 
#'  {alpha=0, beta=0}'
#'  parameters_table <- read_pcs_file(text=pcs_table)
#'  cat(parameters_table)
#'  parameters <- readParameters(text=parameters_table)
#'  str(parameters)
#' 
#' @author Manuel López-Ibáñez
#' @export
read_pcs_file <- function(file, digits = 4L, debugLevel = 0L, text)
{
  if (missing(file) && !missing(text)) {
    filename <- paste0("text=", deparse(substitute(text)))
    file <- textConnection(text)
    on.exit(close(file))
  } else if (is.character(file)) {
    filename <- file
    file.check (file, readable = TRUE, text = "read_pcs_file: parameter file")
  } else {
    irace.error("'file' must be a character string")
  }
  lines <- readLines(con = file)
  lines <- trim(lines) # Remove leading and trailing whitespace
  lines <- lines[!grepl("Conditionals:", lines, fixed=TRUE)] # useless line
  conditions <- list()
  forbidden <- NULL
  regex_cond <- "^([^[:space:]]+)[[:space:]]+\\|[[:space:]]+(.+)$"
  regex_forbidden <- "^{(.+)}$"
  for (k in seq_along(lines)) {
    if (grepl(regex_cond, lines[k], perl=TRUE)) {
      matches <- regmatches(lines[k],
                            regexec(regex_cond, lines[k], perl=TRUE))[[1L]]
      stopifnot(length(matches) > 0)
      lines[k] <- NA
      conditions[[matches[[2L]]]] <- matches[[3L]]
    } else if (grepl(regex_forbidden, lines[k], perl=TRUE)) {
      forbidden <- c(forbidden, sub(regex_forbidden, "\\1", lines[k], perl=TRUE))
      lines[k] <- NA
    }
  }
  
  parse_pcs_condition <- function(x, types) {
    if (is.null(x)) return ("")
    matches <- regmatches(x, regexec("([^[:space:]]+)[[:space:]]+in[[:space:]]+\\{([^}]+)\\}$", x, perl=TRUE))[[1L]]
    if (length(matches) == 0L) irace.error("unknown condition ", x)
    param <- matches[[2L]]
    type <- types[[param]]
    if (is.null(type)) irace.error("unknown type for ", param, " in condition: ", x)
    cond <- matches[[3L]]
    if (type == "c" || type == "o") {
      cond <- strsplit(cond, ",[[:space:]]*")[[1L]]
      equal <- (length(cond) == 1L)
      cond <- paste0('"', cond, '"', collapse=',')
    } else { 
      equal <- grepl(",", cond, fixed=TRUE)
    }
    if (equal)
      return(paste0(" | ", param, ' == ', cond))
    return(paste0(" | ", param, " %in% c(", cond, ")"))
  }
  param_types <- list()
  param_domains <- list()
  param_comments <- list()
  lines <- lines[!is.na(lines)]
  for (line in lines) {
    if (startsWith(line, "#") || line == "") next
    # match a parameter
    matches <- regmatches(line, regexec("^([^[:space:]]+)[[:space:]]+\\[([^,]+),[[:space:]]*([^]]+)\\][[:space:]]*\\[[^]]+\\](i?l?i?)(.*)$", line, perl=TRUE))[[1]]
    if (length(matches) > 0) {
      param_name <- matches[[2L]]
      
      param_type <- paste0(if(grepl("i", matches[5L], fixed=TRUE)) "i" else "r",
                           if(grepl("l", matches[5L], fixed=TRUE)) ",log" else "")
      param_types[[param_name]] <- param_type
      param_domains[[param_name]] <- paste0("(", matches[3L], ", ", matches[4L], ")")
      param_comments[[param_name]] <- matches[6L]
      next
    }
    matches <- regmatches(line, regexec("^([^[:space:]]+)[[:space:]]+\\{([^}]+)\\}[[:space:]]*\\[[^]]+\\](.*)$", line, perl=TRUE))[[1L]]
    if (length(matches) > 0) {
      param_name <- matches[[2L]]
      param_type <- "c"
      param_types[[param_name]] <- param_type
      param_types[[param_name]] <- param_type
      param_domains[[param_name]] <- paste0("(", matches[3L], ")")
      param_comments[[param_name]] <- matches[4L]
      next
    }
  }
  output <- ""
  for (line in lines) {
    if (startsWith(line, "#") || line == "") {
      output <- paste0(output, line, "\n")
      next
    }
    # match a parameter
    matches <- regmatches(line, regexec("^([^[:space:]]+)[[:space:]]+", line, perl=TRUE))[[1L]]
    if (length(matches) > 0) {
      param_name <- matches[[2L]]
      cond <- parse_pcs_condition(conditions[[param_name]], param_types)
      output <- paste0(output,
                       sprintf('%s "%s" %s %s%s%s\n',
                               param_name, param_name, param_types[[param_name]], param_domains[[param_name]], cond, param_comments[[param_name]]))
      next
    }
    irace.error("unrecognized line: ", line)
  }
  if (length(forbidden) > 0L) {
    exp <- sapply(forbidden, function(x) {
      # FIXME: this will break if there are "," within the values.
      x <- strsplit(x, ",[[:space:]]*")[[1L]]
      paste0(collapse=" & ",
             sapply(regmatches(x, regexec("^([^=]+)=(.+)$", x, perl=TRUE)), function(matches) {
               rhs <- trim(matches[[3L]])
               if (!any(startsWith(rhs, c("'", "\""))) && suppressWarnings(is.na(as.numeric(rhs))))
                 rhs <- paste0('"', rhs, '"')
               paste0("(", trim(matches[[2L]]), " == ", rhs, ")")
             }, USE.NAMES=FALSE))
    }, USE.NAMES=FALSE)
    output <- paste0(output, "\n[forbidden]\n", paste0(collapse="\n", exp), "\n")
  }
  output
}

#' checkParameters
#'
#' FIXME: This is incomplete, for now we only repair inputs from previous irace
#' versions.
#'
#' @template arg_parameters
#' @export
checkParameters <- function(parameters)
{
  if (is.null(parameters$isDependent)) {
    parameters$isDependent <- sapply(parameters$domain, is.expression)
    names(parameters$isDependent) <- parameters$names
  }
  parameters
}

#' Print parameter space in the textual format accepted by irace.
#' 
#' @template arg_parameters
#'
#' @return `character()`
#' 
#' @seealso [readParameters()]
#' @examples
#' parameters.table <- '
#'  # name       switch           type  values               [conditions (using R syntax)]
#'  algorithm    "--"             c     (as,mmas,eas,ras,acs)
#'  localsearch  "--localsearch " c     (0, 1, 2, 3)
#'  alpha        "--alpha "       r     (0.00, 5.00)
#'  beta         "--beta "        r     (0.00, 10.00)
#'  rho          "--rho  "        r     (0.01, 1.00)
#'  ants         "--ants "        i,log (5, 100)
#'  q0           "--q0 "          r     (0.0, 1.0)           | algorithm == "acs"
#'  q0dep       "--q0 "           r     (0.0, q0)            | algorithm != "acs"
#'  rasrank      "--rasranks "    i     (1, "min(ants, 10)") | algorithm == "ras"
#'  elitistants  "--elitistants " i     (1, ants)            | algorithm == "eas"
#'  nnls         "--nnls "        i     (5, 50)              | localsearch %in% c(1,2,3)
#'  dlb          "--dlb "         c     (0, 1)               | localsearch %in% c(1,2,3)
#'  
#'  [forbidden]
#'  (alpha == 0.0) & (beta == 0.0)
#' '
#' parameters <- readParameters(text=parameters.table)
#' printParameters(parameters)
#' @export
printParameters <- function(parameters)
{
  names_len <- max(nchar(parameters$names))
  label_len <- max(nchar(parameters$switches)) + 2
  for (name in parameters$names) {
    label <- paste0('"', parameters$switches[[name]], '"')
    type <- parameters$types[[name]]
    transf <- parameters$transform[[name]]
    domain <- parameters$domain[[name]]
    if (is.expression(domain)) {
      domain <- sapply(domain, function(x) {
        if (is.numeric(x)) {
          x <- as.numeric(x)
          if (type == "r")
            x <- formatC(x, digits = parameters$digits[[name]], format="f", drop0trailing=TRUE)
        } else if (is.character(x)) {
          x <- as.character(x)
        } else {
          x <- paste0("\"", deparse(x, width.cutoff = 500L), "\"")
        }
        x
      })
    }
    domain <- paste0('(', paste0(domain, collapse=","), ')')
    condition <- parameters$conditions[[name]]
    condition <- if (isTRUE(condition)) "" else paste0(" | ", condition)
    if (!is.null(transf) && transf != "") type <- paste0(type, ",", transf)
    cat(sprintf('%*s %*s %s %-15s%s\n', -names_len, name, -label_len, label, type, domain, condition))
  }
  if (!is.null(parameters$forbidden)) {
    cat("\n[forbidden]\n",
        paste0(collapse="\n", sapply(parameters[["forbidden"]], attr, "source")),
        "\n", sep="")
  }
}
