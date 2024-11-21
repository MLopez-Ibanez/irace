#' Reads the parameters to be tuned by \pkg{irace} from a file or from a
#' character string.
#' 
#' @param file `character(1)`\cr Filename containing the definitions of
#'   the parameters to be tuned.
#' @param digits `integer(1)`\cr The number of decimal places to be considered for real-valued parameters.
#' @param debugLevel `integer(1)`\cr Larger values produce more verbose output.
#' @param text `character(1)`\cr If \code{file} is not supplied and this is,
#'  then parameters are read from the value of \code{text} via a text connection.
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
#'     \item{`is_dependent`}{Logical vector that specifies which parameter has
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
#'  parameters_table <- '
#'  # name       switch           type  values               [conditions (using R syntax)]
#'  algorithm    "--"             c     (as,mmas,eas,ras,acs)
#'  localsearch  "--localsearch " o     (0, 1, 2, 3)
#'  alpha        "--alpha "       r     (0.00, 5.00)
#'  beta         "--beta "        r     (0.00, 10.00)
#'  rho          "--rho  "        r     (0.01, 1.00)
#'  ants         "--ants "        i,log (5, 100)
#'  q0           "--q0 "          r     (0.0, 1.0)           | algorithm == "acs"
#'  rasrank      "--rasranks "    i     (1, "min(ants, 10)") | algorithm == "ras"
#'  elitistants  "--elitistants " i     (1, ants)            | algorithm == "eas"
#'  nnls         "--nnls "        i     (5, 50)              | localsearch %in% c(1,2,3)
#'  dlb          "--dlb "         c     (0, 1)               | localsearch %in% c(1,2,3)
#'  
#'  [forbidden]
#'  (alpha == 0.0) & (beta == 0.0)
#'  [global]
#'  digits = 4
#'  '
#'  parameters <- readParameters(text=parameters_table)
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
    if (all(sapply(positions, "[[", 1L) == -1L)) {
      #cat("no match: NULL\n")
      return (list(match = NULL, line = line))
    }
    pos_matched <- lapply(seq_along(pattern), function(x) regexpr (paste0("^", pattern[x]), line))
    #cat("pos.matched:", pos.matched, "\n")
    if (all(sapply(pos_matched, "[[", 1L) == -1L)) {
      #cat(line)
      return (list(match = NULL, line = line))
    }
    position <- which(sapply(pos_matched, `[[`,1L) != -1L)
    if (length(position) > 1L) {
      position <- position[1L]
    }
    pos_matched <- pos_matched[[position]]
    delimited <- as.integer(delimited)
    match <- substr(line, pos_matched[1L] + delimited,
                    attr(pos_matched, "match.length") - delimited)
    #cat("match:",match, "\n")
    line <- substr(line, pos_matched[1L] + attr(pos_matched, "match.length"), nchar(line))
    line <- trim_leading (line)
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
    v
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
  
  params <- list()
  pnames <- c()
  lines <- readLines(con = file)
  # Delete comments 
  lines <- trim(sub("#.*$", "", lines))
  within_global <- FALSE
  nbLines <- 0L
  # Parse [global] first.
  for (line in lines) {
    nbLines <- nbLines + 1L
    if (nchar(line) == 0L) next
    if (grepl("^[[:space:]]*\\[forbidden\\]", line)) {
      if (within_global)
        break
      next
    }
    if (within_global) {
      if (grepl("^[[:space:]]*digits[[:space:]]*=[[:space:]]*[0-9]+[[:space:]]*$", line)) {
        eval(parse(text=line))
        if (!is.wholenumber(digits) || digits > 15 || digits < 1)
          errReadParameters(filename, nbLines, line, "'digits' must be an integer within [1, 15]")
        digits <- as.integer(digits)
      } else 
        errReadParameters(filename, nbLines, line, "Unknown global option")
      lines[nbLines] <- "" # Do not parse it again.
      next
    }
    if (grepl("^[[:space:]]*\\[global\\]", line)) {
      within_global <- TRUE
      lines[nbLines] <- "" # Do not parse it again.
      next
    }
  }
  forbidden <- NULL
  within_forbidden <- FALSE
  nbLines <- 0L
  for (line in lines) {
    nbLines <- nbLines + 1L
    if (nchar(line) == 0L) next
    
    if (within_forbidden) {
      # FIXME: Better error reporting.
      exp <- parse_condition(line, filename, nbLines, line, " for forbidden expressions")
      forbidden <- c(forbidden, exp)
      next
    }
    if (grepl("^[[:space:]]*\\[forbidden\\]", line)) {
      within_forbidden <- TRUE
      next
    }
    ## Match name (unquoted alphanumeric string)
    result <- field.match (line, "[._[:alnum:]]+")
    name <- result$match
    line <- result$line
    if (is.null(result$match)) {
      errReadParameters (filename, nbLines, line,
                         "Parameter name must be alphanumeric")
    }

    if (name %in% pnames) {
      errReadParameters (filename, nbLines, NULL,
                         "Duplicated parameter name '", name, "'")
    }
    
    ## Match p_switch (quoted string)
    result <- field.match (line, "\"[^\"]*\"", delimited = TRUE)
    p_label <- result$match
    line <- result$line
    if (is.null(p_label)) {
      errReadParameters (filename, nbLines, line,
                         "Parameter label (switch) must be a double-quoted string")
    }
    
    ## Match param.type (longer matches must precede shorter ones)
    result <- field.match (line, c("i,log", "r,log", "c", "i", "r", "o"))
    param.type <- result$match
    line <- result$line
    if (is.null (param.type)) {
      errReadParameters(filename, nbLines, line,
                        "Parameter type must be a single character in {'c','i','r','o'}, ",
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
    
    ## Match domain (delimited by parenthesis)
    # Regexp to detect dependent domains of the type ("min(p1)", 100)
    result <- field.match (line, "\\([^|]+\\)", delimited = TRUE, sep = "")
    domain_str <- result$match
    line <- result$line
    if (is.null (domain_str)) {
      errReadParameters (filename, nbLines, line,
                         "Allowed values must be a list within parenthesis")
    }
    # For numerical parameters domains could be dependent
    # thus, we keep the string values in a variable
    # for example (10, param1+2)
    if (param.type %in% c("r","i")) {
      domain <- eval(parse(text=paste0("expression(", domain_str, ")"), keep.source=FALSE))
      # Some expressions like -10 may need to be evaluated again.
      domain <- sapply(domain, function(x) if (!is.expression(x) || length(all.vars(x, unique=FALSE))) x else eval(x), USE.NAMES=FALSE)
      if (is.list(domain)) domain <- as.expression(domain)
      # For dependent domains domain will be NA (we will parse it later)
      if (length(suppressWarnings(as.numeric(sapply(domain, function(x) if(is.language(x)) NA else x)))) != 2L) {
        errReadParameters (filename, nbLines, NULL,
                           "Incorrect numeric range (", result$match,
                           ") for parameter '", name, "'")
      }
    } else { # type %in% c("c", "o")
      domain <- string2vector(domain_str)
    }
    ## Match start of conditions 
    result <- field.match (line, "\\|", sep="")
    line <- result$line
    if (!is.null(result$match) && nchar(result$match)) {
      result <- field.match (line, ".*$", sep="")
      condition <- result$match
      if (is.null(result$match) || !nchar(result$match))
        errReadParameters (filename, nbLines, line,
                           "Expected condition after '|'")
      line <- result$line
    } else if (!is.null(result$line) && nchar(result$line)) {
      errReadParameters (filename, nbLines, line,
                         "Expected '|' before condition")
    } else {
      condition <- TRUE
    }
    # *****************************************************************
    p <- tryCatch(
      Parameter(name = name, type = param.type, domain = domain, label = p_label, condition = condition,
        transf = param.transform, digits = digits),
      invalid_domain = function(c) structure(paste0("For parameter '", name, "' of type 'i' values must be integers (",
                                             domain_str, ")"), class = "try-error"),
      invalid_range = function(c) structure(paste0("Lower bound must be smaller than upper bound in numeric range (",
                                                   domain_str, ") for parameter '", name, "'"), class="try-error"),
      
      error = function(c) structure(conditionMessage(c), class="try-error")
    )
    if (inherits(p, "try-error")) errReadParameters(filename, nbLines, NULL, p)
    params <- c(params, list(p))
    pnames <- c(pnames, name)
  } # end loop on lines

  # Check that we have read at least one parameter
  if (length(params) == 0) {
    if (is.na(filename))
      irace.error("No parameter definition found in the input text")
    else
      irace.error("No parameter definition found, check that the parameter file '",  filename, "' is not empty.")
  }

  if (length(forbidden)) {
    irace.note(length(forbidden), " expression(s) specifying forbidden configurations read.\n")
    check_forbidden_params(forbidden, pnames, filename = filename)
  }
  parameters <- do.call(parametersNew, c(params, list(forbidden=forbidden, debugLevel = debugLevel)))
  if (debugLevel >= 2) {
    print(parameters, digits = 15L)
    irace.note("Parameters have been read\n")
  }
  parameters
}

#' Read parameters in PCS (AClib) format and write them in irace format.
#'
#' @inheritParams readParameters
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
    if (length(matches) > 0L) {
      param_name <- matches[[2L]]
      
      param_type <- paste0(if(grepl("i", matches[5L], fixed=TRUE)) "i" else "r",
                           if(grepl("l", matches[5L], fixed=TRUE)) ",log" else "")
      param_types[[param_name]] <- param_type
      param_domains[[param_name]] <- paste0("(", matches[3L], ", ", matches[4L], ")")
      param_comments[[param_name]] <- matches[6L]
      next
    }
    matches <- regmatches(line, regexec("^([^[:space:]]+)[[:space:]]+\\{([^}]+)\\}[[:space:]]*\\[[^]]+\\](.*)$", line, perl=TRUE))[[1L]]
    if (length(matches) > 0L) {
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
    if (length(matches) > 0L) {
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
#' @inheritParams printParameters
#' @export
checkParameters <- function(parameters)
{
  ## if (is.null(parameters$isDependent)) {
  ##   parameters$isDependent <- sapply(parameters$domains, is.expression)
  ##   names(parameters$isDependent) <- parameters$names
  ## }
  if (!inherits(parameters, "ParameterSpace")) {
    irace.error("parameters must be an object of class 'ParameterSpace'")
  }
  parameters
}

