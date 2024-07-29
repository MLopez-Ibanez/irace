#' Read parameter configurations from a file
#' 
#' Reads a set of target-algorithm configurations from a file and puts them in
#' \pkg{irace} format. The configurations are checked to match the parameters
#' description provided.
#' 
#' @param filename `character(1)`\cr Filename from which the configurations should be read.
#' The contents should be readable by `read.table( , header=TRUE)`.
#' @param text `character(1)`\cr If \code{file} is not supplied and this is,
#'  then configurations are read from the value of \code{text} via a text connection.
#' @inheritParams readParameters
#' @inheritParams printParameters
#' 
#' @return A data frame containing the obtained configurations. 
#'   Each row of the data frame is a candidate configuration, 
#'   the columns correspond to the parameter names in `parameters`.
#'
#' @details
#' Example of an input file:
#' ```
#' # This is a comment line
#' param_1    param_2
#'     0.5  "value_1"
#'     1.0         NA
#'     1.2  "value_3"
#' ```
#' The order of the columns does not necessarily have to be the same
#' as in the file containing the definition of the parameters.
#' 
#' @seealso 
#'   [readParameters()] to obtain a valid parameter structure from a parameters file.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
readConfigurationsFile <- function(filename, parameters, debugLevel = 0L, text)
{
  if (missing(filename) && !missing(text)) {
    filename <- NULL
    configurationTable <- read.table(text = text, header = TRUE,
                                     na.strings = c("NA", "<NA>"),
                                     colClasses = "character",
                                     stringsAsFactors = FALSE)
  } else {
    # Read the file.
    configurationTable <- read.table(filename, header = TRUE,
                                     na.strings = c("NA", "<NA>"),
                                     colClasses = "character",
                                     stringsAsFactors = FALSE)
  }
  irace.assert(is.data.frame(configurationTable))
  irace.note("Read ", nrow(configurationTable), " configuration(s)",
             if (is.null(filename)) "\n" else paste0(" from file '", filename, "'\n"))
  fix_configurations(configurationTable, parameters, debugLevel = debugLevel,
                     filename = filename)
}

fix_configurations <- function(configurations, parameters, debugLevel = 0L, filename = NULL)
{
  conf_error <- function(k, ...)
    irace.error("Configuration number ", k,
      if (is.null(filename)) "" else paste0(" from file '", filename, "'"),
      ...)
  
  if (debugLevel >= 2L) print(configurations, digits=15L)
  nbConfigurations <- nrow(configurations)
  namesParameters <- parameters[["names"]]
  # This ignores fixed parameters unless they are given with a different value.
  if (ncol(configurations) != length(namesParameters)
    || !setequal(colnames(configurations), namesParameters)) {
    # Column names must match a parameter, including fixed ones.
    missing <- setdiff(colnames(configurations), namesParameters)
    if (length(missing) > 0L) {
      if (is.null(filename)) {
        irace.error("The parameter names (", strlimit(paste(missing, collapse=", ")),
          ") do not match the parameter names: ", paste(namesParameters, collapse=", "))
      } else {
        irace.error("The parameter names (",
          strlimit(paste(missing, collapse=", ")),
          ") given in the first row of file ", filename,
          " do not match the parameter names: ",
          paste(namesParameters, collapse=", "))
      }
      return(NULL)
    }
    # All non-fixed parameters must appear in column names.
    varParameters <- parameters$names_variable
    missing <- setdiff (varParameters, colnames(configurations))
    if (length(missing) > 0) {
      if (is.null(filename)) {
        irace.error("The parameter names (",
          strlimit(paste(missing, collapse=", ")),
          ") are missing from the configurations provided.")
      } else {
        irace.error("The parameter names (",
          strlimit(paste(missing, collapse=", ")),
          ") are missing from the first row of file ", filename)
      }
      return(NULL)
    }
    # Add any missing fixed parameters.
    missing <- setdiff (namesParameters, colnames(configurations))
    if (length(missing) > 0L) {
      irace.assert (all(parameters$isFixed[missing]))
      configurations <- cbind.data.frame(configurations, parameters$domains[missing],
        stringsAsFactors = FALSE)
    }
  }
  # Reorder columns.
  configurations <- configurations[, namesParameters, drop = FALSE]

  # Loop over all parameters.
  for (param in parameters$get()) {
    pname <- param[["name"]]
    type <- param[["type"]]
    domain <- param[["domain"]]
    is_dep_param <- param[["is_dependent"]]
    condition <- param[["condition"]]
    # Fix up numeric columns.
    if (type == "i") {
      # For integers, only accept an integer.
      configurations[[pname]] <-
        suppressWarnings(as.numeric(configurations[[pname]]))
      # Remove NAs for this check.
      values <- configurations[[pname]]
      values[is.na(values)] <- 0L
      if (any(as.integer(values) != values)) {
        k <- which(as.integer(values) != values)[1L]
        conf_error (k, " is invalid because parameter ", pname,
          " is of type integer but its value ", values[k], " is not an integer")
        return(NULL)
      }
    } else if (type == "r") {
      configurations[[pname]] <- round(
        suppressWarnings(as.numeric(configurations[[pname]])),
        digits = parameters$get(pname)[["digits"]])
    }
    # Loop over all configurations in configurations.
    # FIXME: Vectorize this loop
    values <- configurations[[pname]]
    for (k in seq_len(nbConfigurations)) {
      currentValue <- values[k]
      # Check the status of the conditions for this parameter to know whether
      # it must be enabled.
      if (conditionsSatisfied(condition, configurations[k, ])) {
        # Check that the value is among the valid ones.
        if (is_dep_param) {
          dep_domain <- getDependentBound(param, configurations[k, ])
          if (is.na(dep_domain[1L])) {
            # Dependencies are not satisfied, so skip
            if (is.na(currentValue)) next
            conf_error (k, " is invalid because parameter \"", pname,
              "\" is not enabled, because its domain ",
              sub("expression", "", deparse(domain)),
              " depends on parameters that are not enabled, but its value is \"",
              currentValue, "\" instead of NA")
            return(NULL)
          }
          lower <- dep_domain[1L]
          upper <- dep_domain[2L]
          if (is.na(currentValue) || currentValue < lower || currentValue > upper) {
            conf_error (k, " is invalid because the value \"",
                        configurations[k, pname], "\" for the parameter ",
                        pname, " is not within the valid range ",
                        sub("expression", "", deparse(domain)),
                        ", that is, [", lower,", ", upper,"]")
            return(NULL)
          }
        } else if (type == "i" || type == "r") {
          lower <- domain[[1L]]
          upper <- domain[[2L]]
          if (is.na(currentValue) || currentValue < lower || currentValue > upper) {
            conf_error (k, " is invalid because the value \"",
                        configurations[k, pname], "\" for the parameter ",
                        pname, " is not within the valid range [",
                        lower,", ", upper,"]")
            return(NULL)
          }
          # type == "o" or "c"
        } else if (currentValue %not_in% domain) {
          conf_error (k, " is invalid because the value \"",
                      currentValue, "\" for the parameter \"",
                      pname, "\" is not among the valid values: (\"",
                      paste0(domain, collapse="\", \""), "\")")
          return(NULL)
        }
      } else if (!is.na(currentValue)) {
        conf_error (k, " is invalid because parameter \"", pname,
                    "\" is not enabled because of condition \"",
                    param[["condition"]], "\" but its value is \"",
                    currentValue, "\" instead of NA")
        return(NULL)
      }
    }
  }
  if (anyDuplicated(configurations)) {
    irace.error("Duplicated configurations",
                if (is.null(filename)) "" else paste0(" in file '", filename, "'"),
                ":\n",
                paste0(capture.output(
                  configurations[duplicated(configurations), , drop=FALSE]), "\n"))
  }
  configurations
}

compile_forbidden <- function(x)
{
  if (is.null(x) || is.bytecode(x)) return(x)
  # If we are given an expression, it must be a single one.
  irace.assert(is.language(x) && (!is.expression(x) || length(x) == 1L))
  if (is.expression(x)) x <- x[[1L]]
  # When a is NA and we check a == 5, we would get NA, which is
  # always FALSE, when we actually want to be TRUE, so we test
  # is.na() first below.
  
  # We expect that there will be undefined variables, since the expressions
  # will be evaluated within a data.frame later.
  expr <- compiler::compile(substitute(is.na(x) | !(x), list(x = x)),
                           options = list(suppressUndefined=TRUE))
  attr(expr, "source") <- as.character(as.expression(x))
  expr
}

buildForbiddenExp <- function(configurations)
{
  if (is.null(configurations) || nrow(configurations) == 0L)
    return(NULL)
  pnames <- colnames(configurations)
  lines <- c()
  # We cannot use apply() because it converts numeric to character.
  for (k in seq_nrow(configurations)) {
    values <- as.list(configurations[k, ])
    has.value <- !is.na(values)
    values <- lapply(values[has.value], function(x) deparse(substitute(x, list(x=x))))
    lines <- c(lines,
               paste0("(", pnames[has.value]," == ", values, ")", collapse = "&"))
  }
  exps <- parse(text = lines)
  lapply(exps, compile_forbidden)
}
