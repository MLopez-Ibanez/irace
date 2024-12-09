# Returns a data.table
configurations_alloc <- function(colnames, nrow, parameters)
{
  parameter_type <- function(type) {
    switch(type,
           i = NA_integer_,
           r = NA_real_,
           c = NA_character_,
           o = NA_character_,
           irace_internal_error("Unknown type '", type, "'"))
  }

  column_type <- function(x, n, types)
    rep(switch(x,
               .ID. = NA_integer_,
               .PARENT. = NA_integer_,
               .WEIGHT. = NA_real_,
               parameter_type(types[x])), n)

  x <- sapply(colnames, column_type, n=nrow, types = parameters[["types"]],
              simplify=FALSE, USE.NAMES=TRUE)
  setDT(x)
  x
}

# FIXME: It may be faster to create a single expression that concatenates all
# the elements of forbidden using '|'
checkForbidden <- function(configurations, forbidden)
{
  # We have to use a variable name that will never appear in
  # configurations, so .FORBIDDEN .
  for (.FORBIDDEN in forbidden) {
    #print(.FORBIDDEN)
    configurations <- subset(configurations, eval(.FORBIDDEN))
    #print(configurations)
    #print(str(configurations))
    ## FIXME: This is normally called with a single configuration. Thus, it
    ## would be faster to break as soon as nrow(configurations) < 1
  }
  #print(nrow(configurations))
  configurations
}

# FIXME: It may be faster to create a single expression that concatenates all
# the elements of forbidden using '|'
filter_forbidden <- function(configurations, forbidden)
{
  # We have to use a variable name that will never appear in
  # configurations, so .FORBIDDEN
  for (.FORBIDDEN in forbidden) {
    configurations <- configurations[eval(.FORBIDDEN)]
    #print(configurations)
    #print(str(configurations))
    if (nrow(configurations) == 0L) return(configurations)
  }
  #print(nrow(configurations))
  configurations
}

which_satisfied <- function(configurations, condition)
{
  # If there is no condition, do not waste time evaluating it.
  if (isTRUE(condition))
    return(seq_nrow(configurations))
  r <- eval(condition, configurations)
  # Return TRUE if TRUE, FALSE if FALSE or NA
  ## FIXME: If we byte-compile the condition, then we should incorporate the
  ## following into the condition directly.
  # r & !is.na(r)
  # Return indexes where r is TRUE.
  which(r)
}

#' removeConfigurationsMetaData
#'
#' Remove the columns with "metadata" of a data frame containing
#' configurations. Currently, metadata corresponds to column names starting
#' with a period.  This function should be used before printing the
#' configurations to output only the values for the parameters of the
#' configuration without metadata possibly useless to the user.
#'   
#' @param configurations `data.frame`\cr Parameter configurations of the
#'   target algorithm (one per row).
#' 
#' @return The same data frame without "metadata".
#'    
#' @seealso 
#'   [configurations_print_command()] to print the configurations as command lines.
#'   [configurations_print()] to print the configurations as a data frame.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
removeConfigurationsMetaData <- function(configurations)
  configurations[, !startsWith(colnames(configurations), "."), drop = FALSE]

#' Print configurations as a data frame
#' 
#' @inheritParams removeConfigurationsMetaData
#' 
#' @param metadata `logical(1)`\cr whether to print the metadata or
#' not. The metadata are data for the configurations (additionally to the
#' value of each parameter) used by \pkg{irace}.
#' 
#' @return None.
#'
#' @seealso
#'  [configurations_print_command()] to print the configurations as command-line strings.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
configurations_print <- function(configurations, metadata = FALSE)
{
  if (!is.data.frame(configurations))
    configurations <- as.data.frame(configurations, stringsAsFactors = FALSE)

  rownames(configurations) <- configurations[[".ID."]]
  if (!metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  
  print.data.frame(configurations, digits = 15L)
}

#' Print configurations as command-line strings.
#' 
#' Prints configurations after converting them into a representation for the
#' command-line.
#' 
#' @inheritParams removeConfigurationsMetaData
#' @inheritParams printParameters
#' 
#' @return None.
#'
#' @seealso
#'  [configurations_print()] to print the configurations as a data frame.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
configurations_print_command <- function(configurations, parameters)
{
  if (nrow(configurations) <= 0L) return(invisible())
  ids <- as.numeric(configurations$.ID.)
  configurations <- removeConfigurationsMetaData(configurations)
  # Re-sort the columns
  configurations <- configurations[, parameters$names, drop = FALSE]
  # A better way to do this? We cannot use apply() because that coerces
  # to a character matrix thus messing up numerical values.
  len <- nchar(max(ids))
  for (i in seq_nrow(configurations)) {
    cat(sprintf("%-*d %s\n", len, ids[i],
                buildCommandLine(configurations[i, , drop=FALSE], parameters$switches)))
  }
}
