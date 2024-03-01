# Returns a data.table
configurations_alloc <- function(colnames, nrow, parameters)
{
  parameter_type <- function(type) {
    switch(type,
           i = NA_integer_,
           r = NA_real_,
           c = NA_character_,
           o = NA_character_,
           irace.internal.error("Unknown type '", type, "'"))
  }

  column_type <- function(x, n, types)
    rep(switch(x,
               .ID. = NA_integer_,
               .PARENT. = NA_integer_,
               .WEIGHT. = NA_real_,
               parameter_type(types[x])), n)

  x <- sapply(colnames, column_type, n=nrow, types = parameters$types,
              simplify=FALSE, USE.NAMES=TRUE)
  setDT(x)
  x
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
    return(seq_len(nrow(configurations)))
  r <- eval(condition, configurations)
  # Return TRUE if TRUE, FALSE if FALSE or NA
  ## FIXME: If we byte-compile the condition, then we should incorporate the
  ## following into the condition directly.
  # r & !is.na(r)
  # Return indexes where r is TRUE.
  which(r)
}

