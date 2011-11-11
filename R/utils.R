## Functions to print error messages and\or exit

.irace.bug.report <-
  paste("An unexpected condition ocurred.",
        "Please report this bug to the authors of the irace package <http://iridia.ulb.ac.be/irace>")

# Print a user-level fatal error message, when the calling context
# cannot help the user to understand why the program failed.
## FIXME: rename this function to irace.error
tunerError <- function(...)
{
  stop (..., call. = FALSE)
}

# FIXME: Isn't a R function to do this? More portable?
canonical.dirname <- function(dirname = stop("required parameter"))
{
  return (sub ("([^/])$", "\\1/", dirname))
}

trim.leading <- function(str)
{
  return (sub('^[[:space:]]+', '', str)) ## white space, POSIX-style
}
trim.trailing <- function(str)
{
  return (sub('[[:space:]]+$', '', str)) ## white space, POSIX-style
}
# remove leading and trailing white space characters
trim <- function(str)
{
  return (trim.trailing(trim.leading(str)))
}

isFixed <- function (paramName, parameters)
{
  return (as.logical(parameters$isFixed[paramName]))
}

oneParamBoundary <- function (paramName, parameters)
{
  return (parameters$boundary[[paramName]])
}

oneParamLowerBound <- function (paramName, parameters)
{
  return (as.numeric(parameters$boundary[[paramName]][1]))
}

oneParamUpperBound <- function (paramName, parameters)
{
  return (as.numeric(parameters$boundary[[paramName]][2]))
}

nbParam <- function (parameters)
{
  return (length(parameters$names))
}


## extractElites
# Input: the candidates with the .RANK. field filled.
#        the number of elites wished
# Output: nbElites elites, sorted by ranks, with the weights assigned.
extractElites <- function(candidates, nbElites)
{
  if (nbElites < 1) {
    ## ??? Should this be an error or should we handle it in some other way?
    stop("nbElites is lower or equal to zero.") 
  }
  # Sort by rank.
  elites <- candidates[order(as.numeric(candidates$.RANK.)), , drop = FALSE]
  elites <- elites[1:nbElites, , drop = FALSE]
  elites[, ".WEIGHT."] <- ((nbElites - (1:nbElites) + 1)
                           / (nbElites * (nbElites + 1) / 2))
  return (elites)
}

## Keep only parameters values
removeCandidatesMetaData <- function(candidates)
{
  # Meta-data colnames begin with "."
  return (candidates[, grep("^[_.]", colnames(candidates), invert=TRUE),
                     drop = FALSE])
}

candidates.print <- function(cand, metadata = FALSE)
{
  rownames(cand) <- cand$.ID.
  if (!metadata) {
    cand <- removeCandidatesMetaData(cand)
  } 
  print(as.data.frame(cand, stringsAsFactors = FALSE))
}

candidates.print.command <- function(cand, parameters)
{
  rownames(cand) <- cand$.ID.
  cand <- removeCandidatesMetaData(cand)
  if (nrow(cand) <= 0) return(invisible())
  print(data.frame(command =
                 apply(cand[,unlist(parameters$names), drop = FALSE],
                       1, buildCommandLine, switches = parameters$switches,
                       signifDigits = 4), stringsAsFactors = FALSE))
}


# FIXME: This may not work when working interactively. For example,
# one cannot change the number of slaves. Using .Last is dangerous
# because other package or the user may override it.
mpiInit <- function(nslaves)
{
  # Load the R MPI package if it is not already loaded. 
  if (!is.loaded("mpi_initialize")) {
    if (! require("Rmpi", quietly = TRUE))
      stop("the `Rmpi' package is required for using MPI.")

    # When R exits, finalize MPI.
    .Last <<- function() {
      if (is.loaded("mpi_initialize")) {
        cat("# Finalize MPI...\n")
        if (Rmpi::mpi.comm.size(1) > 0)
          Rmpi::mpi.close.Rslaves()
        # FIXME: How to avoid the message?
        # "Rmpi cannot be used unless relaunching R."
        Rmpi::mpi.finalize()
      }
    }
    # Create slaves
    Rmpi::mpi.spawn.Rslaves(nslaves = nslaves)
  }
}