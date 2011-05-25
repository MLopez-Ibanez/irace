## Functions to print error messages and\or exit

irace.bug.report <-
  paste("An unexpected condition ocurred.",
        "Please report this bug to the authors of the I/F-race package")

# Print a user-level fatal error message, when the calling context
# cannot help the user to understand why the program failed.
tunerError <- function(...)
{
  stop (..., call. = FALSE)
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
  # Sort by rank.
  elites <- candidates[order(as.numeric(candidates$.RANK.)), , drop = FALSE]
  if (nbElites < 1) {
    ## ??? Should this be an error or should we handle it in some other way?
    stop("nbElites is lower or equal to zero.") 
  }
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

# Initialize Rmpi and create slaves.
mpiInit <- function(nslaves)
{
  # Load the R MPI package if it is not already loaded. 
  if (!is.loaded("mpi_initialize")) {
    library("Rmpi")

    # When R exits, finalize MPI.
    .Last <<- function() {
      if (is.loaded("mpi_initialize")) {
        cat("# Finalize MPI...\n")
        if (mpi.comm.size(1) > 0)
          mpi.close.Rslaves()
        mpi.finalize()
      }
    }
    # Create slaves
    mpi.spawn.Rslaves(nslaves = nslaves)
  }
}
