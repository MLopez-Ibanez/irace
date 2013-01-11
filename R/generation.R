#######################################
## GENERATE CANDIDATES 
#######################################

## Important: call this function with an unconstrained parameter, it
## will output TRUE
constraintsSatisfied <- function (parameters, partialCandidate, paramName)
{
  constraint <- parameters$constraints[[paramName]]
  # If there is no constraint, do not waste time evaluating it.
  if(!length(all.vars(constraint, max.names = 1L))) return(TRUE)

  v <- eval(constraint, as.list(partialCandidate))
  # Return TRUE if TRUE, FALSE if FALSE or NA
  v <- !is.na(v) && v 
  return(v)
}

new.empty.candidate <- function(parameters)
{
  namesParameters <- names(parameters$constraints)
  newCandidatesColnames <- c(namesParameters, ".PARENT.")
  empty.candidate <- as.list(rep(NA, length(newCandidatesColnames)))
  names(empty.candidate) <- newCandidatesColnames
  return(empty.candidate)
}

get.fixed.value <- function(param, parameters)
{
  value <- parameters$boundary[[param]][1]
  type <- parameters$types[[param]]
  if (type == "i") {
    return (as.integer(value))
  } else if (type == "c" || type == "o") {
    return (value)
  } else if (type == "r") {
    return (as.double(value))
  } 
  stop (.irace.bug.report)
}

### Uniform sampling for the initial generation
sampleUniform <- function (tunerConfig, parameters, nbCandidates)
{
  namesParameters <- names(parameters$constraints)
  newCandidatesColnames <- c(namesParameters, ".PARENT.")
  newCandidates  <-
    as.data.frame(matrix(nrow = nbCandidates,
                         ncol = length(newCandidatesColnames)))
  colnames(newCandidates) <- newCandidatesColnames
  empty.candidate <- new.empty.candidate(parameters)
  
  for (idxCandidate in seq_len(nbCandidates)) {
    candidate <- empty.candidate
    for (p in seq_along(namesParameters)) {
      # FIXME: We must be careful because parameters$types does not
      # have the same order as parameters$constraints. Ideally, we
      # should fix this or make it impossible to confuse them.
      currentParameter <- namesParameters[p]
      currentType <- parameters$types[[currentParameter]]
      if (!constraintsSatisfied(parameters, candidate, currentParameter)) {
        candidate[[p]] <- NA
        next
      }
      if (isFixed(currentParameter, parameters)) {
        # We don't even need to sample, there is only one possible value !
        newVal <- get.fixed.value (currentParameter, parameters)
        # The parameter is not a fixed and should be sampled          
      } else if (currentType == "i") {
        lowerBound <- as.integer(parameters$boundary[[currentParameter]][1])
        upperBound <- as.integer(parameters$boundary[[currentParameter]][2])
        # FIXME: replace with sample.int!
        newVal <- sample(lowerBound:upperBound, 1)
      } else if (currentType == "r") {
        lowerBound <- parameters$boundary[[currentParameter]][1]
        upperBound <- parameters$boundary[[currentParameter]][2]
        newVal <- runif(1, as.double(lowerBound), as.double(upperBound))
        newVal <- round(newVal, tunerConfig$digits)
      } else if (currentType == "c" || currentType == "o") {
        possibleValues <- parameters$boundary[[currentParameter]]
        newVal <- sample(possibleValues, 1)
      } else {
        stop (.irace.bug.report);
      }
      candidate[[p]] <- newVal
    }
    newCandidates[idxCandidate,] <- candidate
  }
  return (newCandidates)
}

# To be called the first time before the second race (with indexIter =
# 2) Nb candidates is the number of candidates at the end
# included the elite ones obtained from the previous iteration
sampleModel <- function (tunerConfig, parameters, eliteCandidates, model,
                         nbNewCandidates)
{
  if (nbNewCandidates <= 0) {
    stop ("The number of candidates to generate appears to be negative or zero.")
  }
  namesParameters <- names(parameters$constraints)
  newCandidatesColnames <- c(namesParameters, ".PARENT.")
  newCandidates  <-
    as.data.frame(matrix(nrow = nbNewCandidates,
                         ncol = length(newCandidatesColnames)))
  colnames(newCandidates) <- newCandidatesColnames
  empty.candidate <- new.empty.candidate(parameters)

  for (idxCandidate in seq_len(nbNewCandidates)) {
    # Choose the elite which will be the parent.
    indexEliteParent <- sample.int (n = nrow(eliteCandidates), size = 1,
                                    prob = eliteCandidates[[".WEIGHT."]])
    eliteParent <- eliteCandidates[indexEliteParent, ]
    idEliteParent <- eliteParent[[".ID."]]
    candidate <- empty.candidate
    candidate[[".PARENT."]] <- idEliteParent

    # Sample a value for every parameter of the new candidate.
    for (p in seq_along(namesParameters)) {
      # FIXME: We must be careful because parameters$types does not
      # have the same order as parameters$constraints. Ideally, we
      # should fix this or make it impossible to confuse them.
      currentParameter <- namesParameters[p]
      currentType <- parameters$types[[currentParameter]]
      if (!constraintsSatisfied(parameters, candidate, currentParameter)) {
        # Some constraints are unsatisfied.
        # Should be useless, NA is ?always? assigned when matrix created
        newVal <- NA

      } else if (isFixed(currentParameter, parameters)) {
        # We don't even need to sample, there is only one possible value !
        newVal <- get.fixed.value (currentParameter, parameters)
        # The parameter is not a fixed and should be sampled
      } else if (currentType == "i" || currentType == "r") {
        lowerBound <- oneParamLowerBound(currentParameter, parameters)
        upperBound <- oneParamUpperBound(currentParameter, parameters)
        mean <- as.numeric(eliteParent[currentParameter])
        if (is.na(mean)) {
          # The elite parent does not have any value for this
          # parameter, let's sample uniformly.
          newVal <- ifelse(currentType == "i",
                           sample(lowerBound:upperBound, 1),
                           runif(1, lowerBound, upperBound))
        } else {
          stdDev <- model[[currentParameter]][[as.character(idEliteParent)]]
          newVal <- ifelse(currentType == "i",
                           rtnorm(1, mean + 0.5, stdDev, lowerBound, upperBound + 1) - 0.5,
                           rtnorm(1, mean, stdDev, lowerBound, upperBound))
        }
        newVal <- ifelse(currentType == "i", round(newVal),
                         round(newVal, tunerConfig$digits))
        
      } else if (currentType == "o") {
        possibleValues <- oneParamBoundary(currentParameter, parameters)  
        value <- eliteParent[currentParameter]
        
        if (is.na(value)) {
          # The elite parent does not have any value for this
          # parameter, let's sample uniformly
          newVal <- sample(possibleValues, 1)
        } else {
          # Find the position within the array of possible
          # values to determine the equivalent integer.
          mean <- match(value, possibleValues) # Return index of value in array
          stdDev <- model[[currentParameter]][[as.character(idEliteParent)]]
          
          # Sample with truncated normal distribution as an integer.
          newValAsInt <- round(rtnorm(1, mean + 0.5, stdDev, 1,
                                      length(possibleValues) + 1) - 0.5)
          
          # Get back to categorical values, find the one corresponding to the
          # newVal
          newVal <- possibleValues[newValAsInt]
        } 
      } else if (currentType == "c") {
        # FIXME: Why is idEliteParent character?
        # FIXME: Why the model is <parameter><Parent>? It makes more sense to be <Parent><parameter>.
        probVector <- model[[currentParameter]][[as.character(idEliteParent)]]
        possibleValues <- oneParamBoundary(currentParameter, parameters)
        newVal <- sample(x = possibleValues, size = 1, prob = probVector)
      } else {
        stop (.irace.bug.report)
      }
      candidate[[p]] <- newVal
    }
    newCandidates[idxCandidate, ] <- candidate
  }
  return (newCandidates)
}
