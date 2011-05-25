#######################################
## GENERATE CANDIDATES 
#######################################

## Important: call this function with an unconstrained parameter, it
## will output TRUE
constraintsSatisfied <- function (parameters, partialCandidate, paramName)
{
  constraint <- parameters$constraints[[paramName]]
  v <- eval(constraint,as.list(partialCandidate))
  # Return TRUE if TRUE, FALSE if FALSE or NA
  v <- !is.na(v) && v 
  return(v)
}

### Uniform sampling for the initial generation
generateCandidatesUniform <- function (tunerConfig, parameters,
                                           nbCandidates)
{
  namesParameters <- names(parameters$constraints)
  newCandidatesColnames <- c(namesParameters, ".PARENT.")
  newCandidates  <-
    as.data.frame(matrix(nrow = nbCandidates,
                         ncol = length(newCandidatesColnames)))
  colnames(newCandidates) <- newCandidatesColnames
  
  for (idxCandidate in seq_len(nbCandidates)) {
    for (currentParameter in namesParameters) {
      currentType <- parameters$types[[currentParameter]]
      if (!constraintsSatisfied(parameters, newCandidates[idxCandidate, ],
                                currentParameter)) {
        newCandidates[idxCandidate, currentParameter] <- NA
        next
      }
      if (isFixed(currentParameter, parameters)) {
        # We don't even need to sample, there is only one possible value !
        lowerBound <- parameters$boundary[[currentParameter]][1]
        if (currentType == "i") {
          newVal <- as.integer(lowerBound)
        } else if ((currentType == "c") || (currentType == "o")) {
          newVal <- lowerBound
        } else if (currentType == "r") {
          newVal <- as.double(lowerBound)
        } else {
          stop (irace.bug.report);
        }
        # The parameter is not a fixed and should be sampled          
      } else if (currentType == "i") {
        lowerBound <- parameters$boundary[[currentParameter]][1]
        upperBound <- parameters$boundary[[currentParameter]][2]
        newVal <- runif(1, as.integer(lowerBound), as.integer(upperBound))
        newVal <- round(newVal)
      } else if (currentType == "r") {
        lowerBound <- parameters$boundary[[currentParameter]][1]
        upperBound <- parameters$boundary[[currentParameter]][2]
        newVal <- runif(1, as.real(lowerBound), as.real(upperBound))
        newVal <- signif(newVal, tunerConfig$signifDigits)
      } else if ((currentType == "c") || (currentType == "o")) {
        possibleValues <- parameters$boundary[[currentParameter]]
        newVal <- sample(possibleValues, 1)
      } else {
        stop (irace.bug.report);
      }
      newCandidates[idxCandidate, currentParameter] <- newVal
    }
  }
  return (newCandidates)
}

# To be called the first time before the second race (with indexIter =
# 2) Nb candidates is the number of candidates at the end
# included the elite ones obtained from the previous iteration
generateCandidatesNormal <- function (tunerConfig, parameters,
                                      eliteCandidates, model,
                                      nbNewCandidates)
{
  nbCandidates <- nbNewCandidates + nrow(eliteCandidates)
  if (nbNewCandidates <= 0) {
    stop ("The number of candidates to generate appears to be negative, or zero.")
  }

  namesParameters <- names(parameters$constraints)
  newCandidatesColnames <- c(namesParameters, ".PARENT.")
  newCandidates  <-
    as.data.frame(matrix(nrow = nbNewCandidates,
                         ncol = length(newCandidatesColnames)))
  colnames(newCandidates) <- newCandidatesColnames

  for (idxCandidate in seq(nbNewCandidates)) {
    # Choose the elite which will be the parent.
    indexEliteParent <- sample(size = 1, x = 1:nrow(eliteCandidates),
                               prob = eliteCandidates[, ".WEIGHT."])
    eliteParent <- eliteCandidates[indexEliteParent, ]
    idEliteParent <- eliteParent[".ID."]
    newCandidates[idxCandidate, ".PARENT."] <- idEliteParent

    # Sample of every parameter for the new candidate.
    for (currentParameter in namesParameters) {
      currentType <- parameters$types[[currentParameter]]
      if (constraintsSatisfied(parameters, newCandidates[idxCandidate, ],
                               currentParameter)) {
        if (isFixed(currentParameter, parameters)) {
          # We don't even need to sample, there is only one possible value !
          lowerBound <- oneParamBoundary(currentParameter, parameters)[1]
          if (currentType == "i") {
            newVal <- as.integer(lowerBound)
          } else if (currentType == "c"  || currentType == "o") {
            newVal <- lowerBound
          } else if (currentType == "r") {
            newVal <- as.double(lowerBound)
          } else {
            stop (irace.bug.report)
          }
          # The parameter is not a fixed and should be sampled
        } else if (currentType == "i" || currentType == "r") {
          lowerBound <- oneParamLowerBound(currentParameter, parameters)
          upperBound <- oneParamUpperBound(currentParameter, parameters)
          mean <- as.numeric(eliteParent[currentParameter])
          if (is.na(mean)) {
            # The elite parent does not have any value for this
            # parameter, let's sample uniformly.
            newVal <- runif(1, lowerBound, upperBound)
          } else {
            stdDev <- model[[currentParameter]][[as.character(idEliteParent)]]
            newVal <- rtnorm(1, mean, stdDev, lowerBound, upperBound)
          }
          newVal <- ifelse((currentType == "i"),
                           round(newVal),
                           signif(newVal, tunerConfig$signifDigits))

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

            # Sample with truncated normal distribution.
            newValAsInt <- rtnorm(1, mean, stdDev, 1, length(possibleValues))

            # Get back to categorical values, find the one corresponding to the
            # newVal
            newVal <- possibleValues[round(newValAsInt)]
          } 
        } else if (currentType == "c") {
          # FIXME: Why is idEliteParent character?
          # FIXME: Why the model is <parameter><Parent>? It makes more sense to be <Parent><parameter>.
          probVector <-
            model[[currentParameter]][[as.character(idEliteParent)]]
          possibleValues <- oneParamBoundary(currentParameter, parameters)
          newVal <- sample(x = possibleValues, size = 1, prob = probVector)
        } else {
          stop (irace.bug.report)
        }
      } else { # Some constraints are unsatisfied.
        # Should be useless, NA is ?always? assigned when matrix created
        newVal <- NA
      }
      newCandidates[idxCandidate, currentParameter] <- newVal
    }
  }
  return (newCandidates)
}
