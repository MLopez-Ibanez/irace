#######################################
## GENERATE CONFIGURATIONS
#######################################

## When called with an unconditional parameter, it
## must return TRUE
conditionsSatisfied <- function (parameters, partialConfiguration, paramName)
{
  condition <- parameters$conditions[[paramName]]
  # If there is no condition, do not waste time evaluating it.
  if (isTRUE(condition)) return(TRUE)

  v <- eval(condition, as.list(partialConfiguration))
  # Return TRUE if TRUE, FALSE if FALSE or NA
  ## FIXME: If we byte-compile the condition, then we should incorporate the
  ## following into the condition directly. See readForbiddenFile.
  v <- !is.na(v) && v
  return(v)
}

new.empty.configuration <- function(parameters)
{
  newConfigurationsColnames <- c(names(parameters$conditions), ".PARENT.")
  return(setNames(as.list(rep(NA, length(newConfigurationsColnames))),
                  newConfigurationsColnames))
}

get.fixed.value <- function(param, parameters)
{
  value <- parameters$domain[[param]][1]
  type <- parameters$types[[param]]
  if (type == "i") {
    return (as.integer(value))
  } else if (type == "c" || type == "o") {
    return (value)
  } else {
    irace.assert (type == "r")
    return (as.double(value))
  }
}

### Uniform sampling for the initial generation
sampleUniform <- function (parameters, nbConfigurations, digits,
                           forbidden = NULL, repair = NULL)
{
  namesParameters <- names(parameters$conditions)
  newConfigurations  <-
    as.data.frame(matrix(nrow = nbConfigurations,
                         ncol = length(namesParameters) + 1,
                         dimnames = list(NULL, c(namesParameters, ".PARENT."))
                         ))
  empty.configuration <- new.empty.configuration(parameters)

  for (idxConfiguration in seq_len(nbConfigurations)) {
    forbidden.retries <- 0
    while (forbidden.retries < 100) {
      configuration <- empty.configuration
      for (p in seq_along(namesParameters)) {
        currentParameter <- namesParameters[p]
        if (!conditionsSatisfied(parameters, configuration, currentParameter)) {
          configuration[[p]] <- NA
          next
        }
        # FIXME: We must be careful because parameters$types does not have the
        # same order as namesParameters, because we sample in the order of the
        # conditions.
        currentType <- parameters$types[[currentParameter]]
        transform <- parameters$transform[[currentParameter]]
        if (isFixed(currentParameter, parameters)) {
          # We don't even need to sample, there is only one possible value !
          newVal <- get.fixed.value (currentParameter, parameters)
          # The parameter is not a fixed and should be sampled          
        } else if (currentType %in% c("i","r")) {
          newVal <- sample.numerical(currentParameter, parameters, currentType, digits)
        } else if (currentType %in% c("c","o")) {
          possibleValues <- parameters$domain[[currentParameter]]
          newVal <- sample(possibleValues, 1)
        } else {
          irace.internal.error("Unexpected condition in sampleUniform")
        }
        configuration[[p]] <- newVal
      }
      configuration <- as.data.frame(configuration, stringsAsFactors=FALSE)
      if (!is.null(repair)) {
        configuration <- repair(configuration, parameters, digits)
      }

      if (is.null(forbidden)
          || nrow(checkForbidden(configuration, forbidden)) == 1) {
        newConfigurations[idxConfiguration,] <- configuration
        break
      }
      forbidden.retries <- forbidden.retries + 1
    }
    if (forbidden.retries >= 100) {
      irace.error("irace tried 100 times to sample from the model a configuration not forbidden without success, perhaps your constraints are too strict?")
    }
  }
  return (newConfigurations)
}

# To be called the first time before the second race (with indexIter =
# 2) Nb configurations is the number of configurations at the end
# included the elite ones obtained from the previous iteration
sampleModel <- function (parameters, eliteConfigurations, model,
                         nbNewConfigurations, digits, forbidden = NULL,
                         repair = NULL)
{
  if (nbNewConfigurations <= 0) {
    irace.error ("The number of configurations to generate appears to be negative or zero.")
  }
  namesParameters <- names(parameters$conditions)
  newConfigurations  <-
    as.data.frame(matrix(nrow = nbNewConfigurations,
                         ncol = length(namesParameters) + 1,
                         dimnames = list(NULL, c(namesParameters, ".PARENT."))
                         ))
  empty.configuration <- new.empty.configuration(parameters)
  
  for (idxConfiguration in seq_len(nbNewConfigurations)) {
    forbidden.retries <- 0
    while (forbidden.retries < 100) {
      # Choose the elite which will be the parent.
      indexEliteParent <- sample.int (n = nrow(eliteConfigurations), size = 1,
                                      prob = eliteConfigurations[[".WEIGHT."]])
      eliteParent <- eliteConfigurations[indexEliteParent, ]
      idEliteParent <- eliteParent[[".ID."]]
      configuration <- empty.configuration
      configuration[[".PARENT."]] <- idEliteParent
      
      # Sample a value for every parameter of the new configuration.
      for (p in seq_along(namesParameters)) {
        # FIXME: We must be careful because parameters$types does not
        # have the same order as parameters$conditions. Ideally, we
        # should fix this or make it impossible to confuse them.
        currentParameter <- namesParameters[p]
        currentType <- parameters$types[[currentParameter]]
        if (!conditionsSatisfied(parameters, configuration, currentParameter)) {
          # Some conditions are unsatisfied.
          # Should be useless, NA is ?always? assigned when matrix created
          newVal <- NA
          
        } else if (isFixed(currentParameter, parameters)) {
          # We don't even need to sample, there is only one possible value !
          newVal <- get.fixed.value (currentParameter, parameters)
          # The parameter is not a fixed and should be sampled
        } else if (currentType %in% c("i", "r")) {
          mean <- as.numeric(eliteParent[currentParameter])
          # If there is not value we obtain it from the model
          if (is.na(mean)) mean <- model[[currentParameter]][[as.character(idEliteParent)]][2]
          if (is.na(mean)) {
            # The elite parent does not have any value for this parameter,
            # let's sample uniformly.
            newVal <- sample.numerical(currentParameter, parameters, currentType, digits)
          } else {
            stdDev <- model[[currentParameter]][[as.character(idEliteParent)]][1]
            newVal <- sample.numerical(currentParameter, parameters, currentType, digits, mean, stdDev)
          }
        } else if (currentType == "o") {
          possibleValues <- paramDomain(currentParameter, parameters)  
          value <- eliteParent[currentParameter]
          
          if (is.na(value)) {
            # The elite parent does not have any value for this
            # parameter, let's sample uniformly
            ## FIXME: We should save the last used parameter in the model and use it here.
            newVal <- sample(possibleValues, 1)
          } else {
            # Find the position within the vector of possible
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
          possibleValues <- paramDomain(currentParameter, parameters)
          newVal <- sample(x = possibleValues, size = 1, prob = probVector)
        } else {
          irace.internal.error("Unexpected condition in sampleModel")
        }
        configuration[[p]] <- newVal
      }
      
      configuration <- as.data.frame(configuration, stringsAsFactors = FALSE)
      if (!is.null(repair)) {
        configuration <- repair(configuration, parameters, digits)
      }
      if (is.null(forbidden)
          || nrow(checkForbidden(configuration, forbidden)) == 1) {
        newConfigurations[idxConfiguration,] <- configuration
        break
      }
      forbidden.retries <- forbidden.retries + 1
    }
    if (forbidden.retries >= 100) {
      irace.error("irace tried 100 times to sample from the model a configuration not forbidden without success, perhaps your constraints are too strict?")
    }
  }
  return (newConfigurations)
}

# Sample value for a numerical parameter.
sample.numerical <- function(param, parameters, type, digits, mean = NULL, stdDev = NULL)
{
  lowerBound <- paramLowerBound(param, parameters)
  upperBound <- paramUpperBound(param, parameters)
  transf <- parameters$transform[[param]]
  if (transf == "log") {
    trLower <- attr(transf, "lower") 
    trUpper <- attr(transf, "upper")
    value <- sample.numeric.log(type, lowerBound, upperBound, trLower, trUpper,
                                digits, mean, stdDev)
  } else {
    value <- sample.numeric.none(type, lowerBound, upperBound, digits, mean, stdDev)
  }
  return(value)
}

sample.numeric.none <- function(type, lowerBound, upperBound, digits, mean, stdDev)
{
  if (type == "i") {
    if (is.null(mean)) {
      # integer uniform
      newVal <- floor(runif(1, min = lowerBound, max = 1 + upperBound))
    } else {
      # integer from model
      newVal <- round(rtnorm(1, mean + 0.5, stdDev, lowerBound, upperBound + 1) - 0.5)
    }
  } else {
    irace.assert(type == "r")
    if (is.null(mean)) {
      # real uniform
      newVal <- runif(1, min = lowerBound, max = upperBound)
    } else {
      # real from model
      newVal <- rtnorm(1, mean, stdDev, lowerBound, upperBound)
    }
    newVal <- round(newVal, digits)
  }
  return(newVal)
}

sample.numeric.log <- function(type, lowerBound, upperBound, trLower, trUpper,
                               digits, mean, stdDev)
{
  if (is.null(mean)) {
    newVal <- runif(1, min = trLower, max = trUpper)
  } else {
    # sample from model
    trMean <- transform.log(mean, lowerBound, digits)
    newVal <- rtnorm(1, trMean, stdDev, trLower, trUpper)
  }
  newVal <- exp(newVal)
  newVal <- check.transformed.log(newVal, lowerBound, upperBound, digits)
  newVal <- if (type == "i") round(newVal) else round(newVal, digits)
  return(newVal)
}

# Shift the value in the positive domain if LB <= 0.  A similar check will be
# required after applying the transformation (see function
# check.transformed.log() ).
transform.log <- function(value, lowerBound, digits)
{
  # If LB <= 0, we cannot compute log(0), so we have to translate it to the
  # positive domain.
  if (lowerBound <= 0) 
    value <- value - lowerBound + 10^-digits
  irace.assert(value > 0)
  return (log(value))
}

# Adjust the sampled value if the lower bound is <=0, and
# check that it does not fall outside the allowed range.
check.transformed.log <- function(value, lowerBound, upperBound, digits)
{
  # Check if LB was not positive, then readjust
  if (lowerBound <= 0)
    value <- value + lowerBound - 10^-digits
  
  irace.assert(is.finite(value))
  # Enforce bounds.
  return (min(max(value, lowerBound), upperBound))
}
