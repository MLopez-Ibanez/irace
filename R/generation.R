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
        } else if (currentType == "i" || currentType == "r") {
          newVal <- sample.numerical(currentParameter, parameters, currentType, digits)
        } else if (currentType == "c" || currentType == "o") {
          possibleValues <- parameters$domain[[currentParameter]]
          newVal <- sample(possibleValues, 1)
        } else {
          stop (.irace.bug.report);
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
        transform <- parameters$transform[[currentParameter]]
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
            # The elite parent does not have any value for this
            # parameter, let's sample uniformly.
            ## FIXME: We should save the last used parameter in the model and use it here.
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
          stop (.irace.bug.report)
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
  transform <- parameters$transform[[param]]

  if (transform == "log") {
    value <- sample.numeric.log(type, lowerBound, upperBound, digits, mean, stdDev)
  } else {
    value <- sample.numeric.norm(type, lowerBound, upperBound, digits, mean, stdDev)
  }
  return(value)
}

sample.numeric.norm <- function(type, lowerBound, upperBound, digits, mean, stdDev)
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
      newVal <- runif(1, min = as.double(lowerBound), max = as.double(upperBound))
    } else {
      # real from model
      newVal <- round(rtnorm(1, mean, stdDev, lowerBound, upperBound), digits)
    }
    newVal <- round(newVal, digits)
  }
  return(newVal)
}

sample.numeric.log <- function(type, lowerBound, upperBound, digits, mean, stdDev)
{
  if (is.null(mean)) {
    trRange <- range.transform.log(lowerBound, upperBound, digits)
  } else {
    trRange <- range.transform.log(lowerBound, upperBound, digits, mean)
  }
  trLb <- trRange[["trLowerBound"]]
  trUb <- trRange[["trUpperBound"]]
  trMean <- trRange[["trMean"]]
  
  if (is.null(mean)) {
    # uniform
    newVal <- exp(runif(1, min = as.double(trLb), max = as.double(trUb)))
  } else {
    # sample from model
    newVal <- exp(rtnorm(1, trMean, abs(stdDev), trLb, trUb)) 
  }

  if (type == "i") {
    newVal <- round(newVal)
  } else {
    newVal <- round(newVal, digits)
  }

  newVal <- check.transform.log(newVal, lowerBound, upperBound)
  return(newVal)
}

# Transform the range [LB,UB] into [log(LB),log(UB)].
# It needs to pay attention to shift the range in the
# positive domain if LB <= 0. The mean is also transformed
# (if provided, otherwise it can be ignored).
# A similar check will be required after applying the
# transformation (see function check.transform.log() ).
range.transform.log <- function(lowerBound, upperBound, digits, mean = 1)
{
  # cannot compute log(0)
  if (lowerBound <= 0) {
    trLowerBound <- -digits
    trUpperBound <- log(upperBound - lowerBound)
    trMean <- log(mean + lowerBound)
  } else {
    trLowerBound <- log(lowerBound)
    trUpperBound <- log(upperBound)
    trMean <- log(mean)
  }
  return(list(trLowerBound = trLowerBound,
              trUpperBound = trUpperBound,
              trMean = trMean))
}

# Adjust the sampled value if the lower bound is <=0, and
# check that it does not fall outside the allowed range.
check.transform.log <- function(newVal, lowerBound, upperBound)
{
  # check if LB was negative, then readjust
  if (lowerBound <= 0) {
    newVal <- newVal + lowerBound
  }
  # never too sure
  if (newVal < lowerBound) {
    newVal <- lowerBound
  } else if (newVal > upperBound) {
    newVal <- upperBound
  }
  return(newVal)
}
