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
  if (type %in% c("i","i,log")) {
    return (as.integer(value))
  } else if (type == "c" || type == "o") {
    return (value)
  } else {
    irace.assert (type %in% c("r","r,log"))
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
        if (isFixed(currentParameter, parameters)) {
          # We don't even need to sample, there is only one possible value !
          newVal <- get.fixed.value (currentParameter, parameters)
          # The parameter is not a fixed and should be sampled          
        } else if (currentType == "i") {
          lowerBound <- as.integer(parameters$domain[[currentParameter]][1])
          upperBound <- as.integer(parameters$domain[[currentParameter]][2])
          newVal <- floor(runif(1, min = lowerBound, max = 1 + upperBound))
        } else if (currentType == "i,log") {
          lowerBound <- as.integer(parameters$domain[[currentParameter]][1])
          upperBound <- as.integer(parameters$domain[[currentParameter]][2])
          # cannot compute the log of non-positive values
          if (lowerBound <= 0) {
              trlb <- -digits
              trub <- log(upperBound - lowerBound)
          } else {
              trlb <- log(lowerBound)
              trub <- log(upperBound)
          }
          newVal <- round(exp(runif(1, min = trlb, max = trub)), 1)
          # re-check if LB was negative, then readjust
          if (lowerBound <= 0) {
            newVal <- newVal + lowerBound
          }
          # never too sure
          if (newVal < lowerBound) {
            newVal <- lowerBound
          } else if (newVal > upperBound) {
            newVal <- upperBound
          }
        } else if (currentType == "r") {
          lowerBound <- parameters$domain[[currentParameter]][1]
          upperBound <- parameters$domain[[currentParameter]][2]
          newVal <- runif(1, as.double(lowerBound), as.double(upperBound))
          newVal <- round(newVal, digits)
        } else if (currentType == "r,log") {
          lowerBound <- parameters$domain[[currentParameter]][1]
          upperBound <- parameters$domain[[currentParameter]][2]
          # cannot compute the log of non-positive values
          if (lowerBound <= 0) {
              trlb <- -digits
              trub <- log(upperBound - lowerBound)
          } else {
              trlb <- log(lowerBound)
              trub <- log(upperBound)
          }
          newVal <- exp(runif(1, min = as.double(trlb), max = as.double(trub)))
          newVal <- round(newVal, digits)
          # re-check if LB was negative, then readjust
          if (lowerBound <= 0) {
            newVal <- newVal + lowerBound
          }
          # never too sure
          if (newVal < lowerBound) {
            newVal <- lowerBound
          } else if (newVal > upperBound) {
            newVal <- upperBound
          }
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
        if (!conditionsSatisfied(parameters, configuration, currentParameter)) {
          # Some conditions are unsatisfied.
          # Should be useless, NA is ?always? assigned when matrix created
          newVal <- NA
          
        } else if (isFixed(currentParameter, parameters)) {
          # We don't even need to sample, there is only one possible value !
          newVal <- get.fixed.value (currentParameter, parameters)
          # The parameter is not a fixed and should be sampled
        } else if (currentType %in% c("i", "i,log", "r", "r,log")) {
          lowerBound <- paramLowerBound(currentParameter, parameters)
          upperBound <- paramUpperBound(currentParameter, parameters)
          mean <- as.numeric(eliteParent[currentParameter])
          if (is.na(mean)) {
            # The elite parent does not have any value for this
            # parameter, let's sample uniformly.
            if (currentType == "i") {
              newVal <- floor(runif(1, min = lowerBound, max = 1 + upperBound))
            } else if (currentType == "i,log") {
              # cannot compute the log of non-positive values
              if (lowerBound <= 0) {
                trlb <- -digits
                trub <- log(upperBound - lowerBound)
              } else {
                trlb <- log(lowerBound)
                trub <- log(upperBound)
              }
              newVal <- round(exp(runif(1, min = trlb, max = trub)), 1)
              # re-check if LB was negative, then readjust
              if (lowerBound <= 0) {
                newVal <- newVal + lowerBound
              }
              # never too sure
              if (newVal < lowerBound) {
                newVal <- lowerBound
              } else if (newVal > upperBound) {
                newVal <- upperBound
              }
            } else if (currenttype == "r") {
              newVal <- runif(1, lowerBound, upperBound)
            } else if (currentType == "r,log") {
              # cannot compute the log of non-positive values
              if (lowerBound <= 0) {
                trlb <- -digits
                trub <- log(upperBound - lowerBound)
              } else {
                  trlb <- log(lowerBound)
                  trub <- log(upperBound)
              }
              newVal <- exp(runif(1, min = as.double(trlb), max = as.double(trub)))
              newVal <- round(newVal, digits)
              # re-check if LB was negative, then readjust
              if (lowerBound <= 0) {
                newVal <- newVal + lowerBound
              }
              # never too sure
              if (newVal < lowerBound) {
                newVal <- lowerBound
              } else if (newVal > upperBound) {
                newVal <- upperBound
              }
            }
          } else {
            stdDev <- model[[currentParameter]][[as.character(idEliteParent)]]
            newVal <- ifelse(currentType == "i",
                             rtnorm(1, mean + 0.5, stdDev, lowerBound, upperBound + 1) - 0.5,
                             rtnorm(1, mean, stdDev, lowerBound, upperBound))
            if (currentType == "i") {
              newVal <- rtnorm(1, mean + 0.5, stdDev, lowerBound, upperBound + 1) - 0.5
              newVal <- round(newVal)
            } else if (currentType == "r") {
              newVal <- rtnorm(1, mean, stdDev, lowerBound, upperBound)
              newVal <- round(newVal, digits)
            } else {
              # cannot compute log(0)
              if (lowerBound == 0) {
                  trLowerBound <- -digits
                  trUpperBound <- log(upperBound)
              } else if (lowerBound < 0) {
                  trLowerBound <- -digits
                  trUpperBound <- log(upperBound - lowerBound)
                  mean <- mean - lowerBound
              } else {
                  trLowerBound <- log(lowerBound)
                  trUpperBound <- log(upperBound)
              }
              # newVal <- exp(runif(1, min=trLowerBound, max=trUpperBound))
              newVal <- exp(rtnorm(1, log(mean), abs(stdDev), trLowerBound, trUpperBound))

              if (lowerBound < 0) {
                newVal <- newVal + lowerBound
              }

               # triple-check
              if (currentType == "i,log") {
                newVal <- round(newVal)
              } else {
                newVal <- round(newVal, digits)
              }
              if (newVal < lowerBound) newVal <- lowerBound
              if (newVal > upperBound) newVal <- upperBound
            }
          }
        } else if (currentType == "o") {
          possibleValues <- paramDomain(currentParameter, parameters)  
          value <- eliteParent[currentParameter]
          
          if (is.na(value)) {
            # The elite parent does not have any value for this
            # parameter, let's sample uniformly
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
