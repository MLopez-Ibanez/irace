####################################################
## INITIALISE AND UPDATE THE MODEL
####################################################

## Initialisation of the model after the first iteration
##
# IN: configurations matrix, parameters datastructure
##
# OUTPUT: A list of list of vectors. The higher-level list contains
# one element per categorical parameter.  Each categorical parameter
# contains a list of vector. This list contains elements which are the
# .ID. of the configuration. 
initialiseModel <- function (parameters, configurations)
{
  model <- list()
  nbConfigurations <- nrow(configurations)
  
  for (currentParameter in parameters$names[!parameters$isFixed]) {
    type <- parameters$types[[currentParameter]]
    nbValues <- length(parameters$domain[[currentParameter]])
    param <- list()
    if (type == "c") {
      value <- rep((1 / nbValues), nbValues)
    } else if (type == "i" || type == "r") {
      lowerBound <- paramLowerBound(currentParameter, parameters)
      upperBound <- paramUpperBound(currentParameter, parameters)
      value <- (upperBound - lowerBound) / 2
    } else {
      irace.assert(type == "o")
      value <- (nbValues - 1) / 2
    }
    for (indexConfig in seq_len(nbConfigurations)) {
      idCurrentConfig <- as.character(configurations[indexConfig, ".ID."])
      param[[idCurrentConfig]] <- value
    }
    model[[currentParameter]] <- param
  }
  return (model)
}

## FIXME (MANUEL): This function needs a description.
## Update the model 
updateModel <- function (parameters, eliteConfigurations, oldModel,
                         indexIteration, nbIterations, nbNewConfigurations, scenario)
{
  newModel <- list()
  
  for (idxConfiguration in seq_len(nrow(eliteConfigurations))) {
    idCurrentConfiguration <- eliteConfigurations[idxConfiguration, ".ID."]
    idCurrentConfiguration <- as.character(idCurrentConfiguration)

    for (currentParameter in parameters$names[!parameters$isFixed]) {
      type <- parameters$types[[currentParameter]]

      ## If the elite is older than the current iteration, it has
      ## its own model that has evolved with time. If the elite is
      ## new (generated in the current iteration), it does not have
      ## any, and we have to copy the one from its parent. The
      ## condition of the IF statement is for checking wether the
      ## configuration already has its model or not.
      
      # FIXME: FIX character IDs, they should be numeric!
      if (idCurrentConfiguration  %in% names(oldModel[[currentParameter]])) {
        # cat("This configuration has already an entry, to be updated\n")
        probVector <- oldModel[[currentParameter]][[idCurrentConfiguration]]
      } else {
        # cat("This configuration does not have any entry, copy the parent one\n")
        idParent <- eliteConfigurations[idxConfiguration, ".PARENT."]
        irace.assert(as.integer(idParent) < as.integer(idCurrentConfiguration))
        idParent <- as.character(idParent)
        # cat("The parent found is ", idParent, "\n")
        probVector <- oldModel[[currentParameter]][[idParent]]
      }
      # cat("probVector: ", probVector)

      if (type == "c") {
        actualValue <- eliteConfigurations[idxConfiguration, currentParameter]
      
        if (is.na(actualValue)) {
          # cat ("NA found, don't change the prob vector")
        } else {
          possibleValues <- parameters$domain[[currentParameter]]
          # Decrease first all values in the vector:
          probVector <- probVector * (1 - ((indexIteration - 1) / nbIterations))
          # cat("new probVector after decrease: ", probVector)
          
          # Find the value that has been "chosen" to increase its probability.
          indexValue <- which (possibleValues == actualValue)
          probVector[indexValue] <- (probVector[indexValue]
                                      + ((indexIteration - 1) / nbIterations))
#                 cat("The value found for the configuration n.",
#                 idxConfiguration, "(ID=",
#                 idCurrentConfiguration, ") is the ", indexValue,
#                 "th.\n")

          # Prevent probabilities from growing too much.
          if (scenario$elitist) {
            probVector <- probVector / sum(probVector)
            probMax    <- 0.2^(1 / parameters$nbVariable)
            probVector <- pmin(probVector, probMax)
          }
          # Normalize probabilities.
          probVector <- probVector / sum(probVector)
          #print("newProbVector after increase: ")
          #print(newVector)  
        }
      } else {
        irace.assert(type == "i" || type == "r" || type == "o")
        # Not really a vector but stdDev factor
        probVector <- probVector * ((1 / nbNewConfigurations)^(1 / parameters$nbVariable))
      }
      newModel[[currentParameter]][[idCurrentConfiguration]] <- probVector
    }
  }
  return (newModel)
}

printModel <- function (model)
{
  cat("# Model:\n")
  print(model)
}

restartConfigurations <- function (configurations, restart.ids, model, parameters,
                               nbConfigurations)
{
  #print(configurations)
  tmp.ids <- c()
  for (param in parameters$names[!parameters$isFixed]) {
    for (id in restart.ids) {
      if (!(id %in% names(model[[param]]))) {
        id <- configurations[configurations$.ID. == id, ".PARENT."]
      }
      tmp.ids <- c(tmp.ids, id)
    }
  }
  restart.ids <- unique(tmp.ids)
  #print(restart.ids)
  for (param in parameters$names[!parameters$isFixed]) {
    type <- parameters$types[[param]]
    for (id in restart.ids) {
      id <- as.character(id)
      irace.assert (id %in% names(model[[param]]))

      if (type == "c") {
        probVector <- model[[param]][[id]]
        probVector <- 0.9 * probVector + 0.1 * max(probVector)
        model[[param]][[id]] <- probVector / sum(probVector)
      } else {
        irace.assert(type == "i" || type == "r" || type == "o")
        if (type == "i" || type == "r") {
          lowerBound <- paramLowerBound(param, parameters)
          upperBound <- paramUpperBound(param, parameters)
          value <- (upperBound - lowerBound) / 2
        } else {
          irace.assert(type == "o")
          value <- (length(parameters$domain[[param]]) - 1) / 2
        }
        # Bring back the value 2 iterations or to the second iteration value.
        model[[param]][[id]] <-
          min(model[[param]][[id]] * (nbConfigurations^(2 / parameters$nbVariable)),
              value * ((1 / nbConfigurations)^(1 / parameters$nbVariable)))
      }
    }
  }
  return (model) 
}
