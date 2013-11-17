####################################################
## INITIALISE AND UPDATE THE MODEL
####################################################

## Initialisation of the model after the first iteration
##
# IN: candidates matrix, parameters datastructure
##
# OUTPUT: A list of list of vectors. The higher-level list contains
# one element per categorical parameter.  Each categorical parameter
# contains a list of vector. This list contains elements which are the
# .ID. of the candidate. 
initialiseModel <- function (parameters, candidates)
{
  model <- list()
  nbCandidates <- nrow(candidates)
  
  for (currentParameter in parameters$names) {
    if (isFixed(currentParameter, parameters)) next
    type <- parameters$types[[currentParameter]]
    nbValues <- length(parameters$boundary[[currentParameter]])
    oneParam <- list()
    if (type == "c") {
      value <- rep((1 / nbValues), nbValues)
    } else if (type == "i" || type == "r") {
      lowerBound <- oneParamLowerBound(currentParameter, parameters)
      upperBound <- oneParamUpperBound(currentParameter, parameters)
      value <- (upperBound - lowerBound) / 2
    } else {
      stopifnot(type == "o")
      value <- (length(oneParamBoundary(currentParameter, parameters)) - 1) / 2
    }
    for (indexConfig in seq_len(nbCandidates)) {
      idCurrentConfig <- as.character(candidates[indexConfig, ".ID."])
      oneParam[[idCurrentConfig]] <- value
    }
    model[[currentParameter]] <- oneParam
  }
  return (model)
}

## FIXME (MANUEL): This function needs a description.
## Update the model 
updateModel <- function (parameters, eliteCandidates, oldModel,
                         indexIteration, nbIterations, nbNewCandidates)
{
  newModel <- list()
  
  for (idxCandidate in seq_len(nrow(eliteCandidates))) {
    idCurrentCandidate <- eliteCandidates[idxCandidate, ".ID."]
    idCurrentCandidate <- as.character(idCurrentCandidate)

    for (currentParameter in parameters$names) {
      type <- parameters$types[[currentParameter]]
      if (isFixed(currentParameter, parameters)) next

      ## If the elite is older than the current iteration, it has
      ## its own model that has evolved with time. If the elite is
      ## new (generated in the current iteration), it does not have
      ## any, and we have to copy the one from its parent. The
      ## condition of the IF statement is for checking wether the
      ## candidate already has its model or not.
      
      # FIXME: FIX character IDs, they should be numeric!
      if (idCurrentCandidate  %in% names(oldModel[[currentParameter]])) {
        # cat("This candidate has already an entry, to be updated\n")
        probVector <- oldModel[[currentParameter]][[idCurrentCandidate]]
      } else {
        # cat("This candidate does not have any entry, copy the parent one\n")
        idParent <- eliteCandidates[idxCandidate, ".PARENT."]
        stopifnot(as.integer(idParent) < as.integer(idCurrentCandidate))
        idParent <- as.character(idParent)
        # cat("The parent found is ", idParent, "\n")
        probVector <- oldModel[[currentParameter]][[idParent]]
      }
      # cat("probVector: ", probVector)

      if (type == "c") {
        actualValue <- eliteCandidates[idxCandidate, currentParameter]
        possibleValues <- oneParamBoundary(currentParameter, parameters)
      
        if (is.na(actualValue)) {
          # cat ("NA found, don't change the prob vector")
        } else {
          # Decrease first all values in the vector:
          probVector <- probVector * (1 - ((indexIteration - 1) / nbIterations))
          # cat("new probVector after decrease: ", probVector)
          
          # Find the value that has been "chosen" to increase its probability.
          # FIXME: This could simply be something like
          # indexValue <- which (possibleValues == actualValue)
          # probVector[indexValue] <- (probVector[indexValue]
          #                            + ((indexIteration - 1) / nbIterations))
          for (indexValue in seq_along(possibleValues)) {
            if (possibleValues[indexValue] == actualValue) {
#                 cat("The value found for the candidate n°",
#                 idxCandidate, "(ID=",
#                 idCurrentCandidate, ") is the ", indexValue,
#                 "th.\n")
              probVector[indexValue] <- (probVector[indexValue]
                                         + ((indexIteration - 1) / nbIterations))
            }
          }
#             print("newProbVector after increase: ")
#             print(newVector)  
        }
      } else {
        stopifnot(type == "i" || type == "r" || type == "o")
        # Not really a vector but stdDev factor
        probVector <- probVector * ((1 / nbNewCandidates)^(1 / parameters$nbVariable))
      }
      newModel[[currentParameter]][[idCurrentCandidate]] <- probVector
    }
  }
  return (newModel)
}

printModel <- function (model)
{
  cat("# Model:\n")
  print(model)
}

restartCandidates <- function (candidates, restart.ids, model, parameters,
                               nbCandidates)
{
  #print(candidates)
  tmp.ids <- c()
  for (param in parameters$names) {
    if (isFixed(param, parameters)) next
    for (id in restart.ids) {
      if (!(id %in% names(model[[param]]))) {
        id <- candidates[candidates$.ID. == id, ".PARENT."]
      }
      tmp.ids <- c(tmp.ids, id)
    }
  }
  restart.ids <- unique(tmp.ids)
  #print(restart.ids)
  for (param in parameters$names) {
    if (isFixed(param, parameters)) next
    type <- parameters$types[[param]]
    for (id in restart.ids) {
      id <- as.character(id)
      stopifnot (id %in% names(model[[param]]))

      if (type == "c") {
        probVector <- model[[param]][[id]]
        probVector <- probVector + 0.1 * (max(probVector) - probVector)
        model[[param]][[id]] <- probVector / sum(probVector)
      } else {
        stopifnot(type == "i" || type == "r" || type == "o")
        if (type == "i" || type == "r") {
          lowerBound <- oneParamLowerBound(param, parameters)
          upperBound <- oneParamUpperBound(param, parameters)
          value <- (upperBound - lowerBound) / 2
        } else {
          stopifnot(type == "o")
          value <- (length(oneParamBoundary(param, parameters)) - 1) / 2
        }
        # Bring back the value 2 iterations or to the second iteration value.
        model[[param]][[id]] <-
          min(model[[param]][[id]] /
              ((1 / nbCandidates)^(2 / parameters$nbVariable)),
              value * ((1 / nbCandidates)^(1 / parameters$nbVariable)))
      }
    }
  }
  return (model) 
}
