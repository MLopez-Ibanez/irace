####################################################
## INITIALISE AND UPDATE THE MODEL
####################################################

# Initial standard deviation for numerical sampling model.
init_sd_numeric <- function(parameters, param)
{
  # Dependent parameters define the standard deviation as
  # a portion of the size of the domain interval. In this case,
  # 0.5 indicates half of the interval, equivalent  to
  # (domain[2] - domain[1]) * 0.5
  if (parameters$isDependent[[param]] || parameters$transform[[param]] == "log")
    return(0.5)
  domain <- parameters$domain[[param]]
  (domain[[2L]] - domain[[1L]]) * 0.5
}

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
  nbConfigurations <- nrow(configurations)
  ids <- as.character(configurations[[".ID."]])
  param_names <- parameters$names[!parameters$isFixed]
  model <- setNames(vector("list", length(param_names)), param_names)
                    
  for (currentParameter in param_names) {
    type <- parameters$types[[currentParameter]]
    if (type == "c") {
      nbValues <- length(parameters$domain[[currentParameter]])
      value <- rep((1 / nbValues), nbValues)
      param <- rep(list(value), nbConfigurations)
    } else {
      if (type == "r" || type == "i") {
        sd <- init_sd_numeric(parameters, currentParameter)
        values <- configurations[[currentParameter]]
      } else if (type == "o") {
        domain <- parameters$domain[[currentParameter]]
        sd <- (length(domain) - 1) * 0.5
        values <- match(configurations[[currentParameter]], domain)
      } else {
        irace.internal.error("Unknown parameter type '", type, "'")
      }
      # Assign current parameter value to model.
      param <- mapply(c, sd, values, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    }
    names(param) <- ids
    model[[currentParameter]] <- param
  }
  model
}

updateModel <- function(parameters, eliteConfigurations, oldModel,
                         indexIteration, nbIterations, nbNewConfigurations, scenario)
{
  dec_factor <- (1 - ((indexIteration - 1) / nbIterations))
  add_factor <- ((indexIteration - 1) / nbIterations)
  num_factor <- ((1 / nbNewConfigurations)^(1 / parameters$nbVariable))
  prob_max <- 0.2^(1. / parameters$nbVariable)
  update_prob <- if (scenario$elitist) function(p, idx) {
    # Decrease first all values in the vector:
    p <- p * dec_factor
    p[idx] <- p[idx] + add_factor
    # Normalize probabilities.
    p <- p / sum(p)
    # Prevent probabilities from growing too much.
    p <- pmin.int(p, prob_max)
    p <- p / sum(p)
    p / sum(p)
  } else function(p, idx) {
    # Decrease first all values in the vector:
    p <- p * dec_factor
    p[idx] <- p[idx] + add_factor
    # Normalize probabilities.
    p <- p / sum(p)
  }
  
  param_names <- parameters$names[!parameters$isFixed]
  model_ids <- names(oldModel[[1L]])
  # If the elite is older than the current iteration, it has its own model
  # that has evolved with time. If the elite is new (generated in the current
  # iteration), it does not have any, and we have to copy the one from its
  # parent. The condition of the IF statement is for checking whether the
  # configuration already has its model or not.
  elite_ids <- as.character(eliteConfigurations[[".ID."]])
  not_in <- elite_ids %not_in% model_ids
  ids_in_model <- elite_ids
  # If a configuration does not have any entry, copy the parent one.
  ids_in_model[not_in] <- as.character(eliteConfigurations[[".PARENT."]][not_in])
  newModel <- setNames(vector("list", length(param_names)), param_names)  
  
  for (currentParameter in param_names) {
    type <- parameters$types[[currentParameter]]
    irace.assert(all(ids_in_model %in% names(oldModel[[currentParameter]])))
    this_model <- oldModel[[currentParameter]][ids_in_model]
    values <- eliteConfigurations[[currentParameter]]
    values_not_na <- !is.na(values)
    values <- values[values_not_na]
    if (type == "c") {
      # Find the value that has been "chosen" to increase its probability.
      values <- match(values, parameters$domain[[currentParameter]])
      this_model[values_not_na] <- mapply(update_prob, this_model[values_not_na], values, SIMPLIFY=FALSE)
    } else {
      irace.assert(type %in% c("i", "r", "o"))
      if (type == "o") 
        values <- match(values, parameters$domain[[currentParameter]])
      this_model[values_not_na] <- mapply(function(p, value) c(p[[1L]] * num_factor, value),
                                          this_model[values_not_na], values, SIMPLIFY=FALSE)
    }
    names(this_model) <- elite_ids
    newModel[[currentParameter]] <- this_model
  }
  newModel
}

## Update the model 
updateModel_old <- function (parameters, eliteConfigurations, oldModel,
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
        # Change the current parameter value of the model
        if (type %in% c("i", "r") &&
            !is.na(eliteConfigurations[idCurrentConfiguration,currentParameter]))
          probVector[2] <- eliteConfigurations[idCurrentConfiguration,currentParameter]
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
        irace.assert(type %in% c("i", "r", "o"))
        probVector[1] <- probVector[1] * ((1 / nbNewConfigurations)^(1 / parameters$nbVariable))
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

restartModel <- function(model, configurations, restart_ids, parameters,
                         nbConfigurations)
{
  back_factor <- nbConfigurations^(2 / parameters$nbVariable)
  second_factor <- (1 / nbConfigurations)^(1 / parameters$nbVariable)

  model_ids <- names(model[[1L]])
  restart_ids <- as.character(sort.int(as.integer(restart_ids)))
  not_in <- restart_ids %not_in% model_ids
  configurations <- configurations[configurations[[".ID."]] %in% restart_ids, c(".ID.", ".PARENT.")]
  restart_ids[not_in] <- configurations[[".PARENT."]][order(as.integer(configurations[[".ID."]]))][not_in]
  restart_ids <- as.character(unique(restart_ids))
  restart_ids <- restart_ids[!is.na(restart_ids)]
  
  namesParameters <- parameters$names[!parameters$isFixed]
  for (param in namesParameters) {
    model_param <- model[[param]]
    irace.assert (all(restart_ids %in% names(model_param)), {
      cat("Param:", param, "\n")
      print(restart_ids)
      print(model)
      print(configurations[, c(".ID.", ".PARENT.")])
    })
    type <- parameters$types[[param]]
    if (type == "c") {
      model[[param]][restart_ids] <- sapply(model_param[restart_ids],
                                            function(p) {
                                              p <- 0.9 * p + 0.1 * max(p)
                                              p / sum(p)
                                            }, simplify=FALSE)
    } else {
      if (type == "i" || type == "r") {
        value <- init_sd_numeric(parameters, param)
      } else {
        irace.assert(type == "o")
        value <- (length(parameters$domain[[param]]) - 1) * 0.5
      }
      # Bring back the value 2 iterations or to the second iteration value.
      value <- value * second_factor
      model[[param]][restart_ids] <- sapply(
        model_param[restart_ids],
        function(x) c(min(x[[1L]] * back_factor, value), x[[2L]]),
        simplify=FALSE)
    }
  }
  model
}
