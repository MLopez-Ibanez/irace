#' Package description
#' 
#' package full description
#' @name essai
#' @docType package
NA

candidates.equal <- function(x, y, parameters, threshold)
{
  d <- 0.0
  # FIXME: If the distance used anything different than the
  # maximum, then fixed parameters should be ignored.
  for (i in seq_along(parameters$names)) {
#  for (param in parameters$names) {
    type <- parameters$types[[i]]
    param <- parameters$names[[i]]
    X <- x[[param]]
    Y <- y[[param]]
    if (is.na (X) && is.na(Y)) {
      # Both NA, just ignore this param
      next
    } else if (xor(is.na (X), is.na(Y))) {
      # Distance is 1.0, so not equal
      return (FALSE)
    } else if (type == "i" || type == "r") {
      lower <- oneParamLowerBound(param, parameters)
      upper <- oneParamUpperBound(param, parameters)
      d <- max(d, abs((as.numeric(X) - as.numeric(Y)) / (upper - lower)))
      if (d > threshold) return (FALSE)
    } else {
      stopifnot(type == "c" || type == "o")
      # Distance is 1.0, so definitely not equal.
      if (X != Y) return (FALSE)
    }
  }
  ## cat ("X:\n")
  ## print (x)
  ## cat ("Y:\n")
  ## print (y)
  ## cat ("D = ", d, "\n")
  return (TRUE)
}

similarCandidates <- function(candidates, parameters)
{
  similar <- c()
  num.candidates <- nrow(candidates)
  #cat("# ", format(Sys.time(), usetz=TRUE), " similarCandidates()\n")
  cat ("# Computing similarity of candidates ")
  for (i in seq_len(num.candidates - 1)) {
    for (j in ((i+1):num.candidates)) {
      if (i == j) next
      if (candidates.equal (candidates[i,],
                            candidates[j,],
                            parameters, threshold = 0.00000001)) {
        similar <- c(similar, candidates[i,".ID."], candidates[j,".ID."])
      }
    }
    cat(".")
  }
  cat(" DONE\n")
  #cat("# ", format(Sys.time(), usetz=TRUE), " similarCandidates() DONE\n")
  return (unique(similar))
}

## Number of iterations.
computeNbIterations <- function(nbParameters)
{
  return (2 + log2(nbParameters))
}

## Computational budget at each iteration.
computeComputationalBudget <- function(remainingBudget, indexIteration,
                                       nbIterations)
{
  return (remainingBudget / (nbIterations - indexIteration + 1))
}

## The number of candidates
computeNbCandidates <- function(currentBudget, indexIteration, mu)
{
  return (floor (currentBudget / (mu + min(5, indexIteration))))
}

## Termination of a race at each iteration. The race will stop if the
## number of surviving configurations is equal or less than this number.
computeTerminationOfRace <- function(nbParameters)
{
  return (2 + log2(nbParameters))
}

# This function is the interface between race and irace. It first
# converts all data structures used in irace to the ones expected by
# race, it calls race, and then conversely converts the resulting data
# into the proper data structures for irace.
oneIterationRace <-
  function(tunerConfig = NULL,
           parameters = NULL,
           candidates = NULL, 
           budget = NULL,
           minSurvival = NULL)
{
  # Call to Race:
  result <- race (maxExp = budget,
                  first.test = tunerConfig$firstTest,
                  each.test = tunerConfig$eachTest,
                  stat.test = tunerConfig$testType,
                  stop.min.cand = minSurvival,
                  # Parameters for race-wrapper.R
                  candidates = removeCandidatesMetaData(candidates),
                  parameters = parameters,
                  config = tunerConfig)

  # Let's transform a bit the matrix with all the results
  # Give colnames the proper global IDs
  colnames(result$results) <- as.character(candidates$.ID.)
  # Create two columns for instances and iteration
  expResults <- as.data.frame(matrix(ncol = 2, nrow = nrow(result$results)))
  colnames(expResults) <- c("instance", "iteration")
  # Fill instances (Iter will be outside this function)
  expResults$instance <- result$race.data$instances[1:result$no.tasks]
  # cbind matrix and two new columns
  expResults <- cbind(expResults, result$results)
  
  # Assign the flag alive or not.
  candidates$.ALIVE. <- as.logical(result$alive)
  # Assign the proper ranks in the candidates data.frame
  candidates$.RANK. <- Inf
  candidates[which(result$alive), ".RANK."] <- result$ranks
  # Now we can sort the data.frame by the rank
  candidates <- candidates[order(as.numeric(candidates[, ".RANK."])), ]

  # Consistency check
  stopifnot (all(as.logical(candidates[1:(result$no.alive), ".ALIVE."])))
  if (result$no.alive < nrow(candidates))
    stopifnot(!any(as.logical(candidates[(result$no.alive + 1):nrow(candidates) , ".ALIVE."])))

  return (list (nbAlive = result$no.alive,
                experimentsUsed = result$no.experiments,
                timeUsed = sum(result$time, na.rm = TRUE),
                candidates = candidates,
                expResults = expResults))
}

#' High-level function to use iterated Race
#' 
#' This function implement iterated Race. It receives some parameters to be tuned and returns the best
#' candidates found, namely, the elite candidates obtained from the last iterations (and sorted by rank).
#'
#' @param parameter data-structure containing the parameter definition. The data-structure has to be the one
#' returned by the function \code{readParameters()}. See documentation of this function for details.
#'
#' @param tunerConfig data-structure containing the tuner configuration.The data-structure has to be the one
#' returned by the function \code{readParameters()}. See documentation of this function for details.
#' @return Elites candidates obtained after the last iteration
#' @callGraphPrimitives
#' @note This is a note for the function \code{iteratedRace}
irace <- function(tunerConfig = stop("parameter `tunerConfig' is mandatory."),
                  parameters = stop("parameter `parameters' is mandatory."))
{
  tunerConfig <- checkConfiguration(defaultConfiguration(tunerConfig))
  
  if (is.na(tunerConfig$seed)) {
    tunerConfig$seed <- runif(1, 1, .Machine$integer.max)
  }
  set.seed(tunerConfig$seed)
  debugLevel <- tunerConfig$debugLevel

  # Data.frame of all candidates ever generated.
  namesParameters <- names(parameters$constraints)
  if (!is.null(tunerConfig$candidatesFile)
      && tunerConfig$candidatesFile != "") {
    allCandidates <- readCandidatesFile(tunerConfig$candidatesFile,
                                                parameters, debugLevel)
    allCandidates <- cbind(.ID. = 1:nrow(allCandidates),
                           allCandidates,
                           .PARENT. = NA)
    rownames(allCandidates) <- allCandidates$.ID.
  } else {
    candidates.colnames <- c(".ID.", namesParameters, ".PARENT.")
    allCandidates <-
      as.data.frame(matrix(ncol = length(candidates.colnames),
                           nrow = 0))
    colnames(allCandidates) <- candidates.colnames
  }
  eliteCandidates <- data.frame()
  
  timeBudget <- tunerConfig$timeBudget
  timeEstimate <- tunerConfig$timeEstimate

  nbIterations <- ifelse (tunerConfig$nbIterations == 0,
                          computeNbIterations(parameters$nbVariable),
                          tunerConfig$nbIterations)
  nbIterations <- floor(nbIterations)

  minSurvival <- ifelse (tunerConfig$minNbSurvival == 0,
                         computeTerminationOfRace(parameters$nbVariable),
                         tunerConfig$minNbSurvival)
  minSurvival <- floor(minSurvival)
  
  indexIteration <- 1
  # Compute the total initial budget, that is, the maximum number of
  # experiments that we can perform.
  remainingBudget <- ifelse (timeBudget > 0,
                             timeBudget / timeEstimate,
                             tunerConfig$maxExperiments)
  experimentsUsedSoFar <- 0
  timeUsedSoFar <- 0

  # To save the logs
  tunerResults <- list()
  tunerResults$tunerConfig <- tunerConfig
  tunerResults$parameters <- parameters
  tunerResults$iterationElites <- NULL
  tunerResults$experiments <- as.data.frame(matrix(ncol=2, nrow=0))
  colnames(tunerResults$experiments) <- c("instance", "iteration")
  
  cat("# ", format(Sys.time(), usetz=TRUE), ": INITIALIZATION \n",
      "# nbIterations: ", nbIterations, "\n",
      "# minSurvival: ", minSurvival, "\n",
      "# nbParameters: ", parameters$nbVariable, "\n",
      "# Seed: ", tunerConfig$seed, "\n",
      sep = "")

  while (TRUE) {
    if (remainingBudget <= 0) {
      cat("# ", format(Sys.time(), usetz=TRUE), ": Stopped because ",
          "budget is exhausted\n",
          "# nbIterations: ", nbIterations, "\n",
          "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
          "# timeUsedSoFar: ", timeUsedSoFar, "\n",
          "# timeEstimate: ", timeEstimate, "\n",
          "# remainingBudget: ", remainingBudget, "\n",
          "# currentBudget: ", currentBudget, "\n",
          "# number of elites: ", nrow(eliteCandidates), "\n",
          "# nbCandidates: ", nbCandidates, "\n", sep="")
      return (eliteCandidates)
    }

    if (indexIteration > nbIterations) {
      cat("# ", format(Sys.time(), usetz=TRUE), ": Limit of iterations reached\n", sep="")
      if (tunerConfig$nbIterations == 0) {
        nbIterations <- indexIteration
      } else {
        return (eliteCandidates)
      }
    }

    # Compute the current budget (nb of experiments for this iteration)
    # or take the value given as parameter.
    currentBudget <-
      ifelse (tunerConfig$nbExperimentsPerIteration == 0,
              computeComputationalBudget(remainingBudget, indexIteration,
                                         nbIterations),
              tunerConfig$nbExperimentsPerIteration)
    currentBudget <- floor (currentBudget)
    
    # Compute the number of candidate configurations for this race or
    # take the value given as a parameter.
    nbCandidates <- ifelse (tunerConfig$nbCandidates == 0,
                            computeNbCandidates(currentBudget, indexIteration,
                                                max(tunerConfig$mu,
                                                    tunerConfig$firstTest)),
                            tunerConfig$nbCandidates)
    
    # Stop if  the number of candidates to produce is not greater than
    # the number of elites...
    if (nbCandidates <= nrow(eliteCandidates)) {
      cat("# ", format(Sys.time(), usetz=TRUE), ": Stopped because ",
          "there is no enough budget to sample new candidates\n",
          #(number of elites  + 1) * (mu + min(5, indexIteration)) > remainingBudget\n",
          "# number of elites: ", nrow(eliteCandidates), "\n",
          "# indexIteration: ", indexIteration, "\n",
          "# mu: ", max(tunerConfig$mu, tunerConfig$firstTest), "\n",
          "# nbIterations: ", nbIterations, "\n",
          "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
          "# timeUsedSoFar: ", timeUsedSoFar, "\n",
          "# timeEstimate: ", timeEstimate, "\n",
          "# remainingBudget: ", remainingBudget, "\n",
          "# currentBudget: ", currentBudget, "\n",
          "# nbCandidates: ", nbCandidates, "\n",
          sep="")
      return (eliteCandidates)
    }
    # ... or the number of candidates to test is NOT larger than the minimum.
    if (nbCandidates <= minSurvival) {
      cat("# ", format(Sys.time(), usetz=TRUE), ": Stopped because ",
          "the number of candidates (", nbCandidates,
          ") is smaller than the minimum (", minSurvival,")\n",
          "# You may either increase the budget or set 'minNbSurvival' to a lower value\n",
          # FIXME: Create a helper function to report all this info
          # and avoid repetition.
          "# number of elites: ", nrow(eliteCandidates), "\n",
          "# indexIteration: ", indexIteration, "\n",
          "# mu: ", max(tunerConfig$mu, tunerConfig$firstTest), "\n",
          "# nbIterations: ", nbIterations, "\n",
          "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
          "# timeUsedSoFar: ", timeUsedSoFar, "\n",
          "# timeEstimate: ", timeEstimate, "\n",
          "# remainingBudget: ", remainingBudget, "\n",
          "# currentBudget: ", currentBudget, "\n",
          "# nbCandidates: ", nbCandidates, "\n",
          sep="")
      return (eliteCandidates)
    }

    cat("# ", format(Sys.time(), usetz=TRUE),
        ": ITERATION ", indexIteration, " of ", nbIterations, "\n",
        "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
        "# timeUsedSoFar: ", timeUsedSoFar, "\n",
        "# timeEstimate: ", timeEstimate, "\n",
        "# remainingBudget: ", remainingBudget, "\n",
        "# currentBudget: ", currentBudget, "\n",
        "# nbCandidates: ", nbCandidates, "\n", sep="")
    
    if (indexIteration == 1) {
      # If we need more candidates, sample uniformly.
      nbNewCandidates <- nbCandidates - nrow(allCandidates)
      if (nbNewCandidates > 0) {
        # Sample new candidates.
        if (debugLevel>= 1) {
          cat(sep="", "# ", format(Sys.time(), usetz=TRUE), ": ",
              "Sample ", nbNewCandidates,
              " candidates from uniform distribution\n")
        }
        newCandidates <- sampleUniform(tunerConfig = tunerConfig,
                                       parameters, nbNewCandidates)        
        newCandidates <-
          cbind (.ID. = max(0, allCandidates$.ID.) + 1:nrow(newCandidates),
                 newCandidates)
        allCandidates <- rbind(allCandidates, newCandidates)
        rownames(allCandidates) <- allCandidates$.ID.
      }
      testCandidates <- allCandidates[1:nbCandidates,]
    } else {
      # How many new candidates should be sampled?
      nbNewCandidates <- nbCandidates - nrow(eliteCandidates)

      # Update the model based on elites candidates
      if (debugLevel >= 1) {
        cat(sep="", "# ", format(Sys.time(), usetz=TRUE), ": ",
            "Update model\n") }
      model <- updateModel(parameters, eliteCandidates, model, indexIteration,
                           nbIterations, nbNewCandidates)
      if (debugLevel >= 2) { printModel (model) }
      
      if (debugLevel >= 1) {
        cat(sep="", "# ", format(Sys.time(), usetz=TRUE), ": ",
            "Sample ", nbNewCandidates, " candidates from model\n") }

      retrial <- FALSE

      tunerResults$softRestart[indexIteration] <- 0

      while (TRUE) {
        #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel()\n")
        newCandidates <- sampleModel(tunerConfig, parameters, eliteCandidates,
                                     model, nbNewCandidates)
        #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel() DONE\n")
        # Set ID of the new candidates.
        newCandidates <-
          cbind (.ID. = max(0, allCandidates$.ID.) + 1:nrow(newCandidates),
                 newCandidates)
        testCandidates <- rbind(eliteCandidates[, 1:ncol(allCandidates)], newCandidates)
        rownames(testCandidates) <- testCandidates$.ID.
        if (!retrial && tunerConfig$softRestart) {
#          Rprof("profile.out")
          tmp.ids <- similarCandidates (testCandidates, parameters)
#          Rprof(NULL)
          if (is.null(tmp.ids)) break
          cat(sep="", "# ", format(Sys.time(), usetz=TRUE), ": ",
              "Soft restart: ", paste(collapse = " ", tmp.ids), " !\n")
          model <- restartCandidates (testCandidates, tmp.ids, model, parameters, nbNewCandidates)
          tunerResults$softRestart[indexIteration] <- tunerResults$softRestart[indexIteration] + 1
          tunerResults$model$afterSR[[indexIteration]] <- model
          if (debugLevel >= 2) { printModel (model) }
          retrial <- TRUE
        } else {
          break
        }
      }

      # Append these candidates to the global table.
      allCandidates <- rbind(allCandidates, newCandidates)
      rownames(allCandidates) <- allCandidates$.ID.
    }

    if (debugLevel >= 1) {
      cat("# Candidates for the race n", indexIteration, ": \n")
      candidates.print(testCandidates, metadata = TRUE)
    }

    if (debugLevel >= 1) {
      cat(sep="", "# ", format(Sys.time(), usetz=TRUE), ": ", "Launch race\n")}
    raceResults <- oneIterationRace (tunerConfig = tunerConfig,
                                     parameters = parameters, 
                                     candidates = testCandidates,
                                     budget = currentBudget, 
                                     minSurvival = minSurvival)
    
    # Set the "iteration" field to iteration index, to save the
    # experimental results in tunerResults.
    raceResults$expResults$iteration <-
      rep(indexIteration, nrow(raceResults$expResults))
    
    tunerResults$experiments <- merge (tunerResults$experiments,
                                       raceResults$expResults,
                                       all = TRUE,
                                       sort = FALSE)

    # Re-order the columns for the exp results to be saved (order broken
    # by merge), note that it is not necessary, but simply more readable.
    tunerResults$experiments <-
      tunerResults$experiments[, c("instance", "iteration",
                                   as.character(seq(ncol(tunerResults$experiments) - 2)))]
    
    experimentsUsedSoFar <- experimentsUsedSoFar + raceResults$experimentsUsed
    if (timeBudget > 0) {
      timeUsedSoFar <- timeUsedSoFar + raceResults$timeUsed
      timeEstimate <- timeUsedSoFar / experimentsUsedSoFar
      remainingBudget <- (timeBudget - timeUsedSoFar) / timeEstimate
    } else {
      if (is.numeric(raceResults$timeUsed))
        timeUsedSoFar <- timeUsedSoFar + raceResults$timeUsed
      remainingBudget <- remainingBudget - raceResults$experimentsUsed
    }

    if (debugLevel >= 2) {
      cat("Results for the race n", indexIteration, ": \n")
      candidates.print (raceResults$candidates, metadata=TRUE)
    }

    if (debugLevel >= 1) { cat("# Extract elites\n") }
    eliteCandidates <- extractElites(raceResults$candidates,
                                     min(raceResults$nbAlive, minSurvival))
    cat("# Elite candidates:\n")
    candidates.print(eliteCandidates, metadata = debugLevel >= 1)
    tunerResults$iterationElites <- c(tunerResults$iterationElites, eliteCandidates$.ID.[1])
    
    if (indexIteration == 1) {
      if (debugLevel >= 1)  { cat("# Initialise model\n") }
      model <- initialiseModel(parameters, eliteCandidates)
    }
      
    if (debugLevel >= 1) { cat("# End of iteration ", indexIteration, "\n") }

    if (debugLevel >= 3) {
      cat("# All candidates:\n")
      candidates.print(allCandidates, metadata = TRUE)
    }

    ## Save to the log file
    tunerResults$allCandidates <- allCandidates
    cwd <- setwd(tunerConfig$execDir)
    save (tunerResults, file = tunerConfig$logFile)
    setwd(cwd)

    indexIteration <- indexIteration + 1
  }

  cat("# ", format(Sys.time(), usetz=TRUE), ": Limit of iterations reached\n",
      "# nbIterations: ", nbIterations, "\n",
      "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
      "# timeUsedSoFar: ", timeUsedSoFar, "\n",
      "# timeEstimate: ", timeEstimate, "\n",
      "# remainingBudget: ", remainingBudget, "\n",
      "# currentBudget: ", currentBudget, "\n",
      "# nbCandidates: ", nbCandidates, "\n", sep="")

  return (eliteCandidates)
}
