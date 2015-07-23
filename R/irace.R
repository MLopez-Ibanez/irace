checkForbidden <- function(configurations, forbidden)
{
  # We have to use a variable name that will never appear in
  # configurations, so .FORBIDDEN .
  for (.FORBIDDEN in forbidden) {
    #print(.FORBIDDEN)
    configurations <- subset(configurations, eval(.FORBIDDEN))
    #print(configurations)
    #print(str(configurations))
  }
  #print(nrow(configurations))
  return(configurations)
}

# Sets irace variables from a recovery file.  It is executed in the
# parent environment.
#
# FIXME: Restoring occurs after reading the command-line/configuration
# file. At least for the irace command-line parameters (tunerConfig),
# it should occur before. We
# would need to:
#
# 1) Read recovery file settings from command-line/config file
#
# 2) if set, then recover irace configuration

# 3) then read other configuration from command-line/config file being
# careful to not override whatever the recovery has set.
#
# A work-around is to modify the recovery file (you can load it in R,
# modify tunerConfig then save it again).
recoverFromFile <- function(filename)
{
  # substitute() is needed to evaluate filename here.
  eval.parent(substitute({
    # This restores tunerResults, thus it doesn't need restoring.
    load (filename)
    # .Random.seed is special
    for (name in setdiff(names(tunerResults$state), ".Random.seed"))
      assign(name, tunerResults$state[[name]])
    assign(".Random.seed", tunerResults$state$.Random.seed, .GlobalEnv)
    # These variables are not state, but they are used directly by irace.
    for (name in c("tunerConfig", "parameters", "allCandidates"))
      assign(name, tunerResults[[name]])
    options(.race.debug.level = tunerConfig$debugLevel)
    options(.irace.debug.level = tunerConfig$debugLevel)
  }))
}

##
## Numerical candidates similarity function
##
numeric.candidates.equal <- function(x, candidates, parameters, threshold, param.names)
{
  d <- rep(0.0, nrow(candidates))
  bmat <- matrix(TRUE, nrow=nrow(candidates),ncol=length(param.names))
  selected <- 1:nrow(candidates)
  for (i in seq_along(param.names)) {
    param <- param.names[i]
    lower <- oneParamLowerBound(param, parameters)
    upper <- oneParamUpperBound(param, parameters)
 
    X <- x[[param]]
    y <- candidates[, param]
    for (j in seq_len(nrow(bmat))) { # Candidates loop
      Y <- y[selected[j]]
      if (is.na (X) && is.na(Y)) { # Both NA, just ignore this param
        next
      } else if (xor(is.na (X), is.na(Y))) { # Distance is 1.0, so not equal
        bmat[j,i] <- FALSE 
      } else {
        d[j] <- max(d[j], abs((as.numeric(X) - as.numeric(Y)) / (upper - lower)))
        if (d[j] > threshold) bmat[j,i] <- FALSE
      }
    }
    index <- which(apply(bmat,1,all))
    bmat <- bmat[index, , drop=FALSE]
    d <- d[index]
    selected  <- selected[index]
    if (nrow(bmat) == 0) break
  }
  
  similar <- c()
  if (length(selected) != 0)
    similar <- c(x[[".ID."]], candidates[selected,".ID."])
  
  return(similar)
}

##
## Identify which configurations are similar.
##
similarCandidates <- function(candidates, parameters)
{
  debug.level <- getOption(".irace.debug.level", 0)
  if (debug.level >= 1) cat ("# Computing similarity of candidates .")

  listCater <- c()
  listNumer <- c()

  # Create vectors of categorical and numerical
  # Change the name to vectorCater, vectorNumer!
  for (p in parameters$names) {
    if (parameters$isFixed[[p]]) next
    if (parameters$types[[p]] %in% c("c","o")) {
      listCater <- c(listCater, p)
    } else {
      listNumer <- c(listNumer, p)
    }
  }
  
  nbCater <- length(listCater)
  nbNumer <- length(listNumer)

  ### Categorical/Ordinal filtering ####
  if (nbCater > 0) {
    ## Build an array with the categorical appended together in a string
    strings <- c()
    for (i in 1:nrow(candidates)) {
      strings[i] <- paste(candidates[i, listCater], collapse = " ; ")
    }

    if (nbNumer != 0) candidates <- candidates[, c(".ID.", listNumer)]
    ord.strings <- order(strings)
    candidates <- candidates[ord.strings, ]
    strings <- strings[ord.strings]

    ## keep similar (index i == true means is the same as i + 1)
    similarIdx <- strings[-length(strings)] == strings[-1]
    
    ## Now let's get just a FALSE if we remove it, TRUE otherwise:
    keepIdx <- c(similarIdx[1],
                 (similarIdx[-1] | similarIdx[-length(similarIdx)]),
                 similarIdx[length(similarIdx)])
    
    ## filtering them out:
    candidates <- candidates [keepIdx, , drop=FALSE]
    ## filtering their strings out (to use them to define blocks):
    strings <- strings [keepIdx]
    
    ## if everything is already filtered out, return
    if (nrow(candidates) == 0) {
      if (debug.level >= 1) cat(" DONE\n")
      return(NULL)
    }
  }
  
  ### Numerical parameters within blocks of the same string ###
  if (nbNumer > 0) {
    similar <- c()
    if (nbCater > 0) {
      ## In this case the object "string" is available to define blocks
      ## Loop over blocks:
      beginBlock <- 1
      while (beginBlock < nrow(candidates)) {
        ## The current block is made of all candidates that have same
        ## categorical string as the one of candidate[beginBlock, ]
        blockIds <- which(strings == strings[beginBlock])
        endBlock <- blockIds[length(blockIds)]

        irace.assert (endBlock > beginBlock)
        ## Loop inside blocks:
        for (i in seq(beginBlock, endBlock-1)) {
          ## Compare candidate i with all the one that are after in the block
          similar <- c(similar,
                       numeric.candidates.equal(candidates[i, ], candidates[(i+1):endBlock,],
                                                parameters, threshold = 0.00000001, param.names = listNumer))
          if (debug.level >= 1) cat(".")
        }
        beginBlock <- endBlock + 1 # Next block starts after the end of the current one
      }
    } else {
      ## No categorical, so no blocks, just do the basic check without blocks
      for (i in seq_len(nrow(candidates) - 1)) {
        similar <- c(similar,
                     numeric.candidates.equal(candidates[i, ], candidates[(i+1):nrow(candidates),],
                                              parameters, threshold = 0.00000001, param.names = listNumer))
        if (debug.level >= 1) cat(".")
      }
    }
    similar <- unique(similar)
    candidates <- candidates[candidates[, ".ID."] %in% similar,]   
  }
  
  if (debug.level >= 1) cat(" DONE\n")
  if (nrow(candidates) == 0) {
    return (NULL)
  } else {
    return(candidates[,".ID."])
  }
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
  function(tunerConfig, candidates, parameters, budget, minSurvival)
{

  result <- race (maxExp = budget,
                  first.test = tunerConfig$firstTest,
                  each.test = tunerConfig$eachTest,
                  stat.test = tunerConfig$testType,
                  conf.level = tunerConfig$confidence,
                  stop.min.cand = minSurvival,
                  # Parameters for race-wrapper.R
                  #candidates = removeCandidatesMetaData(candidates),
                  candidates = candidates,
                  parameters = parameters,
                  config = tunerConfig)

  # Let's transform a bit the matrix with all the results
  # Give colnames the proper global IDs
  colnames(result$results) <- as.character(candidates$.ID.)
  # Create two columns for instances and iteration
  expResults <- as.data.frame(matrix(ncol = 2, nrow = nrow(result$results)))
  colnames(expResults) <- c("instance", "iteration")
  expResults$instance <- result$race.data$race.instances[1:result$no.tasks]
  # Add the results for each configuration as additional columns.
  expResults <- cbind(expResults, result$results)

  candidates$.ALIVE. <- as.logical(result$alive)
  # Assign the proper ranks in the candidates data.frame
  candidates$.RANK. <- Inf
  candidates[which(result$alive), ".RANK."] <- result$ranks
  # Now we can sort the data.frame by the rank
  candidates <- candidates[order(as.numeric(candidates[, ".RANK."])), ]

  # Consistency check
  irace.assert (all(as.logical(candidates[1:(result$no.alive), ".ALIVE."])))
  if (result$no.alive < nrow(candidates))
    irace.assert(!any(as.logical(candidates[(result$no.alive + 1):nrow(candidates) , ".ALIVE."])))

  if (tunerConfig$debugLevel >= 3) {
    irace.note ("Memory used in oneIterationRace():\n")
    irace.print.memUsed()
  }
  
  return (list (nbAlive = result$no.alive,
                experimentsUsed = result$no.experiments,
                timeUsed = sum(result$time, na.rm = TRUE),
                candidates = candidates,
                expResults = expResults))
}

startParallel <- function(config)
{
  cwd <- setwd (config$execDir)
  on.exit(setwd(cwd), add = TRUE)

  parallel <- config$parallel
  if (parallel > 1) {
    if (config$mpi) {
      mpiInit(parallel, config$debugLevel)
    } else {
      requireNamespace("parallel", quietly = TRUE)
      if (.Platform$OS.type == 'windows') {
        .irace$cluster <- parallel::makeCluster(parallel)
      }
    }
  }
}

stopParallel <- function()
{
  if (!is.null(.irace$cluster)) {
    parallel::stopCluster(.irace$cluster)
    .irace$cluster <- NULL
  }
}

irace.init <- function(configuration)
{
  # We need to do this here to use/recover .Random.seed later.
  if (is.na(configuration$seed)) {
    configuration$seed <- trunc(runif(1, 1, .Machine$integer.max))
  }
  set.seed(configuration$seed)
  
  ## FIXME: It would be much better to generate instances at the start of each
  ## race if we need them at all.
  # Generate instance + seed list 
  configuration$instancesList <- generateInstances(configuration)
  return(configuration)
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
  catInfo <- function(..., verbose = TRUE) {
    irace.note (..., "\n")
    if (verbose)
      cat ("# Iteration: ", indexIteration, "\n",
           "# nbIterations: ", nbIterations, "\n",
           "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
           "# timeUsedSoFar: ", timeUsedSoFar, "\n",
           "# timeEstimate: ", timeEstimate, "\n",
           "# remainingBudget: ", remainingBudget, "\n",
           "# currentBudget: ", currentBudget, "\n",
           "# number of elites: ", nrow(eliteCandidates), "\n",
           "# nbCandidates: ", nbCandidates, "\n",
           "# mu: ", max(tunerConfig$mu, tunerConfig$firstTest), "\n",
           sep = "")
  }
  
  tunerConfig <- checkConfiguration(defaultConfiguration(tunerConfig))
  
  # Recover state from file?
  if (!is.null(tunerConfig$recoveryFile)){
    cat ("# ", format(Sys.time(), usetz=TRUE), ": Resuming from file: '",
         tunerConfig$recoveryFile,"'\n", sep="")
    recoverFromFile(tunerConfig$recoveryFile)
  } else {
    tunerConfig <- irace.init (tunerConfig)
    debugLevel <- tunerConfig$debugLevel
    # Set options controlling debug level.
    # FIXME: This should be the other way around, the options set the debugLevel.
    options(.race.debug.level = debugLevel)
    options(.irace.debug.level = debugLevel)
    
    # Create a data frame of all candidates ever generated.
    namesParameters <- names(parameters$conditions)

    if (!is.null(tunerConfig$candidatesFile)
        && tunerConfig$candidatesFile != "") {
      allCandidates <- readCandidatesFile(tunerConfig$candidatesFile,
                                          parameters, debugLevel)
      allCandidates <- cbind(.ID. = 1:nrow(allCandidates),
                             allCandidates,
                             .PARENT. = NA)
      rownames(allCandidates) <- allCandidates$.ID.
      num <- nrow(allCandidates)
      allCandidates <- checkForbidden(allCandidates, tunerConfig$forbiddenExps)
      if (nrow(allCandidates) < num) {
        cat("# Warning: some of the configurations in the candidates file were forbidden and, thus, discarded\n")
      }
      cat("# ", num, " initial configuration(s) read from '",
          tunerConfig$candidatesFile, "'\n", sep="")
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
    currentBudget <-
      ifelse (tunerConfig$nbExperimentsPerIteration == 0,
              computeComputationalBudget(remainingBudget, indexIteration,
                                         nbIterations),
              tunerConfig$nbExperimentsPerIteration)
    currentBudget <- floor (currentBudget)

    # To save the logs
    tunerResults <- list()
    tunerResults$tunerConfig <- tunerConfig
    tunerResults$irace.version <- irace.version
    tunerResults$parameters <- parameters
    tunerResults$iterationElites <- NULL
    tunerResults$allElites <- list()
    tunerResults$experiments <- as.data.frame(matrix(ncol=2, nrow=0))
    colnames(tunerResults$experiments) <- c("instance", "iteration")
    model <- NULL
    nbCandidates <- 0

    ## Compute the minimum budget required, and exit early in case the
    ## budget given by the user is insufficient.
    # This is computed from the default formulas as follows:
    #  B_1 = B / I
    #  B_2 = B -  (B/I) / (I - 1) = B / I
    #  B_3 = B - 2(B/I) / (I - 2) = B / I
    # thus
    #  B_i = B / I
    # and
    #  C_i = B_i / (mu + min(5,i)) = B / (I * (mu + min(5,i))).
    # We want to enforce that C_i >= min_surv + 1, thus
    #  B / (I * (mu + min(5,i))) >= min_surv + 1        (1)
    # becomes
    #  B >= (min_surv + 1) * I * (mu + min(5,i))
    # and the most strict value is for i >= 5, thus
    #  B >= (min_surv + 1) * I * (mu + 5)
    #
    # This is an over-estimation, since actually B_1 = floor(B/I) and if
    # floor(B/I) < B/I, then B_i < B/I, and we could still satisfy Eq. (1)
    # with a smaller budget. However, the exact formula requires computing B_i
    # taking into account the floor() function, which is not obvious.
    minimumBudget <- (minSurvival + 1) * nbIterations *
      (max(tunerConfig$mu, tunerConfig$firstTest) + 5)
     
    if (remainingBudget < minimumBudget) {
      tunerError("Insufficient budget: ",
                 "With the current settings, irace will require a value of ",
                 "'maxExperiments' of at least '",  minimumBudget, "'. ",
                 "You can either increase the budget, ",
                 "or set a smaller value of either 'minNbSurvival' ",
                 "or 'nbIterations'")
    }
  }

  catInfo("Initialization\n", 
          "# nbIterations: ", nbIterations, "\n",
          "# minNbSurvival: ", minSurvival, "\n",
          "# nbParameters: ", parameters$nbVariable, "\n",
          "# seed: ", tunerConfig$seed, "\n",
          "# confidence level: ", tunerConfig$confidence, "\n",
          "# remainingBudget: ", remainingBudget, "\n",
          "# mu: ", max(tunerConfig$mu, tunerConfig$firstTest), "\n",
          verbose = FALSE)

  startParallel(tunerConfig)
  on.exit(stopParallel())

  while (TRUE) {
    # Recovery info 
    tunerResults$state <- list(.Random.seed = .Random.seed, 
                               currentBudget = currentBudget,
                               debugLevel = debugLevel,
                               eliteCandidates = eliteCandidates,
                               experimentsUsedSoFar = experimentsUsedSoFar,
                               indexIteration = indexIteration,
                               minSurvival = minSurvival,
                               model = model,
                               nbCandidates = nbCandidates,
                               nbIterations = nbIterations,
                               remainingBudget = remainingBudget,
                               timeBudget = timeBudget,
                               timeEstimate = timeEstimate,
                               timeUsedSoFar = timeUsedSoFar)
    ## Save to the log file
    tunerResults$allCandidates <- allCandidates
    if (!is.null.or.empty(tunerConfig$logFile)) {
      cwd <- setwd(tunerConfig$execDir)
      save (tunerResults, file = tunerConfig$logFile)
      setwd(cwd)
    }

    if (remainingBudget <= 0) {
      catInfo("Stopped because budget is exhausted")
      return (eliteCandidates)
    }

    if (indexIteration > nbIterations) {
        if (debugLevel >= 3) {
            # This message is more confusing than useful, since this
            # is not really a limit. First, since we require a minimum
            # budget, this number of iterations should always be
            # reached. Second, as long as there is enough budget, we
            # always do more iterations.
            catInfo("Limit of iterations reached", verbose = FALSE)
        }
      if (tunerConfig$nbIterations == 0) {
        nbIterations <- indexIteration
      } else {
        return (eliteCandidates)
      }
    }
    # Compute the current budget (nb of experiments for this iteration),
    # or take the value given as parameter.
    currentBudget <-
      ifelse (tunerConfig$nbExperimentsPerIteration == 0,
              computeComputationalBudget(remainingBudget, indexIteration,
                                         nbIterations),
              tunerConfig$nbExperimentsPerIteration)
    currentBudget <- floor (currentBudget)
    
    # Compute the number of candidate configurations for this race.
    nbCandidates <- computeNbCandidates(currentBudget, indexIteration,
                                        max(tunerConfig$mu,
                                            tunerConfig$firstTest))

    # If a value was given as a parameter, then this value limits the maximum,
    # but if we have budget only for less than this, then we have run out of
    # budget.
    if (tunerConfig$nbCandidates > 0) {
      if (tunerConfig$nbCandidates < nbCandidates) {
        nbCandidates <- tunerConfig$nbCandidates
      } else if (currentBudget < remainingBudget) {
        # We skip one iteration
        indexIteration <- indexIteration + 1
        next
      } else {
        catInfo("Stopped because ",
                "there is no enough budget to enforce the value of nbCandidates")
        return (eliteCandidates)
      }
    }

    # Stop if  the number of candidates to produce is not greater than
    # the number of elites...
    if (nbCandidates <= nrow(eliteCandidates)) {
      catInfo("Stopped because ",
              "there is no enough budget left to race newly sampled configurations")
      #(number of elites  + 1) * (mu + min(5, indexIteration)) > remainingBudget" 
      return (eliteCandidates)
    }
    # ... or if the number of candidates to test is NOT larger than the minimum.
    if (nbCandidates <= minSurvival) {
      catInfo("Stopped because there is no enough budget left to race more than ",
              "the minimum (", minSurvival,")\n",
              "# You may either increase the budget or set 'minNbSurvival' to a lower value")
      return (eliteCandidates)
    }

    catInfo("Iteration ", indexIteration, " of ", nbIterations, "\n",
            "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
            "# timeUsedSoFar: ", timeUsedSoFar, "\n",
            "# timeEstimate: ", timeEstimate, "\n",
            "# remainingBudget: ", remainingBudget, "\n",
            "# currentBudget: ", currentBudget, "\n",
            "# nbCandidates: ", nbCandidates,
            verbose = FALSE)

    # Sample for the first time.
    if (nrow(eliteCandidates) == 0) {
      # If we need more candidates, sample uniformly.
      nbNewCandidates <- nbCandidates - nrow(allCandidates)
      if (nbNewCandidates > 0) {
        # Sample new candidates.
        if (debugLevel >= 1) {
          catInfo("Sample ", nbNewCandidates,
                  " candidates from uniform distribution", verbose = FALSE)
        }
        newCandidates <- sampleUniform(parameters, nbNewCandidates,
                                       digits = tunerConfig$digits,
                                       forbidden = tunerConfig$forbiddenExps)
        newCandidates <-
          cbind (.ID. = max(0, allCandidates$.ID.) + 1:nrow(newCandidates),
                 newCandidates)
        allCandidates <- rbind(allCandidates, newCandidates)
        rownames(allCandidates) <- allCandidates$.ID.
      } else if (nbNewCandidates < 0) {
        # We also truncate allCandidates in case there were too many
        # initial candidates.
        catInfo("Only ", nbCandidates,
                " from candidates file will be used, the rest are discarded",
                verbose = FALSE)
        allCandidates <- allCandidates[1:nbCandidates,]
      }
      testCandidates <- allCandidates
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

      #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel()\n")
      newCandidates <- sampleModel(tunerConfig, parameters, eliteCandidates,
                                   model, nbNewCandidates,
                                   forbidden = tunerConfig$forbiddenExps)
      #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel() DONE\n")
      # Set ID of the new candidates.
      newCandidates <- cbind (.ID. = max(0, allCandidates$.ID.) +
                              1:nrow(newCandidates), newCandidates)
      testCandidates <- rbind(eliteCandidates[, 1:ncol(allCandidates)],
                              newCandidates)
      rownames(testCandidates) <- testCandidates$.ID.
      tunerResults$softRestart[indexIteration] <- FALSE
      if (tunerConfig$softRestart) {
        #          Rprof("profile.out")
        tmp.ids <- similarCandidates (testCandidates, parameters)
        #          Rprof(NULL)
        if (!is.null(tmp.ids)) {
          if (debugLevel >= 1)
            cat(sep="", "# ", format(Sys.time(), usetz=TRUE), ": ",
                "Soft restart: ", paste(collapse = " ", tmp.ids), " !\n")
          model <- restartCandidates (testCandidates, tmp.ids, model,
                                      parameters, nbNewCandidates)
          tunerResults$softRestart[indexIteration] <- TRUE
          tunerResults$model$afterSR[[indexIteration]] <- model
          if (debugLevel >= 2) { printModel (model) }
          # Re-sample after restart like above
          #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel()\n")
          newCandidates <- sampleModel(tunerConfig, parameters, eliteCandidates,
                                       model, nbNewCandidates,
                                       forbidden = tunerConfig$forbiddenExps)
          #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel() DONE\n")
          # Set ID of the new candidates.
          newCandidates <- cbind (.ID. = max(0, allCandidates$.ID.) + 
                                  1:nrow(newCandidates), newCandidates)
          testCandidates <- rbind(eliteCandidates[, 1:ncol(allCandidates)],
                                  newCandidates)
          rownames(testCandidates) <- testCandidates$.ID.
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
      irace.note("Launch race\n")
    }

    .irace$next.instance <- max(tunerResults$experiments$instance, 0) + 1
    raceResults <- oneIterationRace (tunerConfig = tunerConfig,
                                     candidates = testCandidates,
                                     parameters = parameters, 
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
    # FIXME: Since we only actually keep the alive ones, we don't need
    # to carry around rejected ones in raceResults$candidates. This
    # would reduce overhead.
    eliteCandidates <- extractElites(raceResults$candidates,
                                     min(raceResults$nbAlive, minSurvival))
    cat("# Elite candidates:\n")
    candidates.print(eliteCandidates, metadata = debugLevel >= 1)
    tunerResults$iterationElites <- c(tunerResults$iterationElites, eliteCandidates$.ID.[1])
    tunerResults$allElites[[indexIteration]] <- eliteCandidates$.ID.
    
    if (indexIteration == 1) {
      if (debugLevel >= 1)  { cat("# Initialise model\n") }
      model <- initialiseModel(parameters, eliteCandidates)
    }
      
    if (debugLevel >= 1) { cat("# End of iteration ", indexIteration, "\n") }

    if (debugLevel >= 3) {
      cat("# All candidates:\n")
      candidates.print(allCandidates, metadata = TRUE)
    }

    indexIteration <- indexIteration + 1
    if (tunerConfig$debugLevel >= 3) {
      irace.note ("Memory used in irace():\n")
      irace.print.memUsed()
    }
  }
  # This code is actually never executed.
  return (eliteCandidates)
}
