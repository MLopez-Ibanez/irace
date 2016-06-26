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
# FIXME: Restoring occurs after reading the command-line/scenario file. At
# least for the irace command-line parameters (scenario), it should occur
# before. We would need to:
#
# 1) Read recovery file settings from command-line/scenario file
#
# 2) if set, then recover irace scenario

# 3) then read other settings from command-line/scenario file being
# careful to not override whatever the recovery has set.
#
# A work-around is to modify the recovery file (you can load it in R,
# modify scenario then save it again).
recoverFromFile <- function(filename)
{
  # substitute() is needed to evaluate filename here.
  eval.parent(substitute({
    # This restores iraceResults, thus it doesn't need restoring.
    load (filename)
    # .Random.seed and .irace are special
    for (name in setdiff(names(iraceResults$state), c(".Random.seed", ".irace")))
      assign(name, iraceResults$state[[name]])
    assign(".Random.seed", iraceResults$state$.Random.seed, .GlobalEnv)
    for (name in ls(iraceResults$state$.irace))
      assign(name, get(name, envir=iraceResults$state$.irace), envir=.irace)
    # These variables are not state, but they are used directly by irace.
    for (name in c("parameters", "allConfigurations"))
      assign(name, iraceResults[[name]])
    # Restore part of scenario but not all.
    for (name in .irace.params.recover)
      scenario[[name]] <- iraceResults$scenario[[name]]
    options(.race.debug.level = scenario$debugLevel)
    options(.irace.debug.level = scenario$debugLevel)
  }))
}

##
## Numerical configurations similarity function
##
numeric.configurations.equal <- function(x, configurations, parameters, threshold, param.names)
{
  d <- rep(0.0, nrow(configurations))
  bmat <- matrix(TRUE, nrow=nrow(configurations),ncol=length(param.names))
  selected <- 1:nrow(configurations)
  for (i in seq_along(param.names)) {
    param <- param.names[i]
    lower <- paramLowerBound(param, parameters)
    upper <- paramUpperBound(param, parameters)
 
    X <- x[[param]]
    y <- configurations[, param]
    for (j in seq_len(nrow(bmat))) { # Configurations loop
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
    similar <- c(x[[".ID."]], configurations[selected,".ID."])
  
  return(similar)
}

##
## Identify which configurations are similar.
##
# FIXME: It would be nice to print the minimum similarity found to the user.
similarConfigurations <- function(configurations, parameters, threshold)
{
  debug.level <- getOption(".irace.debug.level", 0)
  if (debug.level >= 1) cat ("# Computing similarity of configurations .")

  # Create vectors of categorical and numerical
  p <- parameters$types %in% c("c","o")
  vecCat <- parameters$names[p & !parameters$isFixed]
  vecNum <- parameters$names[!p & !parameters$isFixed]

  irace.assert(all(parameters$types[vecCat] %in% c("c","o")))
  irace.assert(all(!(parameters$types[vecNum] %in% c("c","o"))))
  irace.assert(length(intersect(vecCat, vecNum)) == 0)

  nbCater <- length(vecCat)
  nbNumer <- length(vecNum)

  ### Categorical/Ordinal filtering ####
  if (nbCater > 0) {
    ## Build a vector with the categorical appended together in a string
    ## FIXME: This would be faster as:
    # strings <- apply(configurations[, vecCat], 1, paste0, collapse = " ; ")
    strings <- c()
    for (i in 1:nrow(configurations)) {
      strings[i] <- paste0(configurations[i, vecCat], collapse = " ; ")
    }

    if (nbNumer != 0) configurations <- configurations[, c(".ID.", vecNum)]
    ord.strings <- order(strings)
    configurations <- configurations[ord.strings, ]
    strings <- strings[ord.strings]

    ## keep similar (index i == true means is the same as i + 1)
    similarIdx <- strings[-length(strings)] == strings[-1]
    
    ## Now let's get just a FALSE if we remove it, TRUE otherwise:
    keepIdx <- c(similarIdx[1],
                 (similarIdx[-1] | similarIdx[-length(similarIdx)]),
                 similarIdx[length(similarIdx)])
    
    ## filtering them out:
    configurations <- configurations [keepIdx, , drop=FALSE]
    ## filtering their strings out (to use them to define blocks):
    strings <- strings [keepIdx]
    
    ## if everything is already filtered out, return
    if (nrow(configurations) == 0) {
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
      while (beginBlock < nrow(configurations)) {
        ## The current block is made of all configurations that have the same
        ## categorical string as the one of configuration[beginBlock, ]
        blockIds <- which(strings == strings[beginBlock])
        endBlock <- blockIds[length(blockIds)]

        irace.assert (endBlock > beginBlock)
        ## Loop inside blocks:
        for (i in seq(beginBlock, endBlock - 1)) {
          ## Compare configuration i with all the ones that are after in the block
          similar <- c(similar,
                       numeric.configurations.equal(configurations[i, ], configurations[(i+1):endBlock,],
                                                parameters, threshold = threshold, param.names = vecNum))
          if (debug.level >= 1) cat(".")
        }
        beginBlock <- endBlock + 1 # Next block starts after the end of the current one
      }
    } else {
      ## No categorical, so no blocks, just do the basic check without blocks
      for (i in seq_len(nrow(configurations) - 1)) {
        similar <- c(similar,
                     numeric.configurations.equal(configurations[i, ], configurations[(i+1):nrow(configurations),],
                                              parameters, threshold = threshold, param.names = vecNum))
        if (debug.level >= 1) cat(".")
      }
    }
    similar <- unique(similar)
    configurations <- configurations[configurations[, ".ID."] %in% similar,]   
  }
  
  if (debug.level >= 1) cat(" DONE\n")
  if (nrow(configurations) == 0) {
    return (NULL)
  } else {
    return(configurations[,".ID."])
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
  return (floor (remainingBudget / (nbIterations - indexIteration + 1)))
}

## The number of configurations
computeNbConfigurations <- function(currentBudget, indexIteration, firstTest, eachTest,
                                nElites = 0, nOldInstances = 0, newInstances = 0)
{
  # FIXME: This is slightly incorrect, because we may have elites that have not
  # been executed on all nOldInstances. Thus, we need to pass explicitly the
  # budget that we save (that is, number of entries that are not NA).
  savedBudget <- nElites *  nOldInstances
  n <- max (firstTest + eachTest * min(5, indexIteration),
            round.to.next.multiple(nOldInstances + newInstances, eachTest))
    return (floor ((currentBudget + savedBudget) / n))
 
}

## Termination of a race at each iteration. The race will stop if the
## number of surviving configurations is equal or less than this number.
computeTerminationOfRace <- function(nbParameters)
{
  return (2 + log2(nbParameters))
}

## Compute the minimum budget required, and exit early in case the
## budget given by the user is insufficient.
checkMinimumBudget <- function(remainingBudget, minSurvival, nbIterations,
                               control)
{
  eachTest <- control$eachTest
  Tnew <- control$elitistInstances
  mu <- max(control$mu, control$firstTest)
  
  # This is computed from the default formulas as follows:
  #  B_1 = B / I
  #  B_2 = B -  (B/I) / (I - 1) = B / I
  #  B_3 = B - 2(B/I) / (I - 2) = B / I
  # thus
  #  B_i = B / I
  # and
  #  C_i = B_i / T_i = B / (I * T_i).
  #
  # We want to enforce that C_i >= min_surv + 1, thus
  #  B / (I * T_i) >= min_surv + 1                             (1)
  # becomes
  #  B >= (min_surv + 1) * I * T_i
  #
  # This is an over-estimation, since actually B_1 = floor(B/I) and if
  # floor(B/I) < B/I, then B_i < B/I, and we could still satisfy Eq. (1)
  # with a smaller budget. However, the exact formula requires computing B_i
  # taking into account the floor() function, which is not obvious.

  minimumBudget <- (minSurvival + 1) * nbIterations 

  # We need to compute T_i:
  if (control$elitist) {
    # T_i = max(mu + Teach * min (5, i),
    #           ceiling((T_{i-1} + Tnew) / Teach) * Teach)
    # T_1 = mu + Teach
    # T_2 ~ mu + Teach + max (Teach, Tnew)
    # T_3 ~ max(mu + 3 * Teach,
    #           mu + Teach + max(Teach, Tnew) + T_new)

    #     = mu + Teach + max(Teach + max(Teach, Tnew), 2 * Tnew)

    # if Teach > Tnew then 2*Teach > 2*Tnew then max = 2*Teach
    # if Teach < Tnew then Teach + Tnew < 2*Tnew then max = 2*Tnew
    # hence: T_3 = mu + Teach + 2 * max(Teach, Tnew)

    # T_4 = max(mu + 4 * Teach,
    #           ceiling((mu + Teach + 2 * max(Teach, Tnew)) + Tnew) / Teach) * Teach)
    #     ~ mu + Teach + max(2 * Teach + max(Teach, Tnew), 3 * Tnew)
    #     = mu + Teach + 3 * max(Teach, Tnew)

    # T_i = mu + Teach + (i - 1) * max(Teach, Tnew)

    # T_6 = max (mu + 5*Teach,
    #            mu + Teach + 5 * max(Teach, Tnew) + Tnew)
    #      = mu + Teach + Tnew + 5 * max (Teach, Tnew)

    # T_i = mu + Teach + max(I-5, 0) * Tnew + 5 * max (Teach, Tnew)

    if (nbIterations > 5) {
      minimumBudget <- minimumBudget *
        (mu + eachTest + (nbIterations - 5) * Tnew +  5 * max(eachTest, Tnew))
    } else {
      minimumBudget <- minimumBudget *
        (mu + eachTest + (nbIterations - 1) * max(eachTest, Tnew))
    }
  } else {
    #   T_i = mu + T_each * min (5, i)
    # and the most strict value is for i >= 5, thus
    #   B >= (min_surv + 1) * I * (mu + 5 * T_each)
    minimumBudget <- minimumBudget * (mu + 5 * eachTest)
  }
     
  if (remainingBudget < minimumBudget) {
    irace.error("Insufficient budget: ",
                "With the current settings, irace will require a value of ",
                "'maxExperiments' of at least '",  minimumBudget, "'. ",
                "You can either increase the budget, ",
                "or set a smaller value of either 'minNbSurvival' ",
                "or 'nbIterations'")
  }
}

# This function is the interface between race and irace. It first
# converts all data structures used in irace to the ones expected by
# race, it calls race, and then conversely converts the resulting data
# into the proper data structures for irace.
oneIterationRace <-
  function(scenario, configurations, parameters, budget, minSurvival, elite.data=NULL)
{
  # LESLIE : all this scenario parameters passed to race? why not to take them
  # from the scenario inside of race, since is also passed.
  result <- race (maxExp = budget,
                  stop.min.conf = minSurvival,
                  elite.data = elite.data,
                  configurations = configurations,
                  parameters = parameters,
                  scenario = scenario)

  configurations$.ALIVE. <- as.logical(result$alive)
  # Assign the proper ranks in the configurations data.frame
  configurations$.RANK. <- Inf
  configurations[which(result$alive), ".RANK."] <- result$ranks
  # Now we can sort the data.frame by the rank
  configurations <- configurations[order(as.numeric(configurations[, ".RANK."])), ]

  # Consistency check
  irace.assert (all(as.logical(configurations[1:(result$no.alive), ".ALIVE."])))
  if (result$no.alive < nrow(configurations))
    irace.assert(!any(as.logical(configurations[(result$no.alive + 1):nrow(configurations) , ".ALIVE."])))

  if (scenario$debugLevel >= 3) {
    irace.note ("Memory used in oneIterationRace():\n")
    irace.print.memUsed()
  }
  
  return (list (nbAlive = result$no.alive,
                experimentsUsed = result$no.experiments,
                configurations = configurations,
                experiments = result$results,
                experimentLog = result$experimentLog))
}

startParallel <- function(scenario)
{
  cwd <- setwd (scenario$execDir)
  on.exit(setwd(cwd), add = TRUE)

  parallel <- scenario$parallel
  if (parallel > 1) {
    if (scenario$mpi) {
      mpiInit(parallel, scenario$debugLevel)
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

irace.init <- function(scenario)
{
  # We need to do this here to use/recover .Random.seed later.
  if (is.na(scenario$seed)) {
    scenario$seed <- trunc(runif(1, 1, .Machine$integer.max))
  }
  set.seed(scenario$seed)

  ## FIXME: It would be much better to generate instances at the start of each
  ## race if we need them at all.
  # Generate instance + seed list 
  scenario$instancesList <- generateInstances(scenario)
  return(scenario)
}

## FIXME: Move this to the irace.Rd file
#' High-level function to use iterated Race
#' 
#' This function implement iterated Race. It receives some parameters to be tuned and returns the best
#' configurations found, namely, the elite configurations obtained from the last iterations (and sorted by rank).
#'
#' @param parameter data-structure containing the parameter definition. The data-structure has to be the one
#' returned by the function \code{readParameters()}. See documentation of this function for details.
#'
#' @param scenario data-structure containing irace settings.The data-structure has to be the one
#' returned by the function \code{readScenario()}. See documentation of this function for details.
#' @return Elites configurations obtained after the last iteration
#' @callGraphPrimitives
#' @note This is a note for the function \code{iteratedRace}
irace <- function(scenario = stop("parameter `scenario' is mandatory."),
                  parameters = stop("parameter `parameters' is mandatory."))
{
  catInfo <- function(..., verbose = TRUE) {
    irace.note (..., "\n")
    if (verbose) {
      cat ("# Iteration: ", indexIteration, "\n",
           "# nbIterations: ", nbIterations, "\n",
           "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
           "# remainingBudget: ", remainingBudget, "\n",
           "# currentBudget: ", currentBudget, "\n",
           "# number of elites: ", nrow(eliteConfigurations), "\n",
           "# nbConfigurations: ", nbConfigurations, "\n",
           sep = "")
    }
  }
  
  scenario <- checkScenario(defaultScenario(scenario))
  
  # Recover state from file?
  if (!is.null(scenario$recoveryFile)) {
    irace.note ("Resuming from file: '", scenario$recoveryFile,"'\n")
    recoverFromFile(scenario$recoveryFile)
  } else {
    scenario <- irace.init (scenario)
    debugLevel <- scenario$debugLevel
    # Set options controlling debug level.
    # FIXME: This should be the other way around, the options set the debugLevel.
    options(.race.debug.level = debugLevel)
    options(.irace.debug.level = debugLevel)
    
    # Create a data frame of all configurations ever generated.
    namesParameters <- names(parameters$conditions)

    if (!is.null(scenario$configurationsFile)
        && scenario$configurationsFile != "") {
      allConfigurations <- readConfigurationsFile(scenario$configurationsFile,
                                          parameters, debugLevel)
      allConfigurations <- cbind(.ID. = 1:nrow(allConfigurations),
                             allConfigurations,
                             .PARENT. = NA)
      rownames(allConfigurations) <- allConfigurations$.ID.
      num <- nrow(allConfigurations)
      allConfigurations <- checkForbidden(allConfigurations, scenario$forbiddenExps)
      if (nrow(allConfigurations) < num) {
        cat("# Warning: some of the configurations in the configurations file were forbidden and, thus, discarded\n")
      }
      cat("# ", num, " initial configuration(s) read from '",
          scenario$configurationsFile, "'\n", sep="")
    } else {
      configurations.colnames <- c(".ID.", namesParameters, ".PARENT.")
      allConfigurations <-
        as.data.frame(matrix(ncol = length(configurations.colnames),
                             nrow = 0,
                             dimnames = list(NULL, configurations.colnames)))
    }
    # To save the logs
    iraceResults <- list()
    iraceResults$scenario <- scenario
    iraceResults$irace.version <- irace.version
    iraceResults$parameters <- parameters
    iraceResults$iterationElites <- NULL
    iraceResults$allElites <- list()
    iraceResults$experiments <- matrix(nrow = 0, ncol = 0)
    iraceResults$experimentLog <- matrix(nrow = 0, ncol = 3,
                                         dimnames = list(NULL,
                                           c("iteration", "instance", "configuration")))
    model <- NULL
    nbConfigurations <- 0
    eliteConfigurations <- data.frame()
    
    nbIterations <- ifelse (scenario$nbIterations == 0,
                            computeNbIterations(parameters$nbVariable),
                            scenario$nbIterations)
    nbIterations <- floor(nbIterations)
    
    minSurvival <- ifelse (scenario$minNbSurvival == 0,
                           computeTerminationOfRace(parameters$nbVariable),
                           scenario$minNbSurvival)
    minSurvival <- floor(minSurvival)

    indexIteration <- 1
    experimentsUsedSoFar <- 0

    # Compute the total initial budget, that is, the maximum number of
    # experiments that we can perform.
    remainingBudget <- scenario$maxExperiments

    currentBudget <-
      ifelse (scenario$nbExperimentsPerIteration == 0,
              computeComputationalBudget(remainingBudget, indexIteration,
                                         nbIterations),
              scenario$nbExperimentsPerIteration)

    checkMinimumBudget (remainingBudget, minSurvival, nbIterations,
                        control = scenario)
  }
  if (scenario$elitist) {
    catInfo("Elitist race\n",
            "# Elitist instances: ", scenario$elitistInstances, "\n",
            "# Elitist limit: ",     scenario$elitistLimit, "\n", 
            verbose = FALSE)
  }
  catInfo("Initialization\n", 
          "# nbIterations: ", nbIterations, "\n",
          "# minNbSurvival: ", minSurvival, "\n",
          "# nbParameters: ", parameters$nbVariable, "\n",
          "# seed: ", scenario$seed, "\n",
          "# confidence level: ", scenario$confidence, "\n",
          "# budget: ", remainingBudget, "\n",
          "# mu: ", max(scenario$mu, scenario$firstTest), "\n",
          "# deterministic: ", scenario$deterministic, "\n",
          verbose = FALSE)

  startParallel(scenario)
  on.exit(stopParallel())

  while (TRUE) {
    # Recovery info 
    iraceResults$state <- list(.Random.seed = get(".Random.seed", .GlobalEnv),
                               .irace = .irace,
                               currentBudget = currentBudget,
                               debugLevel = debugLevel,
                               eliteConfigurations = eliteConfigurations,
                               experimentsUsedSoFar = experimentsUsedSoFar,
                               indexIteration = indexIteration,
                               minSurvival = minSurvival,
                               model = model,
                               nbConfigurations = nbConfigurations,
                               nbIterations = nbIterations,
                               remainingBudget = remainingBudget)
    
    ## Save to the log file
    iraceResults$allConfigurations <- allConfigurations
    if (!is.null.or.empty(scenario$logFile)) {
      cwd <- setwd(scenario$execDir)
      save (iraceResults, file = scenario$logFile)
      setwd(cwd)
    }

    if (remainingBudget <= 0) {
      catInfo("Stopped because budget is exhausted")
      return (eliteConfigurations)
    }

    if (indexIteration > nbIterations) {
      if (scenario$nbIterations == 0) {
        nbIterations <- indexIteration
      } else {
        if (debugLevel >= 1) {
          catInfo("Limit of iterations reached", verbose = FALSE)
        }
        return (eliteConfigurations)
      }
    }
    # Compute the current budget (nb of experiments for this iteration),
    # or take the value given as parameter.
    currentBudget <-
      ifelse (scenario$nbExperimentsPerIteration == 0,
              computeComputationalBudget(remainingBudget, indexIteration,
                                         nbIterations),
              scenario$nbExperimentsPerIteration)
    
    # Compute the number of configuration configurations for this race.
    if (scenario$elitist && indexIteration > 1) {
      nOldInstances <- nrow(iraceResults$experiments)
      nbConfigurations <-
        computeNbConfigurations(currentBudget, indexIteration,
                            firstTest = max(scenario$mu, scenario$firstTest),
                            eachTest = scenario$eachTest,
                            nElites = nrow(eliteConfigurations),
                            nOldInstances = nOldInstances,
                            newInstances = scenario$elitistInstances)
    } else {
      nbConfigurations <-
        computeNbConfigurations(currentBudget, indexIteration,
                            firstTest = max(scenario$mu, scenario$firstTest),
                            eachTest = scenario$eachTest,
                            nElites = 0, nOldInstances = 0,
                            newInstances = 0)
    }

    # If a value was given as a parameter, then this value limits the maximum,
    # but if we have budget only for less than this, then we have run out of
    # budget.
    if (scenario$nbConfigurations > 0) {
      if (scenario$nbConfigurations <= nbConfigurations) {
        nbConfigurations <- scenario$nbConfigurations
      } else if (currentBudget < remainingBudget) {
        # We skip one iteration
        indexIteration <- indexIteration + 1
        next
      } else {
        catInfo("Stopped because ",
                "there is no enough budget to enforce the value of nbConfigurations")
        return (eliteConfigurations)
      }
    }

    # Stop if the number of configurations to test is NOT larger than the minimum.
    if (nbConfigurations <= minSurvival) {
      catInfo("Stopped because there is no enough budget left to race more than ",
              "the minimum (", minSurvival,")\n",
              "# You may either increase the budget or set 'minNbSurvival' to a lower value")
      return (eliteConfigurations)
    }

    # Stop if  the number of configurations to produce is not greater than
    # the number of elites.
    if (nbConfigurations <= nrow(eliteConfigurations)) {
      catInfo("Stopped because ",
              "there is no enough budget left to race newly sampled configurations")
      #(number of elites  + 1) * (mu + min(5, indexIteration)) > remainingBudget" 
      return (eliteConfigurations)
    }

    if (nbConfigurations * max(scenario$mu, scenario$firstTest)
        > currentBudget) {
      catInfo("Stopped because there is no enough budget left to race all configurations")
      return (eliteConfigurations)
    }

    catInfo("Iteration ", indexIteration, " of ", nbIterations, "\n",
            "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
            "# remainingBudget: ", remainingBudget, "\n",
            "# currentBudget: ", currentBudget, "\n",
            "# nbConfigurations: ", nbConfigurations,
            verbose = FALSE)
            
    iraceResults$softRestart[indexIteration] <- FALSE
    # Sample for the first time.
    if (nrow(eliteConfigurations) == 0) {
      # If we need more configurations, sample uniformly.
      nbNewConfigurations <- nbConfigurations - nrow(allConfigurations)
      if (nbNewConfigurations > 0) {
        # Sample new configurations.
        if (debugLevel >= 1) {
          catInfo("Sample ", nbNewConfigurations,
                  " configurations from uniform distribution", verbose = FALSE)
        }
        newConfigurations <- sampleUniform(parameters, nbNewConfigurations,
                                           digits = scenario$digits,
                                           forbidden = scenario$forbiddenExps)
        newConfigurations <-
          cbind (.ID. = max(0, allConfigurations$.ID.) + 1:nrow(newConfigurations),
                 newConfigurations)
        allConfigurations <- rbind(allConfigurations, newConfigurations)
        rownames(allConfigurations) <- allConfigurations$.ID.
      } else if (nbNewConfigurations < 0) {
        # We also truncate allConfigurations in case there were too many
        # initial configurations.
        catInfo("Only ", nbConfigurations,
                " from configurations file will be used, the rest are discarded",
                verbose = FALSE)
        allConfigurations <- allConfigurations[1:nbConfigurations,]
      }
      testConfigurations <- allConfigurations
    } else {
      # How many new configurations should be sampled?
      nbNewConfigurations <- nbConfigurations - nrow(eliteConfigurations)

      # Update the model based on elites configurations
      if (debugLevel >= 1) { irace.note("Update model\n") }
      model <- updateModel(parameters, eliteConfigurations, model, indexIteration,
                           nbIterations, nbNewConfigurations, scenario)
      if (debugLevel >= 2) { printModel (model) }
      
      if (debugLevel >= 1) {
        irace.note("Sample ", nbNewConfigurations, " configurations from model\n")
      }

      #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel()\n")
      newConfigurations <- sampleModel(parameters, eliteConfigurations,
                                   model, nbNewConfigurations,
                                   digits = scenario$digits,
                                   forbidden = scenario$forbiddenExps)
      #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel() DONE\n")
      # Set ID of the new configurations.
      newConfigurations <- cbind (.ID. = max(0, allConfigurations$.ID.) +
                              1:nrow(newConfigurations), newConfigurations)
      testConfigurations <- rbind(eliteConfigurations[, 1:ncol(allConfigurations)],
                              newConfigurations)
      rownames(testConfigurations) <- testConfigurations$.ID.

      if (scenario$softRestart) {
        #          Rprof("profile.out")
        tmp.ids <- similarConfigurations (testConfigurations, parameters,
                                      threshold = scenario$softRestartThreshold)
        #          Rprof(NULL)
        if (!is.null(tmp.ids)) {
          if (debugLevel >= 1)
            irace.note("Soft restart: ", paste(collapse = " ", tmp.ids), " !\n")
          model <- restartConfigurations (testConfigurations, tmp.ids, model,
                                      parameters, nbNewConfigurations)
          iraceResults$softRestart[indexIteration] <- TRUE
          iraceResults$model$afterSR[[indexIteration]] <- model
          if (debugLevel >= 2) { printModel (model) }
          # Re-sample after restart like above
          #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel()\n")
          newConfigurations <- sampleModel(parameters, eliteConfigurations,
                                       model, nbNewConfigurations,
                                       digits = scenario$digits,
                                       forbidden = scenario$forbiddenExps)
          #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel() DONE\n")
          # Set ID of the new configurations.
          newConfigurations <- cbind (.ID. = max(0, allConfigurations$.ID.) + 
                                  1:nrow(newConfigurations), newConfigurations)
          testConfigurations <- rbind(eliteConfigurations[, 1:ncol(allConfigurations)],
                                  newConfigurations)
          rownames(testConfigurations) <- testConfigurations$.ID.
        }
      }

      # Append these configurations to the global table.
      allConfigurations <- rbind(allConfigurations, newConfigurations)
      rownames(allConfigurations) <- allConfigurations$.ID.
    }

    if (debugLevel >= 2) {
      irace.note("Configurations for the race n ", indexIteration, ":\n")
      configurations.print(testConfigurations, metadata = TRUE)
    }

    if (debugLevel >= 1) {
      irace.note("Launch race\n")
    }

    # Get data from previous elite tests 
    elite.data <- NULL
    if (scenario$elitist && indexIteration > 1) {
      elite.data <- iraceResults$experiments[, as.character(eliteConfigurations[,".ID."]), drop=FALSE]
    }
    
    .irace$next.instance <- max(nrow(iraceResults$experiments), 0) + 1
    raceResults <- oneIterationRace (scenario = scenario,
                                     configurations = testConfigurations,
                                     parameters = parameters, 
                                     budget = currentBudget, 
                                     minSurvival = minSurvival,
                                     elite.data = elite.data)

    # Update experiments
    # LESLIE: Maybe we can think is make iraceResults an environment, so these values
    # can be updated in the race function.

    # We add indexIteration as an additional column.
    iraceResults$experimentLog <- rbind(iraceResults$experimentLog,
                                        cbind(rep(indexIteration, nrow(raceResults$experimentLog)), raceResults$experimentLog))
    
    # Merge new results
    iraceResults$experiments <- merge.matrix (iraceResults$experiments,
                                              raceResults$experiments)
    
    experimentsUsedSoFar <- experimentsUsedSoFar + raceResults$experimentsUsed
    remainingBudget <- remainingBudget - raceResults$experimentsUsed

    if (debugLevel >= 3) {
      irace.note("Results for the race n ", indexIteration, ":\n")
      configurations.print (raceResults$configurations, metadata=TRUE)
    }

    if (debugLevel >= 1) { irace.note("Extract elites\n") }
    # FIXME: Since we only actually keep the alive ones, we don't need
    # to carry around rejected ones in raceResults$configurations. This
    # would reduce overhead.
    eliteConfigurations <- extractElites(raceResults$configurations,
                                         min(raceResults$nbAlive, minSurvival))
    irace.note("Elite configurations:\n")
    configurations.print(eliteConfigurations, metadata = debugLevel >= 1)
    iraceResults$iterationElites <- c(iraceResults$iterationElites, eliteConfigurations$.ID.[1])
    iraceResults$allElites[[indexIteration]] <- eliteConfigurations$.ID.
    
    if (indexIteration == 1) {
      if (debugLevel >= 1)  { irace.note("Initialise model\n") }
      model <- initialiseModel(parameters, eliteConfigurations)
    }
      
    if (debugLevel >= 1) {
      irace.note("End of iteration ", indexIteration, "\n")
    }

    if (debugLevel >= 3) {
      irace.note("All configurations:\n")
      configurations.print(allConfigurations, metadata = TRUE)
    }

    indexIteration <- indexIteration + 1
    if (scenario$debugLevel >= 3) {
      irace.note ("Memory used in irace():\n")
      irace.print.memUsed()
    }
  }
  # This code is actually never executed because we return above.
  return (eliteConfigurations)
}
