# FIXME: It may be faster to create a single expression that concatenates all
# the elements of forbidden using '|'
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
      assign(name, get(name, envir = iraceResults$state$.irace), envir = .irace)
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
                               scenario)
{
  eachTest <- scenario$eachTest
  Tnew <- scenario$elitistNewInstances
  mu <- max(scenario$mu, scenario$firstTest)
  
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
  if (scenario$elitist) {
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
    if (scenario$maxTime == 0 || nbIterations == 1)
      irace.error("Insufficient budget: ",
                  "With the current settings, irace will require a value of ",
                  "'maxExperiments' of at least '",  minimumBudget, "'. ",
                  "You can either increase the budget, ",
                  "or set a smaller value of either 'minNbSurvival' ",
                  "or 'nbIterations'")
    return(FALSE)
  }
  return(TRUE)
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
  return(scenario)
}

## Generate instances + seed.
generateInstances <- function(scenario, remainingBudget)
{
  instances <- scenario$instances
  ntimes <- if (scenario$deterministic) 1 else
            # "Upper bound"" of instances needed
            # FIXME: We could bound it even further if maxExperiments >> nInstances
            ceiling (remainingBudget / length(instances))

  # Get instances order
  if (scenario$sampleInstances) {
    # Sample instances index in groups (ntimes)
    sindex <- as.vector(sapply(rep(length(instances), ntimes), sample.int, replace = FALSE))
  } else {
    sindex <- rep(1:length(instances), ntimes)
  }
  # Sample seeds.
  # 2147483647 is the maximum value for a 32-bit signed integer.
  # We use replace = TRUE, because replace = FALSE allocates memory for each possible number.
  tmp <- data.frame (instance = sindex,
                     seed = sample.int(2147483647, size = ntimes * length(instances), replace = TRUE))
  return(tmp)
}

addInstances <- function(scenario, instancesList, n.instances)
{
  # Generate instance + seed list 
  if (is.null.or.empty(instancesList))
    instancesList <- generateInstances(scenario, n.instances)
  # If deterministic, we have already added all instances.
  else if (! scenario$deterministic)
    instancesList <- rbind(instancesList, generateInstances(scenario, n.instances))

  # FIXME: Something is adding rownames. Clear them to avoid future problems.
  rownames(instancesList) <- NULL
  return(instancesList)
}

## Estimate the mean execution time
do.experiments <- function(configurations, ninstances, scenario, parameters)
{
  output <- lapply(1:ninstances, race.wrapper, configurations = configurations, 
                   which.alive = 1:nrow(configurations), which.exe = 1:nrow(configurations), 
                   parameters = parameters, scenario = scenario)
                                        
  Results <- matrix(nrow = ninstances, ncol = nrow(configurations),
                    dimnames = list(1:ninstances, as.character(configurations[, ".ID."])))
  experimentLog <- matrix(nrow = 0, ncol = 3,
                          dimnames = list(NULL, c("instance", "configuration", "time")))
                          
  # Extract results
  ## FIXME: There must be a faster way to do this.
  for (j in 1:ninstances) {
    for (i in 1:nrow(configurations)) {
      Results[j, i] <- output[[j]][[i]]$cost
      output.time   <- output[[j]][[i]]$time
      irace.assert(!is.null(output.time))
      ## FIXME: It would be more efficient to build three vectors and cbind them once as:
      # experimentLog <- cbind(instance = rep(1:ninstances, nrow(configurations)), configuration = configurations[, ".ID."], time = output.time)
      experimentLog <- rbind(experimentLog,
                             c(j, configurations[i, ".ID."], output.time))
    }
  }
  
  return (list(experiments = Results, experimentLog = experimentLog))
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
irace <- function(scenario, parameters)
{
  catInfo <- function(..., verbose = TRUE) {
    irace.note (..., "\n")
    if (verbose) {
      cat ("# Iteration: ", indexIteration, "\n",
           "# nbIterations: ", nbIterations, "\n",
           "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
           "# timeUsed: ", timeUsed, "\n",
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
    startParallel(scenario)
    on.exit(stopParallel())
  } else { # Do not recover
    scenario <- irace.init (scenario)
    debugLevel <- scenario$debugLevel
    # Set options controlling debug level.
    # FIXME: This should be the other way around, the options set the debugLevel.
    options(.race.debug.level = debugLevel)
    options(.irace.debug.level = debugLevel)
    
    # Create a data frame of all configurations ever generated.
    namesParameters <- names(parameters$conditions)

    nbUserConfigurations <- 0
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
        cat("# Warning: some of the configurations in the configurations file were forbidden",
            "and, thus, discarded\n")
      }
      cat("# Adding", nrow(allConfigurations), "initial configuration(s) from file",
          shQuote(scenario$configurationsFile), "\n")
      nbUserConfigurations <- nrow(allConfigurations)
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
    iraceResults$experimentLog <- matrix(nrow = 0, ncol = 4,
                                         dimnames = list(NULL,
                                           c("iteration", "instance", "configuration", "time")))

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
    
    # Generate initial instance + seed list
    # LESLIE: This list should go in a variable where all the variables of the race are stored, 
    # the scenario should not have variables that are random no?
    .irace$instancesList <- addInstances(scenario, NULL, 
                                           if (scenario$maxExperiments != 0)
                                             ceiling(scenario$maxExperiments / minSurvival)
                                           else
                                             max (scenario$firstTest, length(scenario$instances)))

    indexIteration <- 1
    experimentsUsedSoFar <- 0
    timeUsed <- 0
    timeEstimate <- NA 

    startParallel(scenario)
    on.exit(stopParallel())                                          
 
    if (scenario$maxTime == 0) {
      remainingBudget <- scenario$maxExperiments
    } else { ## Estimate time when maxTime is defined.
      # Get the number of instances to be used.
      ## IMPORTANT: This is firstTest because these configurations will be
      ## considered elite later, thus preserved up to firstTest, which is
      ## fine. If a larger number of instances is used, it would prevent
      ## discarding these configurations.
      ninstances <- scenario$firstTest
      estimationTime <- ceiling(scenario$maxTime * scenario$budgetEstimation)
      irace.note("Estimating execution time using ", 100 * scenario$budgetEstimation,
                 "% of ", scenario$maxTime, " = ", estimationTime, "\n")

      # Estimate the number of configurations to be used
      nconfigurations <- max(2, floor(scenario$parallel/ninstances))
      
      next.configuration <- 1
      while (TRUE) {
        # Sample new configurations if needed
        if (nrow(allConfigurations) < nconfigurations) {
          newConfigurations <- sampleUniform(parameters,
                                             nconfigurations - nrow(allConfigurations),
                                             digits = scenario$digits,
                                             forbidden = scenario$forbiddenExps)
          newConfigurations <-
            cbind (.ID. = max(0, allConfigurations$.ID.) + 1:nrow(newConfigurations),
                   newConfigurations)
          allConfigurations <- rbind(allConfigurations, newConfigurations) 
          rownames(allConfigurations) <- allConfigurations$.ID.
        }
        
        # Execute tests
        output <- do.experiments(configurations = allConfigurations[next.configuration:nconfigurations, ],
                                 ninstances = ninstances, scenario = scenario, parameters = parameters)  

        iraceResults$experimentLog <- rbind(iraceResults$experimentLog,
                                            cbind(rep(0, nrow(output$experimentLog)),
                                                  output$experimentLog)) 

        timeUsed     <- sum(timeUsed, output$experimentLog[, "time"], na.rm = TRUE)
        timeEstimate <- mean(iraceResults$experimentLog[, "time"], na.rm = TRUE)   
        
        iraceResults$experiments <- merge.matrix (iraceResults$experiments,
                                                  output$experiments)
        rownames(iraceResults$experiments) <- 1:nrow(iraceResults$experiments)
        
        next.configuration <- nconfigurations + 1
        
        # Calculate how many new configurations:
        # 1. We do not want to overrun estimationTime
        new.conf <- floor(((estimationTime - timeUsed) / timeEstimate) / ninstances)
        # 2. But there is no point in executing more configurations than those
        # that we can execute in parallel.
        new.conf <- min(new.conf, max(1, floor(scenario$parallel / ninstances)))

        if (timeUsed >= estimationTime || new.conf == 0) {
          break
        } else {
          nconfigurations <- nconfigurations + new.conf
        }
      }
  
      irace.note("Estimated execution time is ", timeEstimate, " based on ",
                 next.configuration - 1, " configurations and ",
                 ninstances," instances. Used time: ", timeUsed, " .\n")
      
      # Update budget
      remainingBudget <- round((scenario$maxTime - timeUsed) / timeEstimate)

      experimentsUsedSoFar <- experimentsUsedSoFar + nrow(output$experimentLog)
      eliteConfigurations <- allConfigurations[1:(next.configuration - 1),]

      # Without elitist, the racing does not re-use the results computed during
      # the estimation.  This means that the time used during estimation needs
      # to be spent again during racing, thus leaving less time for racing.  We
      # want to avoid having less time for racing, and this is an
      # implementation detail, thus we assume that the time was not actually
      # wasted.
      if (!scenario$elitist) {
        timeUsed <- 0
      }
    } # end of do not recover

    # Compute the total initial budget, that is, the maximum number of
    # experiments that we can perform.

    currentBudget <-
      ifelse (scenario$nbExperimentsPerIteration == 0,
              computeComputationalBudget(remainingBudget, indexIteration,
                                         nbIterations),
              scenario$nbExperimentsPerIteration)

    # Check that the budget is enough, for the time estimation case we reduce
    # the number of iterations..
    repeat {
      if (checkMinimumBudget (remainingBudget, minSurvival, nbIterations,
                              scenario = scenario)
          || scenario$maxTime == 0) {
        break;
      }
      irace.note("Warning:",
                 " with the current settings and estimated time per run,",
                 " irace will not have enough budget to execute the minimum",
                 " number of iterations.",
                 " Execution will continue by assuming that the estimated time",
                 " is too high and reducing the minimum number of iterations,",
                 " however, if the estimation was correct or too low,",
                 " results might not be better than random sampling.")
      nbIterations <- nbIterations - 1
    }
  }
  
  if (scenario$elitist) {
    catInfo("Elitist race\n",
            "# Elitist new instances: ", scenario$elitistNewInstances, "\n",
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
          if (scenario$maxTime == 0) ""
          else paste0("# time budget: ", scenario$maxTime - timeUsed, "\n"),
          "# mu: ", max(scenario$mu, scenario$firstTest), "\n",
          "# deterministic: ", scenario$deterministic, "\n",
          verbose = FALSE)

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
                               remainingBudget = remainingBudget,
                               timeUsed = timeUsed,
                               timeEstimate = timeEstimate)
    
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
    if (scenario$maxTime > 0 && timeUsed >= scenario$maxTime) {
      catInfo("Stopped because time budget is exhausted")
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
    
    # Compute the number of configurations for this race.
    if (scenario$elitist && indexIteration > 1) {
      nOldInstances <- nrow(iraceResults$experiments)
      nbConfigurations <-
        computeNbConfigurations(currentBudget, indexIteration,
                            firstTest = max(scenario$mu, scenario$firstTest),
                            eachTest = scenario$eachTest,
                            nElites = nrow(eliteConfigurations),
                            nOldInstances = nOldInstances,
                            newInstances = scenario$elitistNewInstances)
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
                "there is not enough budget to enforce the value of nbConfigurations")
        return (eliteConfigurations)
      }
    }

    # Stop if the number of configurations to test is NOT larger than the minimum.
    if (nbConfigurations <= minSurvival) {
      catInfo("Stopped because there is not enough budget left to race more than ",
              "the minimum (", minSurvival,")\n",
              "# You may either increase the budget or set 'minNbSurvival' to a lower value")
      return (eliteConfigurations)
    }

    # Stop if  the number of configurations to produce is not greater than
    # the number of elites.
    if (nbConfigurations <= nrow(eliteConfigurations)) {
      catInfo("Stopped because ",
              "there is not enough budget left to race newly sampled configurations")
      #(number of elites  + 1) * (mu + min(5, indexIteration)) > remainingBudget" 
      return (eliteConfigurations)
    }

    if (scenario$elitist) {
      # The non-elite have to run up to the first test. The elites consume
      # budget at most up to the new instances.
      if ((nbConfigurations - nrow(eliteConfigurations)) * max(scenario$mu, scenario$firstTest)
          + nrow(eliteConfigurations) * min(scenario$elitistNewInstances, max(scenario$mu, scenario$firstTest))
          > currentBudget) {
        catInfo("Stopped because there is not enough budget left to race all configurations up to the first test (or mu)")
        return (eliteConfigurations)
      }
    } else if (nbConfigurations * max(scenario$mu, scenario$firstTest)
               > currentBudget) {
      catInfo("Stopped because there is not enough budget left to race all configurations up to the first test (or mu)")
      return (eliteConfigurations)
    }

    catInfo("Iteration ", indexIteration, " of ", nbIterations, "\n",
            "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
            if (scenario$maxTime == 0) ""
            else paste0("# timeUsed: ", timeUsed, "\n",
                        "# timeEstimate: ", timeEstimate, "\n"),
            "# remainingBudget: ", remainingBudget, "\n",
            "# currentBudget: ", currentBudget, "\n",
            "# nbConfigurations: ", nbConfigurations,
            verbose = FALSE)
            
    iraceResults$softRestart[indexIteration] <- FALSE
    # Sample for the first time.
    if (indexIteration == 1) {
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
        # We let the user know that not all configurations will be used
        if (nbUserConfigurations > nbConfigurations) {
          catInfo("Only ", nbConfigurations,
                  " from the configurations file will be used",
                  verbose = FALSE)
          # LESLIE: should we remove them in this case??
          # if (scenario$maxTime <= 0 && nbUserConfigurations > nbConfigurations)
          #   allConfigurations <- allConfigurations[1:nbConfigurations,]
        }
        
        # This is made only in case that the number of configurations used in the
        # time estimation is more than needed.
        if (nrow(eliteConfigurations) > nbConfigurations) {
          eliteConfigurations <- eliteConfigurations[1:nbConfigurations, ]
        }
        
      }
      testConfigurations <- allConfigurations[1:nbConfigurations,]
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
    if (scenario$elitist && nrow(eliteConfigurations) > 0) {
      elite.data <- iraceResults$experiments[, as.character(eliteConfigurations[,".ID."]), drop=FALSE]
    }
    
    .irace$next.instance <- max(nrow(iraceResults$experiments), 0) + 1
 
    # Add instances if needed
    # Calculate budget needed for old instances assuming non elitist irace
    if ((nrow(.irace$instancesList) - (.irace$next.instance - 1))
        < ceiling(remainingBudget / minSurvival)) {
      .irace$instancesList <- addInstances(scenario, .irace$instancesList,  ceiling(remainingBudget/minSurvival))
    }

    raceResults <- race (scenario = scenario,
                         configurations = testConfigurations,
                         parameters = parameters, 
                         maxExp = currentBudget, 
                         minSurvival = minSurvival,
                         elite.data = elite.data,
                         elitistNewInstances = if (indexIteration > 1)
                                                 scenario$elitistNewInstances
                                               else 0)

    # Update experiments
    # LESLIE: Maybe we can think is make iraceResults an environment, so these values
    # can be updated in the race function.

    # We add indexIteration as an additional column.
    iraceResults$experimentLog <- rbind(iraceResults$experimentLog,
                                        cbind(rep(indexIteration, nrow(raceResults$experimentLog)),
                                              raceResults$experimentLog))
    
    # Merge new results
    iraceResults$experiments <- merge.matrix (iraceResults$experiments,
                                              raceResults$experiments)
    
    experimentsUsedSoFar <- experimentsUsedSoFar + raceResults$experimentsUsed
    # Update remaining budget
    if (scenario$maxTime > 0) { 
      timeUsed <- sum (timeUsed, raceResults$experimentLog[,"time"], na.rm=TRUE)
      timeEstimate <- mean(iraceResults$experimentLog[,"time"], na.rm=TRUE)
      remainingBudget <- round((scenario$maxTime - timeUsed) / timeEstimate)
    } else {
      remainingBudget <- remainingBudget - raceResults$experimentsUsed
    }

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
