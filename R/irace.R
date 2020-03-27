# Sets irace variables from a recovery file.  It is executed in the
# parent environment which must be irace().
#
# FIXME: Restoring occurs after reading the command-line/scenario file. At
# least for the irace command-line parameters (scenario), it should occur
# before. We would need to:
#
# 1) Read recovery file settings from command-line/scenario file
#
# 2) if set, then recover irace scenario

# 3) then read other settings from command-line/scenario file being
# careful to not override with defaults whatever the recovery has set.
#
# 4) checkSchenario()
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
# FIXME: This function is too slow and it shows up in profiles.
numeric.configurations.equal <- function(x, configurations, parameters, threshold, param.names)
{
  d <- rep(0.0, nrow(configurations))
  isSimilar.mat <- matrix(TRUE, nrow = nrow(configurations), ncol = length(param.names))
  selected <- 1:nrow(configurations)
  for (i in seq_along(param.names)) {
    param <- param.names[i]
    lower <- parameters$domain[[param]][1]
    upper <- parameters$domain[[param]][2]
 
    X <- x[[param]]
    # FIXME: Since at the end we select a subset of configurations, we could use selected here.
    y <- configurations[, param]
    ## FIXME: This can probably done much faster by doing a matrix operation that updates
    ## isSimilar.mat[, i] in one step instead of the for-loop.
    ## We would need to handle the NAs first.
    for (j in seq_len(nrow(isSimilar.mat))) { # Configurations loop
      Y <- y[selected[j]]
      if (is.na (X) && is.na(Y)) { # Both NA, just ignore this param
        next
      } else if (xor(is.na (X), is.na(Y))) { # Distance is 1.0, so not equal
        isSimilar.mat[j,i] <- FALSE 
      } else {
        # FIXME: Why is this updating d[j]? It seems that if the difference is
        # large for one configuration, then it will be assumed to be large for
        # the rest.
        d[j] <- max(d[j], abs((as.numeric(X) - as.numeric(Y)) / (upper - lower)))
        if (d[j] > threshold) isSimilar.mat[j,i] <- FALSE
      }
    }
    index <- which(apply(isSimilar.mat,1,all))
    isSimilar.mat <- isSimilar.mat[index, , drop=FALSE]
    d <- d[index]
    selected  <- selected[index]
    if (nrow(isSimilar.mat) == 0) break
  }
  
  if (length(selected) != 0)
    return(c(x[[".ID."]], configurations[selected,".ID."]))
  return(NULL)
}

##
## Identify which configurations are similar.
##
# FIXME: It would be nice to print the minimum similarity found to the user.
similarConfigurations <- function(configurations, parameters, threshold)
{
  debug.level <- getOption(".irace.debug.level", 0)
  if (debug.level >= 1) irace.note ("Computing similarity of configurations .")

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
    strings <- do.call(paste, c(configurations[, vecCat, drop=FALSE], sep = " ; "))
    
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
    # FIXME: We have to use unique because we return the same configuration
    # more than once in different calls to numeric.configurations.equal.
    # Currently, we compare each configuration k=1...n with every configuration
    # k+1...n. Instead, we should compare k=1...n with ((k+1...n) notin
    # similar).  It may happen that A ~ B and A ~ C and B /= C, but this is OK
    # because we still return A, B and C. It may also happen that A ~ B, B ~ C
    # and A /= C, but this is also OK because we will compare A with B,C then B
    # with C.
    similar <- unique(similar)
    configurations <- configurations[configurations[, ".ID."] %in% similar, ]
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
                                    nElites = 0, nOldInstances = 0, newInstances = 0, 
                                    maxConfigurations = 1024)
{
  # FIXME: This is slightly incorrect, because we may have elites that have not
  # been executed on all nOldInstances. Thus, we need to pass explicitly the
  # budget that we save (that is, number of entries that are not NA).
  savedBudget <- nElites *  nOldInstances
  n <- max (firstTest + eachTest * min(5, indexIteration),
            round.to.next.multiple(nOldInstances + newInstances, eachTest))
  return (min (floor ((currentBudget + savedBudget) / n), maxConfigurations) )
}

## Termination of a race at each iteration. The race will stop if the
## number of surviving configurations is equal or less than this number.
computeTerminationOfRace <- function(nbParameters)
{
  return (2 + log2(nbParameters))
}

## Compute the minimum budget required, and exit early in case the
## budget given by the user is insufficient.
computeMinimumBudget <- function(scenario, minSurvival, nbIterations, boundEstimate)
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

  return(minimumBudget)
}

checkMinimumBudget <- function(scenario, remainingBudget, minSurvival, nbIterations,
                               boundEstimate, timeUsed)
{
  minimumBudget <- computeMinimumBudget (scenario, minSurvival, nbIterations, boundEstimate)

  if (remainingBudget < minimumBudget) {
    if (scenario$maxTime == 0) {
      irace.error("Insufficient budget: ",
                  "With the current settings, irace will require a value of ",
                  "'maxExperiments' of at least '",  minimumBudget, "'.")
    } else if (nbIterations == 1) {
      irace.error("Insufficient budget: ",
                  "With the current settings and estimated time per run (",
                  boundEstimate, ") irace will require a value of 'maxTime' of at least '",
                  (minimumBudget * boundEstimate) + timeUsed, "'.")
    }
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
      if (.Platform$OS.type == 'windows' && is.null(.irace$cluster)) {
        .irace$cluster <- parallel::makeCluster(parallel)
        # In Windows, this needs to be exported, or we get:
        ## Error in checkForRemoteErrors(val) : 
        ##  2 nodes produced errors; first error: could not find function "target.runner"
        parallel::clusterExport(.irace$cluster, list("target.runner"), envir=.irace)
        # In addition, we export the global environment because the user may
        # have defined stuff there. There must be a better way to do this, but
        # I cannot figure it out. R sucks sometimes.
        parallel::clusterExport(.irace$cluster, ls(envir=.GlobalEnv))
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
  if (scenario$debugLevel > 2) {
    irace.note("RNGkind: ", paste0(RNGkind(), collapse = " "), "\n")
    irace.note(".Random.seed: ", paste0(.Random.seed, collapse = ", "), "\n")
  }
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
                   bounds = rep(scenario$boundMax, nrow(configurations)),
                   which.alive = 1:nrow(configurations), which.exe = 1:nrow(configurations), 
                   parameters = parameters, scenario = scenario)
                                        
  Results <- matrix(nrow = ninstances, ncol = nrow(configurations),
                    dimnames = list(1:ninstances, as.character(configurations[, ".ID."])))
  experimentLog <- matrix(nrow = 0, ncol = 4,
                          dimnames = list(NULL, c("instance", "configuration", "time", "bound")))
                          
  # Extract results
  for (j in seq_len(ninstances)) {
    vcost <- unlist(lapply(output[[j]], "[[", "cost"))
    if (scenario$capping)
      vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
    Results[j, ] <- vcost
    vtimes <- unlist(lapply(output[[j]], "[[", "time"))
    irace.assert(!any(is.null(vtimes)))
    experimentLog <- rbind(experimentLog,
                           cbind(j, configurations$.ID., vtimes, 
                                 if(!is.null(scenario$boundMax)) scenario$boundMax else NA))
  }
  
  rejectedIDs <- configurations[apply(is.infinite(Results), 2, any), ".ID."]
  return (list(experiments = Results, experimentLog = experimentLog, rejectedIDs = rejectedIDs))
}

## Gets the elite configurations time matrix from the experiment log
generateTimeMatrix <- function(elites, experimentLog)
{
  is.elite <- experimentLog[,"configuration"] %in% elites$.ID.
  # Remove everything that we don't need.
  experimentLog <- experimentLog[is.elite, c("configuration", "instance", "time", "bound"), drop = FALSE]
  experimentLog[, "time"] <- pmin(experimentLog[,"time"], experimentLog[, "bound"])
  # FIXME: It would be better to use spread() from tidyr
  resultsTime <- reshape(as.data.frame(experimentLog), direction = "wide",
                         idvar = "instance", timevar = "configuration",
                         drop = "bound")
  rownames(resultsTime) <- resultsTime$instance
  resultsTime <- resultsTime[order(resultsTime$instance), , drop = FALSE]
  colnames(resultsTime) <- substring(colnames(resultsTime), nchar("time.") + 1)
  resultsTime <- as.matrix(resultsTime[, as.character(elites$.ID.), drop = FALSE])
  return(resultsTime)           
}

## Initialize allConfigurations with any initial configurations provided.
allConfigurationsInit <- function(scenario, parameters)
{
  initConfigurations <- scenario$initConfigurations
  
  confs_from_file <- NULL
  if (!is.null.or.empty(scenario$configurationsFile)) {
    confs_from_file <- readConfigurationsFile(scenario$configurationsFile,
                                              parameters, scenario$debugLevel)
  }
  if (!is.null.or.empty(initConfigurations)) {
    if (!identical(initConfigurations, confs_from_file))
      irace.warning("'initConfigurations' provided in 'scenario',",
                    " thus ignoring configurations from file '",
                    scenario$configurationsFile, "'.")
    cat("# Adding", nrow(initConfigurations), "initial configuration(s)\n")
    if (scenario$debugLevel >= 2)
      print(as.data.frame(scenario$initConfigurations, stringAsFactor = FALSE), digits=15)
  } else {
    initConfigurations <- confs_from_file
  }
    
  if (!is.null.or.empty(initConfigurations)) {
    allConfigurations <- initConfigurations
    allConfigurations <- cbind(.ID. = 1:nrow(allConfigurations),
                               allConfigurations, .PARENT. = NA)
    rownames(allConfigurations) <- allConfigurations$.ID.
    num <- nrow(allConfigurations)
    allConfigurations <- checkForbidden(allConfigurations, scenario$forbiddenExps)
    if (nrow(allConfigurations) < num) {
      irace.warning(num - nrow(allConfigurations), " of the ", num,
                    " initial configurations were forbidden",
                    " and, thus, discarded")
    }
  } else {
    configurations.colnames <- c(".ID.", names(parameters$conditions), ".PARENT.")
    allConfigurations <-
      as.data.frame(matrix(ncol = length(configurations.colnames),
                           nrow = 0,
                           dimnames = list(NULL, configurations.colnames)))
  }
  return(allConfigurations)
}

#' irace
#'
#' \code{irace} implements iterated Race. It receives some parameters to be tuned 
#'   and returns the best configurations found, namely, the elite configurations 
#'   obtained from the last iterations (and sorted by rank).
#' 
#' @template arg_scenario
#' @template arg_parameters
#'
#' @details The function \code{irace} executes the tuning procedure using 
#'  the information provided in \code{scenario} and \code{parameters}. Initially it checks 
#'  the correctness of \code{scenario} and recovers a previous execution if 
#'  \code{scenario$recoveryFile} is set. A R data file log of the execution is created 
#'  in \code{scenario$logFile}.
#'
#' @template return_irace
#' @examples
#' \dontrun{
#' parameters <- readParameters("parameters.txt")
#' scenario <- readScenario(filename = "scenario.txt",
#'                          scenario = defaultScenario())
#' irace(scenario = scenario, parameters = parameters)
#' }
#'
#' @seealso
#'  \describe{
#'  \item{\code{\link{irace.main}}}{a higher-level command-line interface to \code{irace}.}
#'  \item{\code{\link{readScenario}}}{for reading a configuration scenario from a file.}
#'  \item{\code{\link{readParameters}}}{read the target algorithm parameters from a file.}
#'  \item{\code{\link{defaultScenario}}}{returns the default scenario settings of \pkg{irace}.}
#'  \item{\code{\link{checkScenario}}}{to check that the scenario is valid.}
#' }
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
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
    # We call checkScenario again to fix any inconsistencies in the recovered
    # data.
    firstRace <- FALSE
    scenario <- checkScenario(scenario)
    startParallel(scenario)
    on.exit(stopParallel(), add = TRUE)

  } else { # Do not recover
    firstRace <- TRUE
    scenario <- irace.init (scenario)
    forbiddenExps <- scenario$forbiddenExps
    debugLevel <- scenario$debugLevel
    # Set options controlling debug level.
    # FIXME: This should be the other way around, the options set the debugLevel.
    options(.race.debug.level = debugLevel)
    options(.irace.debug.level = debugLevel)
    
    # Create a data frame of all configurations ever generated.
    allConfigurations <- allConfigurationsInit(scenario, parameters)
    nbUserConfigurations <- nrow(allConfigurations)
  
    # To save the logs
    iraceResults <- list(
      scenario = scenario,
      irace.version = irace.version,
      parameters = parameters,
      allElites = list(),
      experiments = matrix(nrow = 0, ncol = 0),
      experimentLog = matrix(nrow = 0, ncol = 5,
                              dimnames = list(NULL,
                                              c("iteration", "instance", "configuration", "time", "bound")))
    )
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
    .irace$instancesList <- addInstances(scenario, NULL, 
                                         if (scenario$maxExperiments != 0)
                                           ceiling(scenario$maxExperiments / minSurvival)
                                         else
                                           max (scenario$firstTest, length(scenario$instances)))

    indexIteration <- 1
    experimentsUsedSoFar <- 0
    timeUsed <- 0
    boundEstimate <- NA 
    rejectedIDs <- c()

    startParallel(scenario)
    on.exit(stopParallel(), add = TRUE)
 
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
      nconfigurations <- max(2, floor(scenario$parallel / ninstances))
      next.configuration <- 1

      while (TRUE) {
        # Sample new configurations if needed
        if (nrow(allConfigurations) < nconfigurations) {
          newConfigurations <- sampleUniform(parameters,
                                             nconfigurations - nrow(allConfigurations),
                                             digits = scenario$digits,
                                             forbidden = forbiddenExps,
                                             repair = scenario$repairConfiguration)
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
        
        iraceResults$experiments <- merge.matrix (iraceResults$experiments,
                                                  output$experiments)
        rownames(iraceResults$experiments) <- 1:nrow(iraceResults$experiments)
        rejectedIDs <- c(rejectedIDs, output$rejectedIDs)
        iraceResults$rejectedConfigurations <- rejectedIDs
        forbiddenExps <- c(forbiddenExps,
                           buildForbiddenExp(configurations = allConfigurations[
                                               allConfigurations$.ID. %in% output$rejectedIDs, , drop = FALSE],
                                             parameters = parameters))
                
        # For the used time, we count the time reported in all configurations
        # including rejected ones. 
        timeUsed <- sum(timeUsed, output$experimentLog[, "time"], na.rm = TRUE)
        # User should return time zero for rejectedIDs.
        boundEstimate <- mean(iraceResults$experimentLog[, "time"], na.rm = TRUE)
        if (boundEstimate <= 0)
          boundEstimate <- if (!is.null(scenario$boundMax)) scenario$boundMax else 1.0
        
        next.configuration <- nconfigurations + 1L
        
        # Calculate how many new configurations:
        # 1. We do not want to overrun estimationTime
        new.conf <- floor(((estimationTime - timeUsed) / boundEstimate) / ninstances)
        # 2. But there is no point in executing more configurations than those
        # that we can execute in parallel.
        new.conf <- min(new.conf, max(1, floor(scenario$parallel / ninstances)))

        if (timeUsed >= estimationTime || new.conf == 0 || nconfigurations == 1024) {
          break
        } else {
          nconfigurations <- min(1024, nconfigurations + new.conf)
        }
      }
      
      if (length(rejectedIDs) > 0) {
        irace.note ("Immediately rejected configurations: ",
                    paste0(rejectedIDs, collapse = ", ") , "\n")
      }
  
      # Update budget
      remainingBudget <- round((scenario$maxTime - timeUsed) / boundEstimate)
      experimentsUsedSoFar <- experimentsUsedSoFar + nrow(iraceResults$experimentLog)
      eliteConfigurations <- allConfigurations[allConfigurations$.ID. %!in% rejectedIDs, ,drop = FALSE]

      # Without elitist, the racing does not re-use the results computed during
      # the estimation.  This means that the time used during estimation needs
      # to be spent again during racing, thus leaving less time for racing.  We
      # want to avoid having less time for racing, and this is an
      # implementation detail, thus we assume that the time was not actually
      # wasted.
      if (!scenario$elitist) timeUsed <- 0

      irace.note("Estimated execution time is ", boundEstimate, " based on ",
                 next.configuration - 1, " configurations and ",
                 ninstances," instances. Used time: ", timeUsed,
                 ", remaining time: ", (scenario$maxTime - timeUsed),
                 ", remaining budget (experiments): ", remainingBudget, "\n")
    } # end of time estimation

    # Compute the total initial budget, that is, the maximum number of
    # experiments that we can perform.
    currentBudget <-
      ifelse (scenario$nbExperimentsPerIteration == 0,
              computeComputationalBudget(remainingBudget, indexIteration,
                                         nbIterations),
              scenario$nbExperimentsPerIteration)

    # Check that the budget is enough, for the time estimation case we reduce
    # the number of iterations.
    warn_msg <- NULL
    while (!checkMinimumBudget(scenario, remainingBudget, minSurvival, nbIterations,
                               boundEstimate, timeUsed))
    {
      if (is.null(warn_msg))
        warn_msg <- 
          paste0("with the current settings and estimated time per run (",
                 boundEstimate,
                 ") irace will not have enough budget to execute the minimum",
                 " number of iterations (", nbIterations, "). ",
                 "Execution will continue by assuming that the estimated time",
                 " is too high and reducing the minimum number of iterations,",
                 " however, if the estimation was correct or too low,",
                 " results might not be better than random sampling.\n")
      nbIterations <- nbIterations - 1
    }
    if (!is.null(warn_msg)) irace.warning(warn_msg)
    
  } #end of do not recover
  
  catInfo("Initialization\n",
          if (scenario$elitist)
            paste0("# Elitist race\n",
                   "# Elitist new instances: ", scenario$elitistNewInstances, "\n",
                   "# Elitist limit: ",         scenario$elitistLimit, "\n")
          else paste0("# Non-elitist race\n"),
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
            
          if (scenario$capping) 
            paste0("# capping: ", scenario$cappingType, "\n",
                   "# type bound: ", scenario$boundType, "\n", 
                   "# maxBound: ", scenario$boundMax, "\n",
                   "# par bound: ", scenario$boundPar, "\n", 
                   "# bound digits: ", scenario$boundDigits, "\n")
          else if (!is.null(scenario$boundMax))
            paste0("# maxBound: ", scenario$boundMax, "\n"),
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
                               boundEstimate = boundEstimate,
                               rejectedIDs = rejectedIDs,
                               forbiddenExps = forbiddenExps,
                               completed = list(flag=FALSE, msg=""))
    # Consistency checks
    irace.assert(sum(!is.na(iraceResults$experiments)) == experimentsUsedSoFar)
    irace.assert(nrow(iraceResults$experimentLog) == experimentsUsedSoFar)

    ## Save to the log file
    iraceResults$allConfigurations <- allConfigurations
    irace_save_logfile(iraceResults, scenario)

    if (remainingBudget <= 0) {
      catInfo("Stopped because budget is exhausted")
      iraceResults$state$completed$flag = TRUE
      iraceResults$state$completed$msg = "Budget exhausted"
      irace_save_logfile(iraceResults, scenario)
      return (eliteConfigurations)
    }
    if (scenario$maxTime > 0 && timeUsed >= scenario$maxTime) {
      catInfo("Stopped because time budget is exhausted")
      iraceResults$state$completed$flag = TRUE
      iraceResults$state$completed$msg = "Time budget exhausted"
      irace_save_logfile(iraceResults, scenario)
      return (eliteConfigurations)
    }

    if (indexIteration > nbIterations) {
      if (scenario$nbIterations == 0) {
        nbIterations <- indexIteration
      } else {
        if (debugLevel >= 1) {
          catInfo("Limit of iterations reached", verbose = FALSE)
        }
        iraceResults$state$completed$flag = TRUE
        iraceResults$state$completed$msg = "Limit of iterations reached"
        irace_save_logfile(iraceResults, scenario)
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
    if (scenario$elitist && !firstRace) {
      nbConfigurations <-
        computeNbConfigurations(currentBudget, indexIteration,
                                firstTest = max(scenario$mu, scenario$firstTest),
                                eachTest = scenario$eachTest,
                                nElites = nrow(eliteConfigurations),
                                nOldInstances = nrow(iraceResults$experiments),
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
        catInfo("Not enough budget for this iteration, ",
                "skipping to the next one.")
        indexIteration <- indexIteration + 1
        next
      } else {
        catInfo("Stopped because ",
                "there is not enough budget to enforce the value of nbConfigurations.")
        iraceResults$state$completed$flag = TRUE
        iraceResults$state$completed$msg = "Not enough budget to enforce the value of nbConfigurations"
        irace_save_logfile(iraceResults, scenario)
        return (eliteConfigurations)
      }
    }
    
    # Stop if the number of configurations to test is NOT larger than the minimum.
    if (nbConfigurations <= minSurvival) {
      catInfo("Stopped because there is not enough budget left to race more than ",
              "the minimum (", minSurvival,")\n",
              "# You may either increase the budget or set 'minNbSurvival' to a lower value")
      iraceResults$state$completed$flag = TRUE
      iraceResults$state$completed$msg = "Not enough budget to race more than the minimum configurations"
      irace_save_logfile(iraceResults, scenario)
      return (eliteConfigurations)
    }


    # If we have too many eliteConfigurations, reduce their number. This can
    # happen before the first race due to the initial budget estimation.
    if (firstRace) {
      if (nbConfigurations < nrow(eliteConfigurations)) {
        eliteRanks <- overall.ranks(iraceResults$experiments, stat.test = scenario$testType)
        eliteConfigurations <- eliteConfigurations[order(eliteRanks), ]
        eliteConfigurations <- eliteConfigurations[1:nbConfigurations, ]
      }
    } else if (nbConfigurations <= nrow(eliteConfigurations)) {
      # Stop if  the number of configurations to produce is not greater than
      # the number of elites.
      catInfo("Stopped because ",
              "there is not enough budget left to race newly sampled configurations")
      #(number of elites  + 1) * (mu + min(5, indexIteration)) > remainingBudget" 
      iraceResults$state$completed$flag = TRUE
      iraceResults$state$completed$msg = "Not enough budget left to race newly sampled configurations"
      irace_save_logfile(iraceResults, scenario)
      return (eliteConfigurations)
    }
    
    if (scenario$elitist) {
      # The non-elite have to run up to the first test. The elites consume
      # budget at most up to the new instances.
      if ((nbConfigurations - nrow(eliteConfigurations)) * max(scenario$mu, scenario$firstTest)
          + nrow(eliteConfigurations) * min(scenario$elitistNewInstances, max(scenario$mu, scenario$firstTest))
          > currentBudget) {
        catInfo("Stopped because there is not enough budget left to race all configurations up to the first test (or mu)")
        iraceResults$state$completed$flag = TRUE
        iraceResults$state$completed$msg = "Not enough budget to race all configurations up to the first test (or mu)"
        irace_save_logfile(iraceResults, scenario)
        return (eliteConfigurations)
      }
    } else if (nbConfigurations * max(scenario$mu, scenario$firstTest)
               > currentBudget) {
      catInfo("Stopped because there is not enough budget left to race all configurations up to the first test (or mu)")
      iraceResults$state$completed$flag = TRUE
      iraceResults$state$completed$msg  = "Not enough budget to race all configurations up to the first test (or mu)"
      irace_save_logfile(iraceResults, scenario)
      return (eliteConfigurations)
    }

    catInfo("Iteration ", indexIteration, " of ", nbIterations, "\n",
            "# experimentsUsedSoFar: ", experimentsUsedSoFar, "\n",
            if (scenario$maxTime == 0) ""
            else paste0("# timeUsed: ", timeUsed, "\n",
                        "# boundEstimate: ", boundEstimate, "\n"),
            "# remainingBudget: ", remainingBudget, "\n",
            "# currentBudget: ", currentBudget, "\n",
            "# nbConfigurations: ", nbConfigurations,
            verbose = FALSE)
            
    iraceResults$softRestart[indexIteration] <- FALSE
    # Sample for the first time.
    if (firstRace) {
      # If we need more configurations, sample uniformly.
      nbNewConfigurations <- nbConfigurations - sum(allConfigurations$.ID. %!in% rejectedIDs)
      if (nbNewConfigurations > 0) {
        # Sample new configurations.
        if (debugLevel >= 1) {
          catInfo("Sample ", nbNewConfigurations,
                  " configurations from uniform distribution", verbose = FALSE)
        }
        newConfigurations <- sampleUniform(parameters, nbNewConfigurations,
                                           digits = scenario$digits,
                                           forbidden = forbiddenExps,
                                           repair = scenario$repairConfiguration)
        newConfigurations <-
          cbind (.ID. = max(0, allConfigurations$.ID.) + 1:nrow(newConfigurations),
                 newConfigurations)
        allConfigurations <- rbind(allConfigurations, newConfigurations)
        rownames(allConfigurations) <- allConfigurations$.ID.
        raceConfigurations <- allConfigurations[allConfigurations$.ID. %!in% rejectedIDs, , drop = FALSE]
      } else if (nbNewConfigurations < 0) {
        # We let the user know that not all configurations will be used.
        if (nbUserConfigurations > nbConfigurations) {
          catInfo("Only ", nbConfigurations,
                  " from the initial configurations will be used",
                  verbose = FALSE)
        }
        
        # This is made only in case that the number of configurations used in
        # the time estimation is more than needed.
        if (nrow(eliteConfigurations) == nbConfigurations) {
          raceConfigurations <- eliteConfigurations
        } else {
          raceConfigurations <- allConfigurations[allConfigurations$.ID. %!in% rejectedIDs, , drop = FALSE]
          raceConfigurations <- raceConfigurations[1:nbConfigurations,]
        }
      } # end of indexIteration == 1
      
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
      newConfigurations <- sampleModel(parameters, eliteConfigurations,
                                       model, nbNewConfigurations,
                                       digits = scenario$digits,
                                       forbidden = forbiddenExps,
                                       repair = scenario$repairConfiguration)

      # Set ID of the new configurations.
      newConfigurations <- cbind (.ID. = max(0, allConfigurations$.ID.) +
                                    1:nrow(newConfigurations), newConfigurations)
      raceConfigurations <- rbind(eliteConfigurations[, colnames(newConfigurations)],
                                  newConfigurations)
      rownames(raceConfigurations) <- raceConfigurations$.ID.

      if (scenario$softRestart) {
        #          Rprof("profile.out")
        tmp.ids <- similarConfigurations (raceConfigurations, parameters,
                                          threshold = scenario$softRestartThreshold)
        #          Rprof(NULL)
        if (!is.null(tmp.ids)) {
          if (debugLevel >= 1)
            irace.note("Soft restart: ", paste(collapse = " ", tmp.ids), " !\n")
          model <- restartConfigurations (raceConfigurations, tmp.ids, model,
                                          parameters, nbNewConfigurations, scenario$digits)
          iraceResults$softRestart[indexIteration] <- TRUE
          ## FIXME: What is this for?
          # iraceResults$model$afterSR[[indexIteration]] <- model
          if (debugLevel >= 2) { printModel (model) }
          # Re-sample after restart like above
          #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel()\n")
          newConfigurations <- sampleModel(parameters, eliteConfigurations,
                                           model, nbNewConfigurations,
                                           digits = scenario$digits,
                                           forbidden = forbiddenExps,
                                           repair = scenario$repairConfiguration)
          #cat("# ", format(Sys.time(), usetz=TRUE), " sampleModel() DONE\n")
          # Set ID of the new configurations.
          newConfigurations <- cbind (.ID. = max(0, allConfigurations$.ID.) + 
                                  1:nrow(newConfigurations), newConfigurations)
          raceConfigurations <- rbind(eliteConfigurations[, colnames(newConfigurations)],
                                      newConfigurations)
          rownames(raceConfigurations) <- raceConfigurations$.ID.
        }
      }

      # Append these configurations to the global table.
      allConfigurations <- rbind(allConfigurations, newConfigurations)
      rownames(allConfigurations) <- allConfigurations$.ID.
    }

    if (debugLevel >= 2) {
      irace.note("Configurations for the race n ", indexIteration,
                 " (elite configurations listed first, then new configurations):\n")
      configurations.print(raceConfigurations, metadata = TRUE)
    }

    # Get data from previous elite tests 
    if (scenario$elitist && nrow(eliteConfigurations) > 0) {
      elite.data <- list()
      elite.data[["experiments"]] <- iraceResults$experiments[, as.character(eliteConfigurations[,".ID."]), drop=FALSE]
      if (scenario$capping)
        elite.data[["time"]] <- generateTimeMatrix(elites = eliteConfigurations, 
                                                   experimentLog = iraceResults$experimentLog)
    } else elite.data <- NULL
        
    .irace$next.instance <- max(nrow(iraceResults$experiments), 0) + 1
    # Add instances if needed
    # Calculate budget needed for old instances assuming non elitist irace
    if ((nrow(.irace$instancesList) - (.irace$next.instance - 1))
        < ceiling(remainingBudget / minSurvival)) {
      .irace$instancesList <- addInstances(scenario, .irace$instancesList,
                                           ceiling(remainingBudget/minSurvival))
    }

    if (debugLevel >= 1) irace.note("Launch race\n")
    raceResults <- race (scenario = scenario,
                         configurations = raceConfigurations,
                         parameters = parameters, 
                         maxExp = currentBudget, 
                         minSurvival = minSurvival,
                         elite.data = elite.data,
                         elitistNewInstances = if (firstRace) 0
                                               else scenario$elitistNewInstances)
    # Update experiments
    # LESLIE: Maybe we can think is make iraceResults an environment, so these values
    # can be updated in the race function.

    # We add indexIteration as an additional column.
    iraceResults$experimentLog <- rbind(iraceResults$experimentLog,
                                        cbind(rep(indexIteration, nrow(raceResults$experimentLog)),
                                              raceResults$experimentLog))
    
    # Merge new results.
    iraceResults$experiments <- merge.matrix (iraceResults$experiments,
                                              raceResults$experiments)
                                             
    if (length(raceResults$rejectedIDs) > 0) {
      rejectedIDs <- c(rejectedIDs, raceResults$rejectedIDs)
      iraceResults$rejectedConfigurations <- rejectedIDs
      forbiddenExps <- c(forbiddenExps,
                         buildForbiddenExp(
                           configurations = allConfigurations[
                             allConfigurations$.ID. %in% raceResults$rejectedIDs, , drop = FALSE],
                           parameters = parameters))
    }

    experimentsUsedSoFar <- experimentsUsedSoFar + raceResults$experimentsUsed
    # Update remaining budget.
    if (scenario$maxTime > 0) { 
      timeUsed <- sum(timeUsed, raceResults$experimentLog[, "time"], na.rm=TRUE)
      boundEstimate <- mean(iraceResults$experimentLog[, "time"], na.rm=TRUE)
      remainingBudget <- round((scenario$maxTime - timeUsed) / boundEstimate)
    } else {
      remainingBudget <- remainingBudget - raceResults$experimentsUsed
    }

    if (debugLevel >= 3) {
      irace.note("Results for the race of iteration ", indexIteration,
                 " (from best to worst, according to the ",
                 test.type.order.str(scenario$testType), "):\n")
      configurations.print (raceResults$configurations, metadata = TRUE)
    }

    if (debugLevel >= 1) { irace.note("Extracting elites\n") }
    # FIXME: Since we only actually keep the alive ones, we don't need
    # to carry around rejected ones in raceResults$configurations. This
    # would reduce overhead.
    eliteConfigurations <- extractElites(raceResults$configurations,
                                         min(raceResults$nbAlive, minSurvival))
    irace.note("Elite configurations (first number is the configuration ID;",
               " listed from best to worst according to the ",
               test.type.order.str(scenario$testType), "):\n")
    configurations.print(eliteConfigurations, metadata = debugLevel >= 1)
    iraceResults$iterationElites <- c(iraceResults$iterationElites, eliteConfigurations$.ID.[1])
    iraceResults$allElites[[indexIteration]] <- eliteConfigurations$.ID.
    
    if (firstRace) {
      if (debugLevel >= 1)  { irace.note("Initialise model\n") }
      model <- initialiseModel(parameters, eliteConfigurations, scenario$digits)
    }
      
    if (debugLevel >= 1) {
      irace.note("End of iteration ", indexIteration, "\n")
    }

    if (debugLevel >= 3) {
      irace.note("All configurations (sampling order):\n")
      configurations.print(allConfigurations, metadata = TRUE)
    }

    indexIteration <- indexIteration + 1
    firstRace <- FALSE
    if (scenario$debugLevel >= 3) {
      irace.note ("Memory used in irace():\n")
      irace.print.memUsed()
    }
  }
  # This code is actually never executed because we return above.
  # Leslie: adding this just in case 
  iraceResults$state$completed$flag = TRUE
  irace_save_logfile(iraceResults, scenario)
  return (eliteConfigurations)
}
