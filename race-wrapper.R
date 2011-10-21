###############################################################################
# FIXME: This file needs a description
###############################################################################
canonical.dirname <- function(dirname = stop("required parameter"))
{
  return (sub ("([^/])$", "\\1/", dirname))
}

buildCommandLine <- function(values, switches, signifDigits) {
  stopifnot(length(values) == length(switches))
  command <- ""
  # FIXME: This probably can be implemented faster with apply() and
  # paste(collapse=" "). But how to get the index i in that case?
  for (i in seq_along(values)) {
    value <- values[i]
    if (!is.na(value)) {
      if (is.numeric(value)) {
        value <- signif(value, signifDigits)
      }
      command <- paste(command, " ", switches[i], value, sep="")
    }
  }
  return(command)
}

## FIXME: Implement this using Rsge package.
## Launch a job with qsub and return its jobID. This function does not
## call qsub directly, but instead command should be a script that
## invokes qsub and prints its output.
sge.cluster.qsub <- function(command, debugLevel = 0)
{
  if (debugLevel >= 1) {
    cat (format(Sys.time(), usetz=TRUE), ":", command, "\n")
  }
  rawOutput <- system (paste(command, " 2>&1"), intern = TRUE)
  if (debugLevel >= 1) { cat(rawOutput, sep="\n") }
  ## ??? Can we make this more robust against different languages and
  ## variants of qsub?
  match <- grep("Your job \\d+ ", rawOutput, perl = TRUE)
  if (length(match) != 1) {
    # Some error matching.
    return(NULL)
  }
  jobID <- as.integer(grep("\\d+", unlist(strsplit(rawOutput[match], " ")),
                           perl = TRUE, value = TRUE)[1])
  if (length(jobID) == 1 && is.integer(jobID)) {
    return(jobID)
  } else return(NULL)
}

pbs.cluster.qsub <- function(command, debugLevel = 0)
{
  if (debugLevel >= 1) {
    cat (format(Sys.time(), usetz=TRUE), ":", command, "\n")
  }
  rawOutput <- system (paste(command, " 2>&1"), intern = TRUE)
  if (debugLevel >= 1) { cat(rawOutput, sep="\n") }
  ## ??? Can we make this more robust against different languages and
  ## variants of qsub?
  jobID <- grep("\\d+\\.[a-z]+", rawOutput, perl = TRUE,value=TRUE)
  if (length(jobID) == 1) {
    return(jobID)
  } else return(NULL)
}

sge.job.status <- function(jobid)
{
  return(system (paste("qstat -j", jobid, "&> /dev/null")))
}

pbs.job.status <- function(jobid)
{
  return(system (paste("qstat -J", jobid, "&> /dev/null")))
}

## ??? This function needs a description
race.init <- function(candidates, parameters, tunerConfig)
{
  instances.extra.params <- NULL
  instance.dir <- tunerConfig$instanceDir
  instance.dir <- canonical.dirname (instance.dir)
  instanceFile <- tunerConfig$instanceFile
  if (!is.null (instanceFile) && instanceFile != "") {
    if (as.logical (file.access (instanceFile, mode=4))) {
      tunerError ("instanceFile `", instanceFile, "' cannot be read!\n")
    } else if (as.logical (file.info (instanceFile)$isdir)) {
      tunerError ("instanceFile `", instanceFile,
                  "' is a directory, not a file!\n")
    }
    lines <- readLines (instanceFile)
    lines <- sub("#.*$", "", lines) # Remove comments
    lines <- sub("^[[:space:]]+", "", lines) # Remove extra spaces
    lines <- lines[lines != ""] # Delete empty lines
    instances <- sub("^([^[:space:]]+).*$", "\\1", lines)
    instances <- paste (instance.dir, instances, sep="")
    instances.extra.params <- sub("^[^[:space:]]+(.*)$", "\\1", lines)
    names (instances.extra.params) <- instances
  } else {
    # The files are sorted in alphabetical order, on the full path if
    # 'full.names = TRUE'.
    instances <- list.files (path = instance.dir, full.names = TRUE,
                             recursive = TRUE)
    if (length (instances) == 0)
      tunerError("No instances found in `", instance.dir, "' !\n")
  }
  # FIXME: Ideally, we wouldn't do this here but dynamically as we
  # need more instances to avoid the artificial limit of 1000
  # instances.
  if (tunerConfig$sampleInstances) {
    instances <- rep(sample(instances), length.out = 1000)
  } else {
    instances <- rep(instances, length.out = 1000)
  }
  # Return init list
  return (list (no.candidates = nrow(candidates), 
                no.tasks = length(instances),
                instances = instances,
                candidates = candidates, 
                parameters = parameters,
                instances.extra.params = instances.extra.params,
                hookRun = tunerConfig$hookRun,
                hookEvaluate = tunerConfig$hookEvaluate,
                tunerConfig = tunerConfig
                ))
}

## ??? This function needs a description
race.info <- function(data)
  return(list(race.name = data$tunerConfig$expName, 
              no.candidates = data$no.candidates, 
              no.tasks = data$no.tasks, 
              extra = data$tunerConfig$expDescription))

## ??? This function needs a description, what is candidate, task and data?
race.wrapper <- function(candidate, task, data)
{
  debugLevel <- data$tunerConfig$debugLevel
  execDir <- data$tunerConfig$execDir
  signifDigits <- data$tunerConfig$signifDigits
  sgeCluster <- data$tunerConfig$sgeCluster
  cluster.qsub <- sge.cluster.qsub
  cluster.job.status <- sge.job.status
  jobIDs <- c() # SGE Cluster jobIDs
  
  if (!isTRUE (file.info(execDir)$isdir)) {
    stop("Execution directory '", execDir, "' is not found or not a directory\n")
  }

  hookRun <- data$hookRun
  if (as.logical(file.access(hookRun, mode = 1))) {
    stop ("hookRun `", hookRun, "' cannot be found or is not executable!\n")
  }

  hookEvaluate <- data$hookEvaluate
  if (as.logical(file.access(hookEvaluate, mode = 1))) {
    stop ("hookEvaluate `", hookEvaluate,
          "' cannot be found or is not executable!\n")
  }
  
  if (debugLevel >= 4) { print(data$candidates) }
  stopifnot (data$parameters$nbParameters > 0)
  stopifnot (length(data$parameters$names) == data$parameters$nbParameters)
  
  cnd <- c()
  ins <- data$instances[task];

  extra.params <- ""
  if (!is.null (data$instances.extra.params)
      && !is.na (data$instances.extra.params[ins]))
    extra.params <- data$instances.extra.params[ins]

  commands <- c()
  
  ## This is testing if this is the first candidate. If so, record all
  ## candidates. Otherwise, just collect the results.
  ## FIXME: which.alive should not be a global variable.
  if (candidate == which.alive[1]) {
    for (candi in which.alive) {
      # First parameter is the instance file, second is the candidate number.
      command <- paste (hookRun, ins, candi, extra.params)
      # Use drop = FALSE to avoid decaying to a simple vector.
      cnd <- data$candidates[candi, , drop = FALSE]
      ## Constructs the command line
      ## FIXME: Use buildCommandLine function above
      for (i in seq_len (data$parameters$nbParameters)) {
        param.name <- data$parameters$names[[i]]
        param.switch <- data$parameters$switches[[i]]
        if (debugLevel >= 4) {
          print (cnd)
          print (param.name)
          print (param.switch)
          print (cnd[[param.name]])
        }
        param.value <- cnd[[param.name]]
        if (!is.na(param.value)) {
          if (is.numeric(param.value)) {
            param.value <- signif(param.value, signifDigits)
          }
          command <- paste(command, " ", param.switch, param.value, sep="")
        }
      }

      ## Record the command
      commands <- c(commands, command)
    }
  
    runcommand <- function(command) {
      if (debugLevel >= 1) {
        cat (format(Sys.time(), usetz=TRUE), ":", command, "\n")
      }
      command.exit.value <- system (command)
      if (!command.exit.value && debugLevel >= 1) {
        cat (format(Sys.time(), usetz=TRUE), ": DONE\n")
      }
      return (command.exit.value)
    }

    cwd <- setwd (execDir)
    returnCodes <- list()
    # Execute commands
    if (tunerConfig$parallel > 1) {
      if (tunerConfig$mpi) {
        mpiInit(tunerConfig$parallel)
        returnCodes <- mpi.applyLB(commands, runcommand)
      } else {
        library(multicore)
        returnCodes <- mclapply(commands, runcommand, mc.cores = tunerConfig$parallel)
      }
    } else {
      # One process, all sequential
      for (command in commands) {
        if (sgeCluster) {
          jobID <- cluster.qsub(command, debugLevel)
          jobIDs <- c(jobIDs, jobID)
          command.exit.value <- is.null(jobID)
        } else {
          command.exit.value <- runcommand (command)
          returnCodes <- c(returnCodes, command.exit.value)
        }
        if (command.exit.value) break
      }
    }
    # Check return codes obtained:
    for (i in seq_along(returnCodes) ) {
      if (returnCodes[[i]]) {
        tunerError("Command `", commands[i], "' returned non-zero exit status (",
                   returnCodes[[i]]/256, ")!\n",
                   "This is not a bug in irace, ",
                   "but means that something failed when running your program ",
                   "or it was terminated before completion. ",
                   "Try to run the command above from the execution directory '",
                   execDir, "' to investigate the issue.")
      }
    }
    setwd (cwd)
  }

  ## Wait for SGE Cluster jobs to finish.
  jobwaitTime <- 2
  if (length(jobIDs) > 0 && debugLevel >= 1) {
    cat(format(Sys.time(), usetz=TRUE), ": Waiting for jobs ('.' ==",
        jobwaitTime, "s) ")
  }
  for (jobID in jobIDs) {
    while (!cluster.job.status(jobID)) {
      if (debugLevel >= 1) { cat(".") }
      Sys.sleep(jobwaitTime)
    }
  }
  if (length(jobIDs) > 0 && debugLevel >= 1) {
    cat ("\n", format(Sys.time(), usetz=TRUE), ": DONE\n")
  }

  ## Evaluate candidates
  ## Redirects STDERR so outputRaw captures the whole output.
  command <- paste (hookEvaluate, ins, candidate, length(which.alive), "2>&1")
  if (debugLevel >= 1) { cat(command, "\n") }
  cwd <- setwd (execDir)
  outputRaw <- system (command, intern = TRUE)
  setwd (cwd)
  if (debugLevel >= 1) { cat (outputRaw, sep="\n") }

  output <- NULL
  # strsplit crashes if outputRaw == character(0)
  if (length(outputRaw) > 0) {
    output <- strsplit(trim(outputRaw), "[[:space:]]+")[[1]]
    # suppressWarnings to avoid NAs introduced by coercion
    output <- suppressWarnings (as.numeric (output))
  }
  
  err.msg <- NULL
  if (length(output) < 1 || length(output) > 2 || any (is.na (output)))
    err.msg <- paste("The output of `", command, "' is not numeric!\n", sep = "")

  if (tunerConfig$timeBudget > 0 && length(output) < 2)
    err.msg <- paste("When timeBudget > 0, the output of `", command, "' must be two numbers 'cost time'!\n", sep = "")

  if (!is.null(err.msg)) {
    tunerError(err.msg,
               "The output was:\n", paste(outputRaw, sep="\n"),
               "\nThe call to hook-run was: ",
               # First parameter is the candidate number, second is the instance file.
               paste (hookRun, ins, candidate, extra.params,
                      buildCommandLine(data$candidates[candidate,unlist(parameters$names)],
                                       data$parameters$switches, signifDigits)),
               "\nThis is not a bug in irace, but means that something failed when",
               " running the command above or it was terminated before completion.",
               " Try to run the commands above from the execution directory '",
               execDir, "' to investigate the issue.")
  }
  return (output)
}

race.describe <- function(candidate, data)
{
  return (data$candidates[candidate, ])
}
