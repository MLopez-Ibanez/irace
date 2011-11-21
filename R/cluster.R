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
  # FIXME: This is a bit too complex for just parsing a number and
  # returning the number or NULL.
  match <- grep("Your job \\d+ ", rawOutput, perl = TRUE)
  if (length(match) != 1) {
    # Some error matching.
    return(NULL)
  }
  jobID <-
    suppressWarnings(as.integer(grep("\\d+",
                                     unlist(strsplit(rawOutput[match], " ")),
                                     perl = TRUE, value = TRUE)[1]))
  if (length(jobID) == 1 && is.numeric(jobID)) {
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
  return(system (paste("qstat ", jobid, "&> /dev/null")))
}

hook.run.qsub <- function(instance, candidate, extra.params, config, cluster.qsub)
{
  debugLevel <- config$debugLevel
  
  hookRun <- config$hookRun
  if (as.logical(file.access(hookRun, mode = 1))) {
    stop ("hookRun `", hookRun, "' cannot be found or is not executable!\n")
  }
  command <- paste (hookRun, instance, candidate$index, extra.params,
                    buildCommandLine(candidate$values, candidate$labels))

  jobID <- cluster.qsub (command, debugLevel)
  exit.code <- is.null(jobID)

  if (exit.code) {
    tunerError("Command `", command, "' returned non-zero exit status (",
               exit.code / 256, ")!\n",
               "This is not a bug in crace, ",
               "but means that something failed when running your program ",
               "or it was terminated before completion. ",
               "Try to run the command above from the execution directory '",
               config$execDir, "' to investigate the issue.")
  }
  
  return(list(command = command, jobID = jobID))
}

cluster.lapply <- function(X, ..., config,
                           cluster.qsub = sge.cluster.qsub,
                           cluster.job.status = sge.job.status,
                           poll.time = 2)
{
  debugLevel <- config$debugLevel
  output <- lapply(X, hook.run.qsub, ..., config = config, cluster.qsub = cluster.qsub)
  jobIDs <- sapply(output, "[[", "jobID")
  
  ## Wait for SGE Cluster jobs to finish.
  poll.time <- 2
  if (length(jobIDs) > 0 && debugLevel >= 1) {
    cat(format(Sys.time(), usetz=TRUE), ": Waiting for jobs ('.' ==",
        poll.time, "s) ")
  }
  for (jobID in jobIDs) {
    while (!cluster.job.status(jobID)) {
      if (debugLevel >= 1) { cat(".") }
      Sys.sleep(poll.time)
    }
  }
  if (length(jobIDs) > 0 && debugLevel >= 1) {
    cat ("\n", format(Sys.time(), usetz=TRUE), ": DONE\n")
  }
  return(lapply(output, "[[", "command"))
}
