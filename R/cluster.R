## FIXME: Implement this using Rsge package.

## Parse the output of qsub.
sge.cluster.qsub <- function(outputRaw)
{
  ## FIXME: Can we make this more robust against different languages and
  ## variants of qsub?
  # FIXME: This is a bit too complex for just parsing a number and
  # returning the number or NULL.
  match <- grep("Your job \\d+ ", outputRaw, perl = TRUE)
  if (length(match) == 1) {
    jobID <-
      suppressWarnings(as.integer(grep("\\d+",
                                       unlist(strsplit(outputRaw[match], " ")),
                                       perl = TRUE, value = TRUE)[1]))
    if (length(jobID) == 1 && is.numeric(jobID)) {
      return(list(jobID = jobID))
    }
  }
  # Some error matching.
  return(list(jobID = NULL,
              error = "The output of targetRunner should be 'Your job ID'",
              outputRaw = outputRaw))
}

pbs.cluster.qsub <- function(outputRaw)
{
  ## FIXME: Can we make this more robust against different languages and
  ## variants of qsub?
  jobID <- grep("\\d+\\.[a-z]+", outputRaw, perl = TRUE, value = TRUE)
  if (length(jobID) == 1) {
    return(list(jobID = jobID))
  } else { # Some error matching.
    return(list(jobID = NULL,
                error = "The output of targetRunner should be an ID of the form '\\d+\\.[a-z]+'",
                outputRaw = outputRaw))
  }
}

sge.job.status <- function(jobid)
{
  return(system (paste("qstat -j", jobid),
                 ignore.stdout = TRUE, ignore.stderr = TRUE,
                 intern = FALSE, wait = TRUE))
}

pbs.job.status <- function(jobid)
{
  return(system (paste("qstat", jobid),
                 ignore.stdout = TRUE, ignore.stderr = TRUE,
                 intern = FALSE, wait = TRUE))
}

## Launch a job with qsub and return its jobID. This function does not
## call qsub directly, but instead command should be a script that
## invokes qsub and prints its output.
target.runner.qsub <- function(experiment, scenario, cluster.parse.qsub)
{
  debugLevel       <- scenario$debugLevel
  configuration.id <- experiment$id.configuration
  instance.id      <- experiment$id.instance
  seed             <- experiment$seed
  configuration    <- experiment$configuration
  instance         <- experiment$instance
  extra.params     <- experiment$extra.params
  switches         <- experiment$switches
  
  targetRunner <- scenario$targetRunner
  if (as.logical(file.access(targetRunner, mode = 1))) {
    irace.error ("targetRunner ", shQuote(targetRunner), " cannot be found or is not executable!\n")
  }

  args <- paste(configuration.id, instance.id, seed, instance, extra.params,
                buildCommandLine(configuration, switches))
  output <- runcommand(targetRunner, args, configuration.id, debugLevel)

  outputRaw <- output$output
  err.msg <- output$error
  if (is.null(err.msg)) {
    # FIXME: Output should be parsed by targetRunner, which should return just
    # the jobID as an alphanumeric string.
    cluster.parse.qsub <- sge.cluster.qsub
    return(c(cluster.parse.qsub(outputRaw), call = paste(targetRunner, args)))
  }
  return (list(jobID = NULL,
               error = err.msg, outputRaw = outputRaw,
               call = paste(targetRunner, args)))
}

cluster.lapply <- function(X, scenario,
                           # FIXME: This should depend on a parameter --qsub [pbs|sge]
                           cluster.job.status = sge.job.status,
                           poll.time = 2)
{
  debugLevel <- scenario$debugLevel
  output <- lapply(X, exec.target.runner, scenario = scenario, target.runner = target.runner.qsub)
  jobIDs <- sapply(output, "[[", "jobID")
  
  ## Wait for cluster jobs to finish.
  if (length(jobIDs) > 0 && debugLevel >= 1) {
    irace.note("Waiting for jobs ('.' ==", poll.time, "s) ")
  }
  for (jobID in jobIDs) {
    while (!cluster.job.status(jobID)) {
      if (debugLevel >= 1) { cat(".") }
      Sys.sleep(poll.time)
    }
    if (debugLevel >= 1) {
      irace.note ("DONE (", jobID, ")")
    }
  }
  cat ("\n")
  return(output)
}
