### Submit/wait for jobs in batch clusters.
sge.job.finished <- function(jobid)
{
  return(system (paste("qstat -j", jobid),
                 ignore.stdout = TRUE, ignore.stderr = TRUE,
                 intern = FALSE, wait = TRUE))
}

pbs.job.finished <- function(jobid)
{
  return(system (paste("qstat", jobid),
                 ignore.stdout = TRUE, ignore.stderr = TRUE,
                 intern = FALSE, wait = TRUE))
}

torque.job.finished <- function(jobid)
{
  output <- suppressWarnings(system2("qstat", jobid, stdout = TRUE, stderr = TRUE))
  ## 1. If the return code of qstat in not 0, then no such job is in the queue
  ## anymore. That means that the job was (assumption) successfully submitted,
  ## executed and completed. Then, for some time the job id was marked as
  ## completed in the jobs queue and the job status removed from the queue list
  ## after some time. No job completed status of successfully submitted and
  ## completed jobs in the queue list happens if a small task was executed
  ## first and a task of some hours execution time finishes much later. The job
  ## status of the small task will not be in the queue anymore after some
  ## minutes. So: If no job id is in the queue list anymore and torq's qstat
  ## returns an error (return code > 0), then the job has been successfully
  ## executed (if it has been started successfully before).
  if (!is.null(attr(output, "status"))) return(TRUE)
  # 2. If qstat returns OK (return code ==0), then one has to parse qstat's
  # output. If the 5th token in the last line is a 'C', then the job has
  # terminated and its output files can be processed. Otherwise the job is not
  # completed (queued, running, exiting...)
  return(any(grepl(paste0(jobid, ".*\\sC\\s"), output)))
}

slurm.job.finished <- function(jobid)
{
  output <- suppressWarnings(system2("squeue", c("-j", jobid, "--noheader"),
                                     stdout = TRUE, stderr = TRUE))
  # If the above returns non-zero, either the job terminated or it never
  # existed.
  if (!is.null(attr(output, "status"))) return(TRUE)
  # If may return zero, but the job is not in the system anymore because it
  # completed. This is different from the Torque case.
  return (!any(grepl(paste0("\\s", jobid, "\\s"), output)))
}

## Launch a job with qsub and return its jobID. This function does not
## call qsub directly, but instead targetRunner should be a script that
## invokes qsub and returns a jobID.
target.runner.qsub <- function(experiment, scenario)
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

  jobID <- NULL
  outputRaw <- output$output
  err.msg <- output$error
  if (is.null(err.msg)) {
    # We cannot use parse.output because that tries to convert to numeric.
    if (scenario$debugLevel >= 2) { cat (outputRaw, sep = "\n") }
    # Initialize output as raw. If it is empty stays like this.
    # strsplit crashes if outputRaw == character(0)
    if (length(outputRaw) > 0) {
      jobID <- strsplit(trim(outputRaw), "[[:space:]]+")[[1]]
    }
    if (length(jobID) != 1) {
      err.msg <- paste0("The output of targetRunner should be only the jobID!")
      jobID <- NULL
    }
  }
  return(list(jobID = jobID, error = err.msg, outputRaw = outputRaw,
              call = paste(targetRunner, args)))
}

cluster.lapply <- function(X, scenario, poll.time = 2)
{
  debugLevel <- scenario$debugLevel

  cluster.job.finished <-
    switch(scenario$batchmode,
           sge = sge.job.finished,
           pbs = pbs.job.finished,
           torque = torque.job.finished,
           slurm = slurm.job.finished,
           irace.error ("Invalid value of scenario$batchmode = ", scenario$batchmode))

  # Parallel controls how many jobs we send at once. Some clusters have low
  # limits.
  ## FIXME: It would be better to submit up to the limit, then one by one as jobs finish.
  chunksize <- scenario$parallel
  if (chunksize < 0) {
    chunksize <- length(X)
  }
  chunks <- split(X, ceiling(seq_along(X) / chunksize))
  for (chunk in chunks) {
    if (debugLevel >= 1) {
      irace.note ("Sending ", length(chunk), " / ", length(X), " jobs\n")
    }
    output <- lapply(chunk, exec.target.runner, scenario = scenario, target.runner = target.runner.qsub)
    jobIDs <- sapply(output, "[[", "jobID")
  
    ## Wait for cluster jobs to finish.
    if (length(jobIDs) > 0 && debugLevel >= 1) {
      irace.note("Waiting for jobs ('.' == ", poll.time, " s) ")
    }
    for (jobID in jobIDs) {
      while (!cluster.job.finished(jobID)) {
        if (debugLevel >= 1) { cat(".") }
        Sys.sleep(poll.time)
      }
      if (debugLevel >= 1) {
        cat("\n")
        irace.note ("DONE (", jobID, ")\n")
      }
    }
  }
  return(output)
}
