###############################################################################
# FIXME: This file needs a description
###############################################################################

## ??? This function needs a description
race.init <- function(candidatesConfig, maxIns, experiment.name, extra.description, executable, instance.dir, parameter.name.list)
{
  candidates <- candidatesConfig
  ins <- list.files(path=instance.dir, pattern="", full.names=TRUE)
  if (length (ins) == 0)
    stop("No instances found in ", instance.dir, " !")
  ins <- sample(ins)
  instances <- sample(ins, replace=TRUE, size=maxIns)
  # Return init list
  return(list(no.candidates=nrow(candidates), 
              no.tasks=length(instances), 
              experiment.name=experiment.name, 
              extra.description=extra.description, 
              executable=executable, 
              #executablecomp=executablecomp, 
              #instance.name=instance.name, 
              instances=instances, 
	      #timeWindows=timeWindows, 	
              candidates=candidates, 
              parameter.name.list=parameter.name.list,
              parameter.param.list=parameter.param.list))
}

## ??? This function needs a description
race.info <- function(data)
  return(list(race.name=data$experiment.name, 
              no.candidates=(data$no.candidates), 
              no.tasks=data$no.tasks, 
              extra=data$extra.description))

## ??? This function needs a description, what is candidate, task and data?
race.wrapper <- function(candidate, task, data)
{
  ## This is not configurable. Just replace the file with the appropriate one.
  ## ??? FIXME: check that the hooks exist and are executable.
  hookInstanceFinished <- "../hooks/hook-instance-finished"
  hookRun <- "../hooks/hook-run"

  ## FIXME: this probably can be calculated much earlier and there is
  ## no need to recalculate it here.
  parameters.num <- ncol (data$candidates)
  parameters.names <- names(data$candidates)
  stopifnot (parameters.num > 0)
  stopifnot (length(parameters.names) == parameters.num)
  
  cnd <- c()
  ins <- data$instances[task];

  if (candidate == which.alive[1]) {
    numJobs <- max(5, round(length(which.alive)/20))
    counter <- 0
    for (candi in which.alive)  {
      # First parameter is the candidate number, second is the instance file
      command <- paste (hookRun, candi, ins)
      cnd <- data$candidates[candi, ]
      
      ## ??? This could be for (p in seq_along(params)) { p$names, p$param, etc }
      ## Constructs the command line
      for (i in seq_len(parameters.num)) {
        if (debug.level >= 2) {
          print(parameters.names[i])
          print(data$parameter.param.list[i])
          print (cnd[i])
        }
        tmp <- cnd[i]
        if (! is.na(tmp)) {
          if (is.numeric(tmp)) {
            tmp <- signif(tmp, signif.digit)
          }
          command <- paste(command, " ", data$parameter.param.list[[i]], tmp, sep="")
        }
      }

      if (debug.level >= 1) { print(command) }
      #command="echo \$RANDOM"

      ## FIXME: for the future: Investigate the multicore package to execute
      ## nParallel (a new parameter) calls in parallel and collect the
      ## results. This would allow to execute on multi-core computers
      ## and also in the cluster by using 'qsub -sync y'

      ## Run the command
      if (system (command))
        stop("Command `", command, "' returned non-zero exit status!\n")

      counter <- counter + 1
    }
  }

  ## FIXME: this should be silent
  output <- as.numeric (system (paste (hookInstanceFinished, ins, candidate),
                                intern=TRUE))
  ## This should handle vectors of NAs
  if (is.na (output))
    stop ("The output of `", hookInstanceFinished, " ", ins, " ", candidate,
          "' is not a number!\n")
  return (output)
}

race.describe <- function(candidate, data) {
  return (data$candidates[candidate, ])
}
