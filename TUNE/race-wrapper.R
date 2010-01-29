###############################################################################
# This file needs a description
###############################################################################

# global variable indicating the significant digit
signif.digit <- 4

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
  debug.level <- 1
  ## This is not configurable. Just replace the file with the appropriate one.
  ## ??? FIXME: check that the hooks exist and are executable.
  hookInstanceFinished <- "../hooks/hook-instance-finished"
  hookRun <- "../hooks/hook-run"
  
  #print (data)
  #q()
  cnd <- data$candidates[candidate, ];
  ins <- data$instances[task];
  #parameter.type.list <- data$parameter.type.list
  #timeWindow <- data$timeWindows[task]
  l <- length(names(cnd))
  names <- names(cnd)[1:l]
  
  #print(cnd[["alpha"]])
  #print(cnd$"alpha")
  #q()

  real.names <- data$parameter.name.list

  #timeWindow <- floor(runif(1, 0, 121))

  if (candidate == which.alive[1]) {
    system("rm -f *.job", intern=TRUE, TRUE)
    #system("rm -f DBLS.e*", intern=TRUE, TRUE)
    #system("rm -f DBLS.o*", intern=TRUE, TRUE)
    
    numJobs <- max(5, round(length(which.alive)/20))
    #numJobs <- 1
  
    counter <- 0
    for (candi in which.alive)  {
      # First parameter is the candidate number, second is the instance file
      command <- paste (hookRun, candi, ins)
      #command <- paste(command, " -i ", ins, " -w ", timeWindow, sep="")

      cnd <- data$candidates[candi, ];
      l <- length(names(cnd))
      cnd.names <- names(cnd)[1:l]

      ## ??? This could be for (p in seq_along(params)) { p$names, p$param, etc }
      ## Constructs the command line
      for (i in 1:l) {
        if (debug.level >= 2) {
          print(cnd.names[i])
          print(data$parameter.param.list[i])
        }
        ## ??? Why as.vector()
        tmp <- as.vector(cnd[[cnd.names[i]]])
        if (! is.na(tmp)) {
          if (is.numeric(tmp)) {
            tmp <- signif(tmp, signif.digit)
          }
          command <- paste(command, " ", data$parameter.param.list[[i]], tmp, sep="")
        }
#        command <- paste(command, params.param[[i]], " ", signif(as.numeric(tmp, signif.digit)), sep="")
##         # added specifically for ACOTSP,  mode can be as,  eas,  mmas,  acs,  bwas,  ras. 
##         # to be modified for general boolean parameters,  cuz they are called as "--level".
##         #cat("candi",  candi,  "i",  i,  "\n")
##         #print(cnd)
##         if (i == "mode") {
##           command <- paste(command, "  --", cnd[[i]], sep="")
##           #print(command)
##         } else {
##           #cat("i",  i,  "cnd[[i]]",  cnd[[i]],  "\n")
##           #print(cnd[[i]])
##           #print(cnd[["alpha"]])
##         print(tmp)
## ##           #cat("tmp", tmp, "\n")
## ##           #command <- paste(command, "  --", i, "=", signif(as.numeric(cnd[[i]]), 2), sep="")
        
## ##           # FIXed take FALSE as flag might not be a standard way
##           if (! is.na(tmp)) {
##             if (!is.null(real.names) & length(real.names[[cnd.names[i]]])>0 & !is.null(real.names[[cnd.names[i]]])) {
##               i <- real.names[[cnd.names[i]]]
##               print(paste("real.names", i))
##             }
##           }
##         }
      }

      if (debug.level >= 1) { print(command) }
      #q()
    
      #command="echo \$RANDOM"

      ## FIXME: for the future: Investigate the multicore package to execute
      ## nParallel (a new parameter) calls in parallel and collect the
      ## results. This would allow to execute on multi-core computers
      ## and also in the cluster by using 'qsub -sync y'

      ## Run the command
      if (system (command))
        stop("Command `", command, "' returned non-zero exit status!\n")

      counter <- counter+1
    }
  
    #    system("sleep 1")
  }

#  print(candidate)
  ## FIXME: this should be silent
  output <- as.numeric (system (paste (hookInstanceFinished, ins, candidate), intern=TRUE))
  ## This should handle vectors of NAs
  if (is.na (output))
    stop ("The output of `", hookInstanceFinished, " ", ins, " ", candidate,
          "' is not a number!\n")
  return (output)
}

race.describe <- function(candidate, data) {
  return (data$candidates[candidate, ])
}
