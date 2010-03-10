###############################################################################
# FIXME: This file needs a description
###############################################################################

canonical.dirname <- function(dirname = stop("required parameter"))
  {
    return (sub ("([^/])$", "\\1/", dirname))
  }

## ??? This function needs a description
race.init <- function(candidatesConfig, maxIns, experiment.name, extra.description, executable, instance.dir, parameter.name.list)
{
  candidates <- candidatesConfig
  instances.extra.params <- NULL
  instance.dir <- canonical.dirname (instance.dir)

  if (!is.null (.tune.tuning.instances.file) && .tune.tuning.instances.file != "") {
    if (as.logical (file.access (.tune.tuning.instances.file, mode=4))) {
      stop (".tune.tuning.instance.file `", .tune.tuning.instances.file, "' cannot be read!\n")
    } else if (as.logical (file.info (.tune.tuning.instances.file)$isdir)) {
      stop (".tune.tuning.instance.file `", .tune.tuning.instances.file, "' is a directory, not a file!\n")
    }
    lines <- readLines (.tune.tuning.instances.file)
    lines <- sub("#.*$", "", lines) # Remove comments
    lines <- sub("^[[:space:]]+$", "", lines) # Remove extra spaces
    lines <- lines[lines != ""] # Delete empty lines
    ins <- sub("[[:space:]]+.*$", "", lines)
    ins <- sub ("^", instance.dir, ins)
    instances.extra.params <- sub("^[^[:space:]]+ ", "", lines)
    names (instances.extra.params) <- ins
  } else {
    ins <- list.files (path = instance.dir, full.names = TRUE)
    if (length (ins) == 0)
      stop("No instances found in `", instance.dir, "' !\n")
  }
  ins <- sample(ins)
  instances <- sample(ins, replace=TRUE, size=maxIns)
  # Return init list
  return(list(no.candidates=nrow(candidates), 
              no.tasks=length(instances),
              experiment.name=experiment.name, 
              extra.description=extra.description, 
              executable=executable, 
              instances=instances,
              candidates=candidates, 
              parameter.name.list=parameter.name.list,
              parameter.param.list=parameter.param.list,
              instances.extra.params = instances.extra.params
              ))
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
  ## These files are relative to .tune.execdir
  ## FIXME: make the paths absolute or relative to getwd() to print
  ## nicer error messages.
  hookRun <- paste (getwd(), .tune.hook.run, sep="/")
  if (as.logical(file.access(hookRun, mode = 1))) {
    stop ("hookRun `", hookRun, "' cannot be found or is not executable!\n")
  }

  hookInstanceFinished <- paste (getwd(), .tune.hook.instance.finished, sep="/")
  if (as.logical(file.access(hookInstanceFinished, mode = 1))) {
    stop ("hookInstanceFinished `", hookInstanceFinished,
          "' cannot be found or is not executable!\n")
  }
  
  ## FIXME: this probably can be calculated much earlier and there is
  ## no need to recalculate it here.
  parameters.num <- ncol (data$candidates)
  parameters.names <- names(data$candidates)
  stopifnot (parameters.num > 0)
  stopifnot (length(parameters.names) == parameters.num)
  
  cnd <- c()
  ins <- data$instances[task];

  extra.params <- ""
  if (!is.null (data$instances.extra.params) && !is.na (data$instances.extra.params[ins]))
    extra.params <- data$instances.extra.params[ins]

  ## FIXME: What is this really testing?
  if (candidate == which.alive[1]) {
    # FIXME: This is never used!
    #numJobs <- max(5, round(length(which.alive)/20))
    counter <- 0
    for (candi in which.alive)  {
      # First parameter is the candidate number, second is the instance file
      command <- paste (hookRun, candi, ins, extra.params)
      cnd <- data$candidates[candi, ]
      ## FIXME This could be for (p in seq_along(params)) { p$names, p$param, etc }
      ## Constructs the command line
      for (i in seq_len(parameters.num)) {
        param.value <- cnd[[i]]
        param.switch <- data$parameter.param.list[[i]]
        if (debug.level >= 2) {
          print (parameters.names[i])
          print (param.switch)
          print (param.value)
        }
        if (! is.na(param.value)) {
          if (is.numeric(param.value)) {
            param.value <- signif(param.value, signif.digit)
          }
          command <- paste(command, " ", param.switch, param.value, sep="")
        }
      }

      ## FIXME: for the future: Investigate the multicore package to execute
      ## nParallel (a new parameter) calls in parallel and collect the
      ## results. This would allow to execute on multi-core computers
      ## and also in the cluster by using 'qsub -sync y'

      ## Run the command
      if (debug.level >= 1) { cat (format(Sys.time(), usetz=TRUE), ":", command, "\n") }
      cwd <- setwd (.tune.execdir)
      command.exit.value <- system (command)
      setwd (cwd)
      if (command.exit.value) {
        stop("Command `", command, "' returned non-zero exit status (", command.exit.value, ")!\n")
      }
      if (debug.level >= 1) { cat (format(Sys.time(), usetz=TRUE), ": DONE\n") }
      counter <- counter + 1
    }
  }

  ## 
  ## FIXME: this should be silent
  cwd <- setwd (.tune.execdir)
  if (debug.level >= 1) {
    cat(paste (hookInstanceFinished, ins, candidate, "\n"))
  }
  output <- as.numeric (system (paste (hookInstanceFinished, ins, candidate),
                                intern = TRUE))
  setwd (cwd)
  if (debug.level >= 2) {
    print (output)
  }
  ## This should handle vectors of NAs
  if (any (is.na (output)))
    stop ("The output of `", hookInstanceFinished, " ", ins, " ", candidate,
          "' is not a number!\n")
  return (output)
}

race.describe <- function(candidate, data) {
  return (data$candidates[candidate, ])
}
