## Read some candidates from a file.
## Example of an input file,
## it should be readable with read.table( , header=TRUE).
## -------------------------------
## <param_name_1>  <param_name_2> 
##       1           "value_1"
##       2           "value_2"
## # This is a comment line
##       3           "value_3"         
## -------------------------------
## The order of the columns does not necessarily have to be the same
## as in the file containing the definition of the parameters.
##
## FIXME: What about digits?
readCandidatesFile <-
  function(fileName = stop("'fileName' is mandatory"), 
           parameters = stop("'parameters' is mandatory"), 
           debugLevel = 0)
{
  namesParameters <- names(parameters$constraints)

  # Read the file.
  candidateTable <- read.table(fileName, header = TRUE,
                               colClasses = "character",
                               stringsAsFactors = FALSE)
  stopifnot(is.data.frame(candidateTable))
  nbCandidates <- nrow(candidateTable)
  # Print the table that has been read.
  if (debugLevel >= 1) {
    cat("# Read ", nbCandidates, " candidates from file '", fileName, "'\n", sep="")
    if (debugLevel >= 2) {
      print(as.data.frame(candidateTable, stringAsFactor = FALSE))
    }
  }

  # FIXME: This should ignore fixed parameters. Only error out if they
  # are given with a different value.
  if (ncol(candidateTable) != length(namesParameters)
      || !setequal (colnames(candidateTable), namesParameters)) {
    tunerError("The parameter names given in the first row of file ", fileName,
               " do not match the parameter names: ",
               paste(namesParameters, collapse=", "))
    return(NULL)
  }

  # Loop over all candidates in candidateTable
  for (k in seq_len(nbCandidates)) {
    # Loop over all parameters, in an order taken from the constraints
    for (currentParameter in namesParameters) {
      currentValue <- candidateTable[k, currentParameter]
      currentType <- parameters$types[[currentParameter]]
      
      # Check the status of the constraints for this parameter to know
      # if it must be enabled or not.
      if (constraintsSatisfied(parameters, candidateTable[k, ], 
                               currentParameter)) {
        # Check that the value is among the valid ones.
        if (currentType == "i" || currentType == "r") {
          currentValue <- as.numeric(currentValue)
          lower <- oneParamLowerBound(currentParameter, parameters)
          upper <- oneParamUpperBound(currentParameter, parameters)
          if (is.na(currentValue)
              || currentValue < lower || currentValue > upper) {
            tunerError ("Candidate n. ", k, " from file ", fileName,
                        " is invalid because the value \"",
                        candidateTable[k, currentParameter],
                        "\" for the parameter ",
                        currentParameter, " is not within the valid range [",
                        lower,", ", upper,"]")
            return(NULL)
          }
          # For integers, only accept an integer.
          if (currentType == "i" && as.integer(currentValue) != currentValue) {
            tunerError ("Candidate n. ", k, " from file ", fileName,
                        " is invalid because parameter ", currentParameter,
                        " is of type integer but its value ",
                        currentValue, " is not an integer")
            return(NULL)
          }
          # type == "o" or "c"
        } else if (!(currentValue %in% oneParamBoundary(currentParameter, parameters))) {
          tunerError ("Candidate n. ", k, " from file ", fileName,
                      " is invalid because the value \"",
                      currentValue, "\" for the parameter \"",
                      currentParameter, "\" is not among the valid values: (\"",
                      paste(oneParamBoundary(currentParameter, parameters),
                            collapse="\", \""),
                      "\")")
          return(NULL)
        }
        
      } else if (!is.na(currentValue)) {
        tunerError ("Candidate n. ", k, " from file ", fileName,
                    " is invalid because parameter \"", currentParameter,
                    "\" is not enabled but its value is \"",
                    currentValue, "\" instead of NA")
        return(NULL)
      }
    }
  }
  # Reorder columns.
  candidateTable <- candidateTable[, namesParameters, drop = FALSE]
  # Fix up numeric columns.
  for (currentParameter in namesParameters) {
    type <- parameters$types[[currentParameter]]
    if (currentType == "i" || currentType == "r") {
      candidateTable[, currentParameter] <- as.numeric(candidateTable[, currentParameter])
    }
  }
  return (candidateTable)
}

readConfiguration <- function(filename = "", configuration = list())
{
  parameters <- rownames(.irace.params.def)[rownames(.irace.params.def) != ""]

  # First find out which file...
  if (filename == ""
      && file.exists(.irace.params.def["configurationFile","default"])) {
    filename <- .irace.params.def["configurationFile","default"]
    cat("Warning: A default configuration file '", 
        filename, "' has been found and will be read\n")
  }

  if (filename != "") {
    if (file.exists (filename)) {
      # FIXME: subordinate on debuglevel
      cat ("Note: Reading configuration file '", filename, "'.......")
      source(filename, local = TRUE)
      # FIXME: subordinate on debuglevel
      cat (" done!\n")
    } else {
      stop ("The configuration file ", filename, " does not exist.")
    }
  }
  ## read configuration file variables
  for (param in parameters) {
    configuration[[param]] <- ifelse (exists (param, inherits = FALSE),
                                      get(param, inherits = FALSE), NA)
  }
  return (configuration)
}

checkConfiguration <- function(configuration)
{
  ## Check that everything is fine with external parameters
  # Check that the files exists and are readable.
  configuration$parameterFile <- path.rel2abs(configuration$parameterFile)
  # We don't check this file here because the user may give the
  # parameters explicitly. And it is checked in readParameters anyway.

  configuration$execDir <- path.rel2abs(configuration$execDir)
  file.check (configuration$execDir, isdir = TRUE, text = "execution directory")

  if (is.function.name(configuration$hookRun)) {
    .irace$hook.run <- configuration$hookRun
  } else if (is.character(configuration$hookRun)) {
    configuration$hookRun <- path.rel2abs(configuration$hookRun)
    file.check (configuration$hookRun, executable = TRUE,
                text = "run program hook")
    .irace$hook.run <- hook.run.default
  } else {
    stop("hookRun must be a function or an executable program")
  }
  
  if (configuration$hookEvaluate == "") configuration$hookEvaluate <- NULL
  if (is.null(configuration$hookEvaluate)) {
    .irace$hook.evaluate <- NULL
  } else if (is.function.name(configuration$hookEvaluate)) {
    .irace$hook.evaluate <- configuration$hookEvaluate
  } else if (is.character(configuration$hookEvaluate)) {
    configuration$hookEvaluate <- path.rel2abs(configuration$hookEvaluate)
    file.check (configuration$hookEvaluate, executable = TRUE,
                text = "evaluate hook")
    .irace$hook.evaluate <- hook.evaluate.default
  } else {
    stop("hookEvaluate must be a function or an executable program")
  }

  if (is.null.or.empty(configuration$instances.extra.params)) {
    configuration$instances.extra.params <- NULL
  }
  
  if (is.null.or.empty(configuration$instances)) {
    configuration$instanceDir <- path.rel2abs(configuration$instanceDir)
    instance.dir <- canonical.dirname (configuration$instanceDir)
    if (!is.null(configuration$instanceFile) && configuration$instanceFile != "") {
      configuration$instanceFile <- path.rel2abs(configuration$instanceFile)
      file.check (configuration$instanceFile, readable = TRUE,
                  text = "instance file")
      lines <- readLines (configuration$instanceFile)
      lines <- sub("#.*$", "", lines) # Remove comments
      lines <- sub("^[[:space:]]+", "", lines) # Remove extra spaces
      lines <- lines[lines != ""] # Delete empty lines
      instances <- sub("^([^[:space:]]+).*$", "\\1", lines)
      instances <- paste (instance.dir, instances, sep="")
      instances.extra.params <- sub("^[^[:space:]]+(.*)$", "\\1", lines)
      names (instances.extra.params) <- instances
      configuration$instances.extra.params <- instances.extra.params
    } else {
      file.check (configuration$instanceDir, isdir = TRUE, notempty = TRUE,
                  text = "instances directory")
      # The files are sorted in alphabetical order, on the full path if
      # 'full.names = TRUE'.
      instances <- list.files (path = instance.dir, full.names = TRUE,
                               recursive = TRUE)
      if (length (instances) == 0)
        tunerError("No instances found in `", instance.dir, "' !\n")
    }
    configuration$instances <- instances
  }
  
  if (!is.null.or.empty(configuration$candidatesFile)) {
    configuration$candidatesFile <- path.rel2abs(configuration$candidatesFile)
    file.check (configuration$candidatesFile, readable = TRUE,
                text = "candidates file")
  }

  # We have characters everywhere, set to the right types to avoid
  # problem later.
  intParams <- c("maxExperiments", "digits", "debugLevel",
                 "nbIterations", "nbExperimentsPerIteration",
                 "firstTest", "eachTest", "minNbSurvival", "nbCandidates",
                 "mu", "timeBudget", "timeEstimate", "seed",
                 "parallel")

  # TODO: Avoid the for-loop using lapply and configuration[intParams]
  for (param in intParams) {
    if (is.na(configuration[[param]]))
      next # Allow NA default values
    if (!is.null(configuration[[param]]))
      configuration[[param]] <- suppressWarnings(as.numeric(configuration[[param]]))
    if (is.null(configuration[[param]])
        || is.na (configuration[[param]])
        || !is.wholenumber(configuration[[param]]))
      tunerError ("'", param, "' must be an integer.")
  }

  if (configuration$mu < configuration$firstTest) {
    warning("Assuming 'mu <- firstTest' because 'mu' cannot be lower than 'firstTest'")
    configuration$mu <- configuration$firstTtest
  }
  
  as.boolean.param <- function(x, name, params)
  {
    tmp <- as.integer(x)
    if (is.na (tmp) || (tmp != 0 && tmp != 1)) {
      tunerError ("'", name, "' (", params[name, "long"],
                  ") must be either 0 or 1")
    }
    return(as.logical(tmp))
  }
  for (p in c("sampleInstances", "sgeCluster", "softRestart", "mpi")) {
    configuration[[p]] <- as.boolean.param (configuration[[p]], p, .irace.params.def)
  }

  if (configuration$mpi && configuration$parallel < 2) {
    tunerError ("'parallel' (", .irace.params.def["parallel","long"],
                ") must be larger than 1 when mpi is enabled")
  }

  if (configuration$sgeCluster && configuration$mpi) {
    tunerError("'mpi' (", .irace.params.def["mpi","long"], ") and ",
               "'sgeCluster' (", .irace.params.def["sgeCluster","long"], ") ",
               "cannot be enabled at the same time")
  }

  if (configuration$sgeCluster && configuration$parallel > 1) {
    tunerError("It does not make sense to use ",
               "'parallel' (", .irace.params.def["parallel","long"], ") and ",
               "'sgeCluster' (", .irace.params.def["sgeCluster","long"], ") ",
               "at the same time")
  }
  
  if (configuration$timeBudget > 0 && configuration$timeEstimate <= 0) {
    tunerError ("When using 'timeBudget' (", .irace.params.def["timeBudget","long"],
                "), 'timeEstimate' (",
                .irace.params.def["timeEstimate","long"], ") must be larger than zero")
  }

  if (configuration$timeBudget <= 0 && configuration$timeEstimate > 0) {
    tunerError ("When using 'timeEstimate' (", .irace.params.def["timeEstimate","long"],
                "), 'timeBudget' (",
                .irace.params.def["timeBudget","long"], ") must be larger than zero")
  }

  if (tolower(configuration$testType) %in% c("f-test", "friedman")) {
    configuration$testType <- "friedman"
  } else if (tolower(configuration$testType) %in% c("t-test", "t.none")) {
    configuration$testType <- "t.none"
  } else {
    tunerError ("invalid setting '", configuration$testType,
                "' of 'testType' (", .irace.params.def["testType","long"],
                "), valid values are: F-test, t-test")
  }
  
  return (configuration)
}

printConfiguration <- function(tunerConfig)
{
  cat("###   CONFIGURATION STATE TO BE USED\n")
  for (param in names(tunerConfig)) {
    value <- ifelse(is.character(tunerConfig[[param]]),
                    paste("\"", tunerConfig[[param]], "\"", sep=""),
                    tunerConfig[[param]])
    cat(param, "<-", value, "\n")
  }
  cat("### end of configuration\n")
}

defaultConfiguration <- function(config = list())
{
  config.names <- rownames(.irace.params.def)[rownames(.irace.params.def) != ""]
  if (!is.null(names(config)) && !all(names(config) %in% config.names)) {
    stop("Unknown configuration parameters: ",
         paste(names(config)[which(!names(config) %in% config.names)], sep=", "))
  }

  for (k in config.names) {
    if (is.null.or.na(config[[k]])) {
      config[[k]] <- .irace.params.def[k, "default"]
    }
  }
  return (config)
}
