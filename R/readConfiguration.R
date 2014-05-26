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
  namesParameters <- names(parameters$conditions)

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

  # Reorder columns.
  candidateTable <- candidateTable[, namesParameters, drop = FALSE]
  # Fix up numeric columns.
  for (currentParameter in namesParameters) {
    type <- parameters$types[[currentParameter]]
    if (type == "i" || type == "r") {
      candidateTable[, currentParameter] <- suppressWarnings(as.numeric(candidateTable[, currentParameter]))
    }
  }

  # Loop over all candidates in candidateTable
  for (k in seq_len(nbCandidates)) {
    # Loop over all parameters, in an order taken from the conditions
    for (currentParameter in namesParameters) {
      currentValue <- candidateTable[k, currentParameter]
      type <- parameters$types[[currentParameter]]

      # Check the status of the conditions for this parameter to know
      # if it must be enabled or not.
      if (conditionsSatisfied(parameters, candidateTable[k, ], 
                              currentParameter)) {
        # Check that the value is among the valid ones.
        if (type == "i" || type == "r") {
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
          if (type == "i" && as.integer(currentValue) != currentValue) {
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
                    "\" is not enabled because of condition \"",
                    parameters$conditions[[currentParameter]],
                    "\" but its value is \"",
                    currentValue, "\" instead of NA")
        return(NULL)
      }
    }
  }
  return (candidateTable)
}

# reads configuration from filename and returns it as a list. Anything
# not mentioned in the file is not present in the list (that is, it is
# NULL).
# FIXME: Does passing an initial configuration actually work? It seems
# it gets completely overriden by the loop below.
readConfiguration <- function(filename = "", configuration = list())
{
  # First find out which file...
  if (filename == ""
      && file.exists(.irace.params.def["configurationFile", "default"])) {
    filename <- .irace.params.def["configurationFile","default"]
    cat("Warning: A default configuration file", shQuote(filename),
        "has been found and will be read\n")
  }

  if (filename != "") {
    if (file.exists (filename)) {
      debug.level <- getOption(".irace.debug.level", default = 0)
      if (debug.level >= 1)
        cat ("# Reading configuration file", shQuote(filename), ".......")
      source(filename, local = TRUE)
      if (debug.level >= 1) cat (" done!\n")
    } else {
      stop ("The configuration file ", shQuote(filename), " does not exist.")
    }
  }
  ## read configuration file variables
  for (param in .irace.params.names) {
    configuration[[param]] <- if (exists (param, inherits = FALSE))
      get(param, inherits = FALSE) else NULL
  }
  return (configuration)
}

## FIXME: This function should only do checks and return
## TRUE/FALSE. There should be other function that does the various
## transformations.
checkConfiguration <- function(configuration = defaultConfiguration())
{
  # Fill possible unset (NULL) with default settings.
  configuration <- defaultConfiguration (configuration)
  
  ## Check that everything is fine with external parameters
  # Check that the files exist and are readable.
  configuration$parameterFile <- path.rel2abs(configuration$parameterFile)
  # We don't check this file here because the user may give the
  # parameters explicitly. And it is checked in readParameters anyway.
  configuration$execDir <- path.rel2abs(configuration$execDir)
  file.check (configuration$execDir, isdir = TRUE, text = "execution directory")

  if (configuration$recoveryFile == "")
    configuration$recoveryFile  <- NULL
  if (!is.null(configuration$recoveryFile)) {
    configuration$recoveryFile <- path.rel2abs(configuration$recoveryFile)
    file.check(configuration$recoveryFile, readable = TRUE,
               text = "recovery file")
  }

  if (!is.null.or.empty(configuration$logFile)) {
    configuration$logFile <- path.rel2abs(configuration$logFile,
                                          cwd = configuration$execDir)
  }
  
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

  if (!is.null.or.empty(configuration$forbiddenFile)) {
    configuration$forbiddenFile <- path.rel2abs(configuration$forbiddenFile)
    file.check (configuration$forbiddenFile, readable = TRUE,
                text = "forbidden candidates file")
    # FIXME: Using && or || instead of & and | will not work. Detect
    # this and give an error to the user.
    configuration$forbiddenExps <- parse(file=configuration$forbiddenFile)
    # When a is NA and we check a == 5, we would get NA, which is
    # always FALSE, when we actually want to be TRUE, so we test
    # is.na() first below.
    configuration$forbiddenExps <-
      sapply(configuration$forbiddenExps,
             function(x) substitute(is.na(x) | !(x), list(x = x)))
    # FIXME: Check that the parameter names that appear in forbidden
    # all appear in parameters$names to catch typos.
  }

  # Make it NULL if it is "" or NA
  # FIXME: If it is a non-empty vector of strings, parse them as above.
  if (is.null.or.empty(configuration$forbiddenExps) || is.null.or.na(configuration$forbiddenExps))
    configuration$forbiddenExps <- NULL

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

  configuration$confidence <- suppressWarnings(as.numeric(configuration$confidence))
  if (is.null(configuration$confidence)
      || is.na (configuration$confidence)
      || configuration$confidence < 0.0 || configuration$confidence > 1.0)
    tunerError ("'confidence' must be a real value within [0, 1].")


  if (configuration$firstTest %% configuration$eachTest != 0) {
    tunerError("firstTest (", .irace.params.def["firstTest", "long"],
               ") must be a multiple of eachTest (",
               .irace.params.def["eachTest", "long"], ")")
  }
  
  if (configuration$mu < configuration$firstTest) {
    if (configuration$debugLevel >= 1) {
      cat("Warning: Assuming 'mu <- firstTest' because 'mu' cannot be lower than 'firstTest'\n")
    }
    configuration$mu <- configuration$firstTest
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
    tunerError("'mpi' (", .irace.params.def["mpi", "long"], ") and ",
               "'sgeCluster' (", .irace.params.def["sgeCluster", "long"], ") ",
               "cannot be enabled at the same time")
  }

  if (configuration$sgeCluster && configuration$parallel > 1) {
    tunerError("It does not make sense to use ",
               "'parallel' (", .irace.params.def["parallel", "long"], ") and ",
               "'sgeCluster' (", .irace.params.def["sgeCluster", "long"], ") ",
               "at the same time")
  }
  
  if (configuration$timeBudget > 0 && configuration$timeEstimate <= 0) {
    tunerError ("When using 'timeBudget' (",
                .irace.params.def["timeBudget", "long"], "), 'timeEstimate' (",
                .irace.params.def["timeEstimate", "long"],
                ") must be larger than zero")
  }

  if (configuration$timeBudget <= 0 && configuration$timeEstimate > 0) {
    tunerError ("When using 'timeEstimate' (",
                .irace.params.def["timeEstimate", "long"], "), 'timeBudget' (",
                .irace.params.def["timeBudget", "long"],
                ") must be larger than zero")
  }

  if (tolower(configuration$testType) %in% c("f-test", "friedman")) {
    configuration$testType <- "friedman"
  } else if (tolower(configuration$testType) %in% c("t-test", "t.none")) {
    configuration$testType <- "t.none"
  } else {
    tunerError ("invalid setting '", configuration$testType,
                "' of 'testType' (", .irace.params.def["testType", "long"],
                "), valid values are: F-test, t-test")
  }
  
  return (configuration)
}

printConfiguration <- function(configuration)
{
  cat("## irace configuration:\n")
  for (param in .irace.params.names) {
    # Special case for instances.extra.params
    if (param == "instances.extra.params") {
      if (is.null.or.empty(paste(configuration$instances.extra.params, collapse=""))) {
        cat (param, "<- NULL\n")
      } else {
        cat (param, "<-\n")
        cat(paste(names(configuration$instances.extra.params), configuration$instances.extra.params, sep=" : "), sep="\n")
      }
    } else if (param == "instances") {# Special case for instances
      cat (param, "<- \"")
      cat (configuration$instances, sep=", ")
      cat ("\"\n")
    } else {# All other parameters (no vector, but can be functions)
      # FIXME: Perhaps deparse() is not the right way to do this?
      cat(param, "<-", deparse(configuration[[param]]), "\n")
    }
  }
  cat("## end of irace configuration\n")
}

defaultConfiguration <- function(configuration = list())
{
  if (!is.null(names(configuration))
      && !all(names(configuration) %in% .irace.params.names)) {
    stop("Unknown configuration parameters: ",
         paste(names(configuration)[which(!names(configuration)
                                          %in% .irace.params.names)],
               sep=", "))
  }

  for (k in .irace.params.names) {
    if (is.null.or.na(configuration[[k]])) {
      configuration[[k]] <- .irace.params.def[k, "default"]
    }
  }
  return (configuration)
}
