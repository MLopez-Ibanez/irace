## Read some candidates from a file.
## Example of an input file,
## it should be readable with read.table( , head=TRUE).
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
## TODO: What about signifDigits?
readCandidatesFile <-
  function(fileName = stop("'fileName' is mandatory"), 
           parameters = stop("'parameters' is mandatory"), 
           debugLevel = 0)
{
  namesParameters <- names(parameters$constraints)

  # Read the file.
  candidateTable <- read.table(fileName, head = TRUE,
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
      # FIXME: conditional on debuglevel
      cat ("Note: Reading configuration file '", filename, "'.......")
      source(filename, local = TRUE)
      # FIXME: conditional on debuglevel
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
  for (param in c("parameterFile", "execDir", "instanceDir", "instanceFile",
                  "hookRun", "hookEvaluate","candidatesFile")) {
    configuration[[param]] <- path.rel2abs (configuration[[param]])
  }
  ## Check that everything is fine with external parameters
  file.check <- function (file, executable = FALSE, readable = executable,
                          isdir = FALSE, notempty = FALSE,
                          text = NULL)
    {
      EXEC <- 1 # See documentation of the function file.access()
      READ <- 4

      if (!file.exists(file)) {
        stop (text, " '", file, "' does not exist")
        return(FALSE)
      }
      if (readable && (file.access(file, mode = READ) != 0)) {
        stop(text, " '", file, "' is not readable")
        return (FALSE)
      }
      if (executable && file.access(file, mode = EXEC) != 0) {
        stop(text, " '", file, "' is not executable")
        return (FALSE)
      }

      if (isdir) {
        if (!file.info(file)$isdir) {
          stop(text, " '", file, "' is not a directory")
          return (FALSE)
        }
        if (notempty && length(list.files (file, recursive=TRUE)) == 0) {
          stop(text, " '", file, "' does not contain any file")
          return (FALSE)
        }
        
      }
      return (TRUE)
    }
  # Check that the files exists and are readable.
  file.check (configuration$parameterFile, readable= TRUE,
              text = "parameter file")
  file.check (configuration$execDir, isdir = TRUE,
              text = "execution directory")
  
  file.check (configuration$hookRun, executable = TRUE,
              text = "run program hook")
  file.check (configuration$hookEvaluate, executable = TRUE,
              text = "evaluate hook")
  
  if (!is.null(configuration$instanceFile)
      && configuration$instanceFile != "") {
    file.check (configuration$instanceFile, readable = TRUE,
                text = "instance file")
  }
  else {
    file.check (configuration$instanceDir, isdir = TRUE, notempty = TRUE,
                text = "instances directory")
  }
  
  if (!is.null(configuration$candidatesFile)
      && configuration$candidatesFile != "") {
    file.check (configuration$candidatesFile, readable = TRUE,
                text = "candidates file")
  }

  # We have characters everywhere, set to the right types to avoid
  # problem later.
  intParams <- c("maxNbExperiments", "signifDigits", "debugLevel",
                 "nbIterations", "nbExperimentsPerIteration",
                 "firstTest", "eachTest", "minNbSurvival", "nbCandidates",
                 "mu", "timeBudget", "timeEstimate", "seed",
                 "parallel")

  # TODO: Avoid the for-loop using lapply and configuration[intParams]
  for (param in intParams) {
    if (is.na(configuration[[param]]))
      next # Allow NA default values
    if (is.null(configuration[[param]]))
      tunerError ("'", param, "' must be an integer.")
    configuration[[param]] <- suppressWarnings(as.integer(configuration[[param]]))
    if (is.na (configuration[[param]]))
      tunerError ("'", param, "' must be an integer.")
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

  if (grepl("f-test", tolower(configuration$testType), fixed = TRUE)) {
    configuration$testType <- "friedman"
  } else if (grepl("t-test", tolower(configuration$testType), fixed = TRUE)) {
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

defaultConfiguration <- function(configuration)
{
  for (param in names(configuration)) {
    if (is.na(configuration[[param]])) {
      configuration[[param]] <- .irace.params.def[param, "default"]
    }
  }
  return (configuration)
}
