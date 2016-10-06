## Read some configurations from a file.
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
readConfigurationsFile <- function(filename, parameters, debugLevel = 0)
{
  namesParameters <- names(parameters$conditions)
  
  # Read the file.
  configurationTable <- read.table(filename, header = TRUE,
                               colClasses = "character",
                               stringsAsFactors = FALSE)
  irace.assert(is.data.frame(configurationTable))
  nbConfigurations <- nrow(configurationTable)
  # Print the table that has been read.
  if (debugLevel >= 2) {
    cat("# Read ", nbConfigurations, " configurations from file '", filename, "'\n", sep="")
    print(as.data.frame(configurationTable, stringAsFactor = FALSE))
  }

  # This ignores fixed parameters unless they are given with a different value.
  if (ncol(configurationTable) != length(namesParameters)
      || !setequal (colnames(configurationTable), namesParameters)) {
    # Column names must match a parameter, including fixed ones.
    missing <- setdiff (colnames(configurationTable), namesParameters)
    if (length(missing) > 0) {
      irace.error("The parameter names (",
                  strlimit(paste(missing, collapse=", ")),
                  ") given in the first row of file ", filename,
                  " do not match the parameter names: ",
                  paste(namesParameters, collapse=", "))
      return(NULL)
    }

    # All non-fixed parameters must appear in column names.
    # FIXME: varParameters <- parameters$names[!parameters$isFixed]
    varParameters <- parameters$names[!unlist(parameters$isFixed)]
    missing <- setdiff (varParameters, colnames(configurationTable))
    if (length(missing) > 0) {
      irace.error("The parameter names (",
                  strlimit(paste(missing, collapse=", ")),
                  ") are missing from the first row of file ", filename)
      return(NULL)
    }
    # Add any missing fixed parameters.
    missing <- setdiff (namesParameters, colnames(configurationTable))
    if (length(missing) > 0) {
      irace.assert (all(parameters$isFixed[missing]))
      tmp <- lapply(missing, function(x) get.fixed.value(x, parameters))
      names(tmp) <- missing
      configurationTable <- cbind.data.frame(configurationTable, tmp,
                                         stringsAsFactors = FALSE)
    }
  }

  # Reorder columns.
  configurationTable <- configurationTable[, namesParameters, drop = FALSE]
  # Fix up numeric columns.
  for (currentParameter in namesParameters) {
    type <- parameters$types[[currentParameter]]
    if (type == "i" || type == "r") {
      configurationTable[, currentParameter] <- suppressWarnings(as.numeric(configurationTable[, currentParameter]))
    }
  }

  # Loop over all configurations in configurationTable
  for (k in seq_len(nbConfigurations)) {
    # Loop over all parameters, in an order taken from the conditions
    for (currentParameter in namesParameters) {
      currentValue <- configurationTable[k, currentParameter]
      type <- parameters$types[[currentParameter]]

      # Check the status of the conditions for this parameter to know
      # whether it must be enabled.
      if (conditionsSatisfied(parameters, configurationTable[k, ], 
                              currentParameter)) {
        # Check that the value is among the valid ones.
        if (type == "i" || type == "r") {
          currentValue <- as.numeric(currentValue)
          lower <- paramLowerBound(currentParameter, parameters)
          upper <- paramUpperBound(currentParameter, parameters)
          if (is.na(currentValue)
              || currentValue < lower || currentValue > upper) {
            irace.error ("Configuration number ", k, " from file ", filename,
                         " is invalid because the value \"",
                         configurationTable[k, currentParameter],
                         "\" for the parameter ",
                         currentParameter, " is not within the valid range [",
                         lower,", ", upper,"]")
            return(NULL)
          }
          # For integers, only accept an integer.
          if (type == "i" && as.integer(currentValue) != currentValue) {
            irace.error ("Configuration number ", k, " from file ", filename,
                         " is invalid because parameter ", currentParameter,
                         " is of type integer but its value ",
                         currentValue, " is not an integer")
            return(NULL)
          }
          # type == "o" or "c"
        } else if (!(currentValue %in% paramDomain(currentParameter, parameters))) {
          irace.error ("Configuration number ", k, " from file ", filename,
                       " is invalid because the value \"",
                       currentValue, "\" for the parameter \"",
                       currentParameter, "\" is not among the valid values: (\"",
                       paste(paramDomain(currentParameter, parameters),
                             collapse="\", \""),
                       "\")")
          return(NULL)
        }
        
      } else if (!is.na(currentValue)) {
        irace.error ("Configuration number ", k, " from file ", filename,
                     " is invalid because parameter \"", currentParameter,
                     "\" is not enabled because of condition \"",
                     parameters$conditions[[currentParameter]],
                     "\" but its value is \"",
                    currentValue, "\" instead of NA")
        return(NULL)
      }
    }
  }
  return (configurationTable)
}

# reads scenario setup from filename and returns it as a list. Anything
# not mentioned in the file is not present in the list (that is, it is
# NULL).
# FIXME: Does passing an initial scenario actually work? It seems
# it gets completely overriden by the loop below.
readScenario <- function(filename = "", scenario = list())
{
  # First find out which file...
  if (filename == "") {
    filename <- .irace.params.def["scenarioFile","default"]
    if (file.exists(filename)) {
      cat("Warning: A default scenario file", shQuote(filename),
          "has been found and will be read\n")
    } else {
      irace.error ("Not scenario file given (use ",
                   .irace.params.def["scenarioFile", "short"], " or ",
                   .irace.params.def["scenarioFile", "long"],
                   ") and no default scenario file ", shQuote(filename),
                   " has been found.")
    }
  }
  if (file.exists (filename)) {
    debug.level <- getOption(".irace.debug.level", default = 0)
    if (debug.level >= 1)
      cat ("# Reading scenario file", shQuote(filename), ".......")
    source(filename, local = TRUE)
    if (debug.level >= 1) cat (" done!\n")
  } else {
    irace.error ("The scenario file ", shQuote(filename), " does not exist.")
  }
  ## read scenario file variables
  # If these are given and relative, they should be relative to the
  # scenario file (except logFile, which is relative to execDir).
  pathParams <- setdiff(.irace.params.def[.irace.params.def[, "type"] == "p",
                                          "name"], "logFile")
  for (param in .irace.params.names) {
    if (exists (param, inherits = FALSE)) {
      value <- get(param, inherits = FALSE)
      if (!is.null.or.empty(value) && is.character(value)
          && (param %in% pathParams)) {
        value <- path.rel2abs(value, cwd = dirname(filename))
      }
      scenario[[param]] <- value
    }
  }
  return (scenario)
}

## FIXME: This function should only do checks and return
## TRUE/FALSE. There should be other function that does the various
## transformations.
checkScenario <- function(scenario = defaultScenario())
{
  # Fill possible unset (NULL) with default settings.
  scenario <- defaultScenario (scenario)
  
  ## Check that everything is fine with external parameters
  # Check that the files exist and are readable.
  scenario$parameterFile <- path.rel2abs(scenario$parameterFile)
  # We don't check this file here because the user may give the
  # parameters explicitly. And it is checked in readParameters anyway.
  scenario$execDir <- path.rel2abs(scenario$execDir)
  file.check (scenario$execDir, isdir = TRUE, text = "execution directory")
  
  if (!is.null.or.empty(scenario$logFile)) {
    scenario$logFile <- path.rel2abs(scenario$logFile, cwd = scenario$execDir)
  }

  if (scenario$recoveryFile == "")
    scenario$recoveryFile  <- NULL
  if (!is.null(scenario$recoveryFile)) {
    scenario$recoveryFile <- path.rel2abs(scenario$recoveryFile)
    file.check(scenario$recoveryFile, readable = TRUE,
               text = "recovery file")
    if (scenario$recoveryFile == scenario$logFile) {
      irace.error("log file and recovery file should be different ('",
                  scenario$logFile, "'")
    }
  }


  if (is.null.or.empty(scenario$targetRunnerParallel)) {
    scenario$targetRunnerParallel <- NULL
  } else if (!is.function.name(scenario$targetRunnerParallel)) {
    irace.error("targetRunnerParallel must be a function")
  }

  if (is.function.name(scenario$targetRunner)) {
    .irace$target.runner <- scenario$targetRunner
  } else if (is.null(scenario$targetRunnerParallel)) {
    if (is.character(scenario$targetRunner)) {
      scenario$targetRunner <- path.rel2abs(scenario$targetRunner)
      file.check (scenario$targetRunner, executable = TRUE,
                  text = "target runner")
      .irace$target.runner <- target.runner.default
    } else {
      irace.error("targetRunner must be a function or an executable program")
    }
  }
  
  if (scenario$targetEvaluator == "") scenario$targetEvaluator <- NULL
  if (is.null(scenario$targetEvaluator)) {
    .irace$target.evaluator <- NULL
  } else if (is.function.name(scenario$targetEvaluator)) {
    .irace$target.evaluator <- scenario$targetEvaluator
  } else if (is.character(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- path.rel2abs(scenario$targetEvaluator)
    file.check (scenario$targetEvaluator, executable = TRUE,
                text = "target evaluator")
    .irace$target.evaluator <- target.evaluator.default
  } else {
    irace.error("targetEvaluator must be a function or an executable program")
  }
  
  # Training instances
  if (is.null.or.empty(scenario$instances.extra.params)) {
    scenario$instances.extra.params <- NULL
  }
  if (is.null.or.empty(scenario$instances)) {
    scenario$trainInstancesDir <- path.rel2abs(scenario$trainInstancesDir)
    if (!is.null.or.empty(scenario$trainInstancesFile)) {
      scenario$trainInstancesFile <- path.rel2abs(scenario$trainInstancesFile)
    }
    scenario[c("instances", "instances.extra.params")] <-
      readInstances(instancesDir = canonical.dirname (scenario$trainInstancesDir),
                    instancesFile = scenario$trainInstancesFile)
  }
  
  # Testing instances
  if (is.null.or.empty(scenario$testInstances.extra.params)) {
    scenario$testInstances.extra.params <- NULL
  }

  if (is.null.or.empty(scenario$testInstances)) {
    if (!is.null.or.empty(scenario$testInstancesDir) || 
        !is.null.or.empty(scenario$testInstancesFile)) {
      scenario$testInstancesDir <- path.rel2abs(scenario$testInstancesDir)
      if (!is.null.or.empty(scenario$testInstancesFile)) {
        scenario$testInstancesFile <- path.rel2abs(scenario$testInstancesFile)
      }
      scenario[c("testInstances", "testInstances.extra.params")] <-
        readInstances(instancesDir = canonical.dirname (scenario$testInstancesDir),
                      instancesFile = scenario$testInstancesFile)
    } else {
      scenario$testInstances <- NULL
    }
  }
  
  # Configurations file
  if (!is.null.or.empty(scenario$configurationsFile)) {
    scenario$configurationsFile <- path.rel2abs(scenario$configurationsFile)
    file.check (scenario$configurationsFile, readable = TRUE,
                text = "configurations file")
  }

  # This prevents loading the file two times and overriding forbiddenExps if
  # the user specified them explicitly.
  if (is.null.or.empty(scenario$forbiddenExps)
      && !is.null.or.empty(scenario$forbiddenFile)) {
    scenario$forbiddenFile <- path.rel2abs(scenario$forbiddenFile)
    file.check (scenario$forbiddenFile, readable = TRUE,
                text = "forbidden configurations file")
    # FIXME: Using && or || instead of & and | will not work. Detect
    # this and give an error to the user.
    scenario$forbiddenExps <- parse(file=scenario$forbiddenFile)
    # When a is NA and we check a == 5, we would get NA, which is
    # always FALSE, when we actually want to be TRUE, so we test
    # is.na() first below.
    scenario$forbiddenExps <-
      sapply(scenario$forbiddenExps,
             function(x) substitute(is.na(x) | !(x), list(x = x)))
    # FIXME: Check that the parameter names that appear in forbidden
    # all appear in parameters$names to catch typos.
    cat("# ", length(scenario$forbiddenExps),
        " expression(s) specifying forbidden scenarios read from '",
        scenario$forbiddenFile, "'\n", sep="")
  }

  # Make it NULL if it is "" or NA
  # FIXME: If it is a non-empty vector of strings, parse them as above.
  if (is.null.or.empty(scenario$forbiddenExps) || is.null.or.na(scenario$forbiddenExps))
    scenario$forbiddenExps <- NULL

  # We have characters everywhere, set to the right types to avoid
  # problem later.

  # Integer control parameters
  intParams <- .irace.params.def[.irace.params.def[, "type"] == "i", "name"]
  for (param in intParams) {
    if (is.na(scenario[[param]]))
      next # Allow NA default values
    if (!is.null(scenario[[param]]))
      scenario[[param]] <- suppressWarnings(as.numeric(scenario[[param]]))
    if (is.null(scenario[[param]])
        || is.na (scenario[[param]])
        || !is.wholenumber(scenario[[param]]))
      irace.error ("'", param, "' must be an integer.")
  }
  
  if (scenario$firstTest %% scenario$eachTest != 0) {
    irace.error("firstTest (", .irace.params.def["firstTest", "long"],
               ") must be a multiple of eachTest (",
               .irace.params.def["eachTest", "long"], ")")
  }
  
  if (scenario$mu < scenario$firstTest) {
    if (scenario$debugLevel >= 1) {
      cat("Warning: Assuming 'mu = firstTest' because 'mu' cannot be lower than 'firstTest'\n")
    }
    scenario$mu <- scenario$firstTest
  }
  
  # Real [0, 1] control parameters
  realParams <- .irace.params.def[.irace.params.def[, "type"] == "r", "name"]
  for (param in realParams) {
    if (is.na(scenario[[param]]))
      next # Allow NA default values
    if (!is.null(scenario[[param]]))
      scenario[[param]] <- suppressWarnings(as.numeric(scenario[[param]]))
    if (is.null(scenario[[param]])
        || is.na (scenario[[param]])
        || scenario[[param]] < 0.0 || scenario[[param]] > 1.0)
      irace.error ("'", param, "' must be a real value within [0, 1].")
  }
  
  ## Only maxExperiments or maxTime should be set. Negative values are not allowed.
  if (scenario$maxExperiments == 0 && scenario$maxTime == 0) {
    irace.error("Tuning budget was not provided. Set maxExperiments (",
                .irace.params.def["maxExperiments", "long"], ") or maxTime (",
                .irace.params.def["maxTime", "long"], ").\n" )
  } else if (scenario$maxExperiments > 0 && scenario$maxTime > 0) {
    irace.error("Two different tuning budgets provided, please set only maxExperiments (",
                .irace.params.def["maxExperiments", "long"], ") or only maxTime (",
                .irace.params.def["maxTime", "long"], ").\n" )
  } else if (scenario$maxExperiments < 0 ) {
    irace.error("Negative budget provided, maxExperiments (",
                .irace.params.def["maxExperiments", "long"], ") must be >= 0.\n" )
  } else if (scenario$maxTime < 0) {
    irace.error("Negative budget provided, maxTime  (",
                .irace.params.def["maxTime", "long"], ") must be >= 0.\n" )
  }
  
  if (scenario$maxTime > 0 && (scenario$budgetEstimation <= 0 || scenario$budgetEstimation >= 1)) 
    irace.error("budgetEstimation (", .irace.params.def["budgetEstimation", "long"],
                ") must be within (0,1).")
  
  if (is.na (scenario$softRestartThreshold)) {
    scenario$softRestartThreshold <- 10^(- scenario$digits)
  }
  
  # Boolean control parameters
  as.boolean.param <- function(x, name, params)
  {
    tmp <- as.integer(x)
    if (is.na (tmp) || (tmp != 0 && tmp != 1)) {
      irace.error ("'", name, "' (", params[name, "long"],
                   ") must be either 0 or 1")
    }
    return(as.logical(tmp))
  }
  boolParams <- .irace.params.def[.irace.params.def[, "type"] == "b", "name"]
  for (p in boolParams) {
    scenario[[p]] <- as.boolean.param (scenario[[p]], p, .irace.params.def)
  }

  if (scenario$deterministic &&
      scenario$firstTest > length(scenario$instances)) {
    irace.error("When deterministic == TRUE, the number of instances (",
                length(scenario$instances),
                ") cannot be smaller than firstTest (", scenario$firstTest, ")")
  }

  if (scenario$mpi && scenario$parallel < 2) {
    irace.error ("'parallel' (", .irace.params.def["parallel","long"],
                 ") must be larger than 1 when mpi is enabled")
  }

  if (scenario$sgeCluster && scenario$mpi) {
    irace.error("'mpi' (", .irace.params.def["mpi", "long"], ") and ",
                "'sgeCluster' (", .irace.params.def["sgeCluster", "long"], ") ",
                "cannot be enabled at the same time")
  }

  if (scenario$sgeCluster && scenario$parallel > 1) {
    irace.error("It does not make sense to use ",
                "'parallel' (", .irace.params.def["parallel", "long"], ") and ",
                "'sgeCluster' (", .irace.params.def["sgeCluster", "long"], ") ",
                "at the same time")
  }
  
  scenario$testType <- switch(tolower(scenario$testType),
                                   "f-test" =, # Fall-through
                                   "friedman" = "friedman",
                                   "t-test" =, # Fall-through
                                   "t.none" = "t.none",
                                   "t-test-holm" =, # Fall-through,
                                   "t.holm" = "t.holm",
                                   "t-test-bonferroni" =, # Fall-through,
                                   "t.bonferroni" = "t.bonferroni",
                                   irace.error ("Invalid value '", scenario$testType,
                                                "' of 'testType' (", .irace.params.def["testType", "long"],
                                                "), valid values are: ",
                                                "F-test, t-test, t-test-holm, t-test-bonferroni"))
  return (scenario)
}

print.instances.extra.params <- function(param, value)
{
  if (is.null.or.empty(paste(value, collapse=""))) {
    cat (param, "= NULL\n")
  } else {
    cat (param, "=\n")
    cat(paste(names(value), value, sep=" : "), sep="\n")
  }
}

print.instances <- function(param, value)
{
  cat (param, "= \"")
  cat (value, sep=", ")
  cat ("\"\n")
}

printScenario <- function(scenario)
{
  cat("## irace scenario:\n")
  for (param in .irace.params.names) {
    
    # Special case for instances.extra.params
    if (param == "instances.extra.params"
        || param == "testInstances.extra.params") {
      print.instances.extra.params (param, scenario[[param]])
    } else if (param == "instances" || param == "testInstances") {
      # Special case for instances
      print.instances (param, scenario[[param]])
    } else {# All other parameters (no vector, but can be functions)
      # FIXME: Perhaps deparse() is not the right way to do this?
      # FIXME: Perhaps toString() ?
      cat(param, "=", deparse(scenario[[param]]), "\n")
    }
  }
  cat("## end of irace scenario\n")
}

defaultScenario <- function(scenario = list())
{
  if (!is.null(names(scenario))
      && !all(names(scenario) %in% .irace.params.names)) {
    irace.error("Unknown scenario parameters: ",
                paste(names(scenario)[which(!names(scenario)
                                            %in% .irace.params.names)],
                      sep=", "))
  }

  for (k in .irace.params.names) {
    if (is.null.or.na(scenario[[k]])) {
      scenario[[k]] <- .irace.params.def[k, "default"]
    }
  }
  return (scenario)
}

readInstances <- function(instancesDir = NULL, instancesFile = NULL)
{
  if (is.null.or.empty(instancesDir) && is.null.or.empty(instancesFile))
    irace.error("Both instancesDir and instancesFile are empty: No instances provided")
  
  instances <- instances.extra.params <- NULL
  
  if (!is.null.or.empty(instancesFile)) {
    file.check (instancesFile, readable = TRUE, text = "instance file")
    lines <- readLines (instancesFile)
    lines <- sub("#.*$", "", lines) # Remove comments
    lines <- sub("^[[:space:]]+", "", lines) # Remove extra spaces
    lines <- lines[lines != ""] # Delete empty lines
    instances <- sub("^([^[:space:]]+).*$", "\\1", lines)
    instances <- paste0 (instancesDir, instances)
    instances.extra.params <- sub("^[^[:space:]]+(.*)$", "\\1", lines)
    names (instances.extra.params) <- instances
  } else {
    file.check (instancesDir, isdir = TRUE, notempty = TRUE,
                text = "instances directory")
    # The files are sorted in alphabetical order, on the full path if
    # 'full.names = TRUE'.
    instances <- list.files (path = instancesDir, full.names = TRUE,
                             recursive = TRUE)
    if (length (instances) == 0)
      irace.error("No instances found in `", instancesDir, "'")
  }
  
  return(list(instances = instances,
              instances.extra.params = instances.extra.params))
}

## Check targetRunner execution
checkTargetFiles <- function(scenario, parameters)
{
  result <- TRUE
  ## Create two random configurations
  conf.id <- c("testConfig1","testConfig2")
  configurations <- sampleUniform(parameters, length(conf.id),
                                  digits = scenario$digits,
                                  forbidden = scenario$forbiddenExps)
  configurations <- cbind (.ID. = conf.id, configurations)
  
  # Get info of the configuration
  values <- removeConfigurationsMetaData(configurations)
  values <- values[, parameters$names, drop = FALSE]
  switches <- parameters$switches[parameters$names]
  
  # Create the experiment using the first instance
  experiments <- list()
  for (i in 1:nrow(configurations))
    experiments[[i]] <- list (id.configuration = configurations[i, ".ID."],
                              id.instance  = "instance1",
                              seed = 1234567,
                              configuration = values[i, , drop = FALSE],
                              instance = scenario$instances[1],
                              extra.params = scenario$instances.extra.params[[1]],
                              switches = switches)
  # Executing targetRunner
  cat("# Executing targetRunner (", nrow(configurations), "times)...\n")
  output <-  withCallingHandlers(
    tryCatch(execute.experiments(experiments, scenario),
             error = function(e) {
               cat(sep = "\n",
                   "\n# Error occurred while executing targetRunner:",
                   paste0(conditionMessage(e), collapse="\n"))
               result <<- FALSE
               NULL
             }), warning = function(w) {
               cat(sep = "\n",
                   "\n# Warning occurred while executing targetRunner:",
                   paste0(conditionMessage(w), collapse="\n"))
               invokeRestart("muffleWarning")})

  if (!is.null(.irace$target.evaluator)) {
    cat("# Executing targetEvaluator...\n")
    output <-  withCallingHandlers(
      tryCatch(execute.evaluator(experiments, scenario, output, configurations[, ".ID."]),
                 error = function(e) {
                   cat(sep = "\n",
                       "\n# Error ocurred while executing targetEvaluator:",
                       paste0(conditionMessage(e), collapse="\n"))
                   result <<- FALSE
                   NULL
                 }), warning = function(w) {
                   cat(sep = "\n",
                       "\n# Warning ocurred while executing targetEvaluator:",
                       paste0(conditionMessage(w), collapse="\n"))
                   invokeRestart("muffleWarning")})
  }
  return(result)
}


