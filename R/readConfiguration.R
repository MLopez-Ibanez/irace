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
readConfigurationsFile <- function(filename, parameters, debugLevel = 0, text)
{
  if (missing(filename) && !missing(text)) {
    filename <- strcat("text=", deparse(substitute(text)))
    configurationTable <- read.table(text = text, header = TRUE,
                                     colClasses = "character",
                                     stringsAsFactors = FALSE)
  } else {
    # Read the file.
    configurationTable <- read.table(filename, header = TRUE,
                                     colClasses = "character",
                                     stringsAsFactors = FALSE)
  }
  irace.assert(is.data.frame(configurationTable))
  nbConfigurations <- nrow(configurationTable)
  # Print the table that has been read.
  if (debugLevel >= 2) {
    cat("# Read ", nbConfigurations, " configurations from file '", filename, "'\n", sep="")
    print(as.data.frame(configurationTable, stringAsFactor = FALSE))
  }

  namesParameters <- names(parameters$conditions)
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
    varParameters <- parameters$names[!parameters$isFixed]
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
      configurationTable[, currentParameter] <-
        suppressWarnings(as.numeric(configurationTable[, currentParameter]))
    }
  }

  # Loop over all configurations in configurationTable
  for (k in seq_len(nbConfigurations)) {
    # Loop over all parameters, in hierarchical order.
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

readForbiddenFile <- function(filename)
{
  forbiddenExps <- parse(file = filename)
  # FIXME: Using && or || instead of & and | will not work. Detect
  # this and give an error to the user.

  # When a is NA and we check a == 5, we would get NA, which is
  # always FALSE, when we actually want to be TRUE, so we test
  # is.na() first below.
  forbiddenExps <- sapply(forbiddenExps,
                          function(x) substitute(is.na(x) | !(x), list(x = x)))
  # FIXME: Check that the parameter names that appear in forbidden
  # all appear in parameters$names to catch typos.

  # FIXME: Instead of a list, we should generate a single expression that is
  # the logical-OR of all elements of the list.

  # Byte-compile them. We expect that there will be undefined variables, since
  # the expressions will be evaluated within a data.frame later.
  forbiddenExps <- sapply(forbiddenExps, compiler::compile,
                          options = list(suppressUndefined=TRUE))
  return(forbiddenExps)
}      

# Reads scenario setup from filename and returns it as a list. Anything not
# mentioned in the file is not present in the list (that is, it is NULL).
# FIXME: Does passing an initial scenario actually work? It seems it gets
# completely overriden by the loop below.
readScenario <- function(filename = "", scenario = list())
{
  # This function allows recursively including scenario files.
  envir <- environment()
  include.scenario <- function(rfilename, topfile = filename, envir. = envir)
  {
    if (!file.exists (rfilename)) {
      irace.error ("The scenario file ", shQuote(rfilename), " included from ",
                   shQuote(topfile), " does not exist.")
    }
    source(rfilename, local = envir., chdir = TRUE)
  }

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
    # chdir = TRUE to allow recursive sourcing.
    source(filename, local = TRUE, chdir = TRUE)
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

  quote.param <- function(name)
  {
    return(paste0("'", name, "' (", .irace.params.def[name, "long"], ")"))
  }
  
  if (is.null.or.empty(scenario$targetRunnerParallel)) {
    scenario$targetRunnerParallel <- NULL
  } else if (!is.function.name(scenario$targetRunnerParallel)) {
    irace.error("'targetRunnerParallel' must be a function")
  }
  
  if (is.null.or.empty(scenario$repairConfiguration)) {
    scenario$repairConfiguration <- NULL
  } else if (!is.function.name(scenario$repairConfiguration)) {
    irace.error("'repairConfiguration' must be a function")
  } else {
    # Byte-compile it.
    scenario$repairConfiguration <- bytecompile(scenario$repairConfiguration)
  }

  if (is.function.name(scenario$targetRunner)) {
    .irace$target.runner <- bytecompile(scenario$targetRunner)
  } else if (is.null(scenario$targetRunnerParallel)) {
    if (is.character(scenario$targetRunner)) {
      scenario$targetRunner <- path.rel2abs(scenario$targetRunner)
      file.check (scenario$targetRunner, executable = TRUE,
                  text = "target runner")
      .irace$target.runner <- target.runner.default
    } else {
      irace.error("'targetRunner' must be a function or an executable program")
    }
  }

  if (is.null.or.empty(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- NULL
    .irace$target.evaluator <- NULL
  } else if (is.function.name(scenario$targetEvaluator)) {
    .irace$target.evaluator <- bytecompile(scenario$targetEvaluator)
  } else if (is.character(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- path.rel2abs(scenario$targetEvaluator)
    file.check (scenario$targetEvaluator, executable = TRUE,
                text = "target evaluator")
    .irace$target.evaluator <- target.evaluator.default
  } else {
    irace.error("'targetEvaluator' must be a function or an executable program")
  }

  irace.assert(is.null(scenario$targetEvaluator) == is.null(.irace$target.evaluator))
  
  # Training instances
  if (is.null.or.empty(scenario$instances)) {
    scenario$trainInstancesDir <- path.rel2abs(scenario$trainInstancesDir)
    if (!is.null.or.empty(scenario$trainInstancesFile)) {
      scenario$trainInstancesFile <- path.rel2abs(scenario$trainInstancesFile)
    }
    scenario$instances <-
      readInstances(instancesDir = scenario$trainInstancesDir,
                    instancesFile = scenario$trainInstancesFile)
  }
  
  # Testing instances
  if (is.null.or.empty(scenario$testInstances)) {
    if (!is.null.or.empty(scenario$testInstancesDir) || 
        !is.null.or.empty(scenario$testInstancesFile)) {
      scenario$testInstancesDir <- path.rel2abs(scenario$testInstancesDir)
      if (!is.null.or.empty(scenario$testInstancesFile)) {
        scenario$testInstancesFile <- path.rel2abs(scenario$testInstancesFile)
      }
      scenario$testInstances <-
        readInstances(instancesDir = scenario$testInstancesDir,
                      instancesFile = scenario$testInstancesFile)
    } else {
      scenario$testInstances <- NULL
    }
  }
  if (!is.null(scenario$testInstances)
      && is.null(names(scenario$testInstances))) {
    # Create unique IDs for testInstances
    names (scenario$testInstances) <-
      paste0(1:length(scenario$testInstances), "t")
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
    scenario$forbiddenExps <- readForbiddenFile(scenario$forbiddenFile)
    cat("# ", length(scenario$forbiddenExps),
        " expression(s) specifying forbidden configurations read from '",
        scenario$forbiddenFile, "'\n", sep = "")
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
      irace.error (quote.param (param), " must be an integer.")
  }

  if (scenario$firstTest <= 1) {
    irace.error(quote.param ("firstTest"), " must be larger than 1.")
  }

  if (scenario$firstTest %% scenario$eachTest != 0) {
    irace.error(quote.param("firstTest"), " must be a multiple of ",
                quote.param("eachTest"), ".")
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
      irace.error (quote.param(param), " must be a real value within [0, 1].")
  }
  
  ## Only maxExperiments or maxTime should be set. Negative values are not
  ## allowed.
  if (scenario$maxExperiments == 0 && scenario$maxTime == 0) {
    irace.error("Tuning budget was not provided. Set ",
                quote.param("maxExperiments"), "or ",
                quote.param("maxTime"), ".")
  } else if (scenario$maxExperiments > 0 && scenario$maxTime > 0) {
    irace.error("Two different tuning budgets provided, please set only ",
                quote.param("maxExperiments"), " or only ",
                quote.param ("maxTime"), ".")
  } else if (scenario$maxExperiments < 0 ) {
    irace.error("Negative budget provided, ", quote.param("maxExperiments"),
                "must be >= 0." )
  } else if (scenario$maxTime < 0) {
    irace.error("Negative budget provided, ", quote.param("maxTime"),
                " must be >= 0.")
  }
  
  if (scenario$maxTime > 0 && (scenario$budgetEstimation <= 0 || scenario$budgetEstimation >= 1)) 
    irace.error(quote.param("budgetEstimation"), " must be within (0,1).")
  
  if (is.na (scenario$softRestartThreshold)) {
    scenario$softRestartThreshold <- 10^(- scenario$digits)
  }
  
  # Boolean control parameters
  as.boolean.param <- function(x, name)
  {
    x <- as.integer(x)
    if (is.na (x) || (x != 0 && x != 1)) {
      irace.error (quote.param(name), " must be either 0 or 1.")
    }
    return(as.logical(x))
  }
  boolParams <- .irace.params.def[.irace.params.def[, "type"] == "b", "name"]
  for (p in boolParams) {
    scenario[[p]] <- as.boolean.param (scenario[[p]], p)
  }

  if (scenario$deterministic &&
      scenario$firstTest > length(scenario$instances)) {
    irace.error("When deterministic == TRUE, the number of instances (",
                length(scenario$instances),
                ") cannot be smaller than firstTest (", scenario$firstTest, ")")
  }

  if (scenario$mpi && scenario$parallel < 2) {
    irace.error (quote.param("parallel"),
                 " must be larger than 1 when mpi is enabled.")
  }

  if (is.null.or.empty(scenario$batchmode))
    scenario$batchmode <- 0
  if (scenario$batchmode != 0) {
    scenario$batchmode <- tolower(scenario$batchmode)
    # FIXME: We should encode options in the large table in main.R
    valid.batchmode <- c("sge", "pbs", "torque", "slurm")
    if (!(scenario$batchmode %in% valid.batchmode)) {
      irace.error ("Invalid value '", scenario$batchmode,
                   "' of ", quote.param("batchmode"),
                   ", valid values are: ",
                   paste0(valid.batchmode, collapse = ", "))
    }
  }
  # Currently batchmode requires a targetEvaluator
  if (scenario$batchmode != 0 && is.null(scenario$targetEvaluator)) {
    irace.error(quote.param("batchmode"), " requires using ",
                quote.param("targetEvaluator"), ".")
  }

  if (scenario$batchmode != 0 && scenario$mpi) {
    irace.error(quote.param("mpi"), " and ", quote.param("batchmode"),
                " cannot be enabled at the same time.")
  }

  scenario$testType <-
    switch(tolower(scenario$testType),
           "f-test" =, # Fall-through
           "friedman" = "friedman",
           "t-test" =, # Fall-through
           "t.none" = "t.none",
           "t-test-holm" =, # Fall-through,
           "t.holm" = "t.holm",
           "t-test-bonferroni" =, # Fall-through,
           "t.bonferroni" = "t.bonferroni",
           irace.error ("Invalid value '", scenario$testType,
                        "' of ", quote.param("testType"),
                        ", valid values are: ",
                        "F-test, t-test, t-test-holm, t-test-bonferroni"))
  return (scenario)
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
    
    # Special case for instances
    if (param == "instances" || param == "testInstances") {
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
                      collapse = ", "))
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
  
  instances <- NULL
  
  if (!is.null.or.empty(instancesFile)) {
    file.check (instancesFile, readable = TRUE, text = "instance file")
    # We do not warn if the last line does not finish with a newline.
    instances <- readLines (instancesFile, warn = FALSE)
    instances <- sub("#.*$", "", instances) # Remove comments
    instances <- sub("^[[:space:]]+", "", instances) # Remove leading whitespace
    instances <- instances[instances != ""] # Delete empty lines
    if (is.null.or.empty(instances))
      irace.error("No instances found in `", instancesFile, "'")
    if (!is.null.or.empty(instancesDir))
       instances <- paste0 (instancesDir, "/", instances)
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
  
  return(instances)
}

## Check targetRunner execution
checkTargetFiles <- function(scenario, parameters)
{
  result <- TRUE
  ## Create two random configurations
  conf.id <- c("testConfig1","testConfig2")
  configurations <- sampleUniform(parameters, length(conf.id),
                                  digits = scenario$digits,
                                  forbidden = scenario$forbiddenExps,
                                  repair = scenario$repairConfiguration)
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

  if (scenario$debugLevel >= 1) {
    cat ("# targetRunner returned:\n")
    print(output)
  }
  
  irace.assert(is.null(scenario$targetEvaluator) == is.null(.irace$target.evaluator))

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
    if (scenario$debugLevel >= 1) {
      cat ("# targetEvaluator returned:\n")
      print(output)
    }
  }
  return(result)
}


