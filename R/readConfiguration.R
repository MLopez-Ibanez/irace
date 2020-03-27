#' readConfigurationsFile
#'
#' `readConfigurationsFile` reads a set of target algorithms configurations 
#'  from a file and puts them in \pkg{irace} format. The configurations are checked 
#'  to match the parameters description provided.
#' 
#' @param filename (`character(1)`) \cr Filename from which the configurations should be read.
#' @template arg_parameters
#' @template arg_debuglevel
#' @template arg_text
#' 
#' @return A data frame containing the obtained configurations. 
#'   Each row of the data frame is a candidate configuration, 
#'   the columns correspond to the parameter names in `parameters`.
#'
#' @seealso 
#'   \code{\link{readParameters}} to obtain a valid parameter structure from a parameters list.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @md
#' @export
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
  cat("# Read ", nbConfigurations, " configuration(s) from file '", filename, "'\n", sep="")
  if (debugLevel >= 2) {
    print(as.data.frame(configurationTable, stringAsFactor = FALSE), digits=15)
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
# FIXME: It may be faster to create a single expression that concatenates all
# the elements of forbidden using '|'
checkForbidden <- function(configurations, forbidden)
{
  # We have to use a variable name that will never appear in
  # configurations, so .FORBIDDEN .
  for (.FORBIDDEN in forbidden) {
    #print(.FORBIDDEN)
    configurations <- subset(configurations, eval(.FORBIDDEN))
    #print(configurations)
    #print(str(configurations))
    ## FIXME: This is normally called with a single configuration. Thus, it
    ## would be faster to break as soon as nrow(configurations) < 1
  }
  #print(nrow(configurations))
  return(configurations)
}

compile.forbidden <- function(x)
{
  if (is.bytecode(x)) return(x)
  # If we are given an expression, it must be a single one.
  irace.assert(is.language(x) && (!is.expression(x) || length(x) == 1))
  if (is.expression(x)) x <- x[[1]]
  # When a is NA and we check a == 5, we would get NA, which is
  # always FALSE, when we actually want to be TRUE, so we test
  # is.na() first below.
  
  # We expect that there will be undefined variables, since the expressions
  # will be evaluated within a data.frame later.
  exp <- compiler::compile(substitute(is.na(x) | !(x), list(x = x)),
                           options = list(suppressUndefined=TRUE))
  attr(exp, "source") <- as.character(as.expression(x))
  return(exp)
}

readForbiddenFile <- function(filename)
{
  forbiddenExps <- parse(file = filename)
  # FIXME: Using && or || instead of & and | will not work. Detect
  # this and give an error to the user.

  # FIXME: Check that the parameter names that appear in forbidden
  # all appear in parameters$names to catch typos.

  # FIXME: Instead of a list, we should generate a single expression that is
  # the logical-OR of all elements of the list.

  # Byte-compile them.
  return(sapply(forbiddenExps, compile.forbidden))
}      

buildForbiddenExp <- function(configurations, parameters)
{
  if (nrow(configurations) < 1) return(NULL)

  pnames <- parameters$names
  lines <- c()
  # We cannot use apply() because it converts numeric to character.
  for (k in 1:nrow(configurations)) {
    values <- as.list(configurations[k, pnames])
    has.value <- !is.na(values)
    values <- lapply(values[has.value], function(x) deparse(substitute(x, list(x=x))))
    lines <- c(lines,
               paste0("(", pnames[has.value]," == ", values, ")", collapse = "&"))
  }
  exps <- parse(text = lines)
  # print(exps)
  return(sapply(exps, compile.forbidden))
}

#' readScenario
#'
#' `readScenario` reads from a file the scenario settings to be used by
#' \pkg{irace}..
#' 
#' @param filename (`character(1)`) \cr Filename from which the scenario will
#'   be read. If empty, the default `scenarioFile` is used.  An example
#'   scenario file is provided in `system.file(``package="irace",`
#'   `"templates/scenario.txt.tmpl")`.
#' @templateVar arg_appendix This is an initial scenario that is overwritten
#'   for every setting specified in the file to be read.
#' @template arg_scenario
#'  
#' @return The scenario list read from the file. The scenario settings not
#'   present in the file are not present in the list, i.e., they are `NULL`.
#'
#' @seealso
#'  \describe{
#'  \item{\code{\link{printScenario}}}{prints the given scenario.}
#'  \item{\code{\link{defaultScenario}}}{returns the default scenario settings of \pkg{irace}.}
#'  \item{\code{\link{checkScenario}}}{to check that the scenario is valid.}
#' }
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @md
#' @export
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
    handle.source.error <- function(e) {
      irace.error("Reading scenario file ", shQuote(rfilename),
                  " included from ", shQuote(topfile),
                  " produced the following errors or warnings:\n",
                  paste0(conditionMessage(e), collapse="\n"))
      return(NULL)
    }
    withCallingHandlers(
      tryCatch(source(rfilename, local = envir., chdir = TRUE),
               error = handle.source.error, warning = handle.source.error))
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
    handle.source.error <- function(e) {
      irace.error("Reading scenario file ", shQuote(filename),
                  " produced the following errors or warnings:\n",
                  paste0(conditionMessage(e), collapse="\n"))
      return(NULL)
    }
    withCallingHandlers(
      tryCatch(source(filename, local = TRUE, chdir = TRUE),
               error = handle.source.error, warning = handle.source.error))
    if (debug.level >= 1) cat (" done!\n")
  } else {
    irace.error ("The scenario file ", shQuote(filename), " does not exist.")
  }
  ## Read scenario file variables.
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

#' Check and correct the given scenario
#'
#' `checkScenario` takes a (possibly incomplete) scenario setup of
#' \pkg{irace}, checks for errors and transforms it into a valid scenario.
#' 
#' @template arg_scenario
#' 
#' @return The scenario received as a parameter, possibly corrected. Unset
#' scenario settings are set to their default values.
#'
#' @details   This function checks that the directories and the file names 
#' provided and required by the \pkg{irace} exist. It also checks that the 
#' settings are of the proper type, e.g. that settings expected to be integers 
#' are really integers. Finally, it also checks that there is no inconsistency
#' between settings.  If an error is found that prevents \pkg{irace} from 
#' running properly, it will stop with an error.
#'
#' @seealso
#'  \describe{
#'  \item{\code{\link{readScenario}}}{for reading a configuration scenario from a file.}
#'  \item{\code{\link{printScenario}}}{prints the given scenario.}
#'  \item{\code{\link{defaultScenario}}}{returns the default scenario settings of \pkg{irace}.}
#'  \item{\code{\link{checkScenario}}}{to check that the scenario is valid.}
#' }
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @md
#' @export
## FIXME: This function should only do checks and return TRUE/FALSE. There
## should be other function that does the various transformations.
checkScenario <- function(scenario = defaultScenario())
{
  quote.param <- function(name)
  {
    return(paste0("'", name, "' (", .irace.params.def[name, "long"], ")"))
  }

  as.boolean.param <- function(x, name)
  {
    x <- as.integer(x)
    if (is.na (x) || (x != 0 && x != 1)) {
      irace.error (quote.param(name), " must be either 0 or 1.")
    }
    return(as.logical(x))
  }

  check.valid.param <- function(x, valid)
  {
    if (scenario[[x]] %!in% valid) {
      irace.error ("Invalid value '", scenario[[x]], "' of ",
                   quote.param(x), ", valid values are: ",
                   paste0(valid, collapse = ", "))
    }
  }
  
  
  # Fill possible unset (NULL) with default settings.
  scenario <- defaultScenario (scenario)

  # Duplicated entries will cause confusion.
  dups <- anyDuplicated(names(scenario))
  if (dups > 0)
    irace.error("scenario contains duplicated entries: ", names(scenario)[dups])

  # Boolean control parameters.
  boolParams <- .irace.params.def[.irace.params.def[, "type"] == "b", "name"]
  for (p in boolParams) {
    scenario[[p]] <- as.boolean.param (scenario[[p]], p)
  }

  ## Check that everything is fine with external parameters
  # Check that the files exist and are readable.
  scenario$parameterFile <- path.rel2abs(scenario$parameterFile)
  # We don't read parameterFile here because the user may give the parameters
  # explicitly.  And it is validated in readParameters anyway.
  scenario$execDir <- path.rel2abs(scenario$execDir)
  file.check (scenario$execDir, isdir = TRUE,
              text = paste0("execution directory ", quote.param("execDir")))
  options(.irace.execdir = scenario$execDir)
  if (!is.null.or.empty(scenario$logFile)) {
    scenario$logFile <- path.rel2abs(scenario$logFile, cwd = scenario$execDir)
    file.check(scenario$logFile, writeable = TRUE,
               text = quote.param('logFile'))
  } else {
    scenario$logFile  <- NULL
  }

  if (!is.null.or.empty(scenario$recoveryFile)) {
    scenario$recoveryFile <- path.rel2abs(scenario$recoveryFile)
    file.check(scenario$recoveryFile, readable = TRUE,
               text = paste0("recovery file ", quote.param("recoveryFile")))
    
    if (!is.null.or.empty(scenario$logFile)
        && scenario$recoveryFile == scenario$logFile) {
      irace.error("log file and recovery file should be different ('",
                  scenario$logFile, "'")
    }
  } else {
    scenario$recoveryFile  <- NULL
  }
  
  if (is.null.or.empty(scenario$targetRunnerParallel)) {
    scenario$targetRunnerParallel <- NULL
  } else if (is.function.name(scenario$targetRunnerParallel)) {
    scenario$targetRunnerParallel <- get.function(scenario$targetRunnerParallel)
  } else {
    irace.error("'targetRunnerParallel' must be a function")
  }
  
  if (is.null.or.empty(scenario$repairConfiguration)) {
    scenario$repairConfiguration <- NULL
  } else if (!is.function.name(scenario$repairConfiguration)) {
    irace.error("'repairConfiguration' must be a function")
  } else {
    # Byte-compile it.
    scenario$repairConfiguration <- bytecompile(get.function(scenario$repairConfiguration))
  }

  if (is.function.name(scenario$targetRunner)) {
    scenario$targetRunner <- get.function(scenario$targetRunner)
    .irace$target.runner <- bytecompile(scenario$targetRunner)
  } else if (is.null(scenario$targetRunnerParallel)) {
    if (is.character(scenario$targetRunner)) {
      scenario$targetRunner <- path.rel2abs(scenario$targetRunner)
      file.check (scenario$targetRunner, executable = TRUE,
                  text = paste0("target runner ", quote.param("targetRunner")))
      .irace$target.runner <- if (scenario$aclib)
                                target.runner.aclib else target.runner.default
    } else {
      irace.error(quote.param ('targetRunner'), " must be a function or an executable program")
    }
  }

  if (is.null.or.empty(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- NULL
    .irace$target.evaluator <- NULL
  } else if (is.function.name(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- get.function(scenario$targetEvaluator)
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
    if (is.null.or.empty(scenario$trainInstancesDir)
        && is.null.or.empty(scenario$trainInstancesFile))
      irace.error("Both ", quote.param ("trainInstancesDir"), " and ",
                  quote.param ("trainInstancesFile"),
                  " are empty: No instances provided")
    
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
    names(scenario$testInstances) <- paste0(1:length(scenario$testInstances), "t")
  }
  
  # Configurations file
  if (!is.null.or.empty(scenario$configurationsFile)) {
    scenario$configurationsFile <- path.rel2abs(scenario$configurationsFile)
    file.check (scenario$configurationsFile, readable = TRUE,
                text = "configurations file")
    # We cannot read the configurations here because we need the parameters.
    # FIXME: We should have the parameters inside scenario.
  }
  
  if (is.null.or.empty(scenario$initConfigurations)) {
    scenario$initConfigurations <- NULL
  } else if (!is.data.frame(scenario$initConfigurations) && !is.matrix(scenario$initConfigurations)) {
    irace.error("if given, initConfigurations must be a matrix or data.frame")
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
  # problems later.

  # Integer control parameters
  intParams <- .irace.params.def[.irace.params.def[, "type"] == "i", "name"]
  for (param in intParams) {
    if (is.null.or.empty(scenario[[param]]))
      scenario[[param]] <- NA
    if (is.na(scenario[[param]]))
      next # Allow NA default values
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

  # AClib benchmarks use 15 digits
  if (scenario$aclib)
    scenario$digits <- 15
  if (scenario$digits > 15 || scenario$digits <= 0)
    irace.error (quote.param ("digits"), " must be within [1,15].")
  
  # Real [0, 1] control parameters
  realParams <- .irace.params.def[.irace.params.def[, "type"] == "r", "name"]
  for (param in realParams) {
    if (is.null.or.empty(scenario[[param]]))
      scenario[[param]] <- NA
    if (is.na(scenario[[param]]))
      next # Allow NA default values
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
    check.valid.param("batchmode", valid = c("sge", "pbs", "torque", "slurm"))
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

  if (scenario$capping) {
    if (!scenario$elitist) 
      irace.error("When capping == TRUE, elitist must be enabled.")
    if (scenario$boundMax <= 0) 
      irace.error("When capping == TRUE, boundMax (", scenario$boundMax,
                  ") must be > 0")
    check.valid.param("cappingType",
                      valid = c("median", "mean", "worst", "best"))
    check.valid.param("boundType", valid = c("instance", "candidate"))
        
    if (scenario$boundPar < 1)
      irace.error("Invalid value boundPar (", scenario$boundPar,
                  ") must be >= 1")
  } else { # no capping
    if (scenario$boundMax <= 0 || is.na(scenario$boundMax))
      scenario$boundMax <- NULL
  }


  
  if (is.null.or.empty(scenario$testType) || 
      is.null.or.na(scenario$testType) || 
      scenario$testType =="") {
  	if (scenario$capping) scenario$testType <- "t-test"
  	else scenario$testType <- "f-test"
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
           check.valid.param("testType",
                             c("F-test, t-test, t-test-holm, t-test-bonferroni")))
  
  return (scenario)
}

#' Prints the given scenario
#'
#' @template arg_scenario
#' 
#' @seealso
#'  \describe{
#'  \item{\code{\link{readScenario}}}{for reading a configuration scenario from a file.}
#'  \item{\code{\link{printScenario}}}{prints the given scenario.}
#'  \item{\code{\link{defaultScenario}}}{returns the default scenario settings of \pkg{irace}.}
#'  \item{\code{\link{checkScenario}}}{to check that the scenario is valid.}
#' }
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @md
#' @export
printScenario <- function(scenario)
{
  cat("## irace scenario:\n")
  for (param in .irace.params.names) {
    if (param == "forbiddenExps")
      extra <- paste0(" = expression(", paste0(collapse=", ",
                                  sapply(scenario[[param]], attr, "source")), ")")
    else extra <- ""
    cat(param, " = ", deparse(scenario[[param]]), extra, "\n", sep = "")
  }
  cat("## end of irace scenario\n")
}

#' Default scenario settings
#'
#' Return scenario with default values.
#' 
#' @template arg_scenario
#' 
#' @return A list indexed by the \pkg{irace} parameter names,
#' containing the default values for each parameter, except for those
#' already present in the scenario passed as argument.
#' The scenario list contains the following elements: 
# __IRACE_OPTIONS__BEGIN__
#' \itemize{
#'  \item General options:
#'    \describe{
#'      \item{\code{scenarioFile}}{Path of the file that describes the configuration scenario setup and other irace settings. (Default: \code{"./scenario.txt"})}
#'      \item{\code{execDir}}{Directory where the programs will be run. (Default: \code{"./"})}
#'      \item{\code{logFile}}{File to save tuning results as an R dataset, either absolute path or relative to execDir. (Default: \code{"./irace.Rdata"})}
#'      \item{\code{debugLevel}}{Debug level of the output of \code{irace}. Set this to 0 to silence all debug messages. Higher values provide more verbose debug messages. (Default: \code{0})}
#'      \item{\code{seed}}{Seed of the random number generator (by default, generate a random seed). (Default: \code{NA})}
#'      \item{\code{repairConfiguration}}{User-defined R function that takes a configuration generated by irace and repairs it. (Default: \code{""})}
#'      \item{\code{postselection}}{Percentage of the configuration budget used to perform a postselection race of the best configurations of each iteration after the execution of irace. (Default: \code{0})}
#'      \item{\code{aclib}}{Enable/disable AClib mode. This option enables compatibility with GenericWrapper4AC as targetRunner script. (Default: \code{0})}
#'    }
#'  \item Elitist \code{irace}:
#'    \describe{
#'      \item{\code{elitist}}{Enable/disable elitist irace. (Default: \code{1})}
#'      \item{\code{elitistNewInstances}}{Number of instances added to the execution list before previous instances in elitist irace. (Default: \code{1})}
#'      \item{\code{elitistLimit}}{In elitist irace, maximum number per race of elimination tests that do not eliminate a configuration. Use 0 for no limit. (Default: \code{2})}
#'    }
#'  \item Internal \code{irace} options:
#'    \describe{
#'      \item{\code{nbIterations}}{Number of iterations. (Default: \code{0})}
#'      \item{\code{nbExperimentsPerIteration}}{Number of runs of the target algorithm per iteration. (Default: \code{0})}
#'      \item{\code{sampleInstances}}{Randomly sample the training instances or use them in the order given. (Default: \code{1})}
#'      \item{\code{minNbSurvival}}{Minimum number of configurations needed to continue the execution of each race (iteration). (Default: \code{0})}
#'      \item{\code{nbConfigurations}}{Number of configurations to be sampled and evaluated at each iteration. (Default: \code{0})}
#'      \item{\code{mu}}{Parameter used to define the number of configurations sampled and evaluated at each iteration. (Default: \code{5})}
#'      \item{\code{softRestart}}{Enable/disable the soft restart strategy that avoids premature convergence of the probabilistic model. (Default: \code{1})}
#'      \item{\code{softRestartThreshold}}{Soft restart threshold value for numerical parameters. If \code{NA}, \code{NULL} or \code{""}, it is computed as \code{10^-digits}. (Default: \code{""})}
#'    }
#'  \item Target algorithm parameters:
#'    \describe{
#'      \item{\code{parameterFile}}{File that contains the description of the parameters of the target algorithm. (Default: \code{"./parameters.txt"})}
#'      \item{\code{forbiddenExps}}{Vector of R logical expressions that cannot evaluate to \code{TRUE} for any evaluated configuration. (Default: \code{""})}
#'      \item{\code{forbiddenFile}}{File that contains a list of logical expressions that cannot be \code{TRUE} for any evaluated configuration. If empty or \code{NULL}, do not use forbidden expressions. (Default: \code{""})}
#'      \item{\code{digits}}{Maximum number of decimal places that are significant for numerical (real) parameters. (Default: \code{4})}
#'    }
#'  \item Target algorithm execution:
#'    \describe{
#'      \item{\code{targetRunner}}{Script called for each configuration that executes the target algorithm to be tuned. See templates. (Default: \code{"./target-runner"})}
#'      \item{\code{targetRunnerRetries}}{Number of times to retry a call to \code{targetRunner} if the call failed. (Default: \code{0})}
#'      \item{\code{targetRunnerData}}{Optional data passed to \code{targetRunner}. This is ignored by the default \code{targetRunner} function, but it may be used by custom \code{targetRunner} functions to pass persistent data around. (Default: \code{""})}
#'      \item{\code{targetRunnerParallel}}{Optional R function to provide custom parallelization of \code{targetRunner}. (Default: \code{""})}
#'      \item{\code{targetEvaluator}}{Optional script or R function that provides a numeric value for each configuration. See templates/target-evaluator.tmpl (Default: \code{""})}
#'      \item{\code{deterministic}}{If the target algorithm is deterministic, configurations will be evaluated only once per instance. (Default: \code{0})}
#'      \item{\code{parallel}}{Number of calls to \code{targetRunner} to execute in parallel. Values \code{0} or \code{1} mean no parallelization. (Default: \code{0})}
#'      \item{\code{loadBalancing}}{Enable/disable load-balancing when executing experiments in parallel. Load-balancing makes better use of computing resources, but increases communication overhead. If this overhead is large, disabling load-balancing may be faster. (Default: \code{1})}
#'      \item{\code{mpi}}{Enable/disable MPI. Use \code{Rmpi} to execute \code{targetRunner} in parallel (parameter \code{parallel} is the number of slaves). (Default: \code{0})}
#'      \item{\code{batchmode}}{Specify how irace waits for jobs to finish when \code{targetRunner} submits jobs to a batch cluster: sge, pbs, torque or slurm. \code{targetRunner} must submit jobs to the cluster using, for example, \code{qsub}. (Default: \code{0})}
#'    }
#'  \item Initial configurations:
#'    \describe{
#'      \item{\code{initConfigurations}}{Data frame describing initial configurations (usually read from a file using \code{readConfigurations}). (Default: \code{""})}
#'      \item{\code{configurationsFile}}{File that contains a table of initial configurations. If empty or \code{NULL}, all initial configurations are randomly generated. (Default: \code{""})}
#'    }
#'  \item Training instances:
#'    \describe{
#'      \item{\code{instances}}{Character vector of the instances to be used in the \code{targetRunner}. (Default: \code{""})}
#'      \item{\code{trainInstancesDir}}{Directory where training instances are located; either absolute path or relative to current directory. If no \code{trainInstancesFiles} is provided, all the files in \code{trainInstancesDir} will be listed as instances. (Default: \code{"./Instances"})}
#'      \item{\code{trainInstancesFile}}{File that contains a list of training instances and optionally additional parameters for them. If \code{trainInstancesDir} is provided, \code{irace} will search for the files in this folder. (Default: \code{""})}
#'    }
#'  \item Tuning budget:
#'    \describe{
#'      \item{\code{maxExperiments}}{Maximum number of runs (invocations of \code{targetRunner}) that will be performed. It determines the maximum budget of experiments for the tuning. (Default: \code{0})}
#'      \item{\code{maxTime}}{Maximum total execution time in seconds for the executions of \code{targetRunner}. \code{targetRunner} must return two values: cost and time. (Default: \code{0})}
#'      \item{\code{budgetEstimation}}{Fraction (smaller than 1) of the budget used to estimate the mean computation time of a configuration. Only used when \code{maxTime} > 0 (Default: \code{0.02})}
#'    }
#'  \item Statistical test:
#'    \describe{
#'      \item{\code{testType}}{Statistical test used for elimination. Default test is always \code{F-test} unless \code{capping} is enabled, in which case the default test is \code{t-test}. Valid values are: F-test (Friedman test), t-test (pairwise t-tests with no correction), t-test-bonferroni (t-test with Bonferroni's correction for multiple comparisons), t-test-holm (t-test with Holm's correction for multiple comparisons). (Default: \code{"F-test"})}
#'      \item{\code{firstTest}}{Number of instances evaluated before the first elimination test. It must be a multiple of \code{eachTest}. (Default: \code{5})}
#'      \item{\code{eachTest}}{Number of instances evaluated between elimination tests. (Default: \code{1})}
#'      \item{\code{confidence}}{Confidence level for the elimination test. (Default: \code{0.95})}
#'    }
#'  \item Adaptive capping:
#'    \describe{
#'      \item{\code{capping}}{Enable the use of adaptive capping, a technique designed for minimizing the computation time of configurations. This is only available when \code{elitist} is active. (Default: \code{0})}
#'      \item{\code{cappingType}}{Measure used to obtain the execution bound from the performance of the elite configurations.\itemize{\item median: Median performance of the elite configurations.\item mean: Mean performance of the elite configurations.\item best: Best performance of the elite configurations.\item worst: Worst performance of the elite configurations.} (Default: \code{"median"})}
#'      \item{\code{boundType}}{Method to calculate the mean performance of elite configurations.\itemize{\item candidate: Mean execution times across the executed instances and the current one.\item instance: Execution time of the current instance.} (Default: \code{"candidate"})}
#'      \item{\code{boundMax}}{Maximum execution bound for \code{targetRunner}. It must be specified when capping is enabled. (Default: \code{0})}
#'      \item{\code{boundDigits}}{Precision used for calculating the execution time. It must be specified when capping is enabled. (Default: \code{0})}
#'      \item{\code{boundPar}}{Penalization constant for timed out executions (executions that reach \code{boundMax} execution time). (Default: \code{1})}
#'      \item{\code{boundAsTimeout}}{Replace the configuration cost of bounded executions with \code{boundMax}. (Default: \code{1})}
#'    }
#'  \item Recovery:
#'    \describe{
#'      \item{\code{recoveryFile}}{Previously saved log file to recover the execution of \code{irace}, either absolute path or relative to the current directory.  If empty or \code{NULL}, recovery is not performed. (Default: \code{""})}
#'    }
#'  \item Testing:
#'    \describe{
#'      \item{\code{testInstancesDir}}{Directory where testing instances are located, either absolute or relative to current directory. (Default: \code{""})}
#'      \item{\code{testInstancesFile}}{File containing a list of test instances and optionally additional parameters for them. (Default: \code{""})}
#'      \item{\code{testInstances}}{Character vector of the instances to be used in the \code{targetRunner} when executing the testing. (Default: \code{""})}
#'      \item{\code{testNbElites}}{Number of elite configurations returned by irace that will be tested if test instances are provided. (Default: \code{1})}
#'      \item{\code{testIterationElites}}{Enable/disable testing the elite configurations found at each iteration. (Default: \code{0})}
#'    }
#' }
# __IRACE_OPTIONS__END__
#'
#' @seealso
#'  \describe{
#'  \item{\code{\link{readScenario}}}{for reading a configuration scenario from a file.}
#'  \item{\code{\link{printScenario}}}{prints the given scenario.}
#'  \item{\code{\link{defaultScenario}}}{returns the default scenario settings of \pkg{irace}.}
#'  \item{\code{\link{checkScenario}}}{to check that the scenario is valid.}
#' }
#'
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
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
    # FIXME: is.null.or.empty should handle length() == 0.
    if (is.null.or.empty(instances) || length(instances) == 0)
      irace.error("No instances found in '", instancesFile,
                  "' (whitespace and comments starting with '#' are ignored)")
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
  conf.id <- c("testConfig1", "testConfig2")
  configurations <- sampleUniform(parameters, length(conf.id),
                                  digits = scenario$digits,
                                  forbidden = scenario$forbiddenExps,
                                  repair = scenario$repairConfiguration)
  configurations <- cbind (.ID. = conf.id, configurations)

  bounds <- rep(scenario$boundMax, nrow(configurations))

  instances.ID <- if (scenario$sampleInstances)
                    sample.int(length(scenario$instances), 1) else 1
  experiments <- createExperimentList(
    configurations, parameters, instances = scenario$instances,
    instances.ID = instances.ID, seeds = 1234567, scenario, bounds = bounds)

  startParallel(scenario)
  on.exit(stopParallel(), add = TRUE)

  # FIXME: Create a function try.call(err.msg,warn.msg, fun, ...)
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
    print(output, digits = 15)
  }
  
  irace.assert(is.null(scenario$targetEvaluator) == is.null(.irace$target.evaluator))

  if (!is.null(scenario$targetEvaluator)) {
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
      print(output, digits = 15)
    }
  }
  return(result)
}


