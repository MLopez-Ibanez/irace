#' Read parameter configurations from a file
#' 
#' Reads a set of target-algorithm configurations from a file and puts them in
#' \pkg{irace} format. The configurations are checked to match the parameters
#' description provided.
#' 
#' @param filename (`character(1)`) \cr Filename from which the configurations should be read. The contents should be readable by `read.table( , header=TRUE)`.
#' @template arg_parameters
#' @template arg_debuglevel
#' @template arg_text
#' 
#' @return A data frame containing the obtained configurations. 
#'   Each row of the data frame is a candidate configuration, 
#'   the columns correspond to the parameter names in `parameters`.
#'
#' @details
#' Example of an input file:
#' ```
#' # This is a comment line
#' <param_name_1>  <param_name_2> ... 
#'       0.5           "value_1"    ...
#'       1.0           "value_2"    ...
#'       1.2           "value_3"    ...
#'       ...           ...
#' ```
#' The order of the columns does not necessarily have to be the same
#' as in the file containing the definition of the parameters.
#' 
#' @seealso 
#'   [readParameters()] to obtain a valid parameter structure from a parameters file.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
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
    print(as.data.frame(configurationTable, stringsAsFactor = FALSE), digits=15)
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
  if (anyDuplicated(configurationTable)) {
    irace.error("Duplicated configurations in file ", filename, " :\n",
                paste0(capture.output(
                  configurationTable[duplicated(configurationTable), , drop=FALSE]), "\n"))
  }
  configurationTable
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
  configurations
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
  expr <- compiler::compile(substitute(is.na(x) | !(x), list(x = x)),
                           options = list(suppressUndefined=TRUE))
  attr(expr, "source") <- as.character(as.expression(x))
  expr
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
  # First we would need to handle the "is.na(x) | !(x)" case here.
  # Maybe: sapply(forbiddenExps, function(x) substitute(is.na(x) | !(x), list(x=x)))
  # x <- parse(text=paste0("(", paste0(forbiddenExps,collapse=")||("), ")"))
  # Byte-compile them.
  sapply(forbiddenExps, compile.forbidden)
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
  sapply(exps, compile.forbidden)
}

#' Reads from a file the scenario settings to be used by \pkg{irace}. 
#' 
#' @param filename (`character(1)`) \cr Filename from which the scenario will
#'   be read. If empty, the default `scenarioFile` is used.  An example
#'   scenario file is provided in `system.file(``package="irace",`
#'   `"templates/scenario.txt.tmpl")`.
#' @templateVar arg_appendix This is an initial scenario that is overwritten
#'   for every setting specified in the file to be read.
#' @template arg_scenario
#' @template arg_params_def
#' 
#' @return The scenario list read from the file. The scenario settings not
#'   present in the file are not present in the list, i.e., they are `NULL`.
#'
#' @seealso
#'  \describe{
#'  \item{[printScenario()]}{prints the given scenario.}
#'  \item{[defaultScenario()]}{returns the default scenario settings of \pkg{irace}.}
#'  \item{[checkScenario()]}{to check that the scenario is valid.}
#' }
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
readScenario <- function(filename = "", scenario = list(),
                         params_def = .irace.params.def)
{
  # This function allows recursively including scenario files.
  scenario_env <- new.env()
  include.scenario <- function(rfilename, topfile = filename, envir. = scenario_env)
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
    filename <- path_rel2abs(params_def["scenarioFile","default"])
    if (file.exists(filename)) {
      irace.warning("A default scenario file ", shQuote(filename),
                    " has been found and will be read.")
    } else {
      irace.error ("Not scenario file given (use ",
                   params_def["scenarioFile", "short"], " or ",
                   params_def["scenarioFile", "long"],
                   ") and no default scenario file ", shQuote(filename),
                   " has been found.")
    }
  } else {
    filename <- path_rel2abs(filename)
  }
  
  if (file.exists (filename)) {
    debug.level <- getOption(".irace.debug.level", default = 0)
    if (debug.level >= 1)
      cat("# Reading scenario file", shQuote(filename), ".......")
    # chdir = TRUE to allow recursive sourcing.
    handle.source.error <- function(e) {
      irace.error("Reading scenario file ", shQuote(filename),
                  " produced the following errors or warnings:\n",
                  paste0(conditionMessage(e), collapse="\n"))
      return(NULL)
    }
    withCallingHandlers(
      tryCatch(source(filename, local = scenario_env, chdir = TRUE),
               error = handle.source.error, warning = handle.source.error))
    if (debug.level >= 1) cat (" done!\n")
  } else {
    irace.error ("The scenario file ", shQuote(filename), " does not exist.")
  }
      
  ## Read scenario file variables.
  scenario[["scenarioFile"]] <- filename 
  # If these are given and relative, they should be relative to the
  # scenario file (except logFile, which is relative to execDir).
  pathParams <- setdiff(params_def[params_def[, "type"] == "p",
                                          "name"], "logFile")
  params_names <- params_def[!startsWith(params_def[,"name"], "."), "name"]
  
  for (param in params_names) {
    if (exists (param, envir = scenario_env, inherits = FALSE)) {
      value <- get(param, envir = scenario_env, inherits = FALSE)
      if (!is.null.or.empty(value) && is.character(value)
          && (param %in% pathParams)) {
        value <- path_rel2abs(value, cwd = dirname(filename))
      }
      scenario[[param]] <- value
    }
  }
  unknown_scenario_vars <- setdiff(ls(scenario_env), params_names)
  if (length(unknown_scenario_vars) > 0) {
    # We only accept variables that match irace.params.names and if the user
    # wants to define their own, they should use names starting with ".", which
    # are ignored by ls()
    irace.error("Scenario file ", shQuote(filename), " contains unknown variables: ",
                paste0(unknown_scenario_vars, collapse=", "),
                "\nMAKE SURE NO VARIABLE NAME IS MISSPELL (for example, 'parameterFile' is correct, while 'parametersFile' is not)",
                "\nIf you wish to use your own variables in the scenario file, use names beginning with a dot `.'")
  }
  scenario
}

setup_test_instances <- function(scenario)
{
  if (is.null.or.empty(scenario[["testInstances"]])) {
    if (!is.null.or.empty(scenario$testInstancesDir) || 
        !is.null.or.empty(scenario$testInstancesFile)) {
      scenario$testInstancesDir <- path_rel2abs(scenario$testInstancesDir)
      if (!is.null.or.empty(scenario$testInstancesFile)) {
        scenario$testInstancesFile <- path_rel2abs(scenario$testInstancesFile)
      }
      scenario[["testInstances"]] <-
        readInstances(instancesDir = scenario$testInstancesDir,
                      instancesFile = scenario$testInstancesFile)
    } else {
      scenario[["testInstances"]] <- NULL
    }
  }
  if (!is.null(scenario[["testInstances"]])
      && is.null(names(scenario[["testInstances"]]))) {
    # Create unique IDs for testInstances
    names(scenario[["testInstances"]]) <- paste0(1:length(scenario[["testInstances"]]), "t")
  }
  scenario
}    

#' Check and correct the given scenario
#'
#' Checks for errors a (possibly incomplete) scenario setup of
#' \pkg{irace}  and transforms it into a valid scenario.
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
#'  \item{[readScenario()]}{for reading a configuration scenario from a file.}
#'  \item{[printScenario()]}{prints the given scenario.}
#'  \item{[defaultScenario()]}{returns the default scenario settings of \pkg{irace}.}
#'  \item{[checkScenario()]}{to check that the scenario is valid.}
#' }
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
## FIXME: This function should only do checks and return TRUE/FALSE. There
## should be other function that does the various transformations.
checkScenario <- function(scenario = defaultScenario())
{
  quote.param <- function(name)
  {
    if (.irace.params.def[name, "long"] != "") {
      return(paste0("'", name, "' (", .irace.params.def[name, "long"], ")"))
    }
    paste0("'", name, "'")
  }

  as.boolean.param <- function(x, name)
  {
    x <- as.integer(x)
    if (is.na (x) || (x != 0 && x != 1)) {
      irace.error (quote.param(name), " must be either 0 or 1.")
    }
    as.logical(x)
  }

  check.valid.param <- function(x)
  {
    cat(x, "\n")
    cat(.irace.params.def[x, "domain"], "\n")
    valid <- trimws(strsplit(.irace.params.def[x, "domain"],",",fixed=TRUE)[[1]])
    if (scenario[[x]] %!in% valid) {
      irace.error ("Invalid value '", scenario[[x]], "' of ",
                   quote.param(x), ", valid values are: ",
                   paste0(valid, collapse = ", "))
    }
  }
    
  # Fill possible unset (NULL) with default settings.
  scenario <- defaultScenario(scenario)

  # Duplicated entries will cause confusion.
  dups <- anyDuplicated(names(scenario))
  if (dups > 0)
    irace.error("scenario contains duplicated entries: ", names(scenario)[dups])

  # Boolean control parameters.
  boolParams <- .irace.params.def[.irace.params.def[, "type"] == "b", "name"]
  for (p in boolParams) {
    scenario[[p]] <- as.boolean.param (scenario[[p]], p)
  }
  options(.irace.quiet = scenario$quiet)
  ## Check that everything is fine with external parameters
  # Check that the files exist and are readable.
  scenario$parameterFile <- path_rel2abs(scenario$parameterFile)
  # We don't read parameterFile here because the user may give the parameters
  # explicitly.  And it is validated in readParameters anyway.
  scenario$execDir <- path_rel2abs(scenario$execDir)
  file.check (scenario$execDir, isdir = TRUE,
              text = paste0("execution directory ", quote.param("execDir")))
  options(.irace.execdir = scenario$execDir)
  if (!is.null.or.empty(scenario$logFile)) {
    scenario$logFile <- path_rel2abs(scenario$logFile, cwd = scenario$execDir)
    file.check(scenario$logFile, writeable = TRUE, text = quote.param('logFile'))
  } else {
    # We cannot use NULL because defaultScenario() would override it.
    scenario$logFile <- ""
  }

  if (!is.null.or.empty(scenario$recoveryFile)) {
    scenario$recoveryFile <- path_rel2abs(scenario$recoveryFile)
    file.check(scenario$recoveryFile, readable = TRUE,
               text = paste0("recovery file ", quote.param("recoveryFile")))
    
    if (!is.null.or.empty(scenario$logFile) # Must have been set "" above.
        && scenario$recoveryFile == scenario$logFile) {
      irace.error("log file and recovery file should be different '",
                  scenario$logFile, "'")
    }
  } else {
    # We cannot use NULL because defaultScenario() would override it.
    scenario$recoveryFile <- ""
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
      scenario$targetRunner <- path_rel2abs(scenario$targetRunner)
      .irace$target.runner <- if (scenario$aclib)
                                target.runner.aclib else target.runner.default
      if (is.null.or.empty(scenario$targetRunnerLauncher)) {
        file.check (scenario$targetRunner, executable = TRUE,
                    text = paste0("target runner ", quote.param("targetRunner")))
      } else {
        scenario$targetRunnerLauncher <- path_rel2abs(scenario$targetRunnerLauncher)
        file.check (scenario$targetRunnerLauncher, executable = TRUE,
                    text = paste0("target runner launcher ", quote.param("targetRunnerLauncher")))
        check_launcher_args(scenario$targetRunnerLauncherArgs)
      }
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
    scenario$targetEvaluator <- path_rel2abs(scenario$targetEvaluator)
    file.check (scenario$targetEvaluator, executable = TRUE,
                text = "target evaluator")
    .irace$target.evaluator <- target.evaluator.default
  } else {
    irace.error(quote.param('targetEvaluator'), " must be a function or an executable program")
  }

  irace.assert(is.null(scenario$targetEvaluator) == is.null(.irace$target.evaluator))
  
  # Training instances
  if (is.null.or.empty(scenario$instances)) {
    scenario$trainInstancesDir <- path_rel2abs(scenario$trainInstancesDir)
    if (!is.null.or.empty(scenario$trainInstancesFile)) {
      scenario$trainInstancesFile <- path_rel2abs(scenario$trainInstancesFile)
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
  scenario <- setup_test_instances(scenario)

  # Configurations file
  if (!is.null.or.empty(scenario$configurationsFile)) {
    scenario$configurationsFile <- path_rel2abs(scenario$configurationsFile)
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
    scenario$forbiddenFile <- path_rel2abs(scenario$forbiddenFile)
    file.check (scenario$forbiddenFile, readable = TRUE,
                text = "forbidden configurations file")
    scenario$forbiddenExps <- readForbiddenFile(scenario$forbiddenFile)
    irace.note(length(scenario$forbiddenExps),
               " expression(s) specifying forbidden configurations read from '",
               scenario$forbiddenFile, "'\n")
  }

  # Make it NULL if it is "" or NA
  # FIXME: If it is a non-empty vector of strings, parse them as above.
  if (is_null_or_empty_or_na(scenario$forbiddenExps))
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
      irace.warning("Assuming 'mu = firstTest' because 'mu' cannot be lower than 'firstTest'\n")
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
    check.valid.param("batchmode")
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
    check.valid.param("cappingType")
    check.valid.param("boundType")
    if (scenario$boundPar < 1)
      irace.error("Invalid value (", scenario$boundPar, ") ",
                  quote.param("boundPar"), " must be >= 1")
  } else if (scenario$boundMax <= 0 || is.na(scenario$boundMax)) { # no capping
    scenario$boundMax <- NULL
  }

  if (is_null_or_empty_or_na(scenario$testType)) {
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
           check.valid.param("testType"))
  scenario
}

#' Prints the given scenario
#'
#' @template arg_scenario
#' 
#' @seealso
#'  \describe{
#'  \item{[readScenario()]}{for reading a configuration scenario from a file.}
#'  \item{[printScenario()]}{prints the given scenario.}
#'  \item{[defaultScenario()]}{returns the default scenario settings of \pkg{irace}.}
#'  \item{[checkScenario()]}{to check that the scenario is valid.}
#' }
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
printScenario <- function(scenario)
{
  params_names <- .irace.params.names
  cat("## irace scenario:\n")
  for (param in params_names) {
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
#' Return scenario object with default values.
#' 
#' @template arg_scenario
#'
#' @template arg_params_def
#' 
#' @return A list indexed by the \pkg{irace} parameter names,
#' containing the default values for each parameter, except for those
#' already present in the scenario passed as argument.
#' The scenario list contains the following elements: 
# __IRACE_OPTIONS__BEGIN__
#' \itemize{
#'  \item General options:
#'    \describe{
#'      \item{`scenarioFile`}{Path of the file that describes the configuration scenario setup and other irace settings. (Default: `"./scenario.txt"`)}
#'      \item{`execDir`}{Directory where the programs will be run. (Default: `"./"`)}
#'      \item{`logFile`}{File to save tuning results as an R dataset, either absolute path or relative to execDir. (Default: `"./irace.Rdata"`)}
#'      \item{`quiet`}{Reduce the output generated by irace to a minimum. (Default: `0`)}
#'      \item{`debugLevel`}{Debug level of the output of \code{irace}. Set this to 0 to silence all debug messages. Higher values provide more verbose debug messages. (Default: `0`)}
#'      \item{`seed`}{Seed of the random number generator (by default, generate a random seed). (Default: `NA`)}
#'      \item{`repairConfiguration`}{User-defined R function that takes a configuration generated by irace and repairs it. (Default: `""`)}
#'      \item{`postselection`}{Percentage of the configuration budget used to perform a postselection race of the best configurations of each iteration after the execution of irace. (Default: `0`)}
#'      \item{`aclib`}{Enable/disable AClib mode. This option enables compatibility with GenericWrapper4AC as targetRunner script. (Default: `0`)}
#'    }
#'  \item Elitist `irace`:
#'    \describe{
#'      \item{`elitist`}{Enable/disable elitist irace. (Default: `1`)}
#'      \item{`elitistNewInstances`}{Number of instances added to the execution list before previous instances in elitist irace. (Default: `1`)}
#'      \item{`elitistLimit`}{In elitist irace, maximum number per race of elimination tests that do not eliminate a configuration. Use 0 for no limit. (Default: `2`)}
#'    }
#'  \item Internal `irace` options:
#'    \describe{
#'      \item{`sampleInstances`}{Randomly sample the training instances or use them in the order given. (Default: `1`)}
#'      \item{`softRestart`}{Enable/disable the soft restart strategy that avoids premature convergence of the probabilistic model. (Default: `1`)}
#'      \item{`softRestartThreshold`}{Soft restart threshold value for numerical parameters. If \code{NA}, \code{NULL} or \code{""}, it is computed as \code{10^-digits}. (Default: `""`)}
#'      \item{`nbIterations`}{Maximum number of iterations. (Default: `0`)}
#'      \item{`nbExperimentsPerIteration`}{Number of runs of the target algorithm per iteration. (Default: `0`)}
#'      \item{`minNbSurvival`}{Minimum number of configurations needed to continue the execution of each race (iteration). (Default: `0`)}
#'      \item{`nbConfigurations`}{Number of configurations to be sampled and evaluated at each iteration. (Default: `0`)}
#'      \item{`mu`}{Parameter used to define the number of configurations sampled and evaluated at each iteration. (Default: `5`)}
#'    }
#'  \item Target algorithm parameters:
#'    \describe{
#'      \item{`parameterFile`}{File that contains the description of the parameters of the target algorithm. (Default: `"./parameters.txt"`)}
#'      \item{`forbiddenExps`}{Vector of R logical expressions that cannot evaluate to \code{TRUE} for any evaluated configuration. (Default: `""`)}
#'      \item{`forbiddenFile`}{File that contains a list of logical expressions that cannot be \code{TRUE} for any evaluated configuration. If empty or \code{NULL}, do not use forbidden expressions. (Default: `""`)}
#'      \item{`digits`}{Maximum number of decimal places that are significant for numerical (real) parameters. (Default: `4`)}
#'    }
#'  \item Target algorithm execution:
#'    \describe{
#'      \item{`targetRunner`}{Executable called for each configuration that executes the target algorithm to be tuned. See the templates and examples provided. (Default: `"./target-runner"`)}
#'      \item{`targetRunnerLauncher`}{Executable that will be used to launch the target runner, when \code{targetRunner} cannot be executed directly (.e.g, a Python script in Windows). (Default: `""`)}
#'      \item{`targetRunnerLauncherArgs`}{Command-line arguments provided to \code{targetRunnerLauncher}. The substrings \code{\{targetRunner\}} and \code{\{targetRunnerArgs\}} will be replaced by the value of the option \code{targetRunner} and by the arguments usually passed when calling \code{targetRunner}, respectively. Example: \code{"-m {targetRunner} --args {targetRunnerArgs}"}. (Default: `"{targetRunner} {targetRunnerArgs}"`)}
#'      \item{`targetRunnerRetries`}{Number of times to retry a call to \code{targetRunner} if the call failed. (Default: `0`)}
#'      \item{`targetRunnerData`}{Optional data passed to \code{targetRunner}. This is ignored by the default \code{targetRunner} function, but it may be used by custom \code{targetRunner} functions to pass persistent data around. (Default: `""`)}
#'      \item{`targetRunnerParallel`}{Optional R function to provide custom parallelization of \code{targetRunner}. (Default: `""`)}
#'      \item{`targetEvaluator`}{Optional script or R function that provides a numeric value for each configuration. See templates/target-evaluator.tmpl (Default: `""`)}
#'      \item{`deterministic`}{If the target algorithm is deterministic, configurations will be evaluated only once per instance. (Default: `0`)}
#'      \item{`parallel`}{Number of calls to \code{targetRunner} to execute in parallel. Values \code{0} or \code{1} mean no parallelization. (Default: `0`)}
#'      \item{`loadBalancing`}{Enable/disable load-balancing when executing experiments in parallel. Load-balancing makes better use of computing resources, but increases communication overhead. If this overhead is large, disabling load-balancing may be faster. (Default: `1`)}
#'      \item{`mpi`}{Enable/disable MPI. Use \code{Rmpi} to execute \code{targetRunner} in parallel (parameter \code{parallel} is the number of slaves). (Default: `0`)}
#'      \item{`batchmode`}{Specify how irace waits for jobs to finish when \code{targetRunner} submits jobs to a batch cluster: sge, pbs, torque, slurm or htcondor. \code{targetRunner} must submit jobs to the cluster using, for example, \code{qsub}. (Default: `0`)}
#'    }
#'  \item Initial configurations:
#'    \describe{
#'      \item{`initConfigurations`}{Data frame describing initial configurations (usually read from a file using \code{readConfigurations}). (Default: `""`)}
#'      \item{`configurationsFile`}{File that contains a table of initial configurations. If empty or \code{NULL}, all initial configurations are randomly generated. (Default: `""`)}
#'    }
#'  \item Training instances:
#'    \describe{
#'      \item{`instances`}{Character vector of the instances to be used in the \code{targetRunner}. (Default: `""`)}
#'      \item{`trainInstancesDir`}{Directory where training instances are located; either absolute path or relative to current directory. If no \code{trainInstancesFiles} is provided, all the files in \code{trainInstancesDir} will be listed as instances. (Default: `"./Instances"`)}
#'      \item{`trainInstancesFile`}{File that contains a list of training instances and optionally additional parameters for them. If \code{trainInstancesDir} is provided, \code{irace} will search for the files in this folder. (Default: `""`)}
#'    }
#'  \item Tuning budget:
#'    \describe{
#'      \item{`maxExperiments`}{Maximum number of runs (invocations of \code{targetRunner}) that will be performed. It determines the maximum budget of experiments for the tuning. (Default: `0`)}
#'      \item{`maxTime`}{Maximum total execution time in seconds for the executions of \code{targetRunner}. \code{targetRunner} must return two values: cost and time. (Default: `0`)}
#'      \item{`budgetEstimation`}{Fraction (smaller than 1) of the budget used to estimate the mean computation time of a configuration. Only used when \code{maxTime} > 0 (Default: `0.02`)}
#'      \item{`minMeasurableTime`}{Minimum time unit that is still (significantly) measureable. (Default: `0.01`)}
#'    }
#'  \item Statistical test:
#'    \describe{
#'      \item{`testType`}{Statistical test used for elimination. The default value selects \code{t-test} if \code{capping} is enabled or \code{F-test}, otherwise. Valid values are: F-test (Friedman test), t-test (pairwise t-tests with no correction), t-test-bonferroni (t-test with Bonferroni's correction for multiple comparisons), t-test-holm (t-test with Holm's correction for multiple comparisons). (Default: `""`)}
#'      \item{`firstTest`}{Number of instances evaluated before the first elimination test. It must be a multiple of \code{eachTest}. (Default: `5`)}
#'      \item{`eachTest`}{Number of instances evaluated between elimination tests. (Default: `1`)}
#'      \item{`confidence`}{Confidence level for the elimination test. (Default: `0.95`)}
#'    }
#'  \item Adaptive capping:
#'    \describe{
#'      \item{`capping`}{Enable the use of adaptive capping, a technique designed for minimizing the computation time of configurations. This is only available when \code{elitist} is active. (Default: `0`)}
#'      \item{`cappingType`}{Measure used to obtain the execution bound from the performance of the elite configurations.\itemize{\item median: Median performance of the elite configurations.\item mean: Mean performance of the elite configurations.\item best: Best performance of the elite configurations.\item worst: Worst performance of the elite configurations.} (Default: `"median"`)}
#'      \item{`boundType`}{Method to calculate the mean performance of elite configurations.\itemize{\item candidate: Mean execution times across the executed instances and the current one.\item instance: Execution time of the current instance.} (Default: `"candidate"`)}
#'      \item{`boundMax`}{Maximum execution bound for \code{targetRunner}. It must be specified when capping is enabled. (Default: `0`)}
#'      \item{`boundDigits`}{Precision used for calculating the execution time. It must be specified when capping is enabled. (Default: `0`)}
#'      \item{`boundPar`}{Penalization constant for timed out executions (executions that reach \code{boundMax} execution time). (Default: `1`)}
#'      \item{`boundAsTimeout`}{Replace the configuration cost of bounded executions with \code{boundMax}. (Default: `1`)}
#'    }
#'  \item Recovery:
#'    \describe{
#'      \item{`recoveryFile`}{Previously saved log file to recover the execution of \code{irace}, either absolute path or relative to the current directory.  If empty or \code{NULL}, recovery is not performed. (Default: `""`)}
#'    }
#'  \item Testing:
#'    \describe{
#'      \item{`testInstancesDir`}{Directory where testing instances are located, either absolute or relative to current directory. (Default: `""`)}
#'      \item{`testInstancesFile`}{File containing a list of test instances and optionally additional parameters for them. (Default: `""`)}
#'      \item{`testInstances`}{Character vector of the instances to be used in the \code{targetRunner} when executing the testing. (Default: `""`)}
#'      \item{`testNbElites`}{Number of elite configurations returned by irace that will be tested if test instances are provided. (Default: `1`)}
#'      \item{`testIterationElites`}{Enable/disable testing the elite configurations found at each iteration. (Default: `0`)}
#'    }
#' }
# __IRACE_OPTIONS__END__
#'
#' @seealso
#'  \describe{
#'  \item{[readScenario()]}{for reading a configuration scenario from a file.}
#'  \item{[printScenario()]}{prints the given scenario.}
#'  \item{[defaultScenario()]}{returns the default scenario settings of \pkg{irace}.}
#'  \item{[checkScenario()]}{to check that the scenario is valid.}
#' }
#'
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
defaultScenario <- function(scenario = list(),
                            params_def = .irace.params.def)
{
  params_names <- params_def[!startsWith(params_def[,"name"], "."), "name"]
  if (is.null(names(scenario))) {
    scenario <- setNames(as.list(params_def[params_names,"default"]), params_names)
  } else if (!all(names(scenario) %in% params_names)) {
    irace.error("Unknown scenario parameters: ",
                paste(names(scenario)[!(names(scenario) %in% params_names)],
                      collapse = ", "))
  } else {
    for (k in params_names) {
      if (is.null.or.na(scenario[[k]])) {
        scenario[[k]] <- params_def[k, "default"]
      }
    }
  }
  scenario
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
  instances
}

## Check targetRunner execution
checkTargetFiles <- function(scenario, parameters)
{
  ## Create two random configurations
  conf.id <- c("testConfig1", "testConfig2")
  configurations <- sampleUniform(parameters, length(conf.id),
                                  digits = scenario$digits,
                                  forbidden = scenario$forbiddenExps,
                                  repair = scenario$repairConfiguration)
  configurations <- cbind(.ID. = conf.id, configurations, stringsAsFactors=FALSE)

  # Read initial configurations provided by the user.
  initConfigurations <- allConfigurationsInit(scenario, parameters)
  if (nrow(initConfigurations) > 0L) {
    irace.assert(all(colnames(configurations) == colnames(initConfigurations)))
    configurations <- rbind(configurations, initConfigurations)
  }
  bounds <- rep(scenario$boundMax, nrow(configurations))

  instances.ID <- if (scenario$sampleInstances)
                    sample.int(length(scenario$instances), 1L) else 1L
  experiments <- createExperimentList(
    configurations, parameters, instances = scenario$instances,
    instances.ID = instances.ID, seeds = 1234567, scenario, bounds = bounds)

  startParallel(scenario)
  on.exit(stopParallel(), add = TRUE)

  # FIXME: Create a function try.call(err.msg,warn.msg, fun, ...)
  # Executing targetRunner
  cat("# Executing targetRunner (", nrow(configurations), "times)...\n")
  result <- TRUE
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
  if (!result) return(FALSE)
  
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
  result
}


