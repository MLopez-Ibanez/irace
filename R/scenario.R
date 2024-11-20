#' Reads from a file the scenario settings to be used by \pkg{irace}.
#'
#' The scenario argument is an initial scenario that is overwritten for every
#' setting specified in the file to be read.
#' 
#' @param filename `character(1)`\cr Filename from which the scenario will
#'   be read. If empty, the default `scenarioFile` is used.  An example
#'   scenario file is provided in `system.file(package="irace", "templates/scenario.txt.tmpl")`.
#' @inheritParams defaultScenario 
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
  filename_given <- filename != ""
  if (!filename_given) {
    filename <- path_rel2abs(params_def["scenarioFile","default"])
    if (file.exists(filename)) {
      irace.warning("A default scenario file ", shQuote(filename),
                    " has been found and will be read.")
    } else {
      irace.warning ("No scenario file given (use ",
                   params_def["scenarioFile", "short"], " or ",
                   params_def["scenarioFile", "long"],
                   ") and no default scenario file ", shQuote(filename),
                   " has been found.")
      scenario[["scenarioFile"]] <- filename
      return(scenario)
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
  } else if (filename_given) {
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
      if (filename_given && !is.null.or.empty(value) && is.character(value) && (param %in% pathParams)) {
        value <- path_rel2abs(value, cwd = dirname(filename))
      }
      scenario[[param]] <- value
    }
  }
  unknown_scenario_vars <- setdiff(ls(scenario_env), params_names)
  if (length(unknown_scenario_vars)) {
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
  testInstances <- scenario[["testInstances"]]
  if (is.null.or.empty(testInstances)) {
    if (!is.null.or.empty(scenario$testInstancesDir) || 
        !is.null.or.empty(scenario$testInstancesFile)) {
      scenario$testInstancesDir <- path_rel2abs(scenario$testInstancesDir)
      if (!is.null.or.empty(scenario$testInstancesFile)) {
        scenario$testInstancesFile <- path_rel2abs(scenario$testInstancesFile)
      }
      testInstances <- readInstances(instancesDir = scenario$testInstancesDir,
                                     instancesFile = scenario$testInstancesFile)
    } else {
      testInstances <- NULL
    }
  }
  if (!is.null(testInstances)) {
    if (!is.null(dim(testInstances))) {
      if (length(dim(testInstances)) == 1L ||
          (length(dim(testInstances)) == 2L && dim(testInstances)[1L] == 1L)) {
        # Remove useless dimensions
        testInstances <- c(testInstances)
      } else {
        irace.error("testInstances must be a one-dimensional vector or a list. If your instances are matrices or datasets in R, you can use scenario(testInstances=list(data1, data2, data3))")
      }
    }
    if (is.null(names(testInstances))) {
      # Create unique IDs for testInstances
      names(testInstances) <- paste0(seq_along(testInstances), "t")
    }
  }
  scenario[["testInstances"]] <- testInstances
  scenario
}

#' Check and correct the given scenario
#'
#' Checks for errors a (possibly incomplete) scenario setup of
#' \pkg{irace}  and transforms it into a valid scenario.
#' 
#' @inheritParams defaultScenario
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
    if (is.logical(x)) return(x)
    if (is.na(x)) return(as.logical(NA))
    # We handle "0" and "1" but not "TRUE" and "FALSE".
    x <- suppressWarnings(as.integer(x)) 
    if (is.na(x) || (x != 0L && x != 1L)) {
      irace.error (quote.param(name), " must be either 0 or 1.")
    }
    as.logical(x)
  }

  check.valid.param <- function(x)
  {
    valid <- trimws(strsplit(.irace.params.def[x, "domain"],",",fixed=TRUE)[[1L]])
    if (scenario[[x]] %not_in% valid) {
      irace.error ("Invalid value '", scenario[[x]], "' of ",
                   quote.param(x), ", valid values are: ",
                   paste0(valid, collapse = ", "))
    }
  }
    
  # Fill possible unset (NULL) with default settings.
  scenario <- defaultScenario(scenario)

  # Duplicated entries will cause confusion.
  dups <- anyDuplicated(names(scenario))
  if (dups)
    irace.error("scenario contains duplicated entries: ", names(scenario)[dups])

  # We have characters everywhere, set to the right types to avoid problems
  # later.
  
  # Boolean control parameters.
  boolParams <- .irace.params.def[.irace.params.def[, "type"] == "b", "name"]
  for (p in boolParams) {
    scenario[[p]] <- as.boolean.param(scenario[[p]], p)
  }
  # Real [0, 1] control parameters.
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

  # Integer control parameters
  intParams <- .irace.params.def[.irace.params.def[, "type"] == "i", "name"]
  for (param in intParams) {
    p <- scenario[[param]]
    if (is.null.or.empty(p))
      scenario[[param]] <- NA
    if (is.na(scenario[[param]]))
      next # Allow NA default values
    p <- suppressWarnings(as.numeric(p))
    if (is.null(p) || is.na (p) || !is.wholenumber(p) || p < 0)
      irace.error (quote.param (param), " must be a non-negative integer.")
    scenario[[param]] <- as.integer(p)
  }

  check_positive <- function(scenario, what) {
    if (any(scenario[what] <= 0L)) {
      for (op in what) {
        if (scenario[[op]] <= 0L)
          irace.error(quote.param (op), " = ", scenario[[op]], " must be larger than 0.")
      }
    }
  }
  check_positive(scenario, c("firstTest", "eachTest", "blockSize"))

  options(.irace.quiet = scenario$quiet)
  ## Check that everything is fine with external parameters
  # Check that the files exist and are readable.
  scenario$parameterFile <- path_rel2abs(scenario$parameterFile)
  if (is.null.or.empty(scenario$parameters)) {
    irace.note("Reading parameter file '", scenario$parameterFile, "'.\n")
    scenario$parameters <- readParameters(file = scenario$parameterFile,
      # AClib benchmarks use 15 digits
      digits = if (scenario$aclib) 15L else 4L, debugLevel = scenario$debugLevel)
  }
  scenario$parameters <- checkParameters(scenario$parameters)
    
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

  if (is.null.or.empty(scenario$repairConfiguration))
    scenario$repairConfiguration <- NULL
  else if (is.function.name(scenario$repairConfiguration))    # Byte-compile it.
    scenario$repairConfiguration <- bytecompile(get.function(scenario$repairConfiguration))
  else 
    irace.error("'repairConfiguration' must be a function")
  
  if (is.na(scenario$capping))
    scenario$capping <- (scenario$elitist && scenario$maxTime > 0 &&
                           !is.na(scenario$boundMax) && scenario$boundMax > 0)
  if (scenario$capping) {
    if (!scenario$elitist) 
      irace.error("When capping == TRUE, elitist must be enabled.")
    if (scenario$boundMax <= 0) 
      irace.error("When capping == TRUE, boundMax (", scenario$boundMax, ") must be > 0")
    check.valid.param("cappingType")
    check.valid.param("boundType")
    if (scenario$boundPar < 1)
      irace.error("Invalid value (", scenario$boundPar, ") ",
                  quote.param("boundPar"), " must be >= 1")
  } else if (scenario$boundMax <= 0 || is.na(scenario$boundMax)) { # no capping
    scenario$boundMax <- NULL
  }

  if (is.function.name(scenario$targetRunner)) {
    scenario$targetRunner <- get.function(scenario$targetRunner)
    irace.assert(is.function(scenario$targetRunner))
  } else if (is.null(scenario$targetRunnerParallel)) {
    if (is.character(scenario$targetRunner)) {
      scenario$targetRunner <- path_rel2abs(scenario$targetRunner)
      if (is.null.or.empty(scenario$targetRunnerLauncher)) {
        file.check (scenario$targetRunner, executable = TRUE,
                    text = paste0("target runner ", quote.param("targetRunner")))
      } else { 
        scenario$targetRunnerLauncher <- path_rel2abs(scenario$targetRunnerLauncher)
        file.check (scenario$targetRunnerLauncher, executable = TRUE,
                    text = paste0("target runner launcher ", quote.param("targetRunnerLauncher")))
        file.check (scenario$targetRunner, readable = TRUE,
                    text = paste0("target runner ", quote.param("targetRunner")))
        if (!grepl("{targetRunner}", scenario$targetCmdline, fixed=TRUE)) {
          # If missing, we add it to the beginning for compatibility with irace 3.5.
          scenario$targetCmdline <- paste0("{targetRunner} ", scenario$targetCmdline)
        }
      }
      check_target_cmdline(scenario$targetCmdline, capping = scenario$capping)
    } else {
      irace.error(quote.param ('targetRunner'), " must be a function or an executable program")
    }
  }

  if (is.null.or.empty(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- NULL
  } else if (is.function.name(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- get.function(scenario$targetEvaluator)
    irace.assert(is.function(scenario$targetEvaluator))
  } else if (is.character(scenario$targetEvaluator)) {
    scenario$targetEvaluator <- path_rel2abs(scenario$targetEvaluator)
    file.check (scenario$targetEvaluator, executable = TRUE,
                text = "target evaluator")
  } else {
    irace.error(quote.param('targetEvaluator'), " must be a function or an executable program")
  }

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
  if (length(scenario$instances) == 0L) {
    irace.error("No instances found in `scenario$instances`")
  } else if (!is.null(dim(scenario$instances))) {
    if (length(dim(scenario$instances)) == 1L ||
        (length(dim(scenario$instances)) == 2L && dim(scenario$instances)[1] == 1L)) {
      # Remove useless dimensions
      scenario$instances <- c(scenario$instances)
    } else {
      irace.error("Instances must be a one-dimensional vector or a list. If your instances are matrices or datasets in R, you can use scenario(instances=list(data1, data2, data3))")
    }
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
    irace.error("if given, 'initConfigurations' must be a matrix or data.frame.")
  }
  
  if (length(scenario$instances) %% scenario$blockSize != 0) {
    irace.error("The number of instances (", length(scenario$instances), ") must be a multiple of ",
                quote.param("blockSize"), ".")
  }
  
  if (scenario$firstTest %% scenario$eachTest != 0) {
    irace.error(quote.param("firstTest"), " must be a multiple of ",
                quote.param("eachTest"), ".")
  }
  
  if (scenario$mu < scenario$firstTest * scenario$blockSize) {
    if (scenario$debugLevel >= 1) {
      irace.warning("Assuming 'mu = firstTest * blockSize' because 'mu' cannot be smaller.\n")
    }
    scenario$mu <- scenario$firstTest * scenario$blockSize
  } else if (scenario$mu %% scenario$blockSize != 0) {
    irace.error(quote.param("mu"), " must be a multiple of ",
                quote.param("blockSize"), ".")
  }
  
  if (!is.na(scenario$minExperiments))
    scenario$maxExperiments <- max(scenario$maxExperiments, scenario$minExperiments)
  
  ## Only maxExperiments or maxTime should be set. Negative values are not
  ## allowed.
  if (scenario$maxExperiments == 0 && scenario$maxTime == 0) {
    irace.error("Tuning budget was not provided. Set ",
                quote.param("maxExperiments"), " or ", quote.param("maxTime"), ".")
  } else if (scenario$maxExperiments > 0 && scenario$maxTime > 0) {
    irace.error("Two different tuning budgets provided, please set only ",
                quote.param("maxExperiments"), " or only ", quote.param ("maxTime"), ".")
  } else if (scenario$maxExperiments < 0 ) {
    irace.error("Negative budget provided, ", quote.param("maxExperiments"),
                "must be >= 0." )
  } else if (scenario$maxTime < 0) {
    irace.error("Negative budget provided, ", quote.param("maxTime"), " must be >= 0.")
  }
  
  if (scenario$maxTime > 0 && (scenario$budgetEstimation <= 0 || scenario$budgetEstimation >= 1)) 
    irace.error(quote.param("budgetEstimation"), " must be within (0,1).")

  if (scenario$maxTime > 0 && !scenario$elitist)
    irace.error(quote.param("maxTime"), " requires using 'elitist=1'")
  
  if (scenario$deterministic &&
      scenario$firstTest * scenario$blockSize > length(scenario$instances)) {
    irace.error("When deterministic == TRUE, the number of instances (",
                length(scenario$instances),
                ") cannot be smaller than firstTest (", scenario$firstTest, ") * blockSize (", scenario$blockSize, ")")
  }

  if (scenario$mpi && scenario$parallel < 2)
    irace.error (quote.param("parallel"), " must be larger than 1 when mpi is enabled.")
 
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
#' @inheritParams defaultScenario
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
  scenario_names <- .irace.params.names
  cat("## irace scenario:\n")
  for (name in scenario_names) {
    cat(name, " = ", deparse(scenario[[name]]), "\n", sep = "")
  }
  cat("## end of irace scenario\n")
}

#' Default scenario settings
#'
#' Return scenario object with default values.
#' 
#' @param scenario `list()`\cr Data structure containing \pkg{irace}
#'   settings. The data structure has to be the one returned by the function
#'   [defaultScenario()] or [readScenario()].
#'
#' @param params_def `data.frame()`\cr Definition of the options accepted by
#'   the scenario. This should only be modified by packages that wish to extend
#'   \pkg{irace}.
#'
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
#'      \item{`postselection`}{Perform a postselection race after the execution of irace to consume all remaining budget. Value 0 disables the postselection race. (Default: `1`)}
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
#'      \item{`softRestartThreshold`}{Soft restart threshold value for numerical parameters. (Default: `1e-04`)}
#'      \item{`nbIterations`}{Maximum number of iterations. (Default: `0`)}
#'      \item{`nbExperimentsPerIteration`}{Number of runs of the target algorithm per iteration. (Default: `0`)}
#'      \item{`minNbSurvival`}{Minimum number of configurations needed to continue the execution of each race (iteration). (Default: `0`)}
#'      \item{`nbConfigurations`}{Number of configurations to be sampled and evaluated at each iteration. (Default: `0`)}
#'      \item{`mu`}{Parameter used to define the number of configurations sampled and evaluated at each iteration. (Default: `5`)}
#'    }
#'  \item Target algorithm parameters:
#'    \describe{
#'      \item{`parameterFile`}{File that contains the description of the parameters of the target algorithm. (Default: `"./parameters.txt"`)}
#'      \item{`parameters`}{Parameters space object (usually read from a file using \code{readParameters}). (Default: `""`)}
#'    }
#'  \item Target algorithm execution:
#'    \describe{
#'      \item{`targetRunner`}{Executable called for each configuration that executes the target algorithm to be tuned. See the templates and examples provided. (Default: `"./target-runner"`)}
#'      \item{`targetRunnerLauncher`}{Executable that will be used to launch the target runner, when \code{targetRunner} cannot be executed directly (e.g., a Python script in Windows). (Default: `""`)}
#'      \item{`targetCmdline`}{Command-line arguments provided to \code{targetRunner} (or \code{targetRunnerLauncher} if defined). The substrings \code{\{configurationID\}}, \code{\{instanceID\}},  \code{\{seed\}},  \code{\{instance\}}, and \code{\{bound\}} will be replaced by their corresponding values. The substring \code{\{targetRunnerArgs\}} will be replaced by the concatenation of the switch and value of all active parameters of the particular configuration being evaluated.  The substring \code{\{targetRunner\}}, if present, will be replaced by the value of \code{targetRunner} (useful when using \code{targetRunnerLauncher}). (Default: `"{configurationID} {instanceID} {seed} {instance} {bound} {targetRunnerArgs}"`)}
#'      \item{`targetRunnerRetries`}{Number of times to retry a call to \code{targetRunner} if the call failed. (Default: `0`)}
#'      \item{`targetRunnerTimeout`}{Timeout in seconds of any \code{targetRunner} call (only applies to \code{target-runner} executables not to R functions), ignored if 0. (Default: `0`)}
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
#'      \item{`trainInstancesDir`}{Directory where training instances are located; either absolute path or relative to current directory. If no \code{trainInstancesFiles} is provided, all the files in \code{trainInstancesDir} will be listed as instances. (Default: `""`)}
#'      \item{`trainInstancesFile`}{File that contains a list of training instances and optionally additional parameters for them. If \code{trainInstancesDir} is provided, \code{irace} will search for the files in this folder. (Default: `""`)}
#'      \item{`blockSize`}{Number of training instances, that make up a `block' in \code{trainInstancesFile}. Elimination of configurations will only be performed after evaluating a complete block and never in the middle of a block. Each block typically contains one instance from each instance class (type or family) and the block size is the number of classes. (Default: `1`)}
#'    }
#'  \item Tuning budget:
#'    \describe{
#'      \item{`maxExperiments`}{Maximum number of runs (invocations of \code{targetRunner}) that will be performed. It determines the maximum budget of experiments for the tuning. (Default: `0`)}
#'      \item{`minExperiments`}{Minimum number of runs (invocations of \code{targetRunner}) that will be performed. It determines the minimum budget of experiments for the tuning. The actual budget depends on the number of parameters and \code{minSurvival}. (Default: `NA`)}
#'      \item{`maxTime`}{Maximum total execution time for the executions of \code{targetRunner}. \code{targetRunner} must return two values: cost and time. This value and the one returned by \code{targetRunner} must use the same units (seconds, minutes, iterations, evaluations, ...). (Default: `0`)}
#'      \item{`budgetEstimation`}{Fraction (smaller than 1) of the budget used to estimate the mean computation time of a configuration. Only used when \code{maxTime} > 0 (Default: `0.05`)}
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
#'      \item{`capping`}{Enable the use of adaptive capping, a technique designed for minimizing the computation time of configurations. Capping is enabled by default if \code{elitist} is active, \code{maxTime > 0} and  \code{boundMax > 0}. (Default: `NA`)}
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
    irace.error("Unknown scenario settings: ",
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

update_scenario <- function(scenario, ...)
{
  scenario_args <- list(...)
  if (length(scenario_args) == 0L)
    return(scenario)
  unknown_scenario_args <- setdiff(names(scenario_args), names(scenario))
  if (length(unknown_scenario_args))
    irace.error("Unknown scenario settings given: ", paste0(unknown_scenario_args, collapse=", "))
  modifyList(scenario, scenario_args)
}
