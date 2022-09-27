# =========================================================================
# irace: An implementation in R of Iterated Race.
# -------------------------------------------------------------------------
#
#  Copyright (C) 2010-2020
#  Manuel López-Ibáñez     <manuel.lopez-ibanez@manchester.ac.uk> 
#  Jérémie Dubois-Lacoste  <jeremie.dubois-lacoste@ulb.ac.be>
#  Leslie Perez Caceres    <leslie.perez.caceres@ulb.ac.be>
#
# -------------------------------------------------------------------------
#  This program is free software (software libre); you can redistribute
#  it and/or modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2 of the
#  License, or (at your option) any later version.
# 
#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, you can obtain a copy of the GNU
#  General Public License at:
#                  http://www.gnu.org/copyleft/gpl.html
#  or by writing to the  Free Software Foundation, Inc., 59 Temple Place,
#                  Suite 330, Boston, MA 02111-1307 USA
# -------------------------------------------------------------------------
# $Revision$
# =========================================================================

#' irace.license
#'
#' A character string containing the license information of \pkg{irace}.
#' 
#' @export
## __VERSION__ below will be replaced by the version defined in R/version.R
## This avoids constant conflicts within this file.
irace.license <-
'#------------------------------------------------------------------------------
# irace: An implementation in R of (Elitist) Iterated Racing
# Version: __VERSION__
# Copyright (C) 2010-2020
# Manuel Lopez-Ibanez     <manuel.lopez-ibanez@manchester.ac.uk>
# Jeremie Dubois-Lacoste  
# Leslie Perez Caceres    <leslie.perez.caceres@ulb.ac.be>
#
# This is free software, and you are welcome to redistribute it under certain
# conditions.  See the GNU General Public License for details. There is NO
# WARRANTY; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# irace builds upon previous code from the race package:
#     race: Racing methods for the selection of the best
#     Copyright (C) 2003 Mauro Birattari
#------------------------------------------------------------------------------
'
cat_irace_license <- function()
{
  cat(sub("__VERSION__", irace.version, irace.license, fixed=TRUE))
}

#' Higher-level interface to launch irace.
#'
#' @template arg_scenario
#' 
#' @param output.width (\code{integer(1)}) The width used for the screen
#'   output.
#'
#' @details This function checks the correctness of the scenario, reads the
#'   parameter space from \code{scenario$parameterFile}, invokes [irace()],
#'   prints its results in various formatted ways, (optionally) calls
#'   [psRace()] and, finally, evaluates the best configurations on the test
#'   instances (if provided). If you want a lower-level interface that just
#'   runs irace, please see function [irace()].
#'
#' @templateVar return_invisible TRUE
#' @template return_irace
#' @seealso
#' [irace.cmdline()] a higher-level command-line interface to
#'  [irace()]
#'  [readScenario()] to read the scenario setup from  a file.
#'  [defaultScenario()] to provide a default scenario for \pkg{irace}.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @concept running
#' @export
irace.main <- function(scenario, output.width = 9999L)
  irace_common(scenario = scenario, simple=FALSE, output.width = output.width)

#' Test configurations given in `.Rdata` file
#'
#' `testing_fromlog` executes the testing of the target algorithm configurations
#' found by an \pkg{irace} execution.
#' 
#' @param logFile Path to the `.Rdata` file produced by \pkg{irace}.
#'
#' @param testNbElites Number of (final) elite configurations to test. Overrides
#'   the value found in `logFile`.
#' 
#' @param testIterationElites (`logical(1)`) If `FALSE`, only the final
#'   `testNbElites` configurations are tested; otherwise, also test the best
#'   configurations of each iteration. Overrides the value found in `logFile`.
#'
#' @param testInstancesDir  Directory where testing instances are located, either absolute or relative to current directory.
#'
#' @param testInstancesFile File containing a list of test instances and optionally additional parameters for them.
#'
#' @param testInstances Character vector of the instances to be used in the `targetRunner` when executing the testing.
#'
#' @return Boolean. `TRUE` if the testing ended successfully otherwise, `FALSE`.
#' 
#' @details The function `testing_fromlog` loads the `logFile` and obtains the
#'   testing setup and configurations to be tested.  Within the `logFile`, the
#'   variable `scenario$testNbElites` specifies how many final elite
#'   configurations to test and `scenario$testIterationElites` indicates
#'   whether test the best configuration of each iteration. The values may be
#'   overridden by setting the corresponding arguments in this function.  The
#'   set of testing instances must appear in `scenario[["testInstances"]]`.
#'
#' @seealso [defaultScenario()] to provide a default scenario for \pkg{irace}.
#' [testing_fromfile()] provides a different interface for testing.
#' 
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept running
#' @export
testing_fromlog <- function(logFile, testNbElites, testIterationElites,
                            testInstancesDir, testInstancesFile, testInstances)
{
  if (is.null.or.empty(logFile)) {
    irace.note("No logFile provided to perform the testing of configurations. Skipping testing.\n")
    return(FALSE)
  }
  iraceResults <- read_logfile(logFile)
  scenario <- iraceResults[["scenario"]]
  parameters <- iraceResults[["parameters"]]
  instances_changed <- FALSE
  
  if (!missing(testNbElites))
    scenario[["testNbElites"]] <- testNbElites
  if (!missing(testIterationElites))
    scenario$testIterationElites <- testIterationElites

  if (!missing(testInstances))
    scenario[["testInstances"]] <- testInstances
    
  if (!missing(testInstancesDir)) {
    scenario$testInstancesDir <- testInstancesDir
    instances_changed <- TRUE
  }
  if (!missing(testInstancesFile)) {
    scenario$testInstancesFile <- testInstancesFile
    instances_changed <- TRUE
  }
  
  cat("\n\n# Testing of elite configurations:", scenario$testNbElites, 
      "\n# Testing iteration configurations:", scenario$testIterationElites,"\n")
  if (scenario$testNbElites <= 0)
    return (FALSE)

  # If they are already setup, don't change them.
  if (instances_changed || is.null.or.empty(scenario[["testInstances"]])) {
    scenario <- setup_test_instances(scenario)
    if (is.null.or.empty(scenario[["testInstances"]])) {
      irace.note("No test instances, skip testing\n")
      return(FALSE)
    }
  }
  
  # Get configurations that will be tested
  if (scenario$testIterationElites)
    testing_id <- sapply(iraceResults$allElites, function(x)
      x[1:min(length(x), scenario$testNbElites)])
  else {
    tmp <- iraceResults$allElites[[length(iraceResults$allElites)]]
    testing_id <- tmp[1:min(length(tmp), scenario$testNbElites)]
  }
  testing_id <- unique.default(unlist(testing_id))
  configurations <- iraceResults$allConfigurations[testing_id, , drop=FALSE]

  irace.note ("Testing configurations (in no particular order): ", paste(testing_id, collapse=" "), "\n")
  testing_common(configurations, scenario, parameters, iraceResults)
  return(TRUE)
}

#' Test configurations given an explicit table of configurations and a scenario file
#'
#' Executes the testing of an explicit list of configurations given in
#' `filename` (same format as in [readConfigurationsFile()]). A `logFile` is
#' created unless disabled in `scenario`. This may overwrite an existing one!
#' 
#' @param filename Path to a file containing configurations: one configuration
#'   per line, one parameter per column, parameter names in header.
#'
#' @template arg_scenario
#'
#' @return iraceResults
#'
#' @seealso [testing_fromlog()] provides a different interface for testing.
#' 
#' @author Manuel López-Ibáñez
#' @concept running
#' @export
testing_fromfile <- function(filename, scenario)
{
  irace.note ("Checking scenario\n")
  scenario <- checkScenario(scenario)
  if (!scenario$quiet) printScenario(scenario)

  irace.note("Reading parameter file '", scenario$parameterFile, "'.\n")
  parameters <- readParameters (file = scenario$parameterFile,
                                digits = scenario$digits)
  configurations <- readConfigurationsFile (filename, parameters)
  configurations <- cbind(.ID. = 1:nrow(configurations),
                             configurations,
                             .PARENT. = NA)
  rownames(configurations) <- configurations$.ID.
  num <- nrow(configurations)
  configurations <- checkForbidden(configurations, scenario$forbiddenExps)
  if (nrow(configurations) < num) {
    irace.warning("Some of the configurations in the configurations file were forbidden",
                  "and, thus, discarded")
  }
  # To save the logs
  iraceResults <- list(scenario = scenario,
                       irace.version = irace.version,
                       parameters = parameters,
                       allConfigurations = configurations)
    
  irace.note ("Testing configurations (in the order given as input): \n")
  iraceResults <- testing_common(configurations, scenario, parameters, iraceResults)
  return(iraceResults)
}

testing_common <- function(configurations, scenario, parameters, iraceResults)
{
  verbose <- !scenario$quiet
  if (verbose) configurations.print(configurations)
  iraceResults$testing <- testConfigurations(configurations, scenario, parameters)
  irace_save_logfile (iraceResults, scenario)
  # FIXME : We should print the seeds also. As an additional column?
  irace.note ("Testing results (column number is configuration ID in no particular order):\n")
  if (verbose) print(iraceResults$testing$experiments)
  irace.note ("Finished testing\n")
  return(iraceResults)
}

#' Test that the given irace scenario can be run.
#'
#' Test that the given irace scenario can be run by checking the scenario
#' settings provided and trying to run the target-algorithm.
#' 
#' @template arg_scenario
#' @template arg_parameters
#'
#' @return returns \code{TRUE} if successful and gives an error and returns
#' \code{FALSE} otherwise.
#' 
#' @details If the `parameters` argument is missing, then the parameters 
#'   will be read from the file `parameterFile`  given by `scenario`. If
#'   `parameters` is provided, then `parameterFile` will not be read.  This function will
#'   try to execute the target-algorithm.
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
checkIraceScenario <- function(scenario, parameters)
{
  irace.note ("Checking scenario\n")
  scenario$debugLevel <- 2 
  scenario <- checkScenario(scenario)
  if (!scenario$quiet) printScenario(scenario)
 
  if (missing(parameters)) {
    irace.note("Reading parameter file '", scenario$parameterFile, "'.\n")
    parameters <- readParameters (file = scenario$parameterFile,
                                  digits = scenario$digits,
                                  debugLevel = 2)
  } else if (!is.null.or.empty(scenario$parameterFile)) {
    if (!scenario$quiet) 
      cat("# checkIraceScenario(): 'parameters' provided by user. ",
          "Parameter file '", scenario$parameterFile, "' will be ignored\n", sep = "")
  }
  checkParameters(parameters)
  irace.note("Checking target runner.\n")
  if (checkTargetFiles(scenario = scenario, parameters = parameters)) {
    irace.note("Check successful.\n")
    return(TRUE)
  } else {
    irace.error("Check unsuccessful.\n")
    return(FALSE)
  }
}

init <- function() 
{
  irace.note("Initializing working directory...\n")
  libPath <- system.file(package = "irace")
  tmplFiles <- list.files(file.path(libPath, "templates"))
  for (file in tmplFiles) {
    if (grepl(".tmpl", file) && (file != "target-evaluator.tmpl")) {
      newFile <- gsub(".tmpl", "", file)
      if ((file == "target-runner.tmpl") && .Platform$OS.type == 'windows') {
        file.copy(file.path(libPath, "templates", "windows", "target-runner.bat"), file.path(getwd(), "target-runner.bat"), overwrite = FALSE)
      } else {
        file.copy(file.path(libPath, "templates", file), file.path(getwd(), newFile), overwrite = FALSE)
      }
    }
  }
}


#' Launch `irace` with command-line options.
#'
#' Calls [irace.main()] using command-line options, maybe parsed from the
#' command line used to invoke R.
#' 
#' @param argv (\code{character()}) \cr The arguments 
#' provided on the R command line as a character vector, e.g., 
#' \code{c("--scenario", "scenario.txt", "-p", "parameters.txt")}.
#' Using the  default value (not providing the parameter) is the 
#' easiest way to call \code{irace.cmdline}.
#' 
#' @details The function reads the parameters given on the command line
#' used to invoke R, finds the name of the scenario file,
#'  initializes the scenario from the file (with the function
#'  \code{\link{readScenario}}) and possibly from parameters passed in
#'  the command line. It finally starts \pkg{irace} by calling
#'  \code{\link{irace.main}}.
#'
#' List of command-line options:
#' ```{r echo=FALSE,comment=NA}
#' cmdline_usage(.irace.params.def)
#' ```
#'
#' @templateVar return_invisible TRUE
#' @template return_irace
#' 
#' @seealso
#'  [irace.main()] to start \pkg{irace} with a given scenario.
#' @examples
#' irace.cmdline("--version")
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @concept running
#' @export
irace.cmdline <- function(argv = commandArgs(trailingOnly = TRUE))
{
  parser <- CommandArgsParser$new(argv = argv, argsdef = .irace.params.def)
  quiet <- !is.null(parser$readArg (short = "-q", long = "--quiet")) 
  if (quiet) {
    op <- options(.irace.quiet = TRUE)
    on.exit(options(op))
  } else {
    cat_irace_license()
    cat("# installed at: ", system.file(package="irace"), "\n",
        "# called with: ", paste(argv, collapse = " "), "\n", sep = "")
  }
  if (!is.null(parser$readArg (short = "-h", long = "--help"))) {
    parser$cmdline_usage()
    return(invisible(NULL))
  }
  if (!is.null(parser$readArg(short = "-v", long = "--version"))) {
    print(citation(package="irace"))
    return(invisible(NULL))
  }
  
  if (!is.null(parser$readArg(short = "-i", long = "--init"))) {
    init()
    return(invisible(NULL))
  }
  
  # Read the scenario file and the command line
  scenarioFile <- parser$readCmdLineParameter ("scenarioFile", default = "")
  scenario <- readScenario(scenarioFile)
  for (param in .irace.params.names) {
    scenario[[param]] <-
      parser$readCmdLineParameter(paramName = param,
                                  default = scenario[[param]])
  }
  if (quiet) scenario$quiet <- TRUE
 
  # Check scenario
  if (!is.null(parser$readArg (short = "-c", long = "--check"))) {
    checkIraceScenario(scenario)
    return(invisible(NULL))
  }

  # Only do testing
  testFile <- parser$readArg (long = "--only-test")
  if (!is.null(testFile)) {
    return(invisible(testing_fromfile(testFile, scenario)))
  }

  if (length(parser$argv) > 0) {
    irace.error ("Unknown command-line options: ", paste(parser$argv, collapse = " "))
  }

  irace_common(scenario = scenario, simple=FALSE)
}
