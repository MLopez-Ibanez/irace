# =========================================================================
# irace: An implementation in R of Iterated Race.
# -------------------------------------------------------------------------
#
#  Copyright (C) 2010-2018
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
# Copyright (C) 2010-2019
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
cat.irace.license <- function()
{
  cat(sub("__VERSION__", irace.version, irace.license, fixed=TRUE))
}

#' irace.usage
#'
#' \code{irace.usage}  This function prints all command-line options of \pkg{irace},
#'   with the corresponding switches and a short description.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
irace.usage <- function ()
{
  cat.irace.license()
  cat ("# installed at: ", system.file(package="irace"), "\n", sep = "")

  for (i in seq_len(nrow(.irace.params.def))) {
    short <- .irace.params.def[i,"short"]
    long <- .irace.params.def[i,"long"]
    desc <- .irace.params.def[i,"description"]
    if (desc == "" || (short == "" && long == "")) next
    cat(sep = "\n",
        strwrap(desc, width = 80,
                initial = sprintf("%2s %-20s  ", short, long),
                exdent = 25))
  }
}

#' irace.main
#'
#' \code{irace.main} is a higher-level interface to invoke \code{\link{irace}}.
#' 
#' @param scenario \code{\link{defaultScenario}}() The scenario setup of 
#' \pkg{irace}.
#' @param output.width 9999 The width that must be used for the screen
#' output.
#'
#' @details  The function \code{irace.main} checks the correctness of the
#' scenario, prints it, reads the parameter space from
#' \code{scenario$parameterFile}, invokes \code{\link{irace}} and
#' prints its results in various formatted ways. If you want a
#' lower-level interface, please see function \code{\link{irace}}.
#'
#' @seealso
#'  \code{\link{irace.cmdline}} a higher-level command-line interface to
#'  \code{irace.main}.
#'  \code{\link{readScenario}} to read the scenario setup from  a file.
#'  \code{\link{defaultScenario}} to provide a default scenario for \pkg{irace}.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
irace.main <- function(scenario = defaultScenario(), output.width = 9999)
{
  op <- options(width = output.width) # Do not wrap the output.
  on.exit(options(op), add = TRUE)

  scenario <- checkScenario (scenario)
  debug.level <- scenario$debugLevel
  
  if (debug.level >= 1) {
    op.debug <- options(warning.length = 8170,
                        error = if (interactive()) utils::recover
                                else irace.dump.frames)
    on.exit(options(op.debug), add = TRUE)
    printScenario (scenario)
  }
  
  # Read parameters definition
  parameters <- readParameters (file = scenario$parameterFile,
                                digits = scenario$digits,
                                debugLevel = debug.level)
						
  if (debug.level >= 2) { irace.note("Parameters have been read\n") }
  
  eliteConfigurations <- irace (scenario = scenario, parameters = parameters)
  
  cat("# Best configurations (first number is the configuration ID;",
      " listed from best to worst according to the ",
      test.type.order.str(scenario$testType), "):\n", sep = "")
  configurations.print(eliteConfigurations)
  
  cat("# Best configurations as commandlines (first number is the configuration ID; same order as above):\n")
  configurations.print.command (eliteConfigurations, parameters)
  
  if (scenario$postselection > 0) 
    psRace(iraceLogFile=scenario$logFile, postselection=scenario$postselection, elites=TRUE)
  
  if (length(eliteConfigurations) > 0 &&
      (scenario$testIterationElites != 0 || scenario$testNbElites != 0))
    testing.main(logFile = scenario$logFile)
  
  invisible(eliteConfigurations)
}

#' testing.main
#'
#' \code{testing.main} executes the testing of the target 
#' algorithm configurations found on an \pkg{irace} execution.
#' 
#' @param logFile Path to the \code{.Rdata} file produced by \pkg{irace}.
#'
#' @return Boolean. TRUE if the testing ended successfully otherwise, returns
#'   FALSE.
#' 
#' @details The function \code{testing.main} loads the \code{logFile} and
#'   obtains the needed configurations according to the specified test. Use the
#'   \code{scenario$testNbElites} to test N final elite configurations or use
#'   \code{scenario$testIterationElites} to test the best configuration of each
#'   iteration. A test instance set must be provided through
#'   \code{scenario$testInstancesDir} and \code{testInstancesFile}.
#'
#' @seealso
#'  \code{\link{defaultScenario}} to provide a default scenario for \pkg{irace}.
#' 
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @export
testing.main <- function(logFile)
{
  if (is.null.or.empty(logFile)) {
    irace.note("No logFile provided to perform the testing of configurations. Skipping testing.\n")
    return(FALSE)
  }
  
  file.check(logFile, readable = TRUE, text = "irace log file")

  load (logFile)
  scenario <- iraceResults$scenario
  parameters <- iraceResults$parameters

  if (is.null.or.empty(scenario$testInstances)) {
    return (FALSE)
  }
  
  # Get configurations
  testing.id <- c()
  if (scenario$testIterationElites)
    testing.id <- c(testing.id, iraceResults$iterationElites)
  if (scenario$testNbElites > 0) {
    tmp <- iraceResults$allElites[[length(iraceResults$allElites)]]
    testing.id <- c(testing.id, tmp[1:min(length(tmp), scenario$testNbElites)])
  }
  testing.id <- unique(testing.id)
  configurations <- iraceResults$allConfigurations[testing.id, , drop=FALSE]

  cat(" \n\n")
  irace.note ("Testing configurations (in no particular order): ", paste(testing.id, collapse=" "), "\n")
  configurations.print(configurations)  
  cat("# Testing of elite configurations:", scenario$testNbElites, 
      "\n# Testing iteration configurations:", scenario$testIterationElites,"\n")
  
  iraceResults$testing <- testConfigurations(configurations, scenario, parameters)

  # FIXME : We should print the seeds also. As an additional column?
  irace.note ("Testing results (column number is configuration ID in no particular order):\n")
  print(iraceResults$testing$experiments)
  
  cwd <- setwd(scenario$execDir)
  save (iraceResults, file = scenario$logFile)
  setwd (cwd)

  irace.note ("Finished testing\n")
  return(TRUE)
}

testing.cmdline <- function(filename, scenario)
{
  irace.note ("Checking scenario\n")
  scenario <- checkScenario(scenario)
  printScenario(scenario)

  irace.note("Reading parameter file '", scenario$parameterFile, "'.\n")
  parameters <- readParameters (file = scenario$parameterFile,
                                digits = scenario$digits)
  allConfigurations <- readConfigurationsFile (filename, parameters)
  allConfigurations <- cbind(.ID. = 1:nrow(allConfigurations),
                             allConfigurations,
                             .PARENT. = NA)
  rownames(allConfigurations) <- allConfigurations$.ID.
  num <- nrow(allConfigurations)
  allConfigurations <- checkForbidden(allConfigurations, scenario$forbiddenExps)
  if (nrow(allConfigurations) < num) {
    cat("# Warning: some of the configurations in the configurations file were forbidden",
        "and, thus, discarded\n")
  }

  # To save the logs
  iraceResults <- list(scenario = scenario,
                       irace.version = irace.version,
                       parameters = parameters,
                       allConfigurations = allConfigurations)
    
  irace.note ("Testing configurations (in the order given as input): \n")
  configurations.print(allConfigurations)  
  iraceResults$testing <- testConfigurations(allConfigurations, scenario, parameters)

  # FIXME : We should print the seeds also. As an additional column?
  irace.note ("Testing results (column number is configuration ID in no particular order):\n")
  print(iraceResults$testing$experiments)
  if (!is.null.or.empty(scenario$logFile)) {
    cwd <- setwd(scenario$execDir)
    save (iraceResults, file = scenario$logFile)
    setwd(cwd)
  }
  irace.note ("Finished testing\n")
  return(iraceResults)
}

#' Test that the given irace scenario can be run.
#'
#' @description \code{checkIraceScenario} tests that the given irace scenario
#'   can be run by checking the scenario settings provided and trying to run
#'   the target-algorithm.
#' 
#' @param scenario Data structure containing \pkg{irace} settings.The data structure
#' has to be the one returned by the function \code{\link{defaultScenario}} and
#' \code{\link{readScenario}}.
#' @param parameters Data structure containing the parameter definition. The data
#' structure has to be the one returned by the function \code{\link{readParameters}}.
#' See documentation of this function for details.
#'
#' @return returns \code{TRUE} if succesful and gives an error and returns
#' \code{FALSE} otherwise.
#' 
#' @details Provide the \code{parameters} argument only if the parameter list
#'   should not be obtained from the parameter file given by the scenario. If
#'   the parameter list is provided it will not be checked. This function will
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
checkIraceScenario <- function(scenario, parameters = NULL)
{
  irace.note ("Checking scenario\n")
  scenario$debugLevel <- 2 
  scenario <- checkScenario(scenario)
  printScenario(scenario)
 
  if (is.null(parameters)) {
    irace.note("Reading parameter file '", scenario$parameterFile, "'.\n")
    parameters <- readParameters (file = scenario$parameterFile,
                                  digits = scenario$digits,
                                  debugLevel = 2)
  } else if (!is.null.or.empty(scenario$parameterFile)) {
    cat("# Parameters provided by user.\n",
        "# Parameter file '", scenario$parameterFile, "' will be ignored\n", sep = "")
  }

  irace.note("Checking target execution.\n")
  if (checkTargetFiles(scenario = scenario, parameters = parameters)) {
    irace.note("Check succesful.\n")
    return(TRUE)
  } else {
    irace.error("Check unsuccessful.\n")
    return(FALSE)
  }
}

#' irace.cmdline
#'
#' \code{irace.cmdline} starts \pkg{irace} using the parameters
#'  of the command line used to invoke R.
#' 
#' @param args \code{commandArgs(trailingOnly = TRUE)} The arguments 
#' provided on the R command line as a character vector, e.g., 
#' \code{c("--scenario", "scenario.txt", "-p", "parameters.txt")}.
#' Using the  default value (not providing the parameter) is the 
#' easiest way to call \code{irace.cmdline}.
#' 
#' @return None.
#'
#' @details The function reads the parameters given on the command line
#' used to invoke R, finds the name of the scenario file,
#'  initializes the scenario from the file (with the function
#'  \link{readScenario}) and possibly from parameters passed on
#'  the command line. It finally starts \pkg{irace} by calling
#'  \link{irace.main}.
#'
#' @seealso
#'  \code{\link{irace.main}} to start \pkg{irace} with a given scenario.
#' 
#' @author Manuel López-Ibáñez and Jérémie Dubois-Lacoste
#' @export
irace.cmdline <- function(args = commandArgs (trailingOnly = TRUE))
{
  # Function to read command-line arguments.
  ## FIXME: This function always consumes two arguments. This is problematic
  ## for flags that have no arguments, like --check.
  readArg <- function(short = "", long = "") {
    pos <- c()
    if (length (short) > 0) {
      pos <- grep (paste0("^", short, "$"), args)
      if (length (pos) == 0) {
        pos <- grep (paste0("^", short, "="), args)
      }
    }
    if (length (long) > 0 && length (pos) == 0)  {
      pos <- grep (paste0("^", long, "$"), args)
      if (length (pos) == 0) {
        pos <- grep (paste0("^", long, "="), args)
      }
    }
    
    if (length (pos) == 0) {
      return (NULL)
    } else if (length(pos) > 0) {
      # Allow repeated parameters
      pos <- max(pos)
    }
    
    value <- unlist(strsplit (args[pos], '=', fixed = TRUE))[2]
    if (is.null (value) || is.na(value)) {
      value <- args[pos + 1]
      # We modify the caller's args
      args <<- args[- (pos + 1)]
    }
    # We modify the caller's args
    args <<- args[-pos]
    return (value)
  }

  readCmdLineParameter <- function (paramName, default) {
    x <- readArg (short = .irace.params.def[paramName, "short"],
                  long  = .irace.params.def[paramName,"long"])
    if (is.null(x)) {
      return (if (is.null(default))
                .irace.params.def[paramName, "default"] else default)
    } else if (is.na(x) && .irace.params.def[paramName,"type"] != 'x' ) {
      irace.error ("option '", .irace.params.def[paramName,"long"],
                   "' requires an argument\n")
    }
    return (x)
  }
  
  # Handle the case where we are given a single character string like a
  # command-line.
  if (!missing(args) && length(args) == 1) {
    args <- strsplit(trim(args), " +")[[1]]
  }

  if (!is.null(readArg (short = "-h", long = "--help"))) {
    irace.usage()
    return(invisible(NULL))
  }

  if (!is.null(readArg (short = "-v", long = "--version"))) {
    cat.irace.license()
    cat ("# installed at: ", system.file(package="irace"), "\n", sep = "")
    return(invisible(NULL))
  }

  cat.irace.license()
  cat ("# installed at: ", system.file(package="irace"), "\n",
       "# called with: ", paste(args, collapse = " "), "\n", sep = "")
  
  # Read the scenario file and the command line
  scenarioFile <- readCmdLineParameter ("scenarioFile", default = "")
  scenario <- readScenario(scenarioFile)
  for (param in .irace.params.names) {
    scenario[[param]] <- readCmdLineParameter (paramName = param,
                                               default = scenario[[param]])
  }
 
  # Check scenario
  if (!is.null(readArg (short = "-c", long = "--check"))) {
    checkIraceScenario(scenario)
    return(invisible(NULL))
  }

  # Only do testing
  testFile <- readArg (long = "--only-test")
  if (!is.null(testFile)) {
    return(invisible(testing.cmdline(testFile, scenario)))
  }

  if (length(args) > 0) {
    irace.error ("Unknown command-line options: ", paste(args, collapse = " "))
  }
  
  irace.main(scenario)
}
