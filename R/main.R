# =========================================================================
# irace: An implementation in R of Iterated Race.
# -------------------------------------------------------------------------
#
#  Copyright (C) 2010-2016
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

## __VERSION__ below will be replaced by the version defined in R/version.R
## This avoids constant conflicts within this file.
irace.license <-
'*******************************************************************************
* irace: An implementation in R of Iterated Race
* Version: __VERSION__
* Copyright (C) 2010-2016
* Manuel Lopez-Ibanez     <manuel.lopez-ibanez@manchester.ac.uk>
* Jeremie Dubois-Lacoste  <jeremie.dubois-lacoste@ulb.ac.be>
* Leslie Perez Caceres    <leslie.perez.caceres@ulb.ac.be>
*
* This is free software, and you are welcome to redistribute it under certain
* conditions.  See the GNU General Public License for details. There is NO
* WARRANTY; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*
* irace builds upon previous code from the race package:
*
* race: Racing methods for the selection of the best
* Copyright (C) 2003 Mauro Birattari
*******************************************************************************
'
cat.irace.license <- function()
{
  cat(sub("__VERSION__", irace.version, irace.license, fixed=TRUE))
}
  
read.table.text <- function(text, header = TRUE, stringsAsFactors = FALSE, ...)
{
  con <- textConnection(text)
  x <- read.table(con, header = header, stringsAsFactors = stringsAsFactors,
                  ...)
  close(con)
  return(x)
}

# Non-variable options (such as --help and --version) have names starting with '.'
# Variables that do not have an option have description == ""
# Types are b(oolean), i(nteger), s(tring), r(eal), p(ath), x (R object or no value)
# FIXME: For i and r add their range.
# FIXME: Align columns.
# FIXME: make order of the parameters
.irace.params.def <- read.table.text('
name                       type short  long                           default            description
.help                      x    "-h"   "--help"                       NA                 "Show this help." 
.version                   x    "-v"   "--version"                    NA                 "Show irace package version."
.check                     x    "-c"   "--check"                      NA                 "Check scenario."
scenarioFile               p    "-s"   "--scenario"                   "./scenario.txt"   "File that describes the configuration scenario setup and other irace settings." 
parameterFile              p    "-p"   "--parameter-file"             "./parameters.txt" "File that contains the description of the parameters to be tuned. See the template." 
execDir                    p    ""     "--exec-dir"                   "./"               "Directory where the programs will be run." 
logFile                    p    "-l"   "--log-file"                   "./irace.Rdata"    "File to save tuning results as an R dataset, either absolute path or relative to execDir." 
recoveryFile               p    ""     "--recovery-file"              ""                 "Previously saved log file to recover the execution of irace, either absolute path or relative to the current directory.  If empty or NULL, recovery is not performed."
instances                  s    ""     ""                             ""                 ""
instancesList              s    ""     ""                             ""                 "" # List of instances + seed.
instances.extra.params     s    ""     ""                             ""                 ""
trainInstancesDir          p    ""     "--train-instances-dir"        "./Instances"      "Directory where tuning instances are located; either absolute path or relative to current directory." 
trainInstancesFile         p    ""     "--train-instances-file"       ""                 "File containing a list of instances and optionally additional parameters for them." 
configurationsFile         p    ""     "--configurations-file"        ""                 "File containing a list of initial configurations. If empty or NULL do not use a file."
forbiddenExps              x    ""     ""                             ""                 ""
forbiddenFile              p    ""     "--forbidden-file"             ""                 "File containing a list of logical expressions that cannot be true for any evaluated configuration. If empty or NULL, do not use a file."
targetRunner               p    ""     "--target-runner"              "./target-runner"  "The script called for each configuration that launches the program to be tuned. See templates/."
targetRunnerRetries        i    ""     "--target-runner-retries"      0                  "Number of times to retry a call to target-runner if the call failed."
targetRunnerData           x    ""     ""                             ""                 "Optional data passed to targetRunner. This is ignored by the default targetRunner function, but it may be used by custom targetRunner functions to pass persistent data around."
targetRunnerParallel       x    ""     ""                             ""                 ""
targetEvaluator            p    ""     "--target-evaluator"           ""                 "Optional script that provides a numeric value for each configuration. See templates/target-evaluator.tmpl"
maxExperiments             i    ""     "--max-experiments"            0                  "The maximum number of runs (invocations of targetRunner) that will be performed. It determines the maximum budget of experiments for the tuning."
maxTime                    i    ""     "--max-time"                   0                  "Maximum total execution time for the executions of targetRunner (targetRunner must return two values: [cost] [time] )."
budgetEstimation           r    ""     "--budget-estimation"          0.02               "Fraction of the budget used to estimate the mean computation time of a configuration."
digits                     i    ""     "--digits"                     4                  "Indicates the number of decimal places to be considered for the real parameters." 
debugLevel                 i    ""     "--debug-level"                0                  "A value of 0 silences all debug messages. Higher values provide more verbose debug messages." 
nbIterations               i    ""     "--iterations"                 0                  "Number of iterations." 
nbExperimentsPerIteration  i    ""     "--experiments-per-iteration"  0                  "Number of experiments per iteration." 
sampleInstances            b    ""     "--sample-instances"           1                  "Sample the instances or take them always in the same order." 
testType                   s    ""     "--test-type"                  "F-test"           "Specifies the statistical test type: F-test (Friedman test), t-test (pairwise t-tests with no correction), t-test-bonferroni (t-test with Bonferroni\'s correction for multiple comparisons), t-test-holm (t-test with Holm\'s correction for multiple comparisons)."
firstTest                  i    ""     "--first-test"                 5                  "Specifies how many instances are seen before the first elimination test. It must be a multiple of eachTest."
eachTest                   i    ""     "--each-test"                  1                  "Specifies how many instances are seen between elimination tests." 
minNbSurvival              i    ""     "--min-survival"               0                  "The minimum number of configurations that should survive to continue one iteration." 
nbConfigurations           i    ""     "--num-configurations"         0                  "The number of configurations that should be sampled and evaluated at each iteration."
mu                         i    ""     "--mu"                         5                  "This value is used to determine the number of configurations to be sampled and evaluated at each iteration." 
confidence                 r    ""     "--confidence"                 0.95               "Confidence level for the elimination test."
deterministic              b    ""     "--deterministic"              0                  "If the target algorithm is deterministic, configurations will be evaluated only once per instance."
seed                       i    ""     "--seed"                       NA                 "Seed of the random number generator (must be a positive integer, NA means use a random seed)." 
parallel                   i    ""     "--parallel"                   0                  "Number of calls to targetRunner to execute in parallel. 0 or 1 mean disabled."
loadBalancing              b    ""     "--load-balancing"             1                  "Enable/disable load-balancing when executing experiments in parallel. Load-balancing makes better use of computing resources, but increases communication overhead. If this overhead is large, disabling load-balancing may be faster."
mpi                        b    ""     "--mpi"                        0                  "Enable/disable MPI. Use Rmpi to execute targetRunner in parallel (parameter parallel is the number of slaves)."
sgeCluster                 b    ""     "--sge-cluster"                0                  "Enable/disable SGE cluster mode. Use qstat to wait for cluster jobs to finish (targetRunner must invoke qsub)."
softRestart                b    ""     "--soft-restart"               1                  "Enable/disable the soft restart strategy that avoids premature convergence of the probabilistic model."
softRestartThreshold       r    ""     "--soft-restart-threshold"     NA                 "Soft restart threshold value for numerical parameters. If NA, it computed as 10^-digits."
testInstancesDir           p    ""     "--test-instances-dir"         ""                 "Directory where testing instances are located, either absolute or relative to current directory."
testInstancesFile          p    ""     "--test-instances-file"        ""                 "File containing a list of test instances and optionally additional parameters for them."
testInstances              x    ""     ""                             ""                 ""
testInstances.extra.params x    ""     ""                             ""                 ""
testNbElites               i    ""     "--test-num-elites"            1                  "Number of elite configurations returned by irace that will be tested if test instances are provided."
testIterationElites        b    ""     "--test-iteration-elites"      0                  "Enable/disable testing the elite configurations found at each iteration."
elitist                    b    "-e"   "--elitist"                    1                  "Enable/disable elitist irace."
## MANUEL: These comments are not so clear.
elitistNewInstances        i    ""     "--elitist-new-instances"      1                  "Number of instances added to the execution list before previous instances in elitist irace."
elitistLimit               i    ""     "--elitist-limit"              2                  "Limit for the elitist race, number statistical test without elimination peformed. Use 0 for no limit."
')
rownames (.irace.params.def) <- .irace.params.def[,"name"]
.irace.params.names <- rownames(.irace.params.def)[substring(rownames(.irace.params.def), 1, 1) != "."]
## FIXME: If these values are special perhaps they should be saved in $state ?
.irace.params.recover <- c("instances", "instancesList", "instances.extra.params", "seed",
                           "testInstances", "testInstances.extra.params",
                           # We need this because this data may mutate
                           "targetRunnerData", "elitist", "deterministic")

irace.usage <- function ()
{
  cat.irace.license()

  # FIXME: The output would be nicer if we used cat(sprintf()) to
  # print short and long within a fixed width field. The description
  # can be wrapped with strwrap to the remainder space up to 80
  # columns. We can calculate the field width from the largest string
  # for each of short and long.
  for (i in seq_len(nrow(.irace.params.def))) {
    if (.irace.params.def[i,"description"] != "")
      cat(sprintf("%2s %-20s  %s\n",
                  .irace.params.def[i,"short"],
                  .irace.params.def[i,"long"],
                  .irace.params.def[i,"description"]))
  }
}

irace.main <- function(scenario = defaultScenario(), output.width = 9999)
{
  op <- options(width = output.width) # Do not wrap the output.
  on.exit(options(op), add = TRUE)

  scenario <- checkScenario (scenario)
  debug.level <- scenario$debugLevel
  options(.irace.execdir = scenario$execDir)
  
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
  
  cat("# Best configurations (first number is the configuration ID)\n")
  configurations.print(eliteConfigurations)
  
  cat("# Best configurations as commandlines (first number is the configuration ID)\n")
  configurations.print.command (eliteConfigurations, parameters)
  
  if (length(eliteConfigurations) > 0 &&
      (scenario$testIterationElites != 0 || scenario$testNbElites != 0))
    testing.main(logFile = scenario$logFile)
  
  invisible(eliteConfigurations)
}

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
  irace.note ("Testing configurations: ", paste(testing.id, collapse=" "), "\n")
  configurations.print(configurations)  
  cat("# Testing of elite configurations:", scenario$testNbElites, 
      "\n# Testing iteration configurations:", scenario$testIterationElites,"\n")
  
  iraceResults$testing <- testConfigurations(configurations, scenario, parameters)

  # FIXME : We should print the seeds also. As an additional column?
  irace.note ("Testing results (column number is configuration ID):\n")
  print(iraceResults$testing$experiments)
  
  cwd <- setwd(scenario$execDir)
  save (iraceResults, file = scenario$logFile)
  setwd (cwd)

  irace.note ("Finished testing\n")
  return(TRUE)
}

checkIraceScenario <- function(scenario, parameters = NULL)
{
  irace.note ("Checking scenario\n")
  scenario$debugLevel <- 2 
  scenario <- checkScenario(scenario)
  printScenario(scenario)
 
  if (is.null(parameters)) {
    cat("# Reading parameter file '", scenario$parameterFile, "'.\n", sep = "")
    parameters <- readParameters (file = scenario$parameterFile,
                                  digits = scenario$digits,
                                  debugLevel = 2)
  } else if (!is.null.or.empty(scenario$parameterFile)) {
    cat("# Parameters provided by user.\n",
        "# Parameter file '", scenario$parameterFile, "' will be ignored\n", sep = "")
  }

  cat("# Checking target execution.\n")
  if (checkTargetFiles(scenario = scenario, parameters = parameters))
    cat("\n# Check succesful.\n")
  else
    cat("\n# Check unsuccesful.\n") 
}

irace.cmdline <- function(args = commandArgs (trailingOnly = TRUE))
{
  # Function to read command-line arguments.
  readArg <- function(short = "", long = "") {
    pos <- c()
    if (length (short) > 0) {
      pos <- grep (paste ("^", short, "$", sep=""), args)
      if (length (pos) == 0) {
        pos <- grep (paste ("^", short, "=", sep=""), args)
      }
    }
    if (length (long) > 0 && length (pos) == 0)  {
      pos <- grep (paste ("^", long, "$", sep=""), args)
      if (length (pos) == 0) {
        pos <- grep (paste ("^", long, "=", sep=""), args)
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
    cat ("\tinstalled at: ", system.file(package="irace"), "\n", sep="")
    return(invisible(NULL))
  }

  cat.irace.license()
  cat ("\tinstalled at: ", system.file(package="irace"), "\n", sep="")
  
  # Read the scenario file and the command line
  scenarioFile <- readCmdLineParameter ("scenarioFile", default = "")
  scenario <- readScenario(scenarioFile)
  for (param in .irace.params.names) {
    scenario[[param]] <-
      readCmdLineParameter (paramName = param,
                            default = scenario[[param]])
  }
 
  # Check scenario
  if (!is.null(readArg (short = "-c", long = "--check"))) {
    checkIraceScenario(scenario)
    return(invisible(NULL))
  }
  
  if (length(args) > 0) {
    irace.error ("Unknown command-line options: ", paste(args, collapse = " "))
  }
  
  irace.main(scenario)
}
