# =========================================================================
# irace: An implementation in R of Iterated Race.
# -------------------------------------------------------------------------
#
#  Copyright (C) 2010
#  Manuel López-Ibáñez     <manuel.lopez-ibanez@ulb.ac.be> 
#  Jérémie Dubois-Lacoste  <jeremie.dubois-lacoste@ulb.ac.be>
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

## Assuming irace is installed in "../irace/" directory and we want
## to perform the tuning in "../tuning/", we can run this script
## within R, following these steps:
#  installDir <- "../irace"
#  setwd("../tuning/")
#  source(file.path(installDir, "main.R"))

irace.license <-
'irace: An implementation in R of Iterated Race
Copyright (C) 2010, 2011
Manuel Lopez-Ibanez     <manuel.lopez-ibanez@ulb.ac.be>
Jeremie Dubois-Lacoste  <jeremie.dubois-lacoste@ulb.ac.be>

This is free software, and you are welcome to redistribute it under certain
conditions.  See the GNU General Public License for details. There is NO   
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

'

# Function to read command-line arguments.
readArgOrDefault <- function(args, short="", long="", default=NULL)
{
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
    return (default)
  } else if (length(pos) > 0) {
    # Allow repeated parameters
    pos <- max(pos)
  }

  value <- unlist(strsplit (args[pos], '=', fixed = TRUE))[2]
  if (is.null (value) || is.na(value)) {
    value <- args[pos + 1]
  }
  return(value)
}

# Function to convert a relative to an absolute path
path.rel2abs <- function (path)
{
  if (is.null(path) || is.na(path)) {
    return (NULL)
  } else if (path == "") {
    return ("")
  } else {
    return (sub ('^(\\.)', paste (getwd(), '/\\1', sep=''), path))
  }
}

# FIXME: Convert this to a text table and use read.table to read it.
paramDef <- data.frame(stringsAsFactors = FALSE)
paramDef <-
  rbind (
         c(NA, "-h","--help", NA, "show this help"),
         c("configurationFile", "-c","--config-file", "./tune-conf", 
          "File that contains the configuration for irace."),
         c("parameterFile", "-p","--param-file", "./parameters.txt", 
          "File that contains the description of the parameters to be tuned. See the template."),
         c("execDir", "","--exec-dir", "./TUNE", 
          "Directory where the programs will be run."),
         c("logFile", "-l", "--log-file", "./irace.Rdata", 
          "File (if relative path, relative to execDir) to save tuning results as an R dataset."),
         c("instanceDir","","--instance-dir", "./Instances", 
          "Folder where tuning instances are located, either absolute or relative to execDir."),
         c("instanceFile", "", "--instance-file", "" , 
          "A file containing a list of instances and (optionally) additional parameters for them."),
         c("candidatesFile", "", "--candidates-file", "" , 
           "A file containing a list of initial candidates. If empty or NULL, do not use a file."),
         c("hookRun", "", "--hook-run", "./hook-run", 
          "The script called for each candidate that launches the program to be tuned. See templates/."),
         c("hookEvaluate", "", "--hook-evaluate", "./hook-evaluate", 
          "The scrip that provides a numeric value for each candidate. See templates/."),
         c("expName", "", "--exp-name", "Experiment Name", 
          "Experiment name for output report."),
         c("expDescription", "", "--exp-description", "Experiment Description", 
          "Longer experiment description for output report."),
         c("maxNbExperiments", "", "--max-experiments", 1000,
           "The maximum number of runs (invocations of hookRun) that will performed. It determines the (maximum) budget of experiments for the tuning, unless timeBudget is positive."
           ),
         c("timeBudget", "", "--time-budget", 0,
           "The maximum computation time that should be used for tuning. This only works when tuning for time. 0 means no time limit (use maxNbExperiments)."
           ),
         c("timeEstimate", "", "--time-estimate", 0, 
           "An estimation of the average time required for one experiment. Only required if timeBudget is positive."
           ),
         c("signifDigits", "", "--signif-digits", 4,
          "Indicates the significant digits to be considered for the real parameters."),
         c("debugLevel", "", "--debug-level", 0, 
          "A value of 0 silences all debug messages. Higher values provide more verbose debug messages."),
         c("nbIterations", "", "--iterations", 0, 
          "Number of iterations."),
         c("nbExperimentsPerIteration", "", "--experiments-per-iteration", 0, 
          "Number of experiments per iteration."),
         c("sampleInstances", "", "--sample-instances", 1, 
           "Sample the instances or take them always in the same order."),
         c("testType", "", "--test-type", "F-test", 
           "Specifies the statistical test type: F-test or t-test."),
         c("firstTest", "", "--first-test", 5, 
           "Specifies after how many instances should be performed the first test."),
         c("eachTest", "", "--each-test", 1, 
           "Specifies after how many instances should be performed each test."),
         c("minNbSurvival", "", "--min-survival", 0, 
          "The minimum number of candidates that should survive to continue one iteration."),
         c("nbCandidates", "", "--num-candidates", 0, 
          "The number of candidates that should be sampled and evaluated at each iteration."),
         c("mu", "", "--mu", 5, 
          "This value is used to determine the number of candidates to be sampled and evaluated at each iteration."),
         c("seed", "", "--seed", NA, 
           "Seed of the random number generator (must be a positive integer, NA means use a random seed)."),
         c("parallel", "", "--parallel", 0, 
           "Number of calls to hookRun to execute in parallel. 0 or 1 mean disabled."),
         c("sgeCluster", "", "--sge-cluster", 0, 
           "Enable/disable SGE cluster mode. Use qstat to wait for cluster jobs to finish (hookRun must invoke qsub)."),
         c("mpi", "", "--mpi", 0, 
           "Enable/disable mpi. Use MPI to execute hookRun in parallel (parameter parallel is the number of slaves)."),
         c("softRestart", "", "--soft-restart", 1, 
           "Enable/disable the soft restart strategy that avoids premature convergence of the probabilistic model."),
         NULL
         )
colnames (paramDef) <- c("name", "short", "long", "default", "description")
rownames (paramDef) <- paramDef[,"name"]
  
## read commandline parameters
readCmdLineParameter <- function (args, paramDef, paramName, default)
{
  return (readArgOrDefault (args = args,
                            short = paramDef[paramName, "short"],
                            long   = paramDef[paramName,"long"],
                            default = ifelse(is.na(default),
                              paramDef[paramName,"default"],
                              default)))
}

iraceUsage <- function ()
{
  cat ("irace\tversion ", IRACE.VERSION, "\n\n")
  cat (irace.license)
  for (i in seq_len(nrow(paramDef))) {
    cat(paramDef[i,"short"], ", ", paramDef[i,"long"],
        "\t", paramDef[i,"description"], "\n")
  }
}

irace.cmdline <- function()
{
  op <- options(width = 9999) # Do not wrap the output.
  # Get the installation directory
  args <- commandArgs (trailingOnly = FALSE)
  if (!exists ("installDir")) {
    if (! is.null(args)) {
      installDir <- readArgOrDefault(args,
                                     short="-f", long="--file", default=NULL)
      if(!is.null(installDir))
        installDir <- dirname (installDir)
    } else {
      installDir <- NULL
    }
  }

  if (is.null(installDir)) {
    stop ("Cannot find the installation directory, ",
          "you should invoke this R script with the parameter -f or --file\n")
  } else {
    installDir <- path.rel2abs(installDir)
  }
  
  # Check that installDir exists and is a directory
  if (!file.exists(installDir) || !file.info(installDir)$isdir) {
    stop ("The installation directory '", installDir, "' does not exist\n")
  }
  
  tunerConfig <- list()
  tunerConfig$installDir <- installDir
  
  # Load required files
  requiredFiles <- c ("utils.R", "readConfiguration.R",
                      "generation.R", "model.R", "race.R", "race-wrapper.R", 
                      "readParameters.R", "irace.R", "tnorm.R")
  cwd <- setwd(installDir)
  for (file in requiredFiles) {
    if (!file.exists(file))
      stop ("A file required '", file,
            "' can not be found in the installation directory '",
            installDir, "'.")
    source(file)
  }
  IRACE.VERSION <- ifelse(file.exists("VERSION"),
                          scan("VERSION", what = character(0), quiet = TRUE),
                          "UNKNOWN")
  setwd(cwd)
  
  args <- commandArgs (trailingOnly = TRUE)
  if (!is.null(readArgOrDefault (args,
                                 short = "-h", long="--help", default = NULL))) {
    iraceUsage()
    q()
  }
  
  cat ("irace\tversion", IRACE.VERSION, "\n\n")
  cat(irace.license)
  
  # Read the configuration file and the command line
  configurationFile <-
    readArgOrDefault(args = args,
                     short = paramDef["configurationFile","short"],
                     long = paramDef["configurationFile","long"],
                     default = "")
  
  parameters <- as.vector(paramDef[!is.na(paramDef[, "name"]), "name"])
  tunerConfig <- readConfiguration(tunerConfig, configurationFile, parameters)
  for (param in parameters) {
    tunerConfig[[param]] <- readCmdLineParameter (args = args,
                                                  paramDef = paramDef,
                                                  paramName = param,
                                                  default = tunerConfig[[param]])
  }
  tunerConfig <- checkConfiguration (tunerConfig)
  printConfiguration (tunerConfig)
  
  # Read parameters definition
  parameters <- readParameters (filename = tunerConfig$parameterFile,
                                signifDigits = tunerConfig$signifDigits,
                                debugLevel = tunerConfig$debugLevel)
  if (tunerConfig$debugLevel >= 2) { cat("Parameters have been read\n") }
  
  eliteCandidates <- iteratedRace (tunerConfig = tunerConfig,
                                   parameters = parameters)
  
  cat("# Best candidates\n")
  candidates.print(eliteCandidates)
  
  cat("# Best candidates (as commandlines)\n")
  candidates.print.command (eliteCandidates, parameters)
  
  options(op)

  invisible(eliteCandidates)
}

irace.cmdline()
