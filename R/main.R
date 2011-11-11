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

read.table.text <- function(text, header = TRUE, stringsAsFactors = FALSE, ...)
{
  con <- textConnection(text)
  x <- read.table(con, header = header, stringsAsFactors = stringsAsFactors,
                  ...)
  close(con)
  return(x)
}

.irace.params.def <- read.table.text('
name               short  long             default            description
""               "-h"   "--help"         NA                 "show this help." 
configurationFile  "-c"   "--config-file"  "./tune-conf"      "File that contains the configuration for irace." 
parameterFile      "-p"   "--param-file"   "./parameters.txt" "File that contains the description of the parameters to be tuned. See the template." 
execDir            ""     "--exec-dir"     "./TUNE"           "Directory where the programs will be run." 
"logFile"  "-l"  "--log-file"  "./irace.Rdata"  "File (if relative path  relative to execDir to save tuning results as an R dataset." 
"instanceDir" "" "--instance-dir"  "./Instances"  "Folder where tuning instances are located  either absolute or relative to execDir." 
"instanceFile"  ""  "--instance-file"  ""   "A file containing a list of instances and (optionally additional parameters for them." 
"candidatesFile"  ""  "--candidates-file"  ""   "A file containing a list of initial candidates. If empty or NULL  do not use a file." 
"hookRun"  ""  "--hook-run"  "./hook-run" "The script called for each candidate that launches the program to be tuned. See templates/." 
"hookEvaluate"  ""  "--hook-evaluate"  "./hook-evaluate" "The scrip that provides a numeric value for each candidate. See templates/." 
"expName"  ""  "--exp-name"  "Experiment Name"  "Experiment name for output report." 
"expDescription"  ""  "--exp-description"  "Experiment Description" "Longer experiment description for output report." 
"maxNbExperiments"  ""  "--max-experiments"  1000 "The maximum number of runs (invocations of hookRun that will performed. It determines the (maximum budget of experiments for the tuning  unless timeBudget is positive."
"timeBudget"  ""  "--time-budget"  0 "The maximum computation time that should be used for tuning. This only works when tuning for time. 0 means no time limit (use maxNbExperiments."
"timeEstimate"  ""  "--time-estimate"  0  "An estimation of the average time required for one experiment. Only required if timeBudget is positive."
"signifDigits"  ""  "--signif-digits"  4 "Indicates the significant digits to be considered for the real parameters." 
"debugLevel"  ""  "--debug-level"  0 "A value of 0 silences all debug messages. Higher values provide more verbose debug messages." 
"nbIterations"  ""  "--iterations"  0  "Number of iterations." 
"nbExperimentsPerIteration"  ""  "--experiments-per-iteration"  0  "Number of experiments per iteration." 
"sampleInstances"  ""  "--sample-instances"  1  "Sample the instances or take them always in the same order." 
"testType"  ""  "--test-type"  "F-test"  "Specifies the statistical test type: F-test or t-test." 
"firstTest"  ""  "--first-test"  5    "Specifies after how many instances should be performed the first test." 
"eachTest"  ""  "--each-test"  1    "Specifies after how many instances should be performed each test." 
"minNbSurvival"  ""  "--min-survival"  0   "The minimum number of candidates that should survive to continue one iteration." 
"nbCandidates"  ""  "--num-candidates"  0   "The number of candidates that should be sampled and evaluated at each iteration." 
"mu"  ""  "--mu"  5   "This value is used to determine the number of candidates to be sampled and evaluated at each iteration." 
"seed"  ""  "--seed"  NA    "Seed of the random number generator (must be a positive integer  NA means use a random seed." 
"parallel"  ""  "--parallel"  0    "Number of calls to hookRun to execute in parallel. 0 or 1 mean disabled." 
"sgeCluster"  ""  "--sge-cluster"  0    "Enable/disable SGE cluster mode. Use qstat to wait for cluster jobs to finish (hookRun must invoke qsub." 
"mpi"  ""  "--mpi"  0    "Enable/disable mpi. Use MPI to execute hookRun in parallel (parameter parallel is the number of slaves." 
"softRestart"  ""  "--soft-restart"  1    "Enable/disable the soft restart strategy that avoids premature convergence of the probabilistic model." 
')
rownames (.irace.params.def) <- .irace.params.def[,"name"]
  
## read commandline parameters
readCmdLineParameter <- function (args, params.def, paramName, default)
{
  return (readArgOrDefault (args = args,
                            short = params.def[paramName, "short"],
                            long   = params.def[paramName,"long"],
                            default = ifelse(is.na(default),
                              params.def[paramName,"default"],
                              default)))
}

irace.usage <- function ()
{
  cat ("irace\tversion ", irace.version, "\n\n")
  cat (irace.license)
  # FIXME: The output would be nicer if we used cat(sprintf()) to
  # print short and long within a fixed width field. The description
  # can be wrapped with strwrap to the remainder space up to 80
  # columns. We can calculate the field width from the largest string
  # for each of short and long.
  for (i in seq_len(nrow(.irace.params.def))) {
    cat(.irace.params.def[i,"short"], ", ", .irace.params.def[i,"long"],
        "\t", .irace.params.def[i,"description"], "\n")
  }
}

irace.main <- function(tunerConfig, output.width = 9999)
{
  op <- options(width = output.width) # Do not wrap the output.
  tunerConfig <- checkConfiguration (tunerConfig)
  printConfiguration (tunerConfig)
  
  # Read parameters definition
  parameters <- readParameters (filename = tunerConfig$parameterFile,
                                signifDigits = tunerConfig$signifDigits,
                                debugLevel = tunerConfig$debugLevel)
  if (tunerConfig$debugLevel >= 2) { cat("Parameters have been read\n") }
  
  eliteCandidates <- irace (tunerConfig = tunerConfig,
                            parameters = parameters)
  
  cat("# Best candidates\n")
  candidates.print(eliteCandidates)
  
  cat("# Best candidates (as commandlines)\n")
  candidates.print.command (eliteCandidates, parameters)
  
  options(op)
  invisible(eliteCandidates)
}


irace.cmdline <- function(args = commandArgs (trailingOnly = TRUE))
{
  if (!is.null(readArgOrDefault (args,
                                 short = "-h", long="--help", default = NULL))) {
    irace.usage()
    q()
  }
  
  cat ("irace\tversion", irace.version, "\n\n")
  cat(irace.license)
  
  # Read the configuration file and the command line
  configurationFile <-
    readArgOrDefault(args = args,
                     short = .irace.params.def["configurationFile","short"],
                     long  = .irace.params.def["configurationFile","long"],
                     default = "")
  tunerConfig <- readConfiguration(configurationFile)
  for (param in names(tunerConfig)) {
    tunerConfig[[param]] <-
      readCmdLineParameter (args = args,
                            params.def = .irace.params.def,
                            paramName = param,
                            default = tunerConfig[[param]])
  }
  irace.main(tunerConfig)
}