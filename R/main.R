# =========================================================================
# irace: An implementation in R of Iterated Race.
# -------------------------------------------------------------------------
#
#  Copyright (C) 2010-2013
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
'********************************************************************************
* irace: An implementation in R of Iterated Race                               *
* Copyright (C) 2010-2013                                                      *
* Manuel Lopez-Ibanez     <manuel.lopez-ibanez@ulb.ac.be>                      *
* Jeremie Dubois-Lacoste  <jeremie.dubois-lacoste@ulb.ac.be>                   *
*                                                                              *
* This is free software, and you are welcome to redistribute it under certain  *
* conditions.  See the GNU General Public License for details. There is NO     *
* warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  *
*                                                                              *
* irace builds upon previous code from the race package                        *
* Copyright (C) 2003 Mauro Birattari                                           *
********************************************************************************
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
execDir            ""     "--exec-dir"     "./"           "Directory where the programs will be run." 
"logFile"  "-l"  "--log-file"  "./irace.Rdata"  "File to save tuning results as an R dataset, either absolute path or relative to execDir." 
"instances"  "" "" "" ""
"instances.extra.params"  "" "" "" ""
"instanceDir" "" "--instance-dir"  "./Instances"  "Folder where tuning instances are located  either absolute or relative to working directory." 
"instanceFile"  ""  "--instance-file"  ""   "A file containing a list of instances and optionally additional parameters for them." 
"candidatesFile"  ""  "--candidates-file"  ""   "A file containing a list of initial candidates. If empty or NULL  do not use a file." 
"hookRun"  ""  "--hook-run"  "./hook-run" "The script called for each candidate that launches the program to be tuned. See templates/." 
hookEvaluate  ""  --hook-evaluate  "" "Optional script that provides a numeric value for each candidate. See templates/hook-evaluate.tmpl" 
"maxExperiments"  ""  "--max-experiments"  1000 "The maximum number of runs (invocations of hookRun) that will be performed. It determines the maximum budget of experiments for the tuning unless timeBudget is positive."
"timeBudget"  ""  "--time-budget"  0 "The maximum computation time that should be used for tuning. This only works when tuning for time. 0 means no time limit (then it uses maxExperiments)."
"timeEstimate"  ""  "--time-estimate"  0  "An estimation of the average time required for one experiment. Only required if timeBudget is positive."
"digits"  ""  "--digits"  4 "Indicates the number of decimal places to be considered for the real parameters." 
"debugLevel"  ""  "--debug-level"  0 "A value of 0 silences all debug messages. Higher values provide more verbose debug messages." 
"nbIterations"  ""  "--iterations"  0  "Number of iterations." 
"nbExperimentsPerIteration"  ""  "--experiments-per-iteration"  0  "Number of experiments per iteration." 
"sampleInstances"  ""  "--sample-instances"  1  "Sample the instances or take them always in the same order." 
"testType"  ""  "--test-type"  "F-test"  "Specifies the statistical test type: F-test or t-test." 
"firstTest"  ""  "--first-test"  5    "Specifies how many instances are seen before the first test."
"eachTest"  ""  "--each-test"  1    "Specifies how many instances are seen between tests." 
"minNbSurvival"  ""  "--min-survival"  0   "The minimum number of candidates that should survive to continue one iteration." 
"nbCandidates"  ""  "--num-candidates"  0   "The number of candidates that should be sampled and evaluated at each iteration." 
"mu"  ""  "--mu"  5   "This value is used to determine the number of candidates to be sampled and evaluated at each iteration." 
"seed"  ""  "--seed"  NA    "Seed of the random number generator (must be a positive integer, NA means use a random seed)." 
"parallel"  ""  "--parallel"  0    "Number of calls to hookRun to execute in parallel. 0 or 1 mean disabled." 
"mpi"  ""  "--mpi"  0    "Enable/disable MPI. Use Rmpi to execute hookRun in parallel (parameter parallel is the number of slaves)." 
"sgeCluster"  ""  "--sge-cluster"  0    "Enable/disable SGE cluster mode. Use qstat to wait for cluster jobs to finish (hookRun must invoke qsub)." 
"softRestart"  ""  "--soft-restart"  1    "Enable/disable the soft restart strategy that avoids premature convergence of the probabilistic model." 
')
rownames (.irace.params.def) <- .irace.params.def[,"name"]
.irace.params.names <- rownames(.irace.params.def)[rownames(.irace.params.def) != ""]
  
## read commandline parameters
readCmdLineParameter <- function (args, params.def, paramName, default)
{
  return (readArgOrDefault (args = args,
                            short = params.def[paramName, "short"],
                            long   = params.def[paramName,"long"],
                            default = if (is.null(default))
                            params.def[paramName, "default"] else default))
}

irace.usage <- function ()
{
  # FIXME: It would be nice to put the version number in the license
  # message to avoid having this extra line.
  cat ("irace\tversion ", irace.version, "\n")
  cat (irace.license)
  # FIXME: The output would be nicer if we used cat(sprintf()) to
  # print short and long within a fixed width field. The description
  # can be wrapped with strwrap to the remainder space up to 80
  # columns. We can calculate the field width from the largest string
  # for each of short and long.
  for (i in seq_len(nrow(.irace.params.def))) {
    if (.irace.params.def[i,"description"] != "")
      cat(.irace.params.def[i,"short"], ", ", .irace.params.def[i,"long"],
          "\t", .irace.params.def[i,"description"], "\n")
  }
}

irace.main <- function(tunerConfig = defaultConfiguration(), output.width = 9999)
{
  op <- options(width = output.width) # Do not wrap the output.
  tunerConfig <- checkConfiguration (tunerConfig)
  debug.level <- tunerConfig$debugLevel
  if (debug.level >= 1) printConfiguration (tunerConfig)
  
  # Read parameters definition
  parameters <- readParameters (file = tunerConfig$parameterFile,
                                digits = tunerConfig$digits,
                                debugLevel = debug.level)
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
  if (!is.null(readArgOrDefault (args, short = "-h", long="--help"))) {
    irace.usage()
    return(invisible(NULL))
  }

  # FIXME: It would be nice to put the version number in the license
  # message to avoid having this extra line.
  cat ("irace\tversion", irace.version, "\n")
  cat(irace.license)
  
  # Read the configuration file and the command line
  configurationFile <-
    readArgOrDefault(args = args,
                     short = .irace.params.def["configurationFile","short"],
                     long  = .irace.params.def["configurationFile","long"],
                     default = "")
  tunerConfig <- readConfiguration(configurationFile)
  for (param in .irace.params.names) {
    tunerConfig[[param]] <-
      readCmdLineParameter (args = args,
                            params.def = .irace.params.def,
                            paramName = param,
                            default = tunerConfig[[param]])
  }
  irace.main(tunerConfig)
}
