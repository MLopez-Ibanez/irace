.ablation.params.def <- read.table(header=TRUE, stringsAsFactors = FALSE, text="
name            ab  type short long          default               description
iraceResults     0  p    -l    --log-file    NA                    'Path to the (.Rdata) file created by irace from which the  \"iraceResults\" object will be loaded.'
src              1  i    -S    --src         1                     'Source configuration ID.'
target           1  i    -T    --target      NA                    'Target configuration ID. By default the best configuration found by irace.'
ab.params        1  s    -P    --params      ''                    'Specific parameter names to be used for the ablation (separated with commas). By default use all'
type             1  s    -t    --type        'full'                'Type of ablation to perform: \"full\" will execute each configuration on all \"--n-instances\" to determine the best-performing one; \"racing\" will apply racing to find the best configurations.'
nrep            1  i    -n    --nrep       1                     'Number of replications per instance used in \"full\" ablation.'
seed             1  i    ''    --seed        1234567               'Integer value to use as seed for the random number generation.'
ablationLogFile  1  p    -o    --output-file 'log-ablation.Rdata'  'Log file to save the ablation log. If \"\", the results are not saved to a file.'
instancesFile    1  s    ''    --instances-file   'train'          'Instances file used for ablation: \"train\", \"test\" or a filename containing the list of instances.'
plot             0  s    -p    --plot        ''                    'Output filename (.pdf) for the plot. If not given, no plot is created.'
plot_type        0  s    -O    --plot-type   'mean'                'Type of plot. Supported values are \"mean\", \"boxplot\", \"rank\" or \"rank,boxplot\".'
old_path         0  p    ''    --old-path    NA                    'Old path found in the log-file (.Rdata) given as input to be replaced by --new-path.'
new_path         0  p    ''    --new-path    NA                    'New path to replace the path found in the log-file (.Rdata) given as input.'
execDir          0  p    -e    --exec-dir    NA                    'Directory where the target runner will be run.'
scenarioFile     0  p    -s    --scenario    NA                    'Scenario file to override the scenario given in the log-file (.Rdata)'
parallel         0  i    ''    --parallel    NA                    'Number of calls to targetRunner to execute in parallel. Values 0 or 1 mean no parallelization.'
")

cat_ablation_license <- function()
{
  ablation_license <-
'#------------------------------------------------------------------------------
# ablation: An implementation in R of Ablation Analysis
# Version: __VERSION__
# Copyright (C) 2020--2022
# Manuel Lopez-Ibanez     <manuel.lopez-ibanez@manchester.ac.uk>
# Leslie Perez Caceres    <leslie.perez.caceres@ulb.ac.be>
#
# This is free software, and you are welcome to redistribute it under certain
# conditions.  See the GNU General Public License for details. There is NO
# WARRANTY; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#------------------------------------------------------------------------------
'
  cat(sub("__VERSION__", irace.version, ablation_license, fixed=TRUE))
}

#' Launch ablation with command-line options.
#'
#' Launch [ablation()] with the same command-line options as the command-line
#' executable (`ablation.exe` in Windows).
#' 
#' @param argv (`character()`) \cr The arguments 
#' provided on the R command line as a character vector, e.g., 
#' `c("-i", "irace.Rdata", "--src", 1)`.
#' 
#' @details The function reads the parameters given on the command line
#' used to invoke R, launches [ablation()] and possibly [plotAblation()].
#'
#' List of command-line options:
#' ```{r echo=FALSE,comment=NA}
#' cmdline_usage(.ablation.params.def)
#' ```
#' @template ret_ablog
#' @examples
#' # ablation_cmdline("--help")
#' 
#' @author Manuel López-Ibáñez
#' @concept ablation
#' @export
ablation_cmdline <- function(argv = commandArgs(trailingOnly = TRUE))
{
  op <- options(width = 9999L) # Do not wrap the output.
  on.exit(options(op), add = TRUE)

  cat_ablation_license()
  cat ("# installed at: ", system.file(package="irace"), "\n",
       "# called with: ", paste(argv, collapse = " "), "\n", sep = "")
  parser <- CommandArgsParser$new(argv = argv, argsdef = .ablation.params.def)
  if (!is.null(parser$readArg (short = "-h", long = "--help"))) {
    parser$cmdline_usage()
    return(invisible(NULL))
  }

  if (!is.null(parser$readArg (short = "-v", long = "--version"))) {
    print(citation(package="irace"))
    return(invisible(NULL))
  }

  params <- parser$readAll()
  # TODO: Send the other options to the irace command-line parser so the user
  # can override options in scenario via the command-line.
  if (length(parser$argv) > 0)
    stop("Unknown command-line options: ", paste(parser$argv, collapse = " "))

  if (is.null(params$iraceResults)) {
    irace.error("You must provide the path to the '.Rdata' file that contains the 'iraceResults' object generated by irace.")
    return(invisible(NULL))
  }
  iraceResults <- read_logfile(params$iraceResults)
  if (is.null(params$old_path) != is.null(params$new_path)) {
    irace.error("To update paths you must provide both --old-path and --new-path.")
    return(invisible(NULL))
  } else if (!is.null(params$old_path)) {
    iraceResults$scenario <- scenario_update_paths(iraceResults$scenario, params$old_path, params$new_path)
  }
  if (!is.null(params$scenarioFile)) {
    scenario <- readScenario(params$scenarioFile)
  }
  if (is_null_or_empty_or_na(trim(params$ablationLogFile))) {
    params$ablationLogFile <- NULL
  }
  for (p in c("execDir", "parallel")) {
    if (!is.null(params[[p]])) scenario[[p]] <- params[[p]]
  }
  
  if (!is.null(params$ablationLogFile))
    params$ablationLogFile <- path_rel2abs(params$ablationLogFile)
      
  if (!is.null(params$ab.params))
    params$ab.params <- trimws(strsplit(params$ab.params, ",", fixed=TRUE)[[1]])

  # The shell may introduce extra quotes, remove them.
  params$plot_type <- trimws(gsub("[\"']", "", params$plot_type))
  
  # We want to select elements that actually appear in params, otherwise we get NA names.
  ablation_params <- intersect(.ablation.params.def[.ablation.params.def$ab == 1, "name", drop=TRUE],
                               names(params))
  ablog <- do.call(ablation,
                   args = c(list(iraceResults = iraceResults),
                            params[ablation_params],
                            scenario))
  if (!is.null(params[["plot"]]) || base::interactive()) {
    plotAblation(ablog, pdf.file = params[["plot"]], type = params$plot_type) 
  }
  invisible(ablog)
}

## This function fixes dependent parameters when a parameter value has been
## changed.
fixDependenciesWithReference <- function(configuration, ref.configuration, parameters)
{
  # Search parameters that need a value
  changed <- c()
  for (pname in names(which(!parameters[["isFixed"]]))) {
    # If dependent parameter has been activated, set the value of the reference.
    if (is.na(configuration[,pname]) && conditionsSatisfied(parameters, configuration, pname)) {
       if (!is.null(ref.configuration)) {
         configuration[,pname] <- ref.configuration[pname]
       } 
       changed <- c(changed, pname)
       # MANUEL: Why do we need to recurse here?
       aux <- fixDependenciesWithReference(configuration=configuration, ref.configuration=ref.configuration, parameters)
       changed <- c(changed, aux$changed)
       configuration <- aux$configuration
    }
  }
  list(configuration=configuration, changed=changed)
}

## Function that generates the configurations of the ablation path 
## between initial.configuration and final.configuration.
## parameters can be selected by specifying them in para.names.
generateAblation <- function(initial.configuration, final.configuration,
                             parameters, param.names = NULL)
{ 
  if (is.null(param.names))
    param.names <- parameters[["names"]]

  # Only change non-fixed
  param.names <- param.names[!parameters[["isFixed"]][param.names]]
  
  configurations <- NULL
  changed.params <- list()
  for (pname in param.names) {
    # Check if parameter is active.
    if (!conditionsSatisfied(parameters, initial.configuration, pname)) next
    # Check value is different in the initial and final configuration and if
    # so, change the value.
    if (initial.configuration[, pname] == final.configuration[, pname]) next
    new.configuration <- initial.configuration
    new.configuration[, pname]<- final.configuration[,pname]
    # Set newly activated parameters if needed.
    aux <- fixDependenciesWithReference(new.configuration, final.configuration, parameters)
    new.configuration <- aux[["configuration"]] 
    changed.params[[length(changed.params) + 1]] <- c(pname, aux[["changed"]])
    new.configuration[, ".PARENT."] <- initial.configuration$.ID.
    configurations <- rbind.data.frame(configurations, new.configuration) 
  }
  rownames(configurations) <- NULL
  list(configurations=configurations, changed.params=changed.params)
}

report_duplicated_results <- function(experiments, configurations)
{
  x <- t(experiments)
  x <- x[duplicated(x) | duplicated(x, fromLast = TRUE), , drop=FALSE]
  if (nrow(x) == 0L) return(NULL) # No duplicates
  dups <- split(rownames(x), apply(x, 1, paste0, collapse=""))
  names(dups) <- NULL
  for (g in dups) {
    cat("Warning: The following configurations are different but produced the same results:\n")
    df <- configurations[configurations$.ID. %in% g, , drop=FALSE]
    print(df)
    cat("Parameters with different values from the above configurations:\n")
    df <- removeConfigurationsMetaData(df)
    print(df[, vapply(df, function(x) length(unique(x)) > 1L, logical(1L)), drop=FALSE])
  }
  dups
}

ab_generate_instances <- function(scenario, nrep, type, instancesFile)
{
  nrep <- suppressWarnings(as.integer(nrep))
  if (is.na(nrep) || length(nrep) == 0 || nrep <= 0)
    stop("'nrep' must be an integer larger than zero")
  
  if (nrep != 1L && type == "racing")
    stop("'nrep' has no effect when type == 'racing'")

  if (instancesFile == "test") {
    scenario$instances <- scenario$testInstances
  } else if (instancesFile != "train") {
    scenario$instances <- readInstances(instancesDir = "", instancesFile = instancesFile)
  }
  n_inst <- length(scenario$instances)
  instancesList <- generateInstances(scenario, n_inst * nrep)

  msg <- if (instancesFile %in% c("train", "test"))
           paste0("'", instancesFile, "' instances") else paste0("instances from '", instancesFile, "'")
  if (n_inst > 100L) {
    n_inst <- 100L
    msg <- paste0(msg, " (only showing first 100)")
  }
  cat(sep="", "# Using ", msg, ":\n",
      paste0(collapse="\n", scenario$instances[1L:n_inst]),
      "\n")
  list(instancesList=instancesList, instances=scenario$instances)
}

#' Performs ablation between two configurations (from source to target).
#'
#' @description Ablation is a method for analyzing the differences between two configurations.
#'
#' @template arg_iraceresults
#' @param src,target (`integer(1)`) Source and target configuration IDs. By default, the first configuration ever evaluated (ID 1) is used as `src` and the best configuration found by irace is used as target.
#' @param ab.params Specific parameter names to be used for the ablation. They must be in `parameters$names`. By default, use all parameters.
#' @param type Type of ablation to perform: `"full"` will execute each configuration on all `n_instances` to determine the best-performing one; `"racing"` will apply racing to find the best configurations.
#' @param nrep (`integer(1)`) Number of replications per instance used in `"full"` ablation.
#' @param seed (`integer(1)`) Integer value to use as seed for the random number generation.
#' @param ablationLogFile  (`character(1)`) Log file to save the ablation log. If `NULL`, the results are not saved to a file.
#' @param instancesFile  (`character(1)`) Instances file used for ablation: `'train'`, `'test'` or a filename containing the list of instances.
#' @param ... Further arguments to override scenario settings, e.g., `debugLevel`, `parallel`, etc.
#'
#' @references
#' C. Fawcett and H. H. Hoos. Analysing differences between algorithm
#' configurations through ablation. Journal of Heuristics, 22(4):431–458, 2016.
#' 
#' @template ret_ablog
#' @seealso [plotAblation()]
#' @examples
#' \donttest{
#' logfile <- system.file(package="irace", "exdata", "sann.rda")
#' # Execute ablation between the first and the best configuration found by irace.
#' ablog <- ablation(logfile, ablationLogFile = NULL)
#' plotAblation(ablog)
#' # Execute ablation between two selected configurations, and selecting only a
#' # subset of parameters, directly reading the setup from the irace log file.
#' ablog <- ablation(logfile, src = 1, target = 10,
#'                   ab.params = c("temp"), ablationLogFile = NULL)
#' plotAblation(ablog)
#' }
#'
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @concept ablation
#' @export
ablation <- function(iraceResults, src = 1L, target = NULL,
                     ab.params = NULL, type = c("full", "racing"),
                     nrep = 1L, seed = 1234567,
                     ablationLogFile = "log-ablation.Rdata",
                     instancesFile="train", ...)
{
  # Input check
  if (missing(iraceResults) || is.null(iraceResults)) 
    stop("You must provide an 'iraceResults' object generated by irace or the path to the '.Rdata' file that contains this object.")

  type <- match.arg(type)

  if (!is.null(ablationLogFile))
    file.check(ablationLogFile, writeable = TRUE, text = 'logFile')
  
  save_ablog <- function(complete) {
    ablog <- list(changes = changes,
                  configurations = all_configurations,
                  experiments = results,
                  instances   = .irace$instancesList,
                  parameters = parameters,
                  scenario    = scenario, 
                  trajectory  = trajectory,
                  best = best.configuration,
                  complete = complete)
    if (!is.null(ablationLogFile)) save(ablog, file = ablationLogFile, version = 2)
    ablog
  }
  
  # FIXME: The previous seed needs to be saved and restored at the end.
  set.seed(seed)
  # Load the data of the log file
  iraceResults <- read_logfile(iraceResults)
  parameters <- iraceResults$parameters
  scenario   <- iraceResults$scenario
  scenario_args <- list(...)
  if (length(scenario_args) > 0L) {
    unknown_scenario_args <- setdiff(names(scenario_args), names(scenario))
    if (length(unknown_scenario_args) > 0L)
      irace.error("Unknown scenario settings given: ", paste0(unknown_scenario_args, collapse=", "))
    scenario <- modifyList(scenario, scenario_args)
  }
  scenario$logFile <- ""
  scenario <- checkScenario(scenario)
  # Generate instances
  res <- ab_generate_instances(scenario, nrep, type, instancesFile)
  .irace$instancesList <- res$instancesList
  scenario$instances <- res$instances

  if (is.null(target)) target <- iraceResults$iterationElites[length(iraceResults$iterationElites)]
  irace.note ("Starting ablation from ", src, " to ", target, "\n# Seed: ", seed, "\n")
  if (src %!in% iraceResults$allConfigurations$.ID.)
    stop("Source configuration ID (", src, ") cannot be found")
  if (target %!in% iraceResults$allConfigurations$.ID.)
    stop("Target configuration ID (", target, ") cannot be found")

  cat("# Source configuration (row number is ID):\n")
  src.configuration <- iraceResults$allConfigurations[src, , drop = FALSE]
  configurations.print(src.configuration)
  cat("# Target configuration (row number is ID):\n")
  target.configuration <- iraceResults$allConfigurations[target, , drop = FALSE]
  configurations.print(target.configuration)

  # Select the parameters used for ablation
  if (is.null(ab.params)) {
    ab.params <- parameters$names
  } else if (!all(ab.params %in% parameters$names)) {
    irace.error("Some of the parameters provided (", paste0(setdiff(ab.params, parameters$names), collapse=", "),
                ") are not defined in the parameter space.")
  }
  # Select parameters that are different in both configurations
  neq.params <- which(src.configuration[,ab.params] != target.configuration[,ab.params])
  
  if (length(neq.params) < 1) 
    irace.error("src and target configurations are equal considering the parameters selected.\n")
  param.names <- colnames(src.configuration[,ab.params])[neq.params]
  
  # FIXME: Do we really need to override the ID?
  src.configuration$.ID. <- best.id <-  1
  best.configuration <- all_configurations <- src.configuration
  
  # Execute source and target configurations.
  ## FIXME: We may already have these experiments in the logFile!
  experiments <- createExperimentList(configurations = rbind(src.configuration, target.configuration), 
                                      parameters = parameters,
                                      instances = scenario$instances,
                                      instances.ID = .irace$instancesList[, "instance"],
                                      seeds = .irace$instancesList[, "seed"],
                                      scenario = scenario,
                                      bounds = scenario$boundMax)
  irace.note("Executing source and target configurations on the given instances * nrep (", nrow(.irace$instancesList), ")...\n")
  
  startParallel(scenario)
  on.exit(stopParallel(), add = TRUE)
  target.output <- execute.experiments(experiments, scenario)
  if (!is.null(scenario$targetEvaluator))
    target.output <- execute.evaluator (experiments, scenario, target.output,
                                        src.configuration)
  # Save results
  output <- sapply(target.output, getElement, "cost") 
  results <- matrix(NA, ncol = 1, nrow = nrow(.irace$instancesList), 
                    dimnames = list(seq(1,nrow(.irace$instancesList)), 1))
  results[,1] <- output[1:nrow(.irace$instancesList)]
  lastres <- output[(nrow(.irace$instancesList)+1):(2 * nrow(.irace$instancesList))]
  step <- 1
  # Define variables needed
  trajectory <- 1
  names(trajectory) <- "source"
  # FIXME: changes should only store the changed parameters.
  changes <- list()
  ablog <- save_ablog(complete = FALSE)
  while (length(param.names) > 1) {
    # Generate ablation configurations
    cat("# Generating configurations (row number is ID):", param.names,"\n")
    ab.aux <- generateAblation(best.configuration, target.configuration, parameters, 
                               param.names)
    aconfigurations <- ab.aux$configurations
    if (is.null(aconfigurations)) {
      cat("# Stopping ablation, no parameter change possible.\n")
      break
    }
    ## FIXME: We may already have these configurations in the logFile!
    # New configurations ids
    ## FIXME: These should be generated with respect to the logFile to make
    ## sure we don't have duplicate IDs.
    aconfigurations[,".ID."] <- seq(max(all_configurations$.ID.) + 1,
                                    max(all_configurations$.ID.) + nrow(aconfigurations))
    configurations.print(aconfigurations, metadata = FALSE)
    all_configurations <- rbind(all_configurations, aconfigurations)
    
    # Set variables for the racing procedure
    if (scenario$capping) {
      # For using capping we must set elite data
      elite.data <- list(experiments = results[,best.configuration$.ID., drop=FALSE])
      race.conf <-  rbind(best.configuration, aconfigurations)
      .irace$next.instance <- nrow(.irace$instancesList) + 1
    } else {
      #LESLIE: for now we apply the non-elitis irace when type=="racing"
      # we should define what is the standard
      elite.data <- NULL
      race.conf <-  aconfigurations
      scenario$elitist <- FALSE
      .irace$next.instance <- 1
    }
          
    irace.note("Ablation (", type, ") of ", nrow(aconfigurations),
               " configurations on ", nrow(.irace$instancesList), " instances.\n")
    # Force the race to see all instances in "full" mode
    if (type == "full") scenario$firstTest <- nrow(.irace$instancesList)
    # FIXME: what about blockSize?
    race.output <- elitist_race(maxExp = nrow(aconfigurations) * nrow(.irace$instancesList),
                                minSurvival = 1,
                                elite.data = elite.data,
                                configurations = race.conf,
                                parameters = parameters,
                                scenario = scenario,
                                elitistNewInstances = 0L)
    results <- merge.matrix (results, race.output$experiments)

    # Save log
    ablog <- save_ablog(complete = FALSE)
    
    # Get the best configuration based on the criterion of irace
    # MANUEL: Doesn't race.output already give you all this info???
    cranks <- overall.ranks(results[,aconfigurations$.ID.,drop=FALSE], test = scenario$testType)
    best_id <- which.min(cranks)[1]
    # cand.mean <- colMeans2(results[,aconfigurations$.ID.,drop=FALSE], na.rm=TRUE)
    changes[[step]] <- ab.aux$changed.params
    best.change <- changes[[step]][[best_id]]
    trajectory <- c(trajectory, aconfigurations[best_id, ".ID."])
    
    # Report best
    # FIXME: This ID does not actually match the configuration ID
    # The race already reports the best.
    cat("# Best changed parameters:\n")
    for (i in seq_along(best.change)) {
      cat("#", best.change[i], ":", best.configuration[,best.change[i]], "->",
          aconfigurations[best_id, best.change[i]], "\n")
    }
  
    best.configuration <- aconfigurations[best_id,,drop=FALSE]
    best.id <- best.configuration$.ID.
    param.names <- param.names[!(param.names %in% best.change)]
    step <- step + 1
  }
  
  # Add last configuration and its results
  # FIXME: This may be overriding the ID of an existing configuration!!!
  target.configuration$.ID. <- max(all_configurations$.ID.) + 1
  all_configurations <- rbind(all_configurations, target.configuration)
  results <- cbind(results, matrix(lastres, ncol = 1,
                                   dimnames=list(seq(1, nrow(.irace$instancesList)),
                                                 target.configuration$.ID.)))
  trajectory <- c(trajectory, target.configuration$.ID.)
  
  # Get the overall best
  cranks <- overall.ranks(results[,trajectory, drop=FALSE], test = scenario$testType)
  best_id <- which.min(cranks)[1]
  ## FIXME: At this point, the rownames of all_configurations does not match
  ## all_configurations$.ID.  That is confusing and a potential source of
  ## bugs. Instead of fixing it here, we should not generate the discrepancy
  ## ever.
  best.configuration <- all_configurations[trajectory[best_id],,drop=FALSE]
  irace.note("Final best configuration:\n")
  configurations.print(best.configuration)

  # Check for duplicated results:
  report_duplicated_results(results, all_configurations)

  # LESLIE: If we use racing we can have a matrix of results that is not
  # complete, how should we do the plots?
  # MANUEL: Do not plot anything that was discarded
  
  save_ablog(complete = TRUE)
}

ablation.labels <- function(trajectory, configurations)
{
  configurations <- removeConfigurationsMetaData(configurations[trajectory, , drop = FALSE])
  labels <- names(trajectory)
  last <- configurations[1, , drop = FALSE]
  param.names <- colnames(last)
  for (i in 2:length(trajectory)) {
    current <- configurations[i, , drop = FALSE]
    # Select everything that is NOT NA now and was different or NA before.
    select <- !is.na(current) & (is.na(last) | (current != last))
    irace.assert(!anyNA(select))
    labels[i] <- paste0(param.names[select], "=", current[, select], collapse = "\n")
    last <- current
  }
  labels
}

#' Create plot from an ablation log
#'
#' @param ablog (`list()`|`character(1)`) Ablation log object returned by [ablation()]. Alternatively, the path to an `.Rdata` file, e.g., `"log-ablation.Rdata"`, from which the object will be loaded.

#' @param pdf.file Output filename.
#' @param pdf.width Width provided to create the pdf file.
#' @param type Type of plot. Supported values are `"mean"` and `"boxplot"`. Adding `"rank"` will plot rank per instance instead of raw cost value.
#' @param n (`integer(1)`) Number of parameters included in the plot. By default all parameters are included.
#' @param mar Vector with the margins for the ablation plot.
#' @param ylab Label of y-axis.
#' @param ylim Numeric vector of length 2 giving the y-axis range. 
#' @param ... Further graphical parameters may also be supplied as
#'   arguments. See [graphics::plot.default()].
#'
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @seealso [ablation()]
#' @examples
#' logfile <- file.path(system.file(package="irace"), "exdata", "log-ablation.Rdata")
#' plotAblation(ablog = logfile)
#' plotAblation(ablog = logfile, type = "boxplot")
#' plotAblation(ablog = logfile, type = c("rank","boxplot"))
#' @concept ablation
#' @export
plotAblation <- function (ablog, pdf.file = NULL, pdf.width = 20,
                          type = c("mean", "boxplot", "rank"), n = 0L,
                          mar = par("mar"),
                          ylab = "Mean configuration cost", ylim = NULL,
                          ...)
{
  type <- trimws(unlist(strsplit(type, ",", fixed=TRUE)))
  type <- match.arg(type, several.ok = TRUE)
  if (missing(ylab) && ("rank" %in% type)) ylab <- "Rank per instance"
  
  if (missing(ablog) || is.null(ablog)) {
    irace.error("You must provide an 'ablog' object generated by ablation() or the path to the '.Rdata' file that contains this object.")
  }
  ablog <- read_ablogfile(ablog)
  if (!ablog$complete)
    stop("The ablog shows that the ablation procedure did not complete cleanly and only contains partial information")
  
  
  if (!is.null(pdf.file)) {
    if (!is.file.extension(pdf.file, ".pdf"))
      pdf.file <- paste0(pdf.file, ".pdf")
    cat("Creating PDF file '", pdf.file, "'\n", sep="")
    pdf(file = pdf.file, width = pdf.width,
        title = paste0("Ablation plot: ", pdf.file))
    on.exit(dev.off(), add = TRUE)
  }
  
  trajectory <- ablog$trajectory
  if (n > 0) trajectory <- trajectory[1:(n+1)]

  configurations <- ablog$configurations
  # Generate labels
  # FIXME: allow overriding these labels.
  labels <- ablation.labels(trajectory, configurations)

  inches_to_lines <- (par("mar") / par("mai"))[1]
  lab.width <- max(strwidth(labels, units = "inches")) * inches_to_lines
  old.par <- par(mar = c(lab.width + 2.1, 4.1, 0.1, 0.1), cex.axis = 1)
  if (!is.null(pdf.file))
    on.exit(par(old.par), add = TRUE)

  experiments <- ablog$experiments
  if ("rank" %in% type) {
    experiments <- rowRanks(experiments, ties.method = "average")
    if (is.null(ylim)) ylim <- c(1L, ncol(experiments))
  }
  # FIXME: We could also show the other alternatives at each step not just the
  # one selected. See Leonardo Bezerra's thesis.
  if ("boxplot" %in% type) {
    bx <- boxplot(experiments[, trajectory], plot=FALSE)
    if (is.null(ylim)) {
      ylim <- range(bx$stats[is.finite(bx$stats)],
                    bx$out[is.finite(bx$out)], 
                    bx$conf[is.finite(bx$conf)])
    }
  }
  costs.avg <- colMeans2(experiments, cols = trajectory)
    
  plot(costs.avg, xaxt = "n", xlab = NA, ylab = ylab, ylim = ylim,
       type = "b", pch = 19, ...,
       panel.first = {
         grid(nx = NA, ny = NULL, lwd = 2);
         abline(h = c(costs.avg[1], tail(costs.avg, n = 1)),
                col = "lightgray", lty = "dotted", lwd = 2) })
  axis(1, at = 1:length(costs.avg), labels = labels, las = 3)
  if ("boxplot" %in% type) {
    bxp(bx, show.names = FALSE, add = TRUE)
  }
  invisible()
}

#' Read the log file (`log-ablation.Rdata`) produced by [irace::ablation()].
#'
#' @param filename Filename that contains the log file saved by [ablation()]. Example: `log-ablation.Rdata`.
#' 
#' @return (`list()`)
#' @concept ablation
#' @export
read_ablogfile <- function(filename) read_logfile(filename, name = "ablog")
