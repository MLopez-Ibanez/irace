.ablation.params.def <- utils::read.table(header=TRUE, stringsAsFactors = FALSE, text="
name            ab  type short long          default               description
iraceResults     0  p    -l    --log-file    NA                    'Path to the (.Rdata) file created by irace from which the  \"iraceResults\" object will be loaded.'
src              1  i    -S    --src         1                     'Source configuration ID or the path to a file containing the configuration.'
target           1  i    -T    --target      NA                    'Target configuration ID (by default the best configuration found by irace) or the path to a file containing the configuration.'
ab_params        1  s    -P    --params      ''                    'Specific parameter names to be used for the ablation (separated with commas). By default use all'
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

## __VERSION__ below will be replaced by the version defined in R/version.R
## This avoids constant conflicts within this file.
cat_ablation_license <- function()
{
  ablation_license <-
'#------------------------------------------------------------------------------
# ablation: An implementation in R of Ablation Analysis
# Version: __VERSION__
# Copyright (C) 2020--2025
# Manuel Lopez-Ibanez     <manuel.lopez-ibanez@manchester.ac.uk>
# Leslie Perez Caceres    <leslie.perez.caceres@ulb.ac.be>
#
# This is free software, and you are welcome to redistribute it under certain
# conditions.  See the GNU General Public License for details. There is NO
# WARRANTY; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#------------------------------------------------------------------------------
'
  cat(sub("__VERSION__", irace_version, ablation_license, fixed=TRUE))
}

#' Launch ablation with command-line options.
#'
#' Launch [ablation()] with the same command-line options as the command-line
#' executable (`ablation.exe` in Windows).
#'
#' @param argv `character()`\cr The arguments
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
#' @return A list containing the following elements:
#'  \describe{
#'    \item{allConfigurations}{Configurations tested in the ablation.}
#'    \item{state}{State of the ablation process.}
#'    \item{experiments}{A matrix with the results of the experiments (columns are configurations, rows are instances).}
#'    \item{scenario}{Scenario object with the settings used for the experiments.}
#'    \item{trajectory}{IDs of the best configurations at each step of the ablation.}
#'    \item{best}{Best configuration found in the experiments.}
#'    \item{complete}{`TRUE` if the ablation process was completed.}
#'  }
#'
#' @seealso [plotAblation()] [ablation()]
#' @examples
#' ablation_cmdline("--help")
#' # Find the ablation command-line executable:
#' Sys.glob(file.path(system.file(package="irace", "bin"), "ablation*"))
#' @author Manuel López-Ibáñez
#' @concept ablation
#' @export
ablation_cmdline <- function(argv = commandArgs(trailingOnly = TRUE))
{
  withr::local_options(list(width = 9999L)) # Do not wrap the output.

  cat_ablation_license()
  cat ("# installed at: ", system.file(package="irace"), "\n",
       "# called with: ", paste(argv, collapse = " "), "\n", sep = "")
  parser <- CommandArgsParser$new(argv = argv, argsdef = .ablation.params.def)
  if (!is.null(parser$readArg (short = "-h", long = "--help"))) {
    parser$cmdline_usage()
    return(invisible(NULL))
  }

  if (!is.null(parser$readArg (short = "-v", long = "--version"))) {
    print(utils::citation(package="irace"))
    return(invisible(NULL))
  }

  params <- parser$readAll()
  # TODO: Send the other options to the irace command-line parser so the user
  # can override options in scenario via the command-line.
  if (length(parser$argv) > 0L)
    stop("Unknown command-line options: ", paste(parser$argv, collapse = " "))

  if (is.null(params$iraceResults)) {
    irace_error("You must provide the path to the '.Rdata' file that contains the 'iraceResults' object generated by irace.")
    return(invisible(NULL))
  }
  iraceResults <- read_logfile(params$iraceResults)
  if (is.null(params$old_path) != is.null(params$new_path)) {
    irace_error("To update paths you must provide both --old-path and --new-path.")
    return(invisible(NULL))
  } else if (!is.null(params$old_path)) {
    iraceResults$scenario <- scenario_update_paths(iraceResults$scenario, params$old_path, params$new_path)
  }

  scenario <- list()
  if (!is.null(params$scenarioFile)) {
    scenario <- readScenario(params$scenarioFile)
    # We do not want this seed value to override the the command-line.
    scenario$seed <- NULL
  }
  for (p in c("execDir", "parallel")) {
    if (!is.null(params[[p]]))
      scenario[[p]] <- params[[p]]
  }

  if (is_null_or_empty_or_na(trim(params$ablationLogFile))) {
    params$ablationLogFile <- NULL
  } else {
    params$ablationLogFile <- path_rel2abs(params$ablationLogFile)
  }

  if (!is.null(params$ab_params))
    params$ab_params <- trimws(strsplit(params$ab_params, ",", fixed=TRUE)[[1L]])

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
    plotAblation(ablog, pdf_file = params[["plot"]], type = params$plot_type)
  }
  invisible(ablog)
}

fix_dependents <- function(parameters, dep_names, new_conf, final_conf, changed)
{
  # ... check every dependent parameter...
  for (dep  in dep_names) {
    # If the conditions are satisfied and it doesn't have a value but it
    # should, then change it.  If the conditions are not satisfied and it
    # has a value but it shouldn't, then change it.
    ok <- conditionsSatisfied(parameters$conditions[[dep]], new_conf)
    change <- ok == is.na(new_conf[[dep]])
    dep_param <- parameters$get(dep)
    if (change) {
      # If it doesn't have a value but it should, take it from final.
      if (ok) {
        if (dep_param[["is_dependent"]]) {
          # If the parameter is domain-dependent, then:
          if (anyNA(new_conf[, .SD, .SDcols=all.vars(dep_param$domain)])) {
            # If it depends on a parameter that is disabled, then this is disabled, so no need to change.
            next
          } else if (is.na(final_conf[[dep]]) # If the final value is NA, we cannot fix it, so skip it.
            # If parameter X enables dep but dep has domain that depends on
            # parameter Y, then Y in new_conf may have a value that makes
            # final_conf[[dep]] invalid. We cannot fix this, so skip.
            || !is_within_dependent_bound(dep_param, new_conf, value = final_conf[[dep]]))
            return(NULL)
        }
        set(new_conf, j = dep, value = final_conf[[dep]])
      } else {
        set(new_conf, j = dep, value = NA)
      }
      changed <- c(changed, dep)
    } else if (dep_param[["is_dependent"]] && !is.na(new_conf[[dep]])) {
      # The condition may not have changed but the domain may depend on a parameter that has been changed.
      if (anyNA(new_conf[, .SD, .SDcols=all.vars(dep_param$domain)])) {
        set(new_conf, j = dep, value = NA)
        changed <- c(changed, dep)
      } else if (!is_within_dependent_bound(dep_param, new_conf, value = new_conf[[dep]])) {
        # If the current value is not within bound and the final value is NA, we cannot fix it, so skip it.
        if (is.na(final_conf[[dep]])
          # If parameter X enables dep but dep has domain that depends on
          # parameter Y, then Y in new_conf may have a value that makes
          # final_conf[[dep]] invalid. We cannot fix this, so skip.
          || !is_within_dependent_bound(dep_param, new_conf, value = final_conf[[dep]]))
          return(NULL)
        set(new_conf, j = dep, value = final_conf[[dep]])
        changed <- c(changed, dep)
      }
    }
  }
  changed
}

## Function that generates the configurations of the ablation path
## between initial_configuration and final_configuration.
## parameters can be selected by specifying them in para.names.
generate_ablation <- function(initial_configuration, final_configuration,
                              parameters, param_names)
{
  # Only change variable parameters
  param_names <- setdiff(param_names, parameters$names_fixed)
  # Follow the hierarchy order.
  hierarchy <- sort(parameters$hierarchy)
  param_names <- intersect(names(parameters$hierarchy), param_names)
  configurations <- list()
  changed_params <- list()
  # skip if the value is the same or NA (not active)
  skip <- (initial_configuration[param_names] == final_configuration[param_names])
  skip <- skip[1L, ] # Drop to a vector with names
  skip <- skip | is.na(skip)
  for (pname in param_names) {
    # If parameter is not active, it will only change if its condition becomes true.
    if (skip[[pname]]) next
    param <- parameters$get(pname)
    # If the parameter is domain-dependent, then we can only change its
    # value if the new value is within the domain.
    if (param[["is_dependent"]]
      && !is_within_dependent_bound(param, initial_configuration, value = final_configuration[[pname]]))
      next
    new_configuration <- as.data.table(initial_configuration)
    set(new_configuration, j = pname, value = final_configuration[[pname]])
    changed <- pname
    # If other parameters depend on this one then...
    if (any(sapply(parameters$depends, function(x) any(pname %in% x)))) {
      changed <- fix_dependents(parameters, dep_names = names(hierarchy)[hierarchy > hierarchy[[pname]]],
        new_conf = new_configuration, final_conf = final_configuration, changed = changed)
      if (is.null(changed))
        next
    }
    new_configuration <- filter_forbidden(new_configuration, parameters$forbidden)
    if (nrow(new_configuration) == 0L) next
    changed_params <- c(changed_params, list(changed))
    configurations <- c(configurations, list(new_configuration))
  }
  configurations <- rbindlist(configurations)
  set(configurations, j = ".PARENT.", value = configurations[[".ID."]])
  setDF(configurations)
  rownames(configurations) <- NULL
  list(configurations=configurations, changed_params=changed_params)
}

report_duplicated_results <- function(experiments, configurations)
{
  x <- t(experiments)
  x <- x[duplicated(x) | duplicated(x, fromLast = TRUE), , drop=FALSE]
  if (nrow(x) == 0L) return(NULL) # No duplicates
  dups <- split(rownames(x), apply(x, 1L, paste0, collapse=""))
  names(dups) <- NULL
  for (g in dups) {
    cat("Warning: The following configurations are different but produced the same results:\n")
    df <- configurations[configurations[[".ID."]] %in% g, , drop=FALSE]
    print(df)
    cat("Parameters with different values from the above configurations:\n")
    df <- removeConfigurationsMetaData(df)
    print(df[, vapply(df, function(x) length(unique(x)) > 1L, logical(1L)), drop=FALSE])
  }
  dups
}

ab_generate_instances <- function(race_state, scenario, nrep, type, instancesFile)
{
  nrep <- suppressWarnings(as.integer(nrep))
  if (is.na(nrep) || length(nrep) == 0L || nrep <= 0L)
    stop("'nrep' must be an integer larger than zero")
  if (nrep != 1L && type == "racing")
    stop("'nrep' has no effect when type == 'racing'")
  if (nrep > 1L && scenario$deterministic)
    stop("'nrep > 1' does not make sense with a deterministic scenario")

  if (instancesFile == "test") {
    scenario$instances <- scenario$testInstances
  } else if (instancesFile != "train") {
    scenario$instances <- readInstances(instancesFile = path_rel2abs(instancesFile))
  }
  n_inst <- length(scenario$instances)
  if (type == "full" && n_inst * nrep == 1L)
    stop("'nrep' must be larger than 1 when type == 'full' and a single instance")
  generateInstances(race_state, scenario, n_inst * nrep)

  msg <- if (instancesFile %in% c("train", "test"))
           paste0("'", instancesFile, "' instances") else paste0("instances from '", instancesFile, "'")
  if (n_inst > 100L) {
    n_inst <- 100L
    msg <- paste0(msg, " (only showing first 100)")
  }
  cat(sep="", "# Using ", msg, ":\n",
      paste0(collapse="\n", scenario$instances[1L:n_inst]),
      "\n")
  scenario$instances
}

#' Performs ablation between two configurations (from source to target).
#'
#' @description Ablation is a method for analyzing the differences between two configurations.
#'
#' @inheritParams has_testing_data
#' @param src,target `integer(1)|character(1)`\cr Source and target configuration IDs. By default, the first configuration ever evaluated (ID 1) is used as `src` and the best configuration found by irace is used as target. If the argument is a string, it is interpreted as the path to a file, with the format specified by  [readConfigurationsFile()], that contains the configuration.
#' @param ab_params `character()`\cr Specific parameter names to be used for the ablation. They must be in `parameters$names`. By default, use all parameters.
#' @param type `"full"|"racing"`\cr Type of ablation to perform: `"full"` will execute each configuration on all `n_instances` to determine the best-performing one; `"racing"` will apply racing to find the best configurations.
#' @param nrep `integer(1)`\cr Number of replications per instance used in `"full"` ablation. When `nrep > 1`, each configuration will be executed `nrep` times on each instance with different random seeds.
#' @param seed `integer(1)`\cr Integer value to use as seed for the random number generation.
#' @param ablationLogFile  `character(1)`\cr Log file to save the ablation log. If `NULL`, the results are not saved to a file.
#' @param instancesFile  `character(1)`\cr Instances file used for ablation: `'train'`, `'test'` or a filename containing the list of instances.
#' @param ... Further arguments to override scenario settings, e.g., `debugLevel`, `parallel`, etc.
#'
#' @references
#' C. Fawcett and H. H. Hoos. Analysing differences between algorithm
#' configurations through ablation. Journal of Heuristics, 22(4):431–458, 2016.
#'
#' @inherit ablation_cmdline return
#' @seealso [plotAblation()] [ablation_cmdline()]
#' @examples
#' \donttest{
#' logfile <- system.file(package="irace", "exdata", "sann.rda")
#' # Execute ablation between the first and the best configuration found by irace.
#' ablog <- ablation(logfile, ablationLogFile = NULL)
#' plotAblation(ablog)
#' # Execute ablation between two selected configurations, and selecting only a
#' # subset of parameters, directly reading the setup from the irace log file.
#' ablog <- ablation(logfile, src = 1, target = 10,
#'                   ab_params = c("temp"), ablationLogFile = NULL)
#' plotAblation(ablog)
#' }
#'
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @concept ablation
#' @export
ablation <- function(iraceResults, src = 1L, target = NULL,
                     ab_params = NULL, type = c("full", "racing"),
                     nrep = 1L, seed = 1234567L,
                     ablationLogFile = "log-ablation.Rdata",
                     instancesFile="train", ...)
{
  # Input check
  if (missing(iraceResults) || is.null(iraceResults))
    stop("You must provide an 'iraceResults' object generated by irace or the path to the '.Rdata' file that contains this object.")

  type <- match.arg(type)

  if (!is.null(ablationLogFile)) {
    file.check(ablationLogFile, writeable = TRUE, text = 'ablationLogFile')
  }

  save_ablog <- function(complete) {
    ablog <- list(changes = changes,
      # This name must match the one used in the irace log.
      allConfigurations = as.data.frame(all_configurations),
      experiments = experiments,
      state = race_state,
      scenario = scenario,
      trajectory = trajectory,
      best = best_configuration,
      complete = complete)
    if (!is.null(ablationLogFile))
      save(ablog, file = ablationLogFile, version = 3L)
    invisible(ablog)
  }

  # Load the data of the log file.
  iraceResults <- read_logfile(iraceResults)
  log_version <- get_log_clean_version(iraceResults)
  if (log_version < "3.9.0")
    irace_error("The version of the logfile (", log_version, ") is too old for this version of ablation")

  if (is.null(iraceResults$state$completed) || length(iraceResults$state$completed) != 1L
    || iraceResults$state$completed == "Incomplete")
    stop("The 'iraceResults' logfile seems to belong to an incomplete run of irace.")
  scenario <- update_scenario(scenario = iraceResults$scenario, ...)
  scenario$logFile <- ""
  old_seed <- get_random_seed()
  on.exit(restore_random_seed(old_seed))
  # FIXME: We need to overwrite the seed because RaceState takes it from the scenario and calls set_random_seed().
  scenario$seed <- seed
  scenario <- checkScenario(scenario)
  race_state <- RaceState$new(scenario)
  # Generate instances
  scenario$instances <- ab_generate_instances(race_state, scenario, nrep, type, instancesFile)
  parameters <- scenario$parameters

  # Process src and target.
  if (is.character(src) && is.na(suppressWarnings(as.integer(src)))) {
    src_configuration <- readConfigurationsFile(src, parameters)
    if (nrow(src_configuration) != 1L) {
      stop("Argument of src=\"", src, "\" contains more than one configuration!")
    }
    src <- 1L + max(iraceResults$allConfigurations$.ID.)
    src_configuration$.ID. <- src
    src_configuration$.PARENT. <- NA_integer_
    iraceResults$allConfigurations <- rbind.data.frame(iraceResults$allConfigurations, src_configuration)
    # FIXME: Check for duplicates
  } else if (src %not_in% iraceResults$allConfigurations[[".ID."]])
    stop("Source configuration ID (", src, ") cannot be found!")

  if (is.null(target))
    target <- iraceResults$iterationElites[length(iraceResults$iterationElites)]
  else if (is.character(target) && is.na(suppressWarnings(as.integer(target)))) {
    target_configuration <- readConfigurationsFile(target, parameters)
    if (nrow(target_configuration) != 1L) {
      stop("Argument of target=\"", target, "\" contains more than one configuration!")
    }
    target <- 1L + max(iraceResults$allConfigurations$.ID.)
    target_configuration$.ID. <- target
    target_configuration$.PARENT. <- NA_integer_
    iraceResults$allConfigurations <- rbind.data.frame(iraceResults$allConfigurations, target_configuration)
    # FIXME: Check for duplicates
  } else if (target %not_in% iraceResults$allConfigurations[[".ID."]])
    stop("Target configuration ID (", target, ") cannot be found!")

  if (src == target)
    stop("Source and target configuration IDs must be different!")

  irace_note ("Starting ablation from ", src, " to ", target, "\n# Seed: ", race_state$seed, "\n")
  cat("# Source configuration (row number is ID):\n")
  src_configuration <- iraceResults$allConfigurations[src, , drop = FALSE]
  configurations_print(src_configuration)
  cat("# Target configuration (row number is ID):\n")
  target_configuration <- iraceResults$allConfigurations[target, , drop = FALSE]
  configurations_print(target_configuration)

  # Select the parameters used for ablation
  if (is.null(ab_params)) {
    ab_params <- parameters$names
  } else if (!all(ab_params %in% parameters$names)) {
    irace_error("Some of the parameters provided (", paste0(setdiff(ab_params, parameters$names), collapse=", "),
                ") are not defined in the parameter space.")
  }
  # Select parameters that are different in both configurations
  neq_params <- which(src_configuration[,ab_params] != target_configuration[,ab_params])
  if (length(neq_params) == 0L)
    irace_error("src and target configurations are equal considering the parameters selected.\n")
  param_names <- colnames(src_configuration[,ab_params])[neq_params]

  # FIXME: Do we really need to override the ID?
  src_configuration[[".ID."]] <- 1L
  target_configuration[[".ID."]] <- 2L
  all_configurations <- rbindlist(list(src_configuration, target_configuration), use.names=TRUE)

  race_state$start_parallel(scenario)
  on.exit(race_state$stop_parallel(), add = TRUE)

  # Execute source and target configurations.
  ## FIXME: We may already have these experiments in the logFile!
  irace_note("Executing source and target configurations on the given instances * nrep (", nrow(race_state$instances_log), ")...\n")
  experiments <- do_experiments(race_state,
    configurations = rbind.data.frame(src_configuration, target_configuration),
    ninstances = nrow(race_state$instances_log), scenario = scenario, iteration = 0L)
  irace_assert(ncol(experiments) == 2L)
  step <- 1L
  # Define variables needed
  trajectory <- 1L
  names(trajectory) <- "source"
  # FIXME: changes should only store the changed parameters.
  changes <- list()
  best_configuration <- src_configuration
  save_ablog(complete = FALSE)
  while (length(param_names) > 1L) {
    # Generate ablation configurations
    cat("# Generating configurations (row number is ID):", param_names, "\n")
    ab_aux <- generate_ablation(best_configuration, target_configuration, parameters,
      param_names)
    aconfigurations <- ab_aux$configurations
    if (nrow(aconfigurations) == 0L) {
      cat("# Stopping ablation, no parameter change possible.\n")
      break
    }
    ## FIXME: We may already have these configurations in the logFile!
    # New configurations IDs
    aconfigurations[[".ID."]] <- max(all_configurations[[".ID."]]) + seq_nrow(aconfigurations)
    configurations_print(aconfigurations, metadata = FALSE)
    all_configurations <- rbindlist(list(all_configurations, aconfigurations), use.names=TRUE)

    irace_note("Ablation (", type, ") of ", nrow(aconfigurations),
      " configurations on ", nrow(race_state$instances_log), " instances (this may take a while ...).\n")
    if (type == "full") {
      step_experiments <- do_experiments(race_state,
        configurations = aconfigurations,
        ninstances = nrow(race_state$instances_log),
        scenario = scenario, iteration = step)
    } else {
      # Set variables for the racing procedure
      # FIXME: what about blockSize?
      race_state$next_instance <- 1L
      race_output <- elitist_race(race_state,
        maxExp = nrow(aconfigurations) * nrow(race_state$instances_log),
        minSurvival = 1L,
        elite_data = NULL,
        configurations = aconfigurations,
        scenario = scenario,
        elitist_new_instances = 0L)
      set(race_output$experiment_log, j = "iteration", value = step)
      race_state$experiment_log <-
        rbindlist(list(race_state$experiment_log, race_output$experiment_log),
          use.names=TRUE)
      step_experiments <- race_output$experiments
    }
    experiments <- merge_matrix(experiments, step_experiments)

    save_ablog(complete = FALSE)
    # Get the best configuration based on the criterion of irace
    cranks <- overall_ranks(experiments[, aconfigurations[[".ID."]], drop=FALSE], test = scenario$testType)
    best_id <- which.min(cranks)[1L]
    changes[[step]] <- ab_aux$changed_params
    best_change <- changes[[step]][[best_id]]
    trajectory <- c(trajectory, aconfigurations[[".ID."]][best_id])

    # Report best.
    cat("# Best changed parameters:\n")
    for (i in seq_along(best_change)) {
      cat("#", best_change[i], ":", best_configuration[,best_change[i]], "->",
          aconfigurations[best_id, best_change[i]], "\n")
    }
    best_configuration <- aconfigurations[best_id,,drop=FALSE]
    best_id <- best_configuration[[".ID."]]
    param_names <- param_names[param_names %not_in% best_change]
    step <- step + 1L
  }

  trajectory <- c(trajectory, target_configuration[[".ID."]])
  # Get the overall best
  cranks <- overall_ranks(experiments[,trajectory, drop=FALSE], test = scenario$testType)
  best_id <- which.min(cranks)[1L]
  setDF(all_configurations, rownames = all_configurations[[".ID."]])
  best_configuration <- all_configurations[trajectory[best_id], , drop=FALSE]
  irace_note("Final best configuration:\n")
  configurations_print(best_configuration)

  # Check for duplicated results:
  report_duplicated_results(experiments, all_configurations)

  save_ablog(complete = TRUE)
}

ablation_labels <- function(trajectory, configurations)
{
  configurations <- removeConfigurationsMetaData(configurations[trajectory, , drop = FALSE])
  labels <- names(trajectory)
  last <- configurations[1L, , drop = FALSE]
  param_names <- colnames(last)
  for (i in 2L:length(trajectory)) {
    current <- configurations[i, , drop = FALSE]
    # Select everything that is NOT NA now and was different or NA before.
    select <- !is.na(current) & (is.na(last) | (current != last))
    irace_assert(!anyNA(select))
    labels[i] <- paste0(param_names[select], "=", current[, select], collapse = "\n")
    last <- current
  }
  labels
}

#' Create plot from an ablation log
#'
#' @param ablog (`list()`|`character(1)`) Ablation log object returned by [ablation()]. Alternatively, the path to an `.Rdata` file, e.g., `"log-ablation.Rdata"`, from which the object will be loaded.

#' @param pdf_file Output filename.
#' @param width Width provided to create the PDF file.
#' @param height Height provided to create the PDF file.
#' @param type Type of plot. Supported values are `"mean"` and `"boxplot"`. Adding `"rank"` will plot rank per instance instead of raw cost value.
#' @param n `integer(1)`\cr Number of parameters included in the plot. By default all parameters are included.
#' @param mar Vector with the margins for the ablation plot.
#' @param ylab Label of y-axis.
#' @param ylim Numeric vector of length 2 giving the y-axis range.
#' @param rename_labels `character()`\cr Renaming table for nicer labels. For example, `c("No value"="NA", "LongParameterName"="LPN")`.
#' @param ... Further graphical parameters may also be supplied as
#'   arguments. See [graphics::plot.default()].
#'
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @seealso [ablation()] [ablation_cmdline()]
#' @examples
#' logfile <- file.path(system.file(package="irace"), "exdata", "log-ablation.Rdata")
#' plotAblation(ablog = logfile)
#' plotAblation(ablog = logfile, type = "mean")
#' plotAblation(ablog = logfile, type = c("rank","boxplot"), rename_labels = c(
#'             "localsearch"="ls", algorithm="algo", source="default"))
#' @concept ablation
#' @export
plotAblation <- function (ablog, pdf_file = NULL, width = 20,
                          height = 7,
                          type = c("mean", "boxplot", "rank"), n = 0L,
                          mar = NULL,
                          ylab = "Mean configuration cost", ylim = NULL,
                          rename_labels = NULL,
                          ...)
{
  type <- trimws(unlist(strsplit(type, ",", fixed=TRUE)))
  type <- match.arg(type, several.ok = TRUE)
  if (missing(ylab) && ("rank" %in% type)) ylab <- "Rank per instance"

  if (missing(ablog) || is.null(ablog)) {
    irace_error("You must provide an 'ablog' object generated by ablation() or the path to the '.Rdata' file that contains this object.")
  }
  ablog <- read_ablogfile(ablog)
  if (!ablog$complete)
    stop("The ablog shows that the ablation procedure did not complete cleanly and only contains partial information")


  if (!is.null(pdf_file)) {
    if (!is.file.extension(pdf_file, ".pdf"))
      pdf_file <- paste0(pdf_file, ".pdf")
    cat("Creating PDF file '", pdf_file, "'\n", sep="")
    local_cairo_pdf(pdf_file, width = width, height = height, onefile= TRUE)
  }

  configurations <- ablog$allConfigurations
  # Support irace < 4.2
  if (is.null(configurations))
    configurations <- ablog$configurations

  trajectory <- ablog$trajectory
  if (n > 0L)
    trajectory <- trajectory[seq_len(n+1L)]
  # Generate labels
  labels <- ablation_labels(trajectory, configurations)
  if (!is.null(rename_labels))
    # stringr::str_replace_all() would be better but it has so many dependencies!
    for (i in seq_along(rename_labels))
      labels <- gsub(names(rename_labels)[i], rename_labels[i], labels)

  experiments <- ablog$experiments
  if ("rank" %in% type) {
    experiments <- rowRanks(experiments, ties.method = "average")
    if (is.null(ylim)) ylim <- c(1L, ncol(experiments))
  }
  costs_avg <- colMeans2(experiments, cols = trajectory)

  if (is.null(mar))
    mar <- par("mar")
  inches_to_lines <- (mar / par("mai"))[1L]
  lab_width <- max(strwidth(labels, units = "inches")) * inches_to_lines
  with_par(list(mar = c(lab_width + 2.1, 4.1, 0.1, 0.1), cex.axis = 1), {
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

    plot(costs_avg, xaxt = "n", xlab = NA, ylab = ylab, ylim = ylim,
      type = "b", pch = 19, ...,
      panel.first = {
        grid(nx = NA, ny = NULL, lwd = 2);
        abline(h = c(costs_avg[1], utils::tail(costs_avg, n = 1L)),
          col = "lightgray", lty = "dotted", lwd = 2) })
    axis(1, at = seq_along(costs_avg), labels = labels, las = 3)
    if ("boxplot" %in% type) {
      bxp(bx, show.names = FALSE, add = TRUE)
    }
  })
  invisible()
}

#' Read the log file (`log-ablation.Rdata`) produced by [irace::ablation()].
#'
#' @param filename `character(1)`\cr Filename that contains the log file saved by [ablation()]. Example: `log-ablation.Rdata`.
#'
#' @return `list()`
#' @concept ablation
#' @export
read_ablogfile <- function(filename) read_logfile(filename, name = "ablog")
