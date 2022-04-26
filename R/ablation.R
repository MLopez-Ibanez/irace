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
  final <- list(configuration=configuration, changed=changed)
  return(final)
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
  return (list(configurations=configurations, changed.params=changed.params))
}

#' Performs ablation between two configurations (from source to target).
#'
#' @description Ablation is a method for analyzing the differences between two configurations.
#'
#' @template arg_iraceresults
#' @param src,target Source and target configuration IDs. By default, the first configuration ever evaluated (ID 1) is used as `src` and the best configuration found by irace is used as target.
#' @param ab.params Parameter names to be used for the ablation. They must be in `parameters$names`.
#' @param n.instances Number of instances to be used for the `"full"` ablation, if not provided `scenario$firstTest` instances are used.
#' @param type Type of ablation to perform: `"full"` will execute each configuration on all `n.instances` to determine the best-performing one; `"racing"` will apply racing to find the best configurations.
#' @param seed (`integer(1)`) Integer value to use as seed for the random number generation.
#' @param ablationLogFile  (`character(1)`) Log file to save the ablation log. If `NULL`, the results are not saved to a file.
#' @param ... Further arguments to override scenario settings, e.g., `debugLevel`, `parallel`, etc.
#'
#' @references
#' C. Fawcett and H. H. Hoos. Analysing differences between algorithm
#' configurations through ablation. Journal of Heuristics, 22(4):431–458, 2016.
#' 
#' @return A list containing the following elements:
#'  \describe{
#'    \item{configurations}{Configurations tested in the ablation.}
#'    \item{instances}{A matrix with the instances used in the experiments. First column has the 
#'     instances IDs from \code{iraceResults$scenario$instances}, second column the seed assigned to the instance.}
#'    \item{experiments}{A matrix with the results of the experiments (columns are configurations, rows are instances).}
#'    \item{scenario}{Scenario object with the settings used for the experiments.}
#'    \item{trajectory}{IDs of the best configurations at each step of the ablation.}
#'    \item{best}{Best configuration found in the experiments.}
#'  }
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
#' @export
#' @md
ablation <- function(iraceResults, src = 1L, target = NULL,
                     ab.params = NULL, n.instances = NULL,
                     type = c("full", "racing"), seed = 1234567,
                     ablationLogFile = "log-ablation.Rdata", ...)
{
  # Input check
  if (missing(iraceResults) || is.null(iraceResults)) 
    stop("You must provide an 'iraceResults' object generated by irace or the path to the '.Rdata' file that contains this object.")

  type <- match.arg(type)

  if (!is.null(n.instances) && type == "racing") {
    stop("'n.instances' has no effect when type == 'racing'")
  }

  save_ablog <- function(complete) {
    ablog <- list(changes = changes,
                  configurations = all.configurations,
                  experiments = results,
                  instances   = instances,
                  scenario    = scenario, 
                  trajectory  = trajectory,
                  best = best.configuration,
                  complete = complete)
    if (!is.null(ablationLogFile))
      save(ablog, file = ablationLogFile, version = 2)
    return(ablog)
  }
  
  # FIXME: The previous seed needs to be saved and restored at the end.
  set.seed(seed)
  # Load the data of the log file
  iraceResults <- read_logfile(iraceResults)
  if (is.null(target)) target <- iraceResults$iterationElites[length(iraceResults$iterationElites)]

  irace.note ("Starting ablation from ", src, " to ", target, "\n# Seed: ", seed, "\n")

  if (src %!in% iraceResults$allConfigurations$.ID.)
    stop("Source configuration ID (", src, ") cannot be found")
    
  if (target %!in% iraceResults$allConfigurations$.ID.)
    stop("Target configuration ID (", target, ") cannot be found")
  
  src.configuration <- iraceResults$allConfigurations[src, , drop = FALSE]
  target.configuration <- iraceResults$allConfigurations[target, , drop = FALSE]

  parameters <- iraceResults$parameters
  scenario   <- iraceResults$scenario
  scenario_args <- list(...)
  if (length(scenario_args) > 0) {
    if (any(names(scenario_args) %!in% names(scenario))) {
      irace.error("Unknown scenario settings given")
    }
    scenario <- modifyList(scenario, scenario_args)
  }
  
  if (!is.null(ablationLogFile)) scenario$logFile <- ablationLogFile
  scenario <- checkScenario (scenario)
  startParallel(scenario)
  on.exit(stopParallel(), add = TRUE)

  if (type == "racing")
    n.instances <- length(scenario$instances)
  else if (is.null(n.instances)) 
    n.instances <- scenario$firstTest
  instances <- generateInstances(scenario, n.instances)
  .irace$instancesList <- instances
    
  # Select the parameters used for ablation
  if (is.null(ab.params)) {
    ab.params <- parameters$names
  } else if (!all(ab.params %in% parameters$names)) {
    irace.error("Some of the parameters provided (", paste0(setdiff(ab.params, parameters$names), collapse=", "), ") are not defined in the parameter space.")
  }

  cat("# Source configuration (row number is ID):\n")
  configurations.print(src.configuration)
  cat("# Target configuration (row number is ID):\n")
  configurations.print(target.configuration)
  
  # Select parameters that are different in both configurations
  neq.params <- which(src.configuration[,ab.params] != target.configuration[,ab.params])
  
  if (length(neq.params) < 1) 
    irace.error("Candidates are equal considering the parameters selected\n")
  param.names <- colnames(src.configuration[,ab.params])[neq.params]
  
  # FIXME: Do we really need to override the ID?
  src.configuration$.ID. <- best.id <-  1
  best.configuration <- all.configurations <- src.configuration
  
  # Execute source and target configurations.
  ## FIXME: We may already have these experiments in the logFile!
  experiments <- createExperimentList(configurations = rbind(src.configuration, target.configuration), 
                                      parameters = parameters,
                                      instances = scenario$instances,
                                      instances.ID = instances[, "instance"],
                                      seeds = instances[, "seed"],
                                      scenario = scenario,
                                      bounds = scenario$boundMax)
  irace.note("Executing source and target configurations on the given instances...\n")
  target.output <- execute.experiments(experiments, scenario)
  if (!is.null(scenario$targetEvaluator))
    target.output <- execute.evaluator (experiments, scenario, target.output,
                                        src.configuration)
  # Save results
  output <- unlist(lapply(target.output, "[[", "cost")) 
  results <- matrix(NA, ncol = 1, nrow = nrow(instances), 
                    dimnames = list(seq(1,nrow(instances)), 1))
  results[,1] <- output[1:nrow(instances)]
  lastres <- output[(nrow(instances)+1):(2 * nrow(instances))]

  # Define variables needed
  trajectory <- 1
  names(trajectory) <- "source"
  # FIXME: changes should only store the changed parameters.
  changes <- list()
  step <- 1
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
    aconfigurations[,".ID."] <- seq(max(all.configurations$.ID.) + 1,
                                    max(all.configurations$.ID.) + nrow(aconfigurations))
    configurations.print(aconfigurations, metadata = FALSE)
    all.configurations <- rbind(all.configurations, aconfigurations)
    
    # Set variables for the racing procedure
    if (scenario$capping) {
      # For using capping we must set elite data
      elite.data <- list(experiments = results[,best.configuration$.ID., drop=FALSE])
      race.conf <-  rbind(best.configuration, aconfigurations)
      .irace$next.instance <- nrow(instances) + 1
    } else {
      #LESLIE: for now we apply the non-elitis irace when type=="racing"
      # we should define what is the standard
      elite.data <- NULL
      race.conf <-  aconfigurations
      scenario$elitist <- FALSE
      .irace$next.instance <- 1
    }
          
    irace.note("Ablation (", type, ") of ", nrow(aconfigurations),
               " configurations on ", nrow(instances), " instances.\n")
    # Force the race to see all instances in "full" mode
    if (type == "full") scenario$firstTest <- nrow(instances)
    race.output <- race(maxExp = nrow(aconfigurations) * nrow(instances),
                        minSurvival = 1,
                        elite.data = elite.data,
                        configurations = race.conf,
                        parameters = parameters,
                        scenario = scenario,
                        elitistNewInstances = 0)	
    results <- merge.matrix (results, race.output$experiments)

    # Save log
    ablog <- save_ablog(complete = FALSE)
    
    # Get the best configuration based on the criterion of irace
    # MANUEL: Doesn't race.output already give you all this info???
    cranks <- overall.ranks(results[,aconfigurations$.ID.,drop=FALSE], scenario$testType)
    best_id <- which.min(cranks)[1]
    # cand.mean <- colMeans(results[,aconfigurations$.ID.,drop=FALSE], na.rm=TRUE)
    changes[[step]] <- ab.aux$changed.params
    best.change <- changes[[step]][[best_id]]
    trajectory <- c(trajectory, aconfigurations[best_id, ".ID."])
    
    # Report best
    # FIXME: This ID does not actually match the configuration ID
    # The race already reports the best.
    #cat("# Best configuration ID:", best_id, "mean:", cand.mean[best_id],"\n")
    cat("# Best changed parameters:")
    for(i in seq_along(best.change)) {
      cat("#  ", best.change[i], best.configuration[,best.change[i]], "->",
          aconfigurations[best_id, best.change[i]], "\n")
    }
  
    best.configuration <- aconfigurations[best_id,,drop=FALSE]
    best.id <- best.configuration$.ID.
    param.names <- param.names[!(param.names %in% best.change)]
    step <- step + 1
  }
  
  # Add last configuration and its results
  # FIXME: This may be overriding the ID of an existing configuration!!!
  target.configuration$.ID. <- max(all.configurations$.ID.) + 1
  all.configurations <- rbind(all.configurations, target.configuration)
  results <- cbind(results, matrix(lastres, ncol = 1,
                                   dimnames=list(seq(1, nrow(instances)),
                                                 target.configuration$.ID.)))
  trajectory <- c(trajectory, target.configuration$.ID.)
  
  # Get the overall best
  cranks <- overall.ranks(results[,trajectory, drop=FALSE], scenario$testType)
  best_id <- which.min(cranks)[1]
  best.configuration <- all.configurations[trajectory[best_id],,drop=FALSE]
  irace.note("Final best configuration:\n")
  configurations.print(best.configuration)
  
  # LESLIE: If we use racing we can have a matrix of results that is not
  # complete, how should we do the plots?
  # MANUEL: Do not plot anything that was discarded
  
  # Save final log.
  ablog <- save_ablog(complete = TRUE)
  return(ablog)
}

ablation.labels <- function(trajectory, configurations)
{
  configurations <- removeConfigurationsMetaData(configurations[trajectory, , drop = FALSE])
  labels <- names(trajectory)
  last <- configurations[1, , drop = FALSE]
  param.names <- colnames(last)
  for (i in 2:length(trajectory)) {
    current <- configurations[i, , drop = FALSE]
    # select everything that is NOT NA now and was different or NA before.
    select <- !is.na(current) & (is.na(last) | (current != last))
    irace.assert(!anyNA(select))
    labels[i] <- paste0(param.names[select], "=", current[, select], collapse = "\n")
    last <- current
  }
  return(labels)
}

#' Create plot from an ablation log
#'
#' @param ablog (`list()`|`character(1)`) Ablation log object returned by [ablation()]. Alternatively, the path to an `.Rdata` file, e.g., `"log-ablation.Rdata"`, from which the object will be loaded.

#' @param pdf.file Output filename.
#' @param pdf.width Width provided to create the pdf file.
#' @param type Type of plots. Supported values are `"mean"` and
#'   `"boxplot"`.
#' @param mar Vector with the margins for the ablation plot.
#' @param ylab Label of y-axis.
#' @param ... Further graphical parameters may also be supplied as
#'   arguments. See [graphics::plot.default()].
#'
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @seealso [ablation()]
#' @examples
#' logfile <- file.path(system.file(package="irace"), "exdata", "log-ablation.Rdata")
#' plotAblation(ablog = logfile)
#' @md
#' @export
plotAblation <- function (ablog, pdf.file = NULL, pdf.width = 20,
                          type = c("mean", "boxplot"),
                          mar = par("mar"),
                          ylab = "Mean configuration cost", ...)
{
  type <- match.arg(type)
  if (missing(ablog) || is.null(ablog)) {
    irace.error("You must provide an 'ablog' object generated by ablation() or the path to the '.Rdata' file that contains this object.")
  }

  ablog <- read_logfile(ablog, name = "ablog")
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
  configurations <- ablog$configurations
  # Generate labels
  # FIXME: allow overriding these labels.
  labels <- ablation.labels(trajectory, configurations)

  inches_to_lines <- (par("mar") / par("mai"))[1]
  lab.width <- max(strwidth(labels, units = "inches")) * inches_to_lines
  old.par <- par(mar = mar + c(lab.width - 2, 0, 0, 0), cex.axis = 1)
  on.exit(par(old.par), add = TRUE)

  experiments <- ablog$experiments
  
  # FIXME: We could also show the other alternatives at each step not just the
  # one selected. See Leonardo's thesis.
  ylim <- NULL
  if (type == "boxplot") {
    bx <- boxplot(experiments[, trajectory], plot=FALSE)
    ylim <- range(ylim, bx$stats[is.finite(bx$stats)],
                  bx$out[is.finite(bx$out)], 
                  bx$conf[is.finite(bx$conf)])
  }
  costs.avg <- colMeans(experiments[, trajectory])
    
  plot(costs.avg, xaxt = "n", xlab = NA, ylab = ylab, ylim = ylim,
       type = "b", pch = 19, ...,
       panel.first = {
         grid(nx = NA, ny = NULL, lwd = 2);
         abline(h = c(costs.avg[1], tail(costs.avg, n = 1)),
                col = "lightgray", lty = "dotted", lwd = 2) })
  axis(1, at = 1:length(costs.avg), labels = labels, las = 3)
  if (type == "boxplot") {
    bxp(bx, show.names = FALSE, add = TRUE)
  }
}
