## This function fixes dependent parameters when a parameter values has been
## changed.
fixDependenciesWithReference <- function(configuration, ref.configuration, parameters)
{
  # Search parameters that need a value
  changed <- c()
  for (pname in parameters[["names"]]) {
    if (parameters[["isFixed"]][pname]) next
    # If dependent parameter has been activated, set the value of the reference.
    if (is.na(configuration[,pname]) && conditionsSatisfied(parameters, configuration, pname)) {
       if (!is.null(ref.configuration)) {
         configuration[,pname] <- ref.configuration[pname]
       } 
       changed <- c(changed, pname)
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

#' Performs ablation between two configurations.
#'
#' @description Ablation is a method for analyzing the differences between two configurations.
#' 
#' @param iraceLogFile Log file created by \pkg{irace}, this file must contain
#'   the \code{iraceResults} object.
#' @param iraceResults Object created by \pkg{irace} and saved in
#'   \code{scenario$logFile}.
#' @param src,target Source and target configuration IDs. If \code{NULL}, then
#'   the first configuration ever evaluated is used as source and the best
#'   configuration found is used as target.
#' @param ab.params Parameter names to be used for the ablation. They must be
#'   in parameters$names.
#' @param n.instances Number of instances to be used for the "full" ablation,
#'   if not provided firstTest instances are used.
#' @param type Type of ablation to perform, "full" will execute all instances
#'   in the configurations to determine the best performing, "racing" will
#'   apply racing to find the best configurations.
#' @param seed Numerical value to use as seed for the random number generation.
#' @param ablationLogFile Log file to save the ablation log.
#' @param pdf.file Prefix that will be used to save the plot file of the
#'   ablation results.
#' @param pdf.width Width provided to create the pdf file.
#' @param mar Vector with the margins for the ablation plot.
#' @param debugLevel Integer value. Larger values produce more verbose
#'   output. By default, the debugLevel given by the \code{iraceLogFile} /
#'   \code{iraceResults}.
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
#'
#' @examples
#' \donttest{
#' irace.logfile <- file.path(system.file(package="irace"), "exdata", "sann.rda")
#' load(irace.logfile)
#' # Execute ablation between the first and the best configuration found by irace.
#' ablation(iraceResults = iraceResults, ablationLogFile = NULL)
#' # Execute ablation between two selected configurations, and selecting only a
#' # subset of parameters, directly reading the setup from the irace log file.
#' ablation(iraceLogFile = irace.logfile, src = 1, target = 10,
#'          ab.params = c("temp"), ablationLogFile = NULL)
#' }
#'
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @export
#FIXME: Add check for the inputs!
ablation <- function(iraceLogFile = NULL, iraceResults = NULL,
                     src = NULL, target = NULL,
                     ab.params = NULL, n.instances = NULL,
                     type = "full", seed = 1234567,
                     ablationLogFile = "log-ablation.Rdata",
                     pdf.file = NULL, pdf.width = 20, mar = c(12,5,4,1),
                     debugLevel = NULL)
{
  # Input check
  if (is.null(iraceLogFile) && is.null(iraceResults)) 
    irace.error("You must provide a Rdata file or an iraceResults object.")
  if (!(type %in% c("full", "racing")))
    irace.error("Type of ablation", type, "not recognized.") 
                      	
  irace.note ("Starting ablation:\n# Seed:", seed, "\n")
   
  # Load the data of the log file
  if (!is.null(iraceLogFile)) load(iraceLogFile)
    
  if (is.null(src)) src <- 1
  if (is.null(target)) target <- iraceResults$iterationElites[length(iraceResults$iterationElites)]
  
  if (!(src %in% iraceResults$allConfigurations$.ID.))
    irace.error("Source configuration ID (", src, ") cannot be found")
    
  if (!(target %in% iraceResults$allConfigurations$.ID.))
    irace.error("Target configuration ID (", target, ") cannot be found")
  
  src.configuration <- iraceResults$allConfigurations[src, , drop = FALSE]
  target.configuration <- iraceResults$allConfigurations[target, , drop = FALSE]

  parameters <- iraceResults$parameters
  scenario   <- iraceResults$scenario
  
  if (!is.null(ablationLogFile))
    scenario$logFile <- ablationLogFile
  if (!is.null(debugLevel)) scenario$debugLevel <- debugLevel
  
  scenario <- checkScenario (scenario)
  
  startParallel(scenario)
  on.exit(stopParallel(), add = TRUE)

  # FIXME: The previous seed needs to be saved and restored at the end.
  set.seed(seed)

  # LESLIE: we should decide how to select the instances to perform the ablation
  # for now we generate an instace list with one instance.
  ## MANUEL: I think this should be obtained from parameters like
  ## instanceFile/instanceDir/instance and re-use the code from irace. And by
  ## default, it should use something like firstTest random training instances.
  ## LESLIE: Ok by default we use firstTest and I added a parameter to change it in case needed.
  if (type == "racing")
    n.instances <- length(scenario$instances)
  else if (is.null(n.instances)) 
    n.instances <- scenario$firstTest
  instances <- generateInstances(scenario, n.instances)
  .irace$instancesList <- instances
    
  # Select the parameters used for ablation
  if (is.null(ab.params)) {
    ab.params <-  parameters$names
  } else if (!all(ab.params %in% parameters$names)) {
  	  irace.error("Some of the parameters provided are not defined in the parameters object.")
  }  
  # Select parameters that are different in both configurations
  neq.params <- which(src.configuration[,ab.params] != target.configuration[,ab.params])
  
  if (length(neq.params) < 1) 
    irace.error("Candidates are equal considering the parameters selected\n")
  param.names <- colnames(src.configuration[,ab.params])[neq.params]
  
  cat("# Configurations (row number is ID):\n")
  configurations.print(rbind(src.configuration, target.configuration))

  # FIXME: Do we really need to override the ID?
  src.configuration$.ID. <- best.id <-  1
  best.configuration <- all.configurations <- src.configuration
  
  # Define variables needed                       
  results <- matrix(NA, ncol = 1, nrow = nrow(instances), 
                    dimnames = list(seq(1,nrow(instances)), 1))
  changes <- list()
  trajectory <- 1
  names(trajectory) <- "source"

  # Execute source and target configurations.
  ## FIXME: We may already have these experiments in the logFile!
  experiments <- createExperimentList(configurations = rbind(src.configuration, target.configuration), 
                                      parameters = parameters,
                                      instances = scenario$instances[instances[,"instance"]],
                                      # FIXME: We should create/use unique IDs for these instances.
                                      instances.ID = 1:nrow(instances),
                                      seeds = instances[, "seed"],
                                      scenario = scenario) # FIXME: No bound ???
  irace.note("Executing source and target configurations on the given instances...\n")
  target.output <- execute.experiments(experiments, scenario)
  if (!is.null(scenario$targetEvaluator))
    target.output <- execute.evaluator (experiments, scenario, target.output,
                                        src.configuration)
  # Save results
  output <- unlist(lapply(target.output, "[[", "cost")) 
  results[,1] <- output[1:nrow(instances)]
  lastres <- output[(nrow(instances)+1):(2*nrow(instances))]
  
  # Start ablation
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
    new.ids <- seq(max(all.configurations$.ID.) + 1,
                   max(all.configurations$.ID.) + nrow(aconfigurations)) 
    aconfigurations[,".ID."] <- new.ids
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
    
    # All instances for the firstTest for the full 
    if (type == "full") scenario$firstTest <- nrow(instances)
      
    irace.note("Ablation (", type, ") of ", nrow(aconfigurations),
               " configurations on ", nrow(instances), " instances.\n")
    # MANUEL: Is this racing or just running the configurations?
    race.output <- race(maxExp = nrow(aconfigurations) * nrow(instances),
                        minSurvival = 1,
                        elite.data = elite.data,
                        configurations = race.conf,
                        parameters = parameters,
                        scenario = scenario,
                        elitistNewInstances = 0)	
    results <- merge.matrix (results, race.output$experiments)

    # Save temp log
    ab.log <- list(configurations  = all.configurations,
                 instances   = instances, 
                 experiments = results, 
                 scenario    = scenario, 
                 trajectory  = trajectory, 
                 changes     = changes)
    if (!is.null(ablationLogFile))
      save(ab.log, file = ablationLogFile)
    
    # Get the best configuration based on the criterion of irace
    # MANUEL: Doesn't race.output already give you all this info???
    cranks <- overall.ranks(results[,aconfigurations$.ID.,drop=FALSE], scenario$testType)
    best.ids <- which.min(cranks)
    cand.mean <- colMeans(results[,aconfigurations$.ID.,drop=FALSE], na.rm=TRUE)
    changes[[step]] <- ab.aux$changed.params
    best.change <- changes[[step]][[best.ids[1]]]
    trajectory <- c(trajectory, aconfigurations[best.ids[1], ".ID."])
    
    # Report best
    # FIXME: This ID does not actually match the configuration ID
    # The race already reports the best.
    #cat("# Best configuration ID:", best.ids[1], "mean:", cand.mean[best.ids[1]],"\n")
    for(i in 1:length(best.change)){
      cat("#  ", best.change[i], best.configuration[,best.change[i]], "->",
          aconfigurations[best.ids[1],best.change[i]], "\n")
    }
  
    best.configuration <- aconfigurations[best.ids[1],,drop=FALSE]
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
  best.ids <- which.min(cranks)
  best.configuration <- all.configurations[trajectory[best.ids[1]],,drop=FALSE]
  irace.note("Final best configuration:\n")
  print(best.configuration)
  
  # LESLIE: If we use racing we can have a matrix of results that is not
  # complete, how should we do the plots?
  # MANUEL: Do not plot anything that was discarded
  
  # Save final log
  ab.log <- list(configurations  = all.configurations,
                 instances   = instances, 
                 experiments = results, 
                 scenario    = scenario, 
                 trajectory  = trajectory, 
                 best = best.configuration)

  if (!is.null(ablationLogFile))
    save(ab.log, file = ablationLogFile)
  
  plotAblation(ab.log = ab.log, pdf.file = pdf.file,
               pdf.width = pdf.width, mar = mar, main = "Ablation")
  return(ab.log)
}

ablation.labels <- function(trajectory, configurations)
{
  configurations <- removeConfigurationsMetaData(configurations[trajectory, , drop = FALSE])
  labels <- names(trajectory)[1]
  last <- configurations[1, , drop = FALSE]
  param.names <- colnames(last)
  for (i in 2:length(trajectory)) {
    current <- configurations[i, , drop = FALSE]
    # select everything that is NOT NA now and was different or NA before.
    select <- !is.na(current) & (is.na(last) | (current != last))
    irace.assert(!anyNA(select))
    labels <- c(labels,
                paste0(param.names[select], "=", current[, select], collapse = "\n"))
    last <- current
  }
  return(labels)
}

#' Create plot from an ablation log
#'
#' @param ab.log Ablation log returned by \code{\link{ablation}}.
#' @param abLogFile Rdata file containing the ablation log.
#' @param pdf.file Output filename.
#' @param pdf.width Width provided to create the pdf file.
#' @param type Type of plots. Supported values are \code{"mean"} and
#'   \code{"boxplot"}.
#' @param mar Vector with the margins for the ablation plot.
#' @param ylab Label of y-axis.
#' @param ... Further graphical parameters may also be supplied as
#'   arguments. See \code{plot.default}.
#'
#' @author Leslie Pérez Cáceres and Manuel López-Ibáñez
#' @seealso \code{\link{ablation}}
#' @export
plotAblation <- function (ab.log = NULL, abLogFile = NULL,
                          pdf.file = NULL, pdf.width = 20,
                          type = c("mean", "boxplot"),
                          mar = par("mar"),
                          ylab = "Mean configuration cost", ...)
{
  type <- match.arg(type)
  if (is.null(ab.log)) {
    if (is.null(abLogFile))
      irace.error("You must provide a log file or an ablation log object")
    else {
      load(abLogFile)
      if (is.null(ab.log))
        irace.error("abLogFile '", abLogFile, "' does not contain ab.log")
    }
  }

  if (!is.null(pdf.file)) {
    if (!is.file.extension(pdf.file, ".pdf"))
      pdf.file <- paste0(pdf.file, ".pdf")
    cat(paste0("Creating PDF file '", pdf.file, "'\n"))
    pdf(file = pdf.file, width = pdf.width,
        title = paste0("Ablation plot: ", pdf.file))
    on.exit(dev.off(), add = TRUE)
  }
  
  trajectory <- ab.log$trajectory
  configurations <- ab.log$configurations
  # Generate labels
  # FIXME: allow overriding these labels.
  labels <- ablation.labels(trajectory, configurations)

  inches_to_lines <- (par("mar") / par("mai"))[1]
  lab.width <- max(strwidth(labels, units = "inches")) * inches_to_lines
  old.par <- par(mar = mar + c(lab.width - 2, 0, 0, 0), cex.axis = 1)
  on.exit(par(old.par), add = TRUE)

  experiments <- ab.log$experiments
  
  # FIXME: We should also show the other alternatives at each step not just the
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
