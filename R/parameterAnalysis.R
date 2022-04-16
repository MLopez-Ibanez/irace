#' Plot of histogram of parameter values
#'
#' \code{parameterFrequency} plots the frequency of the parameters values in a
#'  set of target algorithm configurations. It generates plots showing the
#'  frequency of parameter values for each parameter, with \code{rows} *
#'  \code{cols} parameters being shown per plot.  If a filename is provided the
#'  plots are saved in one or more files.
#'   
#' @template arg_configurations
#' @template arg_parameters
#' @param rows Number of plots per column.
#' @param cols Number of plots per row.
#' @param filename Filename prefix to generate the plots. If \code{NULL} the plot 
#'   displayed but not saved.
#' @param pdf.width Width for the pdf file generated.
#' @param col Color of the bar plot.
#' 
#' @examples
#' \donttest{
#'  ## To use data obtained by irace
#' 
#'  # First, load the data produced by irace.
#'  irace.logfile <- file.path(system.file(package="irace"), "exdata", "irace-acotsp.Rdata")
#'  load(irace.logfile) # Creates iraceResults
#'  parameterFrequency(iraceResults$allConfigurations, iraceResults$parameters)
#' }
#'
#' @seealso 
#'  \code{\link{readParameters}} to obtain a valid parameter structure from a parameters file.
#'  \code{\link{readConfigurationsFile}} to obtain a set of target algorithm configurations from 
#'    a configurations file.
#' 
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
# TODO:
# * change slightly background of conditional parameters
#
# * accept a configuration (e.g, best one) and mark its values of the best
#   configuration somehow (bold?)
#
# * print number of NAs in numerical parameters within the plot "# NA = Total
#   (Percentage%)"
#
# * export this function and add an R manual page, perhaps with example.
#
# * how to resize the Window when filename == NULL to have a rows/cols aspect ratio.
#
# * use ggplot2 ?
#
# * add tests!
#parameterFrequency(iraceResults$allConfigurations, iraceResults$parameters)
#best <- colnames(experiments)[which(apply(experiments, 2, function(x) { sum(!is.na(x))}) > 5)]
#parameterFrequency(allConfigurations[best,], parameters)
parameterFrequency <- function(configurations, parameters,
                               rows = 4, cols = 3,
                               filename = NULL,
                               pdf.width = 12,
                               col = "gray")
{
  xlab <- "values"
  ylab.cat <- "Frequency"
  ylab.num <- "Probability density"
  
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  on.exit(par(def.par), add = TRUE)  #- reset to default

  configurations <- removeConfigurationsMetaData(configurations)

  param.names <- as.character(parameters$names)
  nparams <- parameters$nbVariable
  nlines <- ceiling(nparams / cols)  
  if (nlines < rows) rows <- nlines

  nplot <- 1
  cplot <- 1
  if (!is.null(filename)) {
    # Remove possible ".pdf" extension.
    filename <- sub(".pdf", "", filename, fixed = TRUE)
    pdf(file = paste0(filename, "-", cplot, ".pdf"), onefile = TRUE, width = pdf.width)
    on.exit(dev.off(), add = TRUE)
  }
  par(mfrow=c(rows,cols), mar=0.1 + c(4,3,3,1))
  
  for (param.name in param.names) {

    if (parameters$isFixed[[param.name]]){
      cat("Skipping fixed parameter:", param.name, "\n")
      next
    } else {
      cat("Plotting:", param.name, "\n")
    }
    
    if (nplot > rows * cols) {
      cat("Make new plot\n")
      cplot <- cplot + 1
      if (!is.null(filename)) {
        dev.off()
        pdf(file = paste0(filename, "-", cplot, ".pdf"), onefile = TRUE, width = pdf.width)
      } else {
        dev.new()
      }
      par(mfrow=c(rows, cols))
      nplot <- 1
    }

    data <- configurations[, param.name]
    type <- parameters$types[[param.name]]
    domain <- parameters$domain[[param.name]]
    if (type %in% c("c", "o")) {
      data <- factor(data, domain)
      data <- addNA(data, ifany = TRUE)
      levels(data)[is.na(levels(data))] <- "<NA>"
      data <- table(data)
      barplot(data, main = param.name, xlab = xlab, ylab = ylab.cat, col = col)
    } else if (type %in% c("i", "r")) {
      data <- data[!is.na(data)]
      if (length(data) == 0) {
        cat("All values are NA for: ", param.name, ", skipping plot\n")
        next
      } else {
        hist(data, xlim = domain, prob = TRUE,
             main = param.name, xlab = xlab, ylab = ylab.num, col = col)
        if (length(data) > 1) {
          lines(density(data), col = "blue", lwd = 2)
        } else {
          abline(v = data[1], col = "blue", lwd = 2)
        }
      }
    }
    nplot <- nplot + 1
  }
}
#' Return the elite configurations of the final iteration.
#'
#' 
#' @param iraceResults Object created by \pkg{irace} and saved in \code{scenario$logFile}.
#' @param logFile Log file created by \pkg{irace}, this file must contain the 
#' \code{iraceResults} object.
#' @param n Number of elite configurations to return, if \code{n} is larger than the 
#' number of configurations, then only the existing ones are returned. The default (\code{n=0}) returns all of them.
#' @param drop.metadata Remove metadata, such the configuration ID and
#' the ID of the parent, from the returned configurations. See
#' \code{\link{removeConfigurationsMetaData}}.
#' 
#' @return A data frame containing the elite configurations required.
#'
#' @examples
#' log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
#' print(removeConfigurationsMetaData(getFinalElites(logFile=log_file, n=1)))
#' 
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
getFinalElites <- function(iraceResults = NULL, logFile = NULL, n = 0,
                           drop.metadata = FALSE)
{
  if (is.null(iraceResults)) {
    if (is.null(logFile))
      stop("You must supply either 'iraceResults' or 'logFile' argument.\n")
    else
      load(logFile)
  }
  
  last.elites <- iraceResults$allElites[[length(iraceResults$allElites)]]
  
  if (n == 0)
    n <- length(last.elites) 
    
  if (length(last.elites) < n) {
    cat("Only", length(last.elites), "configurations available, reducing n,\n")
    n <- length(last.elites)
  }
  last.elites <- last.elites[1:n]
  
  configurations <- subset(iraceResults$allConfigurations,
                           get(".ID.") %in% as.character(last.elites),
                           drop = FALSE)
  if (drop.metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  return(configurations)
}

#' Returns the configurations selected by ID.
#' 
#' @param iraceResults Object created by \pkg{irace} and saved in \code{scenario$logFile}.
#' @param logFile Log file created by \pkg{irace}, this file must contain the 
#' \code{iraceResults} object.
#' @param ids The id or a vector of ids of the candidates configurations to obtain.
#' @param drop.metadata Remove metadata, such the configuration ID and
#' the ID of the parent, from the returned configurations. See
#' \code{\link{removeConfigurationsMetaData}}.
#' 
#' @return A data frame containing the elite configurations required.
#'
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
## Get configuration(s) by the id(s).
## iraceResults: object created by irace and saved in scenario$logFile.
## iraceLog: log file created by irace, this file must contain the iraceResults object.
## ids: the id or a vector of ids of the candidates configurations to obtain.
## drop.metadata: Remove the internal identifier and parent identifier from the returned 
##    configurations data frame.
## * iraceResults or iraceLog must be provided, in case both are give iraceResults will be used.
## This function returns a data frame containing the selected candidate configurations 
getConfigurationById <- function(iraceResults = NULL, logFile = NULL,
                                 ids, drop.metadata = FALSE)
{
  if (is.null(iraceResults)) {
    if (is.null(logFile))
      stop("You must supply either iraceResults or iraceLog argument.\n")
    else
      load(logFile)
  }
  
  if (length(ids) < 1) stop("You must provide at least one configuration id.\n")
  
  selection <- iraceResults$allConfigurations[,".ID."] %in% ids
  
  if (length(selection) < 1) stop("No configuration found with id", ids,".\n")
  
  configurations <-iraceResults$allConfigurations[selection, , drop = FALSE]
  
  if (drop.metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  return(configurations)
}

#' Returns the configurations by the iteration in which they were executed.
#'
#' @param iraceResults (\code{NULL}) Object created by \pkg{irace} and saved in \code{scenario$logFile}.
#' @param logFile (\code{NULL}) Log file created by \pkg{irace}, this file must contain the 
#' \code{iraceResults} object.
#' @param iterations The iteration number or a vector of iteration numbers from where 
#'  the configurations should be obtained.
#' @param drop.metadata (\code{FALSE}) Remove metadata, such the configuration ID and
#' the ID of the parent, from the returned configurations. See
#' \code{\link{removeConfigurationsMetaData}}.
#' 
#' @return A data frame containing the elite configurations required.
#'
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
## Get configuration(s) by the iteration in which they were executed.
## iraceResults: object created by irace and saved in scenario$logFile.
## iraceLog: log file created by irace, this file must contain the iraceResults object.
## iterations: the iteration or a vector of iterations from where the configurations should be obtained.
## drop.metadata: Remove the internal identifier and parent identifier from the returned 
##    configurations data frame.
## * iraceResults or iraceLog must be provided, in case both are give iraceResults will be used.
## This function returns a data frame containing the selected candidate configurations 
getConfigurationByIteration <- function(iraceResults = NULL, logFile = NULL,
                                        iterations, drop.metadata = FALSE)
{
  if (is.null(iraceResults)) {
    if (is.null(logFile))
      stop("You must supply either iraceResults or iraceLog argument.\n")
    else
      load(logFile)
  }
  
  if (length(iterations) < 1)
    stop("You must provide at least one configuration id.\n")

  # To silence warning.
  iteration <- NULL
  ids <- unique(subset(as.data.frame(iraceResults$experimentLog),
                       iteration %in% iterations,
                       select=c("configuration"), drop=TRUE))
  
  selection <- iraceResults$allConfigurations[,".ID."] %in% ids
  
  if (length(selection) < 1) stop("No configuration found with id", ids,".\n")
  
  configurations <- iraceResults$allConfigurations[selection, , drop=FALSE]
  
  if (drop.metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  return(configurations)
}

