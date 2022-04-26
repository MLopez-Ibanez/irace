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
  if (is.null(iraceResults) && is.null(logFile)) {
    stop("You must supply either 'iraceResults' or 'logFile' argument.")
  }
  if (is.null(iraceResults)) iraceResults <- read_logfile(logFile)
    
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
#' @param iraceResults Object created by `irace` and saved in `scenario$logFile`.
#' @param logFile Log file created by `irace`, this file must contain the 
#' `iraceResults` object.
#' @param ids The id or a vector of ids of the candidates configurations to obtain.
#' @param drop.metadata Remove metadata, such the configuration ID and
#' the ID of the parent, from the returned configurations. See
#' [removeConfigurationsMetaData()].
#' 
#' @return A data frame containing the elite configurations required.
#'
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
#' @md
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

