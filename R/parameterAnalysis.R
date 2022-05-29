#' Return the elite configurations of the final iteration.
#' 
#' @template arg_iraceresults
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
#' print(removeConfigurationsMetaData(getFinalElites(log_file, n=1)))
#' 
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
getFinalElites <- function(iraceResults, n = 0L, drop.metadata = FALSE)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)
    
  last.elites <- iraceResults$allElites[[length(iraceResults$allElites)]]
  
  if (n == 0)
    n <- length(last.elites) 
    
  if (length(last.elites) < n) {
    cat("Only", length(last.elites), "configurations available, reducing n,")
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
#' @template arg_iraceresults
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
getConfigurationById <- function(iraceResults, ids, drop.metadata = FALSE)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)
    
  if (length(ids) < 1) stop("You must provide at least one configuration id.")
  
  selection <- iraceResults$allConfigurations[,".ID."] %in% ids
  
  if (length(selection) < 1) stop("No configuration found with id", ids,".")
  
  configurations <-iraceResults$allConfigurations[selection, , drop = FALSE]
  
  if (drop.metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  return(configurations)
}

#' Returns the configurations by the iteration in which they were executed.
#'
#' @template arg_iraceresults
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
getConfigurationByIteration <- function(iraceResults,
                                        iterations, drop.metadata = FALSE)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)

  if (length(iterations) < 1)
    stop("You must provide at least one configuration id.")

  # To silence warning.
  iteration <- NULL
  ids <- unique(subset(as.data.frame(iraceResults$experimentLog),
                       iteration %in% iterations,
                       select=c("configuration"), drop=TRUE))
  
  selection <- iraceResults$allConfigurations[,".ID."] %in% ids
  
  if (length(selection) < 1) stop("No configuration found with id", ids,".")
  
  configurations <- iraceResults$allConfigurations[selection, , drop=FALSE]
  
  if (drop.metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  return(configurations)
}

