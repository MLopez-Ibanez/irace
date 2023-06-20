#' Return the elite configurations of the final iteration.
#' 
#' @template arg_iraceresults
#' @param n Number of elite configurations to return, if \code{n} is larger than the 
#' number of configurations, then only the existing ones are returned. The default (\code{n=0}) returns all of them.
#' @template arg_drop_metadata
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
  
  if (n == 0L)
    n <- length(last.elites) 
    
  if (length(last.elites) < n) {
    cat("Only", length(last.elites), "configurations available, reducing n,")
    n <- length(last.elites)
  }
  last.elites <- last.elites[seq_len(n)]
  
  configurations <- subset(iraceResults$allConfigurations,
                           get(".ID.") %in% as.character(last.elites),
                           drop = FALSE)
  if (drop.metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  configurations
}

#' Returns the configurations selected by ID.
#' 
#' @template arg_iraceresults
#' @param ids (`integer()`)\cr The id or a vector of ids of the candidates configurations to obtain.
#' @template arg_drop_metadata
#' 
#' @return A data frame containing the elite configurations required.
#' @examples
#' log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
#' getConfigurationById(log_file, ids = c(1,2), drop.metadata = TRUE)
#'
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
getConfigurationById <- function(iraceResults, ids, drop.metadata = FALSE)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)
    
  if (length(ids) < 1L) stop("You must provide at least one configuration id.")

  get_configuration_by_id_helper (iraceResults$allConfigurations, ids, drop.metadata = drop.metadata)
}

#' Returns the configurations by the iteration in which they were executed.
#'
#' @template arg_iraceresults
#' @param iterations (`integer()`)\cr The iteration number or a vector of iteration numbers from where 
#'  the configurations should be obtained. Negative values start counting from the last iteration.
#' @template arg_drop_metadata
#' 
#' @return A data frame containing the elite configurations required.
#'
#' @examples
#' log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
#' getConfigurationByIteration(log_file, iterations = c(-2, -1), drop.metadata = TRUE)
#' 
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @concept analysis
#' @export
getConfigurationByIteration <- function(iraceResults, iterations, drop.metadata = FALSE)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)

  if (length(iterations) < 1L)
    stop("You must provide at least one iteration number.")

  n_iterations <- length(iraceResults$iterationElites)
  iterations <- as.integer(iterations)
  iterations <- ifelse(iterations >= 0L, iterations, n_iterations + 1L + iterations)
  # To silence warning.
  iteration <- NULL
  ids <- unique(subset(as.data.frame(iraceResults$experimentLog),
                       iteration %in% iterations,
                       select="configuration", drop=TRUE))
  get_configuration_by_id_helper(iraceResults$allConfigurations, ids, drop.metadata = drop.metadata)
}


get_configuration_by_id_helper <- function(allConfigurations, ids, drop.metadata)
{  
  configurations <- allConfigurations[allConfigurations[[".ID."]] %in% ids, , drop=FALSE]
  if (nrow(configurations) == 0L)
    stop("No configuration found with ID:", ids, ".")
  
  if (drop.metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  configurations
}

#' Returns the pairs of instance indexes and seeds used as instances in the race.
#'
#' @template arg_iraceresults
#' 
#' @return A data frame containing two columns `"instance"` and `"seed"`.
#'
#' @examples
#' log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
#' head(get_instance_seed_pairs(log_file))
#'
#' @author Manuel López-Ibáñez
#' @concept analysis
#' @export
get_instance_seed_pairs <- function(iraceResults)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)
  iraceResults$state$.irace$instancesList
}
