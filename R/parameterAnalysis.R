#' Return the elite configurations of the final iteration.
#' 
#' @inheritParams has_testing_data
#' @param n Number of elite configurations to return, if \code{n} is larger than the 
#' number of configurations, then only the existing ones are returned. The default (\code{n=0}) returns all of them.
#' @param drop.metadata `logical(1)`\cr Remove metadata, such as the
#'   configuration ID and the ID of the parent, from the returned
#'   configurations.  See [removeConfigurationsMetaData()].
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
#' @param ids (`integer()`)\cr The id or a vector of ids of the candidates configurations to obtain.
#' @inheritParams getFinalElites
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

  get_configuration_by_id_helper(iraceResults$allConfigurations, ids, drop_metadata = drop.metadata)
}

#' Returns the configurations by the iteration in which they were executed.
#'
#' @param iterations (`integer()`)\cr The iteration number or a vector of iteration numbers from where 
#'  the configurations should be obtained. Negative values start counting from the last iteration.
#' @inheritParams getFinalElites
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
  if (is.null(iraceResults$state$experiment_log) ||
        nrow(iraceResults$state$experiment_log) == 0L)
    stop("'iraceResults' does not contain experiment_log, maybe the wrong file, an incomplete run or the wrong version of irace?")
    
  ids <- unique(subset(as.data.frame(iraceResults$state$experiment_log),
                       iteration %in% iterations,
                       select="configuration", drop=TRUE))
  get_configuration_by_id_helper(iraceResults$allConfigurations, ids, drop_metadata = drop.metadata)
}


get_configuration_by_id_helper <- function(allConfigurations, ids, drop_metadata)
{  
  configurations <- allConfigurations[allConfigurations[[".ID."]] %in% ids, , drop=FALSE]
  if (nrow(configurations) == 0L)
    stop("No configuration found with ID:", ids, ".")
  
  if (drop_metadata)
    configurations <- removeConfigurationsMetaData(configurations)
  configurations
}

#' Returns the pairs of instance IDs and seeds used as instances in the race
#' (and optionally the actual instances).
#'
#' @inheritParams getFinalElites
#' @param index  (`integer()`)\cr Indexes of the (instanceID,seed) pairs to be returned. The default returns everything.
#' @param instances (`logical(1)`)\cr Whether to add the actual instances as an additional column (only if the instances are of atomic type).
#' 
#' @return `data.table()`\cr With default arguments, a `data.table` containing two columns
#'   `"instanceID"` and `"seed"`. With `instances=TRUE` and if the instances
#'   are of atomic type (see [is.atomic()]) type, another column `instance` is
#'   added that contains the actual instance.
#'
#' @examples
#' log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
#' head(get_instanceID_seed_pairs(log_file))
#' # Add the instance names
#' get_instanceID_seed_pairs(log_file, index=1:10, instances=TRUE)
#' @author Manuel López-Ibáñez
#' @concept analysis
#' @export
get_instanceID_seed_pairs <- function(iraceResults, index, instances = FALSE)
{
  if (missing(iraceResults)) stop("argument 'iraceResults' is missing")
  iraceResults <- read_logfile(iraceResults)
  instances_log <- iraceResults$state$instances_log
  if (!missing(index))
    instances_log <- instances_log[index, , drop = FALSE]
  if (!instances)
    return(instances_log)

  instances <- iraceResults$scenario$instances
  if (!is.atomic(instances)) {
    warning("instances=TRUE requested, but instances are not of atomic type")
    return(instances_log)
  }
    
  instanceID <- instances_log[["instanceID"]]
  cbind(instances_log, instance = instances[instanceID])
}
