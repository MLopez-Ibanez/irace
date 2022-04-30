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
