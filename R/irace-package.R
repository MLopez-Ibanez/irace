#' The irace package: \packageTitle{irace}
#'
#' \packageDescription{irace}
#'
#' @name irace-package
#' @docType package
#' @import stats utils compiler
#' @importFrom grDevices dev.new dev.off pdf cairo_pdf rgb
#' @importFrom graphics abline axis barplot boxplot hist lines matplot mtext par plot points strwidth text bxp grid
#'  
#' 
#' @details  License: GPL (>= 2)
#' 
#' @author Maintainers: Manuel López-Ibáñez and Leslie Pérez Cáceres
#'         \email{irace-package@googlegroups.com}
#'
#' @keywords package optimize tuning automatic configuration
#'
#' @references
#'   Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Leslie Pérez Cáceres,
#'   Thomas Stützle, and Mauro Birattari. The irace package: Iterated
#'   Racing for Automatic Algorithm Configuration. \emph{Operations Research
#'   Perspectives}, 2016. \doi{10.1016/j.orp.2016.09.002}
#'    
#'   Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Thomas Stützle, and Mauro
#'   Birattari. \emph{The irace package, Iterated Race for Automatic
#'   Algorithm Configuration}. Technical Report TR/IRIDIA/2011-004, IRIDIA,
#'   Université Libre de Bruxelles, Belgium, 2011.
#'   
#'   Manuel López-Ibáñez and Thomas Stützle. The Automatic Design of
#'   Multi-Objective Ant Colony Optimization Algorithms. \emph{IEEE Transactions
#'   on Evolutionary Computation}, 2012.
#'
#' 
#' @examples
#'  #######################################################################
#'  # This example illustrates how to tune the parameters of the simulated
#'  # annealing algorithm (SANN) provided by the optim() function in the
#'  # R base package.  The goal in this example is to optimize instances of
#'  # the following family:
#'  # f(x) = lambda * f_rastrigin(x) + (1 - lambda) * f_rosenbrock(x)
#'  # where lambda follows a normal distribution whose mean is 0.9 and
#'  # standard deviation is 0.02. f_rastrigin and f_rosenbrock are the
#'  # well-known Rastrigin and Rosenbrock benchmark functions (taken from
#'  # the cmaes package). In this scenario, different instances are given
#'  # by different values of lambda.
#'  #######################################################################
#'  ## First we provide an implementation of the functions to be optimized:
#'  f_rosenbrock <- function (x) {
#'    d <- length(x)
#'    z <- x + 1
#'    hz <- z[1:(d - 1)]
#'    tz <- z[2:d]
#'    s <- sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
#'    return(s)
#'  }
#'  f_rastrigin <- function (x) {
#'    sum(x * x - 10 * cos(2 * pi * x) + 10)
#'  }
#'  
#'  ## We generate 200 instances (in this case, weights):
#'  weights <- rnorm(200, mean = 0.9, sd = 0.02)
#'  
#'  ## On this set of instances, we are interested in optimizing two
#'  ## parameters of the SANN algorithm: tmax and temp. We setup the
#'  ## parameter space as follows:
#'  parameters.table <- '
#'  tmax "" i (1, 5000)
#'  temp "" r (0, 100)
#'  '
#'  
#'  ## We use the irace function readParameters to read this table:
#'  parameters <- readParameters(text = parameters.table)
#'  
#'  ## Next, we define the function that will evaluate each candidate
#'  ## configuration on a single instance. For simplicity, we restrict to
#'  ## three-dimensional functions and we set the maximum number of
#'  ## iterations of SANN to 5000.
#'  target.runner <- function(experiment, scenario)
#'  {
#'    instance <- experiment$instance
#'    configuration <- experiment$configuration
#'  
#'    D <- 3
#'    par <- runif(D, min=-1, max=1)
#'    fn <- function(x) {
#'      weight <- instance
#'      return(weight * f_rastrigin(x) + (1 - weight) * f_rosenbrock(x))
#'    }
#'    res <- stats::optim(par,fn, method="SANN",
#'                 control=list(maxit=5000
#'                   , tmax = as.numeric(configuration[["tmax"]])
#'                   , temp = as.numeric(configuration[["temp"]])
#'                   ))
#'    ## New output interface in irace 2.0. This list may also contain:
#'    ## - 'time' if irace is called with 'maxTime'
#'    ## - 'error' is a string used to report an error
#'    ## - 'outputRaw' is a string used to report the raw output of calls to
#'    ##   an external program or function.
#'    ## - 'call' is a string used to report how target.runner called the
#'    ##   external program or function.
#'    return(list(cost = res$value))
#'  }
#'  
#'  ## We define a configuration scenario by setting targetRunner to the
#'  ## function define above, instances to the first 100 random weights, and
#'  ## a maximum budget of 1000 calls to targetRunner.
#'  scenario <- list(targetRunner = target.runner,
#'                   instances = weights[1:100],
#'                   maxExperiments = 1000,
#'                   # Do not create a logFile
#'                   logFile = "")
#'  
#'  ## We check that the scenario is valid. This will also try to execute
#'  ## target.runner.
#'  checkIraceScenario(scenario, parameters = parameters)
#'  
#'  \donttest{
#'  ## We are now ready to launch irace. We do it by means of the irace
#'  ## function. The function will print information about its
#'  ## progress. This may require a few minutes, so it is not run by default.
#'  tuned.confs <- irace(scenario = scenario, parameters = parameters)
#'  
#'  ## We can print the best configurations found by irace as follows:
#'  configurations.print(tuned.confs)
#'  
#'  ## We can evaluate the quality of the best configuration found by
#'  ## irace versus the default configuration of the SANN algorithm on
#'  ## the other 100 instances previously generated.
#'  ## To do so, first we apply the default configuration of the SANN
#'  ## algorithm to these instances:
#'  test <- function(configuration)
#'  {
#'    res <- lapply(weights[101:200],
#'                  function(x) target.runner(
#'                                experiment = list(instance = x,
#'                                                  configuration = configuration),
#'                                scenario = scenario))
#'    return (sapply(res, getElement, name = "cost"))
#'  }
#'  default <- test(data.frame(tmax=10, temp=10))

#'  ## We extract and apply the winning configuration found by irace
#'  ## to these instances:
#'  tuned <- test (removeConfigurationsMetaData(tuned.confs[1,]))
#'  
#'  ## Finally, we can compare using a boxplot the quality obtained with the
#'  ## default parametrization of SANN and the quality obtained with the
#'  ## best configuration found by irace.
#'  boxplot(list(default = default, tuned = tuned))
#' }
#'
#' @seealso
#'  \code{\link{irace.main}} to start \pkg{irace} with a given scenario.
#' 
NULL

