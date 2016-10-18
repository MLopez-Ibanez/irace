\name{irace-package}
\alias{irace-package}
\docType{package}
\title{
The irace package
}
\description{
Iterated racing for Automatic Algorithm Configuration
}
\details{
\tabular{ll}{
Package: \tab irace\cr
Type: \tab Package\cr
Version: \tab 2.1 \cr
Date: \tab 2016-10-18 \cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Maintainer: Manuel López-Ibáñez and Jérémie Dubois-Lacoste
\email{irace@iridia.ulb.ac.be}


Author: Manuel López-Ibáñez, Jérémie Dubois-Lacoste,
Thomas Stützle, Mauro Birattari, Eric Yuan and Prasanna Balaprakash
}
\references{

  Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Thomas Stützle, and Mauro
  Birattari. \emph{The irace package, Iterated Race for Automatic
  Algorithm Configuration}. Technical Report TR/IRIDIA/2011-004, IRIDIA,
  Université Libre de Bruxelles, Belgium, 2011.
  
  Manuel López-Ibáñez and Thomas Stützle. The Automatic Design of
  Multi-Objective Ant Colony Optimization Algorithms. IEEE Transactions
  on Evolutionary Computation, 2012.

}
\keyword{package}
\keyword{optimize}
\keyword{tuning}
\keyword{automatic configuration}

\examples{
#######################################################################
# This example illustrates how to tune the parameters of the simulated
# annealing algorithm (SANN) provided by the optim() function in the
# R base package.  The goal in this example is to optimize instances of
# the following family:
# f(x) = lambda * f_rastrigin(x) + (1 - lambda) * f_rosenbrock(x)
# where lambda follows a normal distribution whose mean is 0.9 and
# standard deviation is 0.02. f_rastrigin and f_rosenbrock are the
# well-known Rastrigin and Rosenbrock benchmark functions (taken from
# the cmaes package). In this scenario, different instances are given
# by different values of lambda.
#######################################################################
## First we provide an implementation of the functions to be optimized:
f_rosenbrock <- function (x) {
  d <- length(x)
  z <- x + 1
  hz <- z[1:(d - 1)]
  tz <- z[2:d]
  s <- sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
  return(s)
}
f_rastrigin <- function (x) {
  sum(x * x - 10 * cos(2 * pi * x) + 10)
}

## We generate 200 instances (in this case, weights):
weights <- rnorm(200, mean = 0.9, sd = 0.02)

## On this set of instances, we are interested in optimizing two
## parameters of the SANN algorithm: tmax and temp. We setup the
## parameter space as follows:
parameters.table <- '
tmax "" i (1, 5000)
temp "" r (0, 100)
'

## We use the irace function readParameters to read this table:
parameters <- readParameters(text = parameters.table)

## Next, we define the function that will evaluate each candidate
## configuration on a single instance. For simplicity, we restrict to
## three-dimensional functions and we set the maximum number of
## iterations of SANN to 5000.
target.runner <- function(experiment, scenario)
{
  instance <- experiment$instance
  configuration <- experiment$configuration

  D <- 3
  par <- runif(D, min=-1, max=1)
  fn <- function(x) {
    weight <- instance
    return(weight * f_rastrigin(x) + (1 - weight) * f_rosenbrock(x))
  }
  res <- optim(par,fn, method="SANN",
               control=list(maxit=5000
                 , tmax = as.numeric(configuration[["tmax"]])
                 , temp = as.numeric(configuration[["temp"]])
                 ))
  return(res$value)
}

\dontrun{
## We are now ready to launch irace. We do it by means of the irace
## function by setting targetRunner to the function define above, instances to
## the first 100 random weights, and a maximum budget of 1000 calls to
## targetRunner. The function irace will print information about its
## progress. This may require a few minutes, so it is not run by default.
result <- irace(scenario = list(
                  targetRunner = target.runner,
                  instances = weights[1:100],
                  maxExperiments = 1000,
                  logFile = ""),
                parameters = parameters)

## We can print the best configurations found by irace as follows:
configurations.print(result)

## We can evaluate the quality of the best configuration found by
## irace versus the default configuration of the SANN algorithm on
## the other 100 instances previously generated.
## To do so, first we apply the default configuration of the SANN
## algorithm to these instances:
default <- sapply(weights[101:200], target.runner,
                  configuration=list(values=list(tmax=10,temp=10)))

## We extract and apply the winning configuration found by irace
## to these instances:
result.list <- as.list(removeConfigurationsMetaData(result[1,]))
tuned <- sapply(weights[101:200], target.runner, configuration=list(values=result.list))

## Finally, we can compare using a boxplot the quality obtained with the
## default parametrization of SANN and the quality obtained with the
## best configuration found by irace.
boxplot(list(default=default, tuned=tuned))
}
}
