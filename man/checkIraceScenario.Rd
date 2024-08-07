% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{checkIraceScenario}
\alias{checkIraceScenario}
\title{Test that the given irace scenario can be run.}
\usage{
checkIraceScenario(scenario)
}
\arguments{
\item{scenario}{\code{list()}\cr Data structure containing \pkg{irace}
settings. The data structure has to be the one returned by the function
\code{\link[=defaultScenario]{defaultScenario()}} or \code{\link[=readScenario]{readScenario()}}.}
}
\value{
returns \code{TRUE} if successful and gives an error and returns \code{FALSE}
otherwise.
}
\description{
Test that the given irace scenario can be run by checking the scenario
settings provided and trying to run the target-algorithm.
}
\details{
If the \code{parameters} argument is missing, then the parameters
will be read from the file \code{parameterFile}  given by \code{scenario}. If
\code{parameters} is provided, then \code{parameterFile} will not be read.  This function will
try to execute the target-algorithm.
}
\seealso{
\describe{
\item{\code{\link{readScenario}}}{for reading a configuration scenario from a file.}
\item{\code{\link{printScenario}}}{prints the given scenario.}
\item{\code{\link{defaultScenario}}}{returns the default scenario settings of \pkg{irace}.}
\item{\code{\link{checkScenario}}}{to check that the scenario is valid.}
}
}
\author{
Manuel López-Ibáñez and Jérémie Dubois-Lacoste
}
