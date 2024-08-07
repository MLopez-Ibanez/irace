% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/race-wrapper.R
\name{check_output_target_runner}
\alias{check_output_target_runner}
\title{Check the output of the target runner and repair it if possible. If the
output is incorrect, this function will throw an error.}
\usage{
check_output_target_runner(output, scenario, bound = NULL)
}
\arguments{
\item{output}{The output from target runner.}

\item{scenario}{\code{list()}\cr Data structure containing \pkg{irace}
settings. The data structure has to be the one returned by the function
\code{\link[=defaultScenario]{defaultScenario()}} or \code{\link[=readScenario]{readScenario()}}.}

\item{bound}{Optional time bound that the target runner should have respected.}
}
\value{
The output with its contents repaired.
}
\description{
Check the output of the target runner and repair it if possible. If the
output is incorrect, this function will throw an error.
}
