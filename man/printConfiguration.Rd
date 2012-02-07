\name{printConfiguration}
\alias{printConfiguration}
\title{printConfiguration}
\description{
  'printConfiguration' print the internal configuration of 
  \pkg{irace}.
}
\usage{
  printConfiguration(tunerConfig)
}
\arguments{
  \item{tunerConfig}{A list indexed by the \pkg{irace} parameter names,
  containing their values. This list can be provided by
  \code{\link{readConfiguration}}, or \code{\link{defaultConfiguration}}.}
}
\seealso{
  \code{\link{readConfiguration}} for reading \pkg{irace} configuration.
  \code{\link{checkConfiguration}} to check that the configuration is valid.
  \code{\link{defaultConfiguration}} to set the configuration to the default.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
