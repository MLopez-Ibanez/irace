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
  'readConfiguration', or 'defaultConfiguration'.}
}
\seealso{
  'readConfiguration' for reading \pkg{irace} configuration.
  'checkConfiguration' to check that the configuration is valid.
  'defaultConfiguration' to set the configuration to the default.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
