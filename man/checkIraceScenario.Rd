\name{checkIraceScenario}
\alias{checkIraceScenario}
\title{checkIraceScenario}
\description{
  'checkIraceScenario' check the given scenario options.
}
\usage{
  checkIraceScenario(scenario, parameters=NULL)
}
\arguments{
   \item{scenario}{Data structure containing \pkg{irace} settings.The data structure
   has to be the one returned by the function \code{defaultScenario()} and
   \code{readScenario()}. See documentation
   of this function for details.}
   \item{parameters}{Data structure containing the parameter definition. The data
   structure has to be the one returned by the function \code{readParameters()}.
   See documentation of this function for details.}
}
\details{
  Provide the \code{parameters} argument only if the parameter list should
  not be obtained from a parameter file. If the parameter list is provided 
  it will be not checked.
}
\seealso{
  \code{\link{readScenario}} for reading a configuration scenario from a
  file.
  \code{\link{defaultScenario}} returns the default scenario settings of
  \pkg{irace}.
  \code{\link{checkScenario}} to check that the scenario is valid.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
