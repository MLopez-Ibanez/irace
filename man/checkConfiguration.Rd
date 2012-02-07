\name{checkConfiguration}
\alias{checkConfiguration}
\title{checkConfiguration}
\description{
  'checkConfiguration' checks that the configuration is valid for
  \pkg{irace}.
}
\usage{
  checkConfiguration(configuration)
}
\arguments{
  \item{configuration}{A list indexed by the \pkg{irace} parameter names,
  containing their values. This list can be provided by
  'readConfiguration'.}
}
\value{
  The configuration received as a parameter, checked and possibly
  corrected.
}
\details{
  This function checks that the directories and the file names provided
  and required by the \pkg{irace} exist (execution directory, run
  program hook, evaluate hook, instance file, candidate file, parameter
  file). It also checks that the parameters are of the proper type,
  e.g. that parameters expected to be integers are really
  integers. Finally it also checks that there is no inconsistency
  between parameter values.  If an error is found that prevents the
  \pkg{irace} from running properly, it will stop and display a message
  explaining this error.
}
\seealso{
  \code{\link{readConfiguration}} for reading \pkg{irace} configuration.
  \code{\link{printConfiguration}} for printing \pkg{irace} configuration.
  \code{\link{defaultConfiguration}} to set the configuration to the default.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
