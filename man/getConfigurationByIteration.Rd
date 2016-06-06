\name{getConfigurationByIteration}
\alias{getConfigurationByIteration}
\title{getConfigurationByIteration}
\description{
  'getConfigurationByIteration' returns the configurations by the iteration in which they were executed.
}
\usage{
getConfigurationByIteration(iraceResults = NULL, irace.logFile = NULL,
                            iterations, drop.internals = FALSE)
}
\arguments{
   \item{iraceResults}{Object created by \pkg{irace} and saved in \code{scenario$logFile}.}
   \item{irace.logFile}{Log file created by \pkg{irace}, this file must contain the \code{iraceResults} object.}
   \item{iterations}{The iteration number or a vector of iterations numbers from where the configurations should be obtained.}
   \item{drop.internals}{Remove the internal identifier and parent identifier from the returned configurations data frame.}
}
\value{A data frame containing the elite configurations required.}
\seealso{
  \code{\link{defaultScenario}} returns the default scenario settings of
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
