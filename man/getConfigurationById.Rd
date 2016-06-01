\name{getConfigurationById}
\alias{getConfigurationById}
\title{getConfigurationById}
\description{
  'getConfigurationById' returns the configurations selected by id.
}
\usage{
  getConfigurationById(iraceResults=NULL, irace.logFile=NULL, ids, drop.internals=FALSE)
}
\arguments{
   \item{iraceResults}{Object created by \pkg{irace} and saved in \code{scenario$logFile}.}
   \item{irace.logFile}{Log file created by \pkg{irace}, this file must contain the \code{iraceResults} object.}
   \item{ids}{The id or a vector of ids of the candidates configurations to obtain.}
   \item{drop.internals}{Remove the internal identifier and parent identifier from the returned configurations data frame.}
}
\value{A data frame containing the elite configurations required.}
\seealso{
  \code{\link{defaultScenario}} returns the default scenario settings of
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
