\name{getFinalElites}
\alias{getFinalElites}
\title{getFinalElites}
\description{
  'getFinalElites' returns the elite configurations of the final iteration.
}
\usage{
  getFinalElites(iraceResults=NULL, irace.logFile=NULL, n=0, drop.internals=FALSE)
}
\arguments{
   \item{iraceResults}{Object created by \pkg{irace} and saved in \code{scenario$logFile}.}
   \item{irace.logFile}{Log file created by \pkg{irace}, this file must contain the \code{iraceResults} object.}
   \item{n}{Number of elite configurations to return, if \code{n} is larger than the number of configurations, then only the existing ones are returned.}
   \item{drop.internals}{Remove the internal identifier and parent identifier from the returned configurations data frame.}
}
\value{A data frame containing the elite configurations required.}
\seealso{
  \code{\link{defaultScenario}} returns the default scenario settings of
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
