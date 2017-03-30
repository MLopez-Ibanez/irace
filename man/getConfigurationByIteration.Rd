\name{getConfigurationByIteration}
\alias{getConfigurationByIteration}
\title{getConfigurationByIteration}
\description{
  'getConfigurationByIteration' returns the configurations by the iteration in which they were executed.
}
\usage{
getConfigurationByIteration(iraceResults = NULL, logFile = NULL,
                            iterations, drop.metadata = FALSE)
}
\arguments{
   \item{iraceResults}{Object created by \pkg{irace} and saved in \code{scenario$logFile}.}
   \item{logFile}{Log file created by \pkg{irace}, this file must contain the \code{iraceResults} object.}
   \item{iterations}{The iteration number or a vector of iteration numbers from where the configurations should be obtained.}
   \item{drop.metadata}{Remove metadata, such the configuration ID and
     the ID of the parent, from the returned configurations. See \code{removeConfigurationsMetaData}.}
}
\value{A data frame containing the elite configurations required.}

