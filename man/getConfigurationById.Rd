\name{getConfigurationById}
\alias{getConfigurationById}
\title{getConfigurationById}
\description{
  'getConfigurationById' returns the configurations selected by id.
}
\usage{
  getConfigurationById(iraceResults=NULL, logFile=NULL, ids, drop.metadata=FALSE)
}
\arguments{
   \item{iraceResults}{Object created by \pkg{irace} and saved in \code{scenario$logFile}.}
   \item{logFile}{Log file created by \pkg{irace}, this file must contain the \code{iraceResults} object.}
   \item{ids}{The id or a vector of ids of the candidates configurations to obtain.}
   \item{drop.metadata}{Remove metadata, such the configuration ID and
     the ID of the parent, from the returned configurations. See
     \code{removeConfigurationsMetaData}.}
}
\value{A data frame containing the elite configurations required.}


