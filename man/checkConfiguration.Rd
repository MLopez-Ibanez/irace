\name{checkConfiguration}
\alias{checkConfiguration}
\title{checkConfiguration}
\description{
  \code{checkConfiguration} takes a (possibly incomplete) configuration
  of \pkg{irace}, checks for errors and transforms it into a valid
  configuration.
}
\usage{
  checkConfiguration(configuration = defaultConfiguration())
}
\arguments{
  \item{configuration}{A list where tagged elements correspond to configuration
    parameters of \pkg{irace}.}
}
\value{
  The configuration received as a parameter, possibly corrected. Unset
  configuration settings are set to their default values.
}
\details{
  This function checks that the directories and the file names provided
  and required by the \pkg{irace} exist.
  It also checks that the parameters are of the proper type,
  e.g. that parameters expected to be integers are really
  integers. Finally, it also checks that there is no inconsistency
  between parameter values.  If an error is found that prevents the
  \pkg{irace} from running properly, it will stop with an error.
}
\seealso{

  \code{\link{readConfiguration}} for reading the \pkg{irace}
  configuration from a file.
  \code{\link{printConfiguration}} for printing the \pkg{irace} configuration.
  \code{\link{defaultConfiguration}} to get the default configuration.
  
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
