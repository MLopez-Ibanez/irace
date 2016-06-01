\name{removeConfigurationsMetaData}
\alias{removeConfigurationsMetaData}
\title{removeConfigurationsMetaData}
\description{
  Remove the columns with "metadata" of a matrix containing some
  configuration configurations. These "metadata" are used internaly
  by \pkg{irace}. This function can be used e.g. before printing
  the configurations, to output only the values for the parameters
  of the configuration without data possibly useless to the user.
}
\usage{
  removeConfigurationsMetaData(configurations)
}
\arguments{
  \item{configurations}{A matrix containg the configurations, one per row.}
}
\value{
  The same matrix without the "metadata".}
\seealso{
  \code{\link{configurations.print.command}} to print the configurations as command lines.
  \code{\link{configurations.print}} to print the configurations as a data frame.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
