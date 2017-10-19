\name{configurations.print}
\alias{configurations.print}
\title{configurations.print}
\description{
  Print configurations.
}
\usage{
  configurations.print(configurations, metadata = FALSE)
}
\arguments{
  \item{configurations}{a data frame containing the configurations (one per row).}
  \item{metadata}{A Boolean specifying whether to print the metadata or
  not. The metadata are data for the configurations (additionally to the
  value of each parameter) used by \pkg{irace}.}
}
\value{
  None.
}
\seealso{
  \code{\link{configurations.print.command}} to print the configurations as command lines.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
