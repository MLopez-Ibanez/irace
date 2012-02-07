\name{candidates.print}
\alias{candidates.print}
\title{candidates.print}
\description{
  Print candidate configurations (hereafter "candidates").
}
\usage{
  candidates.print(cand, metadata = FALSE)
}
\arguments{
  \item{cand}{A matrix containing the candidates (one per row).}
  \item{metadata}{A Boolean specifying whether to print the metadata or
  not. The metadata are data for the candidates (additionally to the
  value of each parameter) used by \pkg{irace}.}
}
\value{
  None.
}
\seealso{
  \code{\link{candidates.print.command}} to print the candidates as command lines.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
