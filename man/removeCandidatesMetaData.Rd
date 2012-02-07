\name{removeCandidatesMetaData}
\alias{removeCandidatesMetaData}
\title{removeCandidatesMetaData}
\description{
  Remove the columns with "metadata" of a matrix containing some
  candidate configurations. These "metadata" are used internaly
  by \pkg{irace}. This function can be used e.g. before printing
  the candidates, to output only the values for the parameters
  of the candidate without data possibly useless to the user.
}
\usage{
  removeCandidatesMetaData(candidates)
}
\arguments{
  \item{candidates}{A matrix containg the candidates, one per row.}
}
\value{
  The same matrix without the "metadata".}
\seealso{
  \code{\link{candidates.print.command}} to print the candidates as command lines.
  \code{\link{candidates.print}} to print the candidates as a data frame.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
