\name{readConfiguration}
\alias{readConfiguration}
\title{readConfiguration}
\description{
  'readConfiguration' read the configuration to be used by 
  \pkg{irace} from a file.
}
\usage{
  readConfiguration(filename = "", configuration = list())
}
\arguments{
  \item{filename}{A character chain that is the name of the file to be read.
  The function stops if this argument is not provided or if the file
  is not readable.}
  \item{configuration}{An initial configuration that can be used.
  In this case, this initial configuration is overwritten for every
  parameters specified in the file to be read.}
}
\value{
  The configuration read from the file.
}
\seealso{
  \code{\link{checkConfiguration}} to check that the configuration is valid.
  \code{\link{defaultConfiguration}} to set the configuration to the default.
  \code{\link{printConfiguration}} to print the configuration.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
