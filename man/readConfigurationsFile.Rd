\name{readConfigurationsFile}
\alias{readConfigurationsFile}
\title{readConfigurationsFile}
\description{
  'readConfigurationsFile' reads a set of target algorithms configurations from a file and puts them in \pkg{irace} format. The configurations are checked to match the parameters description provided.
}
\usage{
  readConfigurationsFile(filename, parameters, debugLevel = 0)
}
\arguments{
  \item{filename}{A filename from which the configurations should be read.}
 \item{parameters}{List of target algorithm parameters in the \pkg{irace} format.}
 \item{debugLevel}{Level of debug. Default: 0.}
}
\value{
  A data frame containing the obtained configurations. 
  Each row of the data frame is a candidate configuration, 
  the columns correspond to the parameter names in \code{parameters}.
}
\seealso{
  \code{\link{readParameters}} to obtain a valid parameter structure from a parameters list.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
