\name{readConfiguration}
\alias{readConfiguration}
\title{readConfiguration}
\description{
  'readConfiguration' reads the configuration to be used by 
  \pkg{irace} from a file.
}
\usage{
  readConfiguration(filename = "", configuration = list())
}
\arguments{
  \item{filename}{A filename from which the configuration will be
    read. If empty, the default \code{configurationFile} is used.
    An example configuration file is provided in
    \code{system.file(package="irace", "templates/tune-conf.tmpl")}.}
 \item{configuration}{A list where tagged elements correspond to configuration
    parameters of \pkg{irace}. This is an initial configuration that is
    overwritten for every parameter specified in the file to be read.}
}
\value{
  The configuration read from the file. Anything not mentioned in the
  file is not present in the list, that is, it is \code{NULL}.
}
\seealso{
  \code{\link{checkConfiguration}} to check that the configuration is valid.
  \code{\link{defaultConfiguration}} to set the configuration to the default.
  \code{\link{printConfiguration}} to print the configuration.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
