\name{irace.main}
\alias{irace.main}
\title{irace.main}
\description{
  'irace.main' starts \pkg{irace} using the configuration
  given as argument.
}
\usage{
  irace.main(tunerConfig = defaultConfiguration(), output.width = 9999)
}
\arguments{
  \item{tunerConfig}{The configuration (as provided by
  'readConfiguration'). If not provided, the configuration provided
  by 'defaultConfiguration' is used instead.}
  \item{output.width}{The width that must be used for the screen
  output.}
}
\seealso{
  \code{\link{irace.cmdline}} to read the command line before calling 'irace.main'.
  \code{\link{readConfiguration}} to read the configuration for \pkg{irace} from
  a file.
  \code{\link{defaultConfiguration}} to provide a default configuration for \pkg{irace}.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
