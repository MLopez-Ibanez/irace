\name{irace.main}
\alias{irace.main}
\title{irace.main}
\description{
  \code{irace.main} is a higher-level interface to invoke \code{\link{irace}}.
}
\usage{
  irace.main(tunerConfig = defaultConfiguration(), output.width = 9999)
}
\arguments{
  \item{tunerConfig}{The configuration of \pkg{irace}.}
  \item{output.width}{The width that must be used for the screen
  output.}
}
\details{
  The function \code{irace.main} checks the correctness of the
  configuration, prints it, reads the parameter space from
  \code{tunerConfig$parameterFile}, invokes \code{\link{irace}} and
  prints its results in various formatted ways. If you want a
  lower-level interface, please see function \code{\link{irace}}.
}

\seealso{
  \code{\link{irace.cmdline}} a higher-level command-line interface to
  \code{irace.main}.
  
  \code{\link{readConfiguration}} to read the configuration of \pkg{irace} from
  a file.
  
  \code{\link{defaultConfiguration}} to provide a default configuration for \pkg{irace}.
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
