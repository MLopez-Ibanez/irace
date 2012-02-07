\name{defaultConfiguration}
\alias{defaultConfiguration}
\title{defaultConfiguration}
\description{
  'defaultConfiguration' set all parameters of \pkg{irace} (the
  so-called "configuration") to their default values.
}
\usage{
 defaultConfiguration(config = list())
}
\arguments{
  \item{config}{A list indexed by the \pkg{irace} parameter names,
  containing their values.}
}
\value{
  A list indexed by the \pkg{irace} parameter names,
  containing the default values for each parameter.
}
\seealso{
  \code{\link{readConfiguration}} for reading \pkg{irace} configuration.
  \code{\link{printConfiguration}} for printing \pkg{irace} configuration.
  \code{\link{checkConfiguration}} to check that a configuration is valid.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
