\name{defaultConfiguration}
\alias{defaultConfiguration}
\title{defaultConfiguration}
\description{
  'defaultConfiguration' returns the default param configuration of
  \pkg{irace}.
}
\usage{
 defaultConfiguration(configuration = list())
}
\arguments{
   \item{configuration}{A list where tagged elements correspond to configuration
    parameters of \pkg{irace}.}
}
%
\value{
  A list indexed by the \pkg{irace} parameter names,
  containing the default values for each parameter, except for those
  already present in the configuration passed as argument.
%% FIXME: This is the proper place to document each configuration
%% parameter! Perhaps we can define it in a txt file and fill this file
%% and main.R using "perl/sed"
}
\seealso{
  \code{\link{readConfiguration}} for reading the \pkg{irace}
  configuration from a file.
  \code{\link{printConfiguration}} for printing the \pkg{irace} configuration.
  \code{\link{checkConfiguration}} to check that a configuration is valid.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
