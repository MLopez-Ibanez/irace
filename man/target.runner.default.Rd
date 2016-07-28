\name{target.runner.default}
\title{target.runner.default}
\alias{target.runner.default}

\description{
  \code{target.runner.default} is the default targetRunner function. You can use it as
  an advanced example of how to create your own targetRunner function.
}
\usage{
target.runner.default(experiment, scenario)
}
\arguments{
    \item{experiment}{A list describing the experiment. It should contain
  at least:
\itemize{
  \item{instance}{A string containing the name of the instance (or
    filename and full path in case the instance is a file).}
  \item{id}{A numeric identifier for the candidate configuration that is
    evaluated.}
  \item{configuration}{The candidate configuration that must be run.}
  \item{seed}{A random seed}
  \item{extra.params}{Extra parameters (like instance-specific ones) to
    be passed when evaluating this candidate configuration.}
  \item{switches}{Command-line switches that correspond to each parameter}
}}
  \item{scenario}{options passed when invoking \pkg{irace}.}
}
\value{
  If \code{targetEvaluator} is \code{NULL}, then this function returns the
  output of evaluating the candidate configuration, which must be a numerical value.
  
  %% MANUEL: This needs updating?
  If \code{maxTime}, then this function returns the
  output of evaluating the candidate configuration and the execution time, if 
  \code{targetEvaluator} is not \code{NULL}, this function must return only the 
  execution time.

  Otherwise, it returns a string. By the default, this string is the
  actual command-line call to the target-runner program. This information is
  only used for debugging purposes if \code{targetEvaluator} fails later.
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
