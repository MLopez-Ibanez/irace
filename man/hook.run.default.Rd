\name{hook.run.default}
\title{hook.run.default}
\alias{hook.run.default}

\description{
  \code{hook.run.default} is the default hookRun function. You can use it as
  an advanced example of how to create your own hooRun function.
}
\usage{
hook.run.default(experiment, config)
}
\arguments{
    \item{experiment}{A list describing the experiment. It should contain
  at least:
\itemize{
  \item{instance}{A string containing the name of the instance (or
    filename and full path in case the instance is a file).}
  \item{id}{A numeric identifier for the candidate that is
    evaluated.}
  \item{candidate}{The candidate configuration that must be run.}
  \item{seed}{A random seed}
  \item{extra.params}{Extra parameters (like instance-specific ones) to
    be passed when evaluating this candidate.}
  \item{switches}{Command-line switches that correspond to each parameter}
}}
  \item{config}{options passed when invoking \pkg{irace}.}
}
\value{
  If \code{hookEvaluate} is \code{NULL}, then this function returns the
  output of evaluating the candidate, which must be a numerical value.

  Otherwise, it returns a string. By the default, this string is the
  actual command-line call to the hook-run program. This information is
  only used for debugging purposes if \code{hookEvaluate} fails later.
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
