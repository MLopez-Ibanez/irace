\name{hook.evaluate.default}
\title{hook.evaluate.default}
\alias{hook.evaluate.default}

\description{
  \code{hook.evaluate.default} is the default \code{hookEvaluate} function that is
  invoked if \code{hookEvaluate} is a string (by default
  \code{hookEvaluate} is \code{NULL} and this function is not invoked). You can use it as
  an advanced example of how to create your own hooEvaluate function.
}
\usage{
hook.evaluate.default(instance, candidate, num.candidates, extra.params,
config, hook.run.command)
}
\arguments{
  \item{instance}{A string containing the name of the instance (or
    filename and full path in case the instance is a file).}
  \item{candidate}{The candidate configuration that must be run.}
  \item{num.candidates}{The total number of candidates evaluated in this
    iteration.}
  \item{extra.params}{Extra parameters (like instance-specific ones) to
    be passed when evaluating this candidate.}
  \item{config}{options passed when invoking \pkg{irace}.}
  \item{hook.run.command}{a string describing the call to hook-run that
    corresponds to this call to hook-evaluate. This is used only for
    providing extra information to the user, for example, in case
    hook-evaluate fails.}

}
\value{
  This function returns the
  output of evaluating the candidate, which must be a numerical value.
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
