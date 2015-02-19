\name{hook.evaluate.default}
\title{hook.evaluate.default}
\alias{hook.evaluate.default}

\description{
  \code{hook.evaluate.default} is the default \code{hookEvaluate} function that is
  invoked if \code{hookEvaluate} is a string (by default
  \code{hookEvaluate} is \code{NULL} and this function is not invoked). You can use it as
  an advanced example of how to create your own \code{hookEvaluate} function.
}
\usage{
hook.evaluate.default(experiment, num.candidates, config, hook.run.call)
}
\arguments{
  \item{experiment}{A list describing the experiment. It should contain
  at least:
\itemize{
  \item{instance}{A string containing the name of the instance (or
    filename and full path in case the instance is a file).}
  \item{id}{A numeric identifier for the candidate that is
    evaluated. This must match the one passed earlier to \code{hook.run}.}
}}
  \item{num.candidates}{The total number of candidates evaluated in this
    iteration.}
  \item{config}{options passed when invoking \pkg{irace}.}
  \item{hook.run.call}{a string describing the call to hookRun that
    corresponds to this call to hookEvaluate. This is used only for
    providing extra information to the user, for example, in case
    hookEvaluate fails.}
}
\value{
  This function returns the
  output of evaluating the candidate, which must be a numerical value.
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
