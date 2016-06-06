\name{target.evaluator.default}
\title{target.evaluator.default}
\alias{target.evaluator.default}

\description{
  \code{target.evaluator.default} is the default \code{targetEvaluator} function that is
  invoked if \code{targetEvaluator} is a string (by default
  \code{targetEvaluator} is \code{NULL} and this function is not invoked). You can use it as
  an advanced example of how to create your own \code{targetEvaluator} function.
}
\usage{
target.evaluator.default(experiment, num.configurations, all.conf.id,
                         scenario, target.runner.call)
}
\arguments{
  \item{experiment}{A list describing the experiment. It should contain
  at least:
\itemize{
  \item{instance}{A string containing the name of the instance (or
    filename and full path in case the instance is a file).}
  \item{id}{A numeric identifier for the candidate configuration that is
    evaluated. This must match the one passed earlier to \code{target.runner}.}
}}
  \item{num.configurations}{The total number of candidate configurations evaluated in this
    iteration.}
  \item{all.conf.id}{List of configuration ids that have been evaluated in the instance This is used, for example, when calculating the bounds for the hyper volume evaluation.}
  \item{scenario}{options passed when invoking \pkg{irace}.}
  \item{target.runner.call}{a string describing the call to targetRunner that
    corresponds to this call to targetEvaluator. This is used only for
    providing extra information to the user, for example, in case
    targetEvaluator fails.}
}
\value{
  This function returns the
  output of evaluating the candidate configuration, which must be a numerical value.
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
