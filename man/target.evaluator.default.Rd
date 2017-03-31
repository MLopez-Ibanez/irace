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
  \item{experiment}{A list describing the experiment. It contains at least:
    \itemize{
      \item{id.configuration}{An alphanumeric string that uniquely identifies a configuration;}
      \item{id.instance}{An alphanumeric string that uniquely identifies a pair (instance, seed);}
      \item{seed}{Seed for the random number generator to be used for this
        evaluation, ignore the seed for deterministic algorithms;}
      \item{instance}{String giving the instance to be used for this evaluation;}
      \item{configuration}{1-row data frame with a column per parameter
        name;}
      \item{switches}{Vector of parameter switches (labels) in the order of
        parameters used in \code{configuration}.}
    }
  }
  \item{num.configurations}{is the number of  configurations alive in the race;}
  \item{all.conf.id}{is the vector of configuration IDs of the alive configurations;}
  \item{scenario}{options passed when invoking \pkg{irace};}
  \item{target.runner.call}{a string describing the call to \code{targetRunner} that
    corresponds to this call to \code{targetEvaluator}. This is used for
    providing extra information to the user, for example, in case
    \code{targetEvaluator} fails.}
}
\value{
  The function \code{targetEvaluator} must return a list with one element
  \code{"cost"}, the numerical value corresponding to the cost measure of the
  given configuration on the given instance.
  
  The return list may also contain the following optional elements that are used
  by \pkg{irace} for reporting errors in \code{targetEvaluator}:
  \itemize{
    \item{error}{is a string used to report an error;}
    \item{outputRaw}{is a string used to report the raw output of calls to
      an external program or function;}
    \item{call}{is a string used to report how \code{targetEvaluator} called 
      an external program or function.}
  }
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
