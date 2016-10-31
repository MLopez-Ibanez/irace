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
  \item{experiment}{A list describing the experiment. It contains at least:
    \itemize{
      \item{id.configuration}{An alphanumeric string that uniquely identifies a configuration;}
      \item{id.instance}{An alphanumeric string that uniquely identifies a pair (instance, seed);}
      \item{seed}{Seed for the random number generator to be used for
        this evaluation, ignore the seed for deterministic algorithms;}
      \item{instance}{String giving the instance to be used for this evaluation;}
      \item{extra.params}{User-defined parameters associated to the instance;}
      \item{configuration}{1-row data frame with a column per parameter
        name;}
      \item{switches}{Vector of parameter switches (labels) in the order
        of parameters used in \code{configuration}.}
    }
  }
  \item{scenario}{Options passed when invoking \pkg{irace}.}
}
\value{

  If \code{targetEvaluator} is \code{NULL}, then the \code{targetRunner}
  function must return a list with at least one element \code{"cost"},
  the numerical value corresponding to the evaluation of the given
  configuration on the given instance.
    
  If the scenario option \code{maxTime} is non-zero, then the list must
  contain at least another element \code{"time"} that reports the
  execution time for this call to \code{targetRunner}.

  The return list may also contain the following optional elements that are used
  by \pkg{irace} for reporting errors in \code{targetRunner}:
  \itemize{
    \item{error}{is a string used to report an error;}
    \item{outputRaw}{is a string used to report the raw output of calls to
      an external program or function;}
    \item{call}{is a string used to report how \code{targetRunner} called 
      an external program or function.}
  }
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
