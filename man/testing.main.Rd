\name{testing.main}
\alias{testing.main}
\title{testing.main}
\description{
  \code{testing.main} executes the testing of the target algorithm configurations found
  on an \pkg{irace} execution.
}
\usage{
  testing.main(logFile)
}
\arguments{
  \item{logFile}{Path to the .Rdata file produced by \pkg{irace}.}
}
\details{
  The function \code{testing.main} load the \code{logFile} and obtains
  the needed configurations according to the specified test. Use the \code{scenario$testNbElites}
  to test N final elite configurations or use  \code{scenario$testIterationElites} to test the
  best configuration of each iteration. A test instance set must be provided through 
  \code{scenario$testInstancesDir} and \code{testInstancesFile}.
}

\seealso{  
  \code{\link{defaultScenario}} to provide a default scenario for \pkg{irace}.
}

\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
