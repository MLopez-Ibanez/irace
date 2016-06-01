\name{irace.cmdline}
\alias{irace.cmdline}
\title{irace.cmdline}
\description{
  'irace.cmdline' starts \pkg{irace} using the parameters
  of the command line used to invoke R.
}
\usage{
  irace.cmdline(args = commandArgs(trailingOnly = TRUE))
}
\arguments{
  \item{args}{The arguments provided on the R command line as a character
    vector, e.g., \code{c("--scenario", "scenario.txt", "-p",
      "parameters.txt")}.
    Using the  default value (not providing the parameter) is the easiest way to
    call \code{irace.cmdline}.}
}
\value{
  None.
}
\details{
  The function reads the parameters given on the command line
  used to invoke R, finds the name of the scenario file,
  initializes the scenario from the file (with the function
  'readScenario) and possibly from parameters passed on
  the command line. It finally starts \pkg{irace} by calling
  'irace.main'.
}
\seealso{
  \code{\link{irace.main}} to start \pkg{irace} with a given scenario.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
