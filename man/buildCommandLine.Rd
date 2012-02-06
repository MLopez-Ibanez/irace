\name{buildCommandLine}
\alias{buildCommandLine}
\title{Build a Command Line}
\description{
  'buildCommandLine' receives two vectors, one containing  the values of
  the parameters, the other containing the switches of
  the parameters. It
  builds a string with the switches and the values that can be used as a
  command line to call the program to be tuned, instanciating one
  candidate configuration.
}
\usage{
  buildCommandLine(values, switches)
}
\arguments{
  \item{values}{A vector containing the value of each parameter for the
  candidate configuration.}
  \item{switches}{A vector containing the switches of each paramter (in an
  order that corresponds to the values vector).}
}
\value{
  A character chain containing the switches and the values.
}
\details{
  The chain concatenates <switch> <value> for all parameters with a
  space between each parameter (but none between the switches and the
  corresponding values).
}
\examples{
switches <- c("--switch1 ", "--switch2 ")
values <- c("value_1", "value_2")
buildCommandLine (values, switches)
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
