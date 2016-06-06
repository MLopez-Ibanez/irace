\name{parameterFrequency}
\alias{parameterFrequency}
\title{parameterFrequency}
\description{
  'parameterFrequency' plots the frequency of the parameters values of a set of target algorithm configurations.
}
\usage{
parameterFrequency(configurations, parameters, rows = 4, cols = 3,
                   filename = NULL, pdf.width = 12, col = "gray")
}
\arguments{
  \item{configurations}{Data frame containing target algorithms configurations in the format used by \pkg{irace}.}
 \item{parameters}{List of target algorithm parameters in the \pkg{irace} format.}
 \item{rows}{Number of plots per column.}
 \item{cols}{Number of plots per row.}
 \item{filename}{Filename prefix to generate the plots. If \code{NULL} the plot displayed but not saved.}
 \item{pdf.width}{Width for the pdf file generated.}
 \item{col}{Color of the bar plot.}
 
}
\value{
  A set of plots showing the Frequency of parameters values. If a filename is provided this plots are saved in one or more files. 
}

\examples{
\dontrun{
## To use data obtained by irace

# First, load the data produced by irace.
load("irace.Rdata")
attach(iraceResults)
parameterFrequency(allConfigurations, parameters)
}
}

\seealso{
  \code{\link{readParameters}} to obtain a valid parameter structure from a parameters file.
  \code{\link{readConfigurationsFile}} to obtain a set of target algorithm configurations from a configurations file.
}
\author{Manuel López-Ibáñez and Leslie Pérez Cáceres}
