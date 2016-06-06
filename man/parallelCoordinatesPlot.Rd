\name{parallelCoordinatesPlot}
\alias{parallelCoordinatesPlot}
\title{parallelCoordinatesPlot}
\description{
  'parallelCoordinatesPlot' plots a set of parameter configurations in parallel coordinates.
}
\usage{
parallelCoordinatesPlot(configurations, parameters,
                        param_names = parameters$names, hierarchy = TRUE,
                        filename = NULL, pdf.width = 14, mar = c(8,1,4,1))
}
\arguments{
  \item{configurations}{Data frame containing target algorithms configurations in the format used by \pkg{irace}.}
 \item{parameters}{List of target algorithm parameters in the \pkg{irace} format.}
 \item{param_names}{Parameters names that should be included. Default: parameters$names.}
 \item{hierarchy}{If \code{TRUE} conditional parameters will be displayed in a different plot. Default \code{TRUE}.}
 \item{filename}{Filename prefix to generate the plots. If \code{NULL} the plot displayed but not saved.}
 \item{pdf.width}{Width for the pdf file generated.}
 \item{mar}{Margin to use for the plot. See \code{\link{par}}.}
}


\value{
  A set of parallel coordinates plots showing the parameters values. If a filename is provided this plots are saved in one or more files. 
}

\examples{
\dontrun{
## To use data obtained by irace

# First, load the data produced by irace.
load("irace.Rdata")
attach(iraceResults)
parallelCoordinatesPlot(allConfigurations, parameters, hierarchy = FALSE)
}
}

\seealso{
  \code{\link{readParameters}} to obtain a valid parameter structure from a parameters file.
  \code{\link{readConfigurationsFile}} to obtain a set of target algorithm configurations from a configurations file.
}
\author{Manuel López-Ibáñez and Leslie Pérez Cáceres}
