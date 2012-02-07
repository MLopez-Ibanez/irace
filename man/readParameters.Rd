\name{readParameters}
\alias{readParameters}
\title{readParameters}
\description{
  'readParameters' reads the parameters to be tuned by 
  \pkg{irace} from a file or directly from a character string.
}
\usage{
  readParameters(file, digits = 4, debugLevel = 0, text)
}
\arguments{
  \item{file}{(optional) character string: the name of the file containing the definitions of the
    parameters to be tuned.}
  \item{digits}{The number of decimal places to be considered for the
    real parameters.}
  \item{debugLevel}{Integer: the debug level to increase the amount of output.}
  \item{text}{(optional) character string: if file is not supplied and this is,
    then parameters are read from the value of text via a text connection.}
}
\value{
  A list containing the definitions of the parameters read.
}
\details{
  Either 'file' or 'text' must be given. If 'file' is given, the
  parameters are read from the file 'file'. If 'text' is given instead,
  the parameters are read directly from the 'text' character string.
  In both cases, the parameters must be given (in 'text' or in the file
  whose name is 'file') in the expected form. See the documentation
  for details. If none of these parameters is given, \pkg{irace}
  will stop with an error.
}
\examples{
## Read the parameters directly from text
parameters.table <- 'tmax "" i (2, 10)
temp "" r (10, 50)
'
parameters <- readParameters(text=parameters.table)
parameters
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
