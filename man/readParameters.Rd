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
  A list containing the definitions of the parameters read. The list is
  structured as follows:

  \item{names}{Vector that contains the names of the parameters.}
  \item{types}{Vector that contains the type of each parameter 'i', 'c', 'r', 'o'.}
  \item{switches}{Vector that contains the switches to be used for the
    parameters on the command line.}
  \item{domain}{List of vectors, where each vector may contain two
    values (minimum, maximum) for real and integer parameters, or
  possibly more for categorical parameters.}
 \item{conditions}{List of R logical expressions, with variables
   corresponding to parameter names.}
 \item{isFixed}{Logical vectors that specifies which parameter is fixed
   and, thus, it does not need to be tuned.}
 \item{nbParameters}{An integer, the total number of parameters.}
 \item{nbFixed}{An integer, the number of parameters with a fixed value.}
 \item{nbVariable}{Number of variable (to be tuned) parameters.}
}
\details{
  Either 'file' or 'text' must be given. If 'file' is given, the
  parameters are read from the file 'file'. If 'text' is given instead,
  the parameters are read directly from the 'text' character string.
  In both cases, the parameters must be given (in 'text' or in the file
  whose name is 'file') in the expected form.  See the documentation
  for details.  If none of these parameters is given, \pkg{irace}
  will stop with an error.

  A fixed parameter is a parameter that should not be sampled but
  instead should be always set to the only value of its domain.  In this
  function we set isFixed to TRUE only if the parameter is a categorical
  and has only one possible value.  If it is an integer and the minimum
  and maximum are equal, or it is a real and the minimum and maximum
  values satisfy 'round(minimum, digits) == round(maximum, digits)',
  then the parameter description is rejected as invalid to identify
  potential user errors.

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
