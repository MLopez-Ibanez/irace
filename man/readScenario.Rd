\name{readScenario}
\alias{readScenario}
\title{readScenario}
\description{
  'readScenario' reads the scenario to be used by 
  \pkg{irace} from a file.
}
\usage{
  readScenario(filename = "", scenario = list())
}
\arguments{
  \item{filename}{A filename from which the scenario will be
    read. If empty, the default \code{scenarioFile} is used.
    An example scenario file is provided in
    \code{system.file(package="irace", "templates/scenario.txt.tmpl")}.}
 \item{scenario}{A list where tagged elements correspond to scenario
    settings for \pkg{irace}. This is an initial scenario that is
    overwritten for every parameter specified in the file to be read.}
}
\value{
  The scenario list read from the file. The scenario parameter not present
  in the file are not present in the list, that is, they are \code{NULL}. 
  
}
\seealso{
  \code{\link{checkScenario}} to check that the scenario is valid.
  \code{\link{defaultScenario}} to set the scenario to the default.
  \code{\link{printScenario}} to print the scenario.
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
