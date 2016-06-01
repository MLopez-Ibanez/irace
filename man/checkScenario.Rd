\name{checkScenario}
\alias{checkScenario}
\title{checkScenario}
\description{
  \code{checkScenario} takes a (possibly incomplete) scenario setup
  of \pkg{irace}, checks for errors and transforms it into a valid
  scenario.
}
\usage{
  checkScenario(scenario = defaultScenario())
}
\arguments{
   \item{scenario}{A list where tagged elements correspond to scenario
    settings of \pkg{irace}.}
}
\value{
  The scenario received as a parameter, possibly corrected. Unset
  scenario settings are set to their default values.
}
\details{
  This function checks that the directories and the file names provided
  and required by the \pkg{irace} exist.
  It also checks that the settings are of the proper type,
  e.g. that settings expected to be integers are really
  integers. Finally, it also checks that there is no inconsistency
  between settings.  If an error is found that prevents
  \pkg{irace} from running properly, it will stop with an error.
}
\seealso{
  \code{\link{readScenario}} for reading a configuration scenario from a
  file.
  \code{\link{printScenario}} prints the given scenario.
  \code{\link{defaultScenario}} to get the default scenario settings.
  
}
\author{Manuel López-Ibáñez and Jérémie Dubois-Lacoste}
