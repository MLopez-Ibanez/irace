#' psRace
#'
#' \code{psRace} performs a postselection race a set of configurations. 
#' 
#' @param iraceLogFile NULL Log file created by \pkg{irace}, this file must contain the 
#' \code{iraceResults} object.
#' @param iraceResults NULL Object created by \pkg{irace} and saved in \code{scenario$logFile}.
#' @param conf.ids NULL IDs of the configurations in iraceResults$allConfigurations to be used for ablation.
#' If NULL, the elites argument will be used.
#' @param postselection NULL Percentage of the maxExperiments provided in the scenario to be used in the race.
#' @param max.experiments NULL Number of experiments available for the race. If NULL budget for the race is set
#'  by the parameter scenario$postselection, which defines the percentage of the total budget of \pkg{irace}
#' (iraceResults$scenario$maxExperiments or iraceResults$scenario$maxTime/iraceResults$state$timeEstimate) to use
#'  for the postselection.
#' @param elites FALSE Flag for selecting configurations. If FALSE, the best configurations of each
#' iteration are used for the race. If TRUE, the elite configurtions of each iteration are used for the race. 
#' @param seed 1234567 Numerical value to use as seed for the random number generation.
#' 
#' @return If iraceLogFile is NULL, it returns a list with the following elements:
#'  \describe{
#'    \item{configurations}{Configurations used in the race.}
#'    \item{instances}{A matrix with the instances used in the experiments. First column has the 
#'    instances ids from iraceResults$scenario$instances, second column the seed assigned to the instance.}
#'    \item{maxExperiments}{Maximum number of experiments set for the race.}
#'    \item{experiments}{A matrix with the results of the experiments (columns are configurations, rows are instances).}
#'    \item{elites}{Best configurations found in the experiments.}
#'  }
#' If \code{iraceLogFile} is provided this list object will be saved in \code{iraceResults$psrace.log}.
#'
#' @examples
#' \dontrun{
#'   # Execute the postselection automatically after irace
#'   parameters <- readParameters("parameters.txt")
#'   scenario <- readScenario(filename="scenario.txt", 
#'                            scenario=defaultScenario())
#'   # Use 10% of the total budget
#'   scenario$postselection <- 0.1
#'   irace(scenario=scenario, parameters=parameters)
#'   # Execute the postselection after the execution of \pkg{irace}.
#'   psRace(iraceLogFile="irace.Rdata", max.experiments=120)
#' }
#'
#' @author Manuel López-Ibáñez and Leslie Pérez Cáceres
#' @export
# This function executes a post selection race
# elites: test all elites
psRace <- function(iraceLogFile=NULL, iraceResults=NULL, conf.ids=NULL, postselection=NULL,
                   max.experiments=NULL, elites=FALSE, seed=1234567)
{
  # Input check
  if (is.null(iraceLogFile) && is.null(iraceResults)) 
    irace.error("You must provide a Rdata file or an iraceResults object.")
    
  irace.note ("Stating post-selection:\n# Seed:", seed, "\n")
  if (!is.null(iraceLogFile))
    cat("# Log file:",iraceLogFile,"\n")
  
  # Load the data of the log file
  if (!is.null(iraceLogFile)) 
  	load(iraceLogFile)
 
  parameters <- iraceResults$parameters
  scenario   <- iraceResults$scenario
  
  # Get selected configurations
  if (!is.null(conf.ids)) {
  	if (!all(conf.ids %in% iraceResults$allConfigurations$.ID.)) 
  	  irace.error("Configuration ids provided", conf.ids,"cannot be found in the configurations.")
  	configurations <- iraceResults$allConfigurations[iraceResults$allConfigurations$.ID.%in% conf.ids,,drop=FALSE] 	
  } else {
  	if (elites)
  	  configurations <- iraceResults$allConfigurations[unique(unlist(iraceResults$allElites)),]
  	else
      configurations <- iraceResults$allConfigurations[unique(iraceResults$iterationElites),]      
  }
  
  if (nrow(configurations)<=1)
    irace.error ("The number configurations should be > 1.")

  # LESLIE: Should we use testing instances?
  # Generate new instances
  instances <- generateInstances(scenario, 1000)
  .irace$instancesList <- instances
  .irace$next.instance <- 1
  # When capping is used we must pass maxBound
  if (scenario$capping)
  scenario$instances <- paste(scenario$instances, scenario$boundMax, sep=" ")
  
  scenario$elitist <- scenario$capping <- FALSE
  
  if (is.null(postselection)) postselection <- scenario$postselection
  # Calculate available budget
  # FIXME: add numerical checks
  if (is.null(max.experiments)) {
    if (scenario$maxExperiments > 0) 
       max.experiments <- ceiling(postselection*scenario$maxExperiments)
    else
       max.experiments <- ceiling(postselection*(scenario$maxTime/iraceResults$state$timeEstimate)) 
  }
  
  cat("# configurations:", nrow(configurations), "\n")
  cat("# postselection %", postselection, "\n")
  cat("# scenario experiments:",scenario$maxExperiments,"\n" )
  cat("# available experiments:",max.experiments,"\n" )
  cat("# minSurvival: 1\n")
  
  if (!is.null(seed))
    set.seed(seed)
  # Should we fix the paramenters for the race?
  race.output <- race(maxExp = max.experiments,
                      minSurvival = 1,
                      elite.data = NULL,
                      configurations = configurations,
                      parameters = parameters,
                      scenario = scenario,
                      elitistNewInstances = 0)
  experiments <-  race.output$experiments
  
  elite.configurations <- extractElites(race.output$configurations,
                                       min(race.output$nbAlive, 1))
  irace.note("Elite configurations (first number is the configuration ID;",
               " listed from best to worst according to the ",
               test.type.order.str(scenario$testType), "):\n")
  configurations.print(elite.configurations, metadata = scenario$debugLevel >= 1)

  psrace.log <-  list(configurations = configurations,
                      instances = instances[1:nrow(experiments),],
                      maxExperiments = max.experiments,
                      experiments = experiments,
                      elites = elite.configurations$.ID.)   
  iraceResults$psrace.log <- psrace.log        
        
  if (!is.null(iraceLogFile))
    save(iraceResults, file=scenario$logFile)
  else 
    return(psrace.log)
                      
}
