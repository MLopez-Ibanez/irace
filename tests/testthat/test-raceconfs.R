withr::with_output_sink("test-raceconfs.Rout", {

parameters_txt <- '
launch_method "--launch_method=" c (L-BFGS-B,SLSQP)
visit_method "--visit_method=" c (DA, DE)
global_method "--global_method=" c (CMAES,DE)
'
confs_txt <- '
launch_method visit_method global_method
L-BFGS-B    DA             CMAES
SLSQP       DA             CMAES
L-BFGS-B    DE             CMAES
SLSQP       DE             CMAES
L-BFGS-B    DA             DE
SLSQP       DA             DE
L-BFGS-B    DE             DE
SLSQP       DE             DE
'

target_runner <- function(experiment, scenario)
  list(cost = 100, call = toString(experiment))

withr::with_options(list(warning=2), {
  parameters <- readParameters(text=parameters_txt)
  initconfs <- readConfigurationsFile(text=confs_txt, parameters=parameters)
  scenario <- list(targetRunner = target_runner,
    instances = 1:10,
    nbConfigurations = 8,
    maxExperiments = 96, logFile = "",
    initConfigurations = initconfs,
    parameters = parameters)
  scenario <- checkScenario (scenario)
  confs <- irace(scenario = scenario)
  confs <- data.table::as.data.table(removeConfigurationsMetaData(confs))
  initconfs <- data.table::as.data.table(initconfs)
  expect_equal(nrow(data.table::fsetdiff(confs, initconfs)), 0L)
})

}) # withr::with_output_sink()
