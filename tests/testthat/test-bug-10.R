# https://github.com/MLopez-Ibanez/irace/issues/10
withr::with_output_sink("test-bug-10.Rout", {
test_that("bug 10", {
  skip_on_cran()
parameters_txt <- '
algorithm    "--"             c    (as,mmas,eas,ras,acs)
localsearch  "--localsearch " c    (0, 1, 2, 3)
alpha        "--alpha "       r    (0.00, 5.00)
beta         "--beta "        r    (0.00, 10.00)
rho          "--rho "         r    (0.01, 1.00)
ants         "--ants "        i    (5, 100)
q0           "--q0 "          r    (0.0, 1.0)           | algorithm == "acs"
rasrank      "--rasranks "    i    (1, 100)             | algorithm == "ras"
elitistants  "--elitistants " i    (1, 750)             | algorithm == "eas"
nnls         "--nnls "        i    (5, 50)              | localsearch %in% c(1,2,3)
dlb          "--dlb "         c    (0, 1)               | localsearch %in% c(1,2,3)
'
target.runner <- function(experiment, scenario)
  list(cost = 100, call = toString(experiment))

parameters <- irace:::readParameters(text=parameters_txt)

withr::with_options(list(warning=2), {
  scenario <- list(targetRunner = target.runner,
                   instances=1:10,
                   maxExperiments = 5000, logFile = "",
                   deterministic = TRUE,
                   elitistNewInstances = 0,
                   elitistLimit = 0,
                   elitist = 0,
                   parameters = parameters)
  scenario <- checkScenario (scenario)
  confs <- irace(scenario = scenario)
  expect_false(is.null(confs))
})
})
})
