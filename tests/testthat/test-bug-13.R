context("bug")
skip_on_cran()
# https://github.com/MLopez-Ibanez/irace/issues/13
withr::with_output_sink("test-bug-13.Rout", {

parameters.txt <- '
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
parameters <- irace:::readParameters(text=parameters.txt)
  
lookup <- readRDS("bug-13-lookup.rds")
counter <- 0
target.runner <- function(experiment, scenario) {
  configuration <- experiment$configuration
  instance <- experiment$instance
  switches <- experiment$switches
  debugLevel <- scenario$debugLevel
  args <- paste(instance, trimws(irace:::buildCommandLine(configuration, switches)))
  pos <- pmatch(args, lookup[,1])
  if(!is.na(pos)) {
    cost <- as.numeric(lookup[pos, 2])
    counter <<- counter + 1
  } else {
    cost <- 10
  }
  if(debugLevel > 0) {
    cat("# ", args, "\n")
    cat(cost, "\n")
  }
  return(list(cost = cost, call = toString(experiment)))
}

withr::with_options(list(warning=2), {
  scenario <- list(instances = scan(what="", quiet=TRUE, text="
2000-121.tsp
2000-122.tsp
2000-123.tsp
2000-124.tsp
2000-125.tsp
2000-126.tsp
2000-127.tsp
2000-128.tsp
2000-129.tsp
2000-130.tsp"),
trainInstancesDir = "",
trainInstancesFile = "",
maxExperiments = 3000,
digits = 3,
seed = 2357,
deterministic = 1,
elitist = 0,
targetRunner = target.runner,
debugLevel=3)
  scenario <- checkScenario (scenario)
  confs <- irace(scenario = scenario, parameters = parameters)
  expect_equal(counter, 548)
})

})
