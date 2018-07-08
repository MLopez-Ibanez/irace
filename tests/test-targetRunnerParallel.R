library(testthat)
library(irace)
setClasses <- function(x, classes)
{
  class(x) = classes
  x
}

# test that targetRunnerParallel works with arbitrary instance objects.
targetRunnerParallel = function(experiment, exec.target.runner, scenario, target.runner)
{
  # get our param settings that irace should try
  cands = lapply(experiment, "[[", "configuration")
  # the instance is always the same for all different param setting
  theinst = experiment[[1L]]$instance
  # we check that we have instances of correct class
  expect_is(theinst, "foo")
  # fabricate some random fitness vals
  ys = rnorm(length(cands))
  ys = lapply(ys, function(y) list(cost = y, time = NA_real_))
  return(ys)
}

n.inst = 7L
instances = replicate(n.inst, setClasses(list(x=123), classes = "foo"), simplify = FALSE)

parameters = readParameters(text='
x1 "" r (0,1)
')
log.file = tempfile()
tuner.config = list(maxExperiments = 40L, nbIterations = 1L, minNbSurvival = 1,
                    targetRunnerParallel = targetRunnerParallel,
                    instances = instances, logFile = log.file)
irace(scenario = tuner.config, parameters = parameters)
