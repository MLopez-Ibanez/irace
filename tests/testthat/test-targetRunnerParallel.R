withr::with_output_sink("test-targetRunnerParallel.Rout", {

test_that("targetRunnerParallel", {

setClasses <- function(x, classes)
{
  class(x) = classes
  x
}

# test that targetRunnerParallel works with arbitrary instance objects.
targetRunnerParallel <- function(experiment, exec_target_runner, scenario, target_runner)
{
  # get our param settings that irace should try
  cands = lapply(experiment, "[[", "configuration")
  # the instance is always the same for all different param setting
  theinst = experiment[[1L]]$instance
  # we check that we have instances of correct class
  expect_s3_class(theinst, "foo", exact = TRUE)
  # fabricate some random fitness vals
  ys = rnorm(length(cands))
  lapply(ys, function(y) list(cost = y, time = NA_real_))
}

   n.inst = 7L
   instances = replicate(n.inst, setClasses(list(x=123), classes = "foo"), simplify = FALSE)
   parameters = readParameters(text='x1 "" r (0,1)')
   log.file = tempfile()
   tuner_config = list(maxExperiments = 40L, nbIterations = 1L, minNbSurvival = 1L,
                       targetRunnerParallel = targetRunnerParallel,
                       instances = instances, logFile = log.file)
   confs <- irace(scenario = tuner_config)
   expect_gt(nrow(confs), 0L)
})

test_that("targetRunnerData", {
  targetRunnerParallel <- function(experiments, exec_target_runner, scenario, target_runner) {
    cat("a = ", scenario$targetRunnerData$a,
        ", b = ", scenario$targetRunnerData$b, "\n", sep = "")
    # get our param settings that irace should try
    cands = lapply(experiments, "[[", "configuration")
    # fabricate some random fitness vals
    ys = rnorm(length(cands))
    ys = lapply(ys, function(y) list(cost = y, time = NA_real_))
    return(ys)
  }
  parameters = readParameters(text='
x "x" r (1,2)
')
  expect_output(
    irace(scenario = list(targetRunnerParallel = targetRunnerParallel,
                          instances = replicate(5, list(10)),
                          targetRunnerData = list(a=1, b=2),
                          maxExperiments = 42L, parameters = parameters)),
    "a = 1, b = 2")
})

}) # with_output_sink()
