withr::with_output_sink("test-dependencies.Rout", {

test_that("param depend error checking", {
  expect_error_readParameters <- function(text, error)
    expect_error(readParameters(text=text), error)

  expect_error_readParameters('
p1 "" r (0, 1)
p2 "" r (p3, 1)
', "parameter 'p2' is not valid: 'p3' cannot be found")
  
  expect_error_readParameters('
p1 "" c (0, 1)
p2 "" r (1, p1)
', "parameter 'p2' depends on non-numerical parameters")
  
  expect_error_readParameters('
p1 "" r (0, 1)
p2 "" r (1, foobar(p1))
', "parameter 'p2' uses function")

  expect_error_readParameters('
p1 "" r (0, p3)
p2 "" r (1, p1)
p3 "" r (p2, 1)
', "Cycle detected")

  expect_error_readParameters('
p1 "" r (0, 1)
p2 "" i (0.1, p1)
', "values must be integers")

})


checkConditionalAndDependency <- function(configuration, parameters)
{
  for (param in parameters$get_ordered()) {
    p <- param[["name"]]
    if (!irace:::conditionsSatisfied(param[["condition"]], configuration)) {
      expect(is.na(configuration[[p]]),
             paste0("Conditional parameter '", p, 
                    "' is not active but it has a value '", configuration[[p]], "' assigned."))
    } else if (param[["is_dependent"]]) {
      bounds <- irace:::getDependentBound(param, configuration)
      if (anyNA(bounds)) {
        expect(is.na(configuration[[p]]),
               paste0("Dependent parameter '", p, 
                      "' has a value '", configuration[[p]], "' but it should be inactive."))
        expect_true(anyNA(configuration[parameters$depends[[p]]]))
      } else {
        expect (configuration[[p]] >= bounds[1],
                paste0("Parameter '", p, "=", configuration[[p]], 
                       "' does not comply with dependency: ", parameters$depends[[p]],
                       " and lower bound: ", bounds[1]))
        expect (configuration[[p]] <= bounds[2],
                paste0("Parameter '", p, " = ", configuration[[p]], 
                       "' does not comply with dependency: ", parameters$depends[[p]],
                       " and upper bound: ", bounds[2]))
      }
    }
  }
}


test_that("test inactive dependent", {
  parameters <- readParameters(text='
p1 "" r (0,1)
p2 "" r (0, p1) | p1 < 0.5
p3 "" r (0, p2)
', digits = 2)
  confs <- irace:::sampleUniform(parameters, 50)
  confs <- as.data.frame(confs)
  for (i in seq_len(nrow(confs))) {
    checkConditionalAndDependency(confs[i,], parameters)
  }
  expect_error(readConfigurationsFile(parameters = parameters, text = '
p1  p2  p3
0.4 0.4 0.4
0.4 0.5 0.4
'), "Configuration number 2 is invalid because the value")

  expect_error(readConfigurationsFile(parameters = parameters, text = '
p1  p2  p3
0.4 0.3 0.2
0.4 0.3 0.4
'), "Configuration number 2 is invalid because the value")

    readConfigurationsFile(parameters = parameters, text = '
p1  p2  p3
0.4 0.3 0.2
0.5 NA  NA
0.4 0.4 0.4
')
})

test_that("checkDependencies", {

test.checkDependencies <- function(parameterFile, ...)
{
  args <- list(...)
  
  target.runner <- function(experiment, scenario) {
    configuration <- experiment$configuration
    tmax <- configuration[["real"]]
    stopifnot(is.numeric(tmax))
    if (configuration[["mode"]] %in% c("x1", "x2"))
      temp <-  configuration[["param1"]]
    else
      temp <- 1
    stopifnot(is.numeric(temp))
    time <- max(1, abs(rnorm(1, mean=(tmax+temp)/10)))
    list(cost = time, time = time, call = toString(experiment))
  }
  
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  parameters <- readParameters(parameterFile)
  scenario <- list(targetRunner = target.runner, instances = weights, seed = 1234567, maxExperiments=200,
    parameters = parameters)
  scenario <- modifyList(scenario, args)
  scenario <- checkScenario (scenario)
 
  nconf <- 100
  conf <- irace:::sampleUniform(parameters, nconf)
  expect_equal(nconf, nrow(conf))
  conf$.ID. <- seq_len(nconf)
  conf <- as.data.frame(conf)
  for (i in seq_len(nconf))
    checkConditionalAndDependency(conf[i,], parameters)
 
  model <- irace:::initialiseModel(parameters, conf)
  conf2 <- irace:::sampleModel(parameters, conf, model, nconf)
  conf2 <- as.data.frame(conf2)
  for (i in seq_len(nconf))
    checkConditionalAndDependency(conf2[i,], parameters)

  confs <- irace(scenario = scenario)
  for (i in seq_len(nrow(confs))) {
    checkConditionalAndDependency(confs[i,], parameters)
  }
}
  test.checkDependencies(parameterFile="dependencies.txt")
  test.checkDependencies(parameterFile="dependencies2.txt")
})

}) # withr::with_output_sink()
