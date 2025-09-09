withr::with_output_sink("test-bug-87.Rout", {

test_that("bug-87", {
  parameters <- parametersNew(
    param_cat(name = "algorithm", values = c("as", "mmas", "eas", "ras", "acs"), label = "--"),
    param_ord(name = "localsearch", values = c("0", "1", "2", "3"), label = "--localsearch "),
    param_real(name = "alpha", lower = 0.0, upper = 5.0, label = "--alpha "),
    param_real(name = "beta", lower = 0.0, upper = 10.0, label = "--beta "),
    param_real(name = "rho", lower = 0.01, upper = 1.00, label = "--rho "),
    param_int(name = "ants", lower = 5, upper = 100, transf = "log", label = "--ants "),
    param_real(name = "q0", label = "--q0 ", lower = 0.0, upper = 1.0,
      condition = expression(algorithm == "acs")),
    param_int(name = "rasrank", label = "--rasranks ", lower = 1, upper = quote(min(ants, 10)),
      condition = 'algorithm == "ras"'),
    param_int(name = "elitistants", label = "--elitistants ", lower = 1, upper = expression(ants),
      condition = 'algorithm == "eas"'),
    param_int(name = "nnls", label = "--nnls ", lower = 5, upper = 50,
      condition = expression(localsearch %in% c("1", "2", "3"))),
    param_cat(name = "dlb", label = "--dlb ", values = c("0", "1"),
      condition = "localsearch %in% c('1','2','3')"),
    forbidden = "(alpha == 0) & (beta == 0)"
  )

  logFile <- withr::local_tempfile(fileext=".Rdata")
  scenario <- defaultScenario(list(parameters = parameters, instances = 1,
    maxExperiments = 300, logFile = logFile,
    targetRunner = function(experiment, scenario) {
      list(cost = experiment$configuration$alpha * experiment$configuration$beta)
    }))
  confs <- irace(scenario)
  best_conf <- getFinalElites(scenario$logFile, n = 1L, drop.metadata = TRUE)
  expect_identical(removeConfigurationsMetaData(confs[1L, , drop = FALSE]), best_conf)
})
}) # withr::with_output_sink()
