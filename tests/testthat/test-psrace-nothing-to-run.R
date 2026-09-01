withr::with_output_sink("test-psrace-nothing-to-run.Rout", {

  # A post-selection race can have nothing left to run: when the budget left is smaller
  # than the number of experiments that any not-yet-fully-evaluated configuration would
  # need, the only configurations that fit are the ones already evaluated on every
  # instance. psRace() used to error with "wrong sign in 'by' argument" and, once that
  # was fixed, the race that then executes no experiments used to error with
  # "columns not found: [instance, configuration]".
  test_that("psRace with nothing left to run", {
    skip_on_cran()
    parameters <- readParameters(text = '
a "" i (1, 10)
b "" i (1, 10)
')
    logFile <- withr::local_tempfile(fileext = ".Rdata")
    scenario <- defaultScenario(list(
      parameters = parameters,
      instances = 1:5,
      deterministic = TRUE,
      elitist = TRUE,
      # The post-selection race is run by hand below, with a budget that leaves it
      # nothing to do.
      postselection = FALSE,
      maxExperiments = 200,
      parallel = 1,
      seed = 1234,
      logFile = logFile,
      targetRunner = function(experiment, scenario)
        list(cost = experiment$configuration$a * as.integer(experiment$instance) +
               experiment$configuration$b)
    ))
    irace(scenario)

    irace_log <- read_logfile(logFile)
    elites <- irace_log$allElites[[length(irace_log$allElites)]]
    conf_ids <- unique(c(elites, irace_log$allConfigurations[[".ID."]]))
    conf_ids <- conf_ids[!is.na(conf_ids)]
    conf_needs <- colSums(
      is.na(irace_log$experiments[, as.character(conf_ids), drop = FALSE]))

    # The two conditions that take psRace() down the path under test, asserted so that
    # this fails loudly if it ever stops reaching it: every instance has been seen, and
    # with this budget every configuration that fits has nothing left to run.
    max_experiments <- 2L
    expect_equal(nrow(irace_log$experiments) - min(conf_needs),
                 length(irace_log$scenario$instances))
    conf_needs <- conf_needs[conf_needs <= max_experiments]
    expect_gt(length(conf_needs), 1L)
    expect_true(all(conf_needs == 0L))

    psrace_logFile <- withr::local_tempfile(fileext = ".Rdata")
    expect_no_error(
      psRace(irace_log, max_experiments = max_experiments,
             psrace_logFile = psrace_logFile))

    psrace_log <- read_logfile(psrace_logFile)
    expect_false(is.null(psrace_log$psrace_log))
    # Nothing new could be measured, so post-selection can only return configurations it
    # was given, and must return at least one.
    psrace_elites <- psrace_log$allElites[[length(psrace_log$allElites)]]
    expect_gt(length(psrace_elites), 0L)
    expect_true(all(psrace_elites %in% conf_ids))
  })

}) # withr::with_output_sink()
