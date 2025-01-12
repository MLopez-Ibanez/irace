withr::with_output_sink("test-ablation.Rout", {
  skip_on_cran()
  withr::local_options(warn=2)

  test_that("generateAblation", {
    parameters  <- parametersNew(param_cat(name = "algorithm", values = c("as", "mmas", "ras", "acs")),
      param_real(name = "alpha", lower = 0.0, upper=5.0),
      param_real(name = "beta", lower = 0.0, upper = 10.0),
      param_int(name = "ants", lower = 2, upper = 100),
      param_real(name = "q0", lower=0.0, upper=1.0, condition = expression(algorithm == "acs")),
      param_int(name = "rasrank", lower=1, upper=quote(min(ants, 10)), condition = 'algorithm == "ras"'),
      param_int(name = "eants", lower=0, upper=expression(rasrank)),
      param_cat(name = "dlb",  values = c(0,1), condition = "localsearch == 1"),
      param_int(name = "nnls", lower = 5, upper = 50, condition = expression(dlb == 1)),
      param_ord(name = "localsearch", values = c("0", "1")),
      param_cat(name = "fixed",  values = "0"),
      forbidden = "(alpha == 0) & (beta == 0)")

    confs <- readConfigurationsFile(parameters = parameters, text ='
algorithm alpha beta ants q0  rasrank eants dlb nnls localsearch
ras       1     0    5    NA  5       5     1   5    1
ras       0.5   0    5    NA  2       2     1   5    1
ras       0.5   0    4    NA  2       2     1   5    1
acs       0     1    2    0.5 NA      NA    NA  NA   0
mmas      1     1    6    NA  NA      NA    0   NA   1
')
    confs[[".ID."]] <- seq_len(nrow(confs))
    colClasses <- c(localsearch="character", q0="numeric",
      rasrank="integer", eants="integer", dlb="character", nnls="integer")

    check_generate_ablation <- function(src, target, configurations_table, changed) {
      aux <- irace:::generate_ablation(confs[src, , drop=FALSE], confs[target, , drop=FALSE],
        parameters, param_names = parameters$names_variable)
      expect_valid_configurations(aux$configurations, parameters)
      configurations <- read.table(header=TRUE, colClasses=colClasses, text=configurations_table)
      configurations[["fixed"]] <- "0"
      configurations[[".ID."]] <- src
      configurations[[".PARENT."]] <- src
      expect_equal(aux, list(configurations=configurations, changed_params = changed))
    }

    check_generate_ablation(1L, 2L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
      ras   0.5    0    5 NA       5     5   1    5           1
      ras   1.0    0    5 NA       2     2   1    5           1
      ras   1.0    0    5 NA       5     2   1    5           1
', changed = list("alpha", c("rasrank", "eants"), "eants"))

    check_generate_ablation(1L, 3L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
      ras   0.5    0    5 NA       5     5   1    5           1
      ras   1.0    0    4 NA       2     2   1    5           1
      ras   1.0    0    5 NA       2     2   1    5           1
      ras   1.0    0    5 NA       5     2   1    5           1
', changed = list("alpha", c("ants", "rasrank", "eants"), c("rasrank", "eants"),"eants"))

    check_generate_ablation(1L, 4L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
      acs   1.0    0    5 0.5     NA    NA   1    5           1
      ras   1.0   1.0   5 NA       5     5   1    5           1
      ras   1.0    0    5 NA       5     5  NA   NA           0
', changed = list(c("algorithm", "q0", "rasrank", "eants"), "beta",
  c("localsearch", "dlb", "nnls")))

    check_generate_ablation(1L, 5L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
     mmas   1.0    0    5 NA      NA    NA   1    5           1
      ras   1.0   1.0   5 NA       5     5   1    5           1
      ras     1    0    6 NA       5     5   1    5           1
      ras   1.0    0    5 NA       5     5   0   NA           1
', changed = list(c("algorithm", "rasrank", "eants"), "beta", "ants", c("dlb", "nnls")))

    check_generate_ablation(2L, 1L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
      ras   1.0    0    5 NA       2     2   1    5           1
      ras   0.5    0    5 NA       5     2   1    5           1
', changed = list("alpha", c("rasrank")))

    check_generate_ablation(3L, 1L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
      ras   1.0    0    4 NA       2     2   1    5           1
      ras   0.5    0    5 NA       2     2   1    5           1
', changed = list("alpha", c("ants")))

    check_generate_ablation(4L, 1L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
      acs     1    1    2 0.5     NA    NA  NA   NA           0
      acs     0    1    5 0.5     NA    NA  NA   NA           0
      acs     0    1    2 0.5     NA    NA    1   5           1
', changed = list("alpha", "ants", c("localsearch", "dlb", "nnls")))

    check_generate_ablation(5L, 1L, '
algorithm alpha beta ants q0 rasrank eants dlb nnls localsearch
      ras     1    1    6 NA       5     5   0   NA           1
     mmas     1    0    6 NA      NA    NA   0   NA           1
     mmas     1    1    5 NA      NA    NA   0   NA           1
     mmas     1    1    6 NA      NA    NA   1    5           1
', changed = list(c("algorithm", "rasrank", "eants"), "beta", "ants", c("dlb", "nnls")))

  })

  test_that("--help", {
    expect_output(ablation_cmdline("--help"))
  })

  outfile <- withr::local_tempfile(pattern = "log-ablation", fileext = ".Rdata")
  logfile <- withr::local_tempfile(pattern = "irace", fileext = ".Rdata")

  parameters  <- parametersNew(
    param_cat("cat", values = c("0", "1", "2", "3", "4")),
    param_real("real", lower = 0.0, upper=1.0),
    param_int("int", lower = 100, upper = 500),
    param_cat("bool", values = c("0", "1")))

  default <- data.frame(cat="4", real=1.0, int=500L, bool = "1")

  target_runner <- function(experiment, scenario) {
    conf <- experiment$configuration
    instance <- experiment$instance
    seed <- experiment$seed
    k <- if (as.logical(as.integer(conf[["bool"]]))) 1000 else 100
    list(cost = instance + 1/seed + k * (conf[["int"]] + as.integer(conf[["cat"]]) + (conf[["real"]]-0.5)^2))
  }

  check_log <- function(log) {
    instances_log <- log$state$instances_log
    instances_log[, instance:=.I]
    experiment_log <- log$state$experiment_log[instances_log, on="instance"]
    experiment_log[["instance_value"]] <- log$scenario$instances[experiment_log[["instanceID"]]]
    experiment_log <- experiment_log[log$allConfigurations, on = c(configuration=".ID.")]
    experiments <- log$experiments
    experiments = data.table(
      instance = rep(seq_len(nrow(experiments)), ncol(experiments)),
      configuration = rep(seq_len(ncol(experiments)), each = nrow(experiments)),
      cost3 = c(experiments)
    )
    experiments <- experiments[!is.na(experiments$cost3),]
    experiment_log <- experiment_log[experiments, on=.NATURAL]
    experiment_log[, cost2:=instance_value + 1/seed + fifelse(as.logical(as.integer(bool)), 1000, 100) * (int + as.integer(cat) + (real - 0.5)^2)]
    if ("bound" %in% colnames(experiment_log)) {
      experiment_log[, cost2 := pmin.int(cost2, bound)]
      experiment_log[, cost3 := pmin.int(cost3, bound)]
    }
    expect_equal(experiment_log[["cost"]], experiment_log[["cost2"]])
    expect_equal(experiment_log[["cost"]], experiment_log[["cost3"]])
  }

  src_file <- withr::local_tempfile(pattern="src", fileext=".txt",
    lines=c("cat real int bool", "4 1.0 500 1"))
  target_file <- withr::local_tempfile(pattern="target", fileext=".txt",
    lines=c("cat real int bool", "0 0.0 100 0"))

  test_that("ablation maxTime", {
    target_runner_time <- function(experiment, scenario)
      list(cost = target_runner(experiment, scenario)$cost,
        time = runif(1, min=0.1, max=1))

    scenario <- list(targetRunner = target_runner_time,
      instances = seq(1000, 10000, 1000),
      seed = 42,
      maxTime = 1000,
      initConfigurations = default,
      logFile = logfile,
      parameters = parameters)
    scenario <- checkScenario (scenario)
    irace(scenario = scenario)
    check_log(read_logfile(logfile))
    res <- ablation(logfile, ablationLogFile = outfile)
    check_log(res)
    expect_true(res$complete)
    res <- ablation(logfile, ablationLogFile = outfile, type = "racing")
    check_log(res)
    expect_true(res$complete)

    res <- ablation(logfile, ablationLogFile = outfile, src = src_file, target = target_file)
    check_log(res)
    expect_true(res$complete)

  })

  test_that("ablation capping", {
    target_runner_capping <- function(experiment, scenario) {
      cost <- min(experiment$bound, target_runner(experiment, scenario)$cost)
      list(cost = cost, time = cost)
    }

    boundMax <- 1000 + (1000 * 505)
    scenario <- list(targetRunner = target_runner_capping,
      instances = seq(1000, 10000, 1000),
      seed = 42,
      maxTime = 100 * boundMax, boundMax = boundMax,
      initConfigurations = default,
      logFile = logfile,
      parameters = parameters)
    scenario <- checkScenario(scenario)
    expect_warning(irace(scenario = scenario), "is too large")
    check_log(read_logfile(logfile))

    res <- ablation(logfile, ablationLogFile = outfile)
    check_log(res)
    expect_true(res$complete)

    res <- ablation(logfile, ablationLogFile = outfile, type = "racing")
    check_log(res)
    expect_true(res$complete)
  })

  test_that("ablation maxExperiments", {
    scenario <- list(targetRunner = target_runner,
      instances = seq(1000, 10000, 1000),
      maxExperiments = 1000, seed = 42,
      initConfigurations = default,
      logFile = logfile,
      parameters = parameters)
    scenario <- checkScenario (scenario)
    irace(scenario = scenario)
    check_log(read_logfile(logfile))

    res <- ablation(logfile, ablationLogFile = outfile)
    check_log(res)
    expect_true(res$complete)

    res <- ablation(logfile, ablationLogFile = outfile, type = "racing")
    check_log(res)
    expect_true(res$complete)

    res <- ablation(logfile, ablationLogFile = outfile, src = src_file, target = target_file)
    check_log(res)
    expect_true(res$complete)

    plotfile <- withr::local_tempfile(pattern = "ablation", fileext = ".pdf")
    res <- ablation_cmdline(paste0("--log-file=", logfile, " -o ", outfile, " -p ", plotfile))
    check_log(res)
    expect_true(res$complete)
  })

}) # withr::with_output_sink()
