withr::with_output_sink("test-target-runner-dummy.Rout", {
  skip_on_cran()
  get_executable <- function(filename, src_dir) {
    exe <- paste0(filename, if (system_os_is_windows()) ".exe" else "")
    p <- file.path(if (.Platform$r_arch == "") "bin" else file.path("bin", .Platform$r_arch), exe)
    res <- system.file(package="irace", mustWork = FALSE, p)
    if (res == "") {
      res <- test_path(file.path(src_dir, exe))
    }
    res
  }
  runexe <- function(exe, args) {
    err <- NULL
    output <- withCallingHandlers(
      tryCatch(system2(exe, args, stdout = TRUE, stderr = TRUE),
               error = function(e) {
                 err <<- c(err, paste(conditionMessage(e), collapse="\n"))
                 NULL
               }), warning = function(w) {
                 err <<- c(err, paste(conditionMessage(w), collapse="\n"))
                 invokeRestart("muffleWarning")
               })
    if (!is.null(err)) {
      err <- paste(err, collapse = "\n")
      if (!is.null(attr(output, "errmsg")))
        err <- paste(sep = "\n", err, attr(output, "errmsg"))
      stop(err)
    }
    if (is.null(output))
      output <- ""
    return(output)
  }

  test_that("irace exe works", {
    iraceexe <- get_executable("irace", "../../src/iracebin")
    expect_true(file.exists(iraceexe))
    skip_on_os("windows")
    # FIXME: For some reason, this does not generate any output on Windows
    expect_match(paste(collapse="", system2(iraceexe, "--help", stdout = TRUE, stderr = TRUE)),
                 "irace: An implementation in R of.*called with: --help")
  })

  test_that("ablation exe works", {
    ablationexe <- get_executable("ablation", "../../src/iracebin")
    expect_true(file.exists(ablationexe))
    skip_on_os("windows")
    # FIXME: For some reason, this does not generate any output on Windows
    expect_match(paste(collapse="", system2(ablationexe, "--help", stdout = TRUE, stderr = TRUE)),
                 "ablation: An implementation in R of Ablation Analysis.*called with: --help")
  })

  target_runner_dummy <- get_executable("target-runner-dummy", "../../src/dummy")
  skip_if_not(file.exists(target_runner_dummy),
              message = sprintf("target_runner_dummy='%s' not installed", target_runner_dummy))
  
  run_cmdline <- function(parameters, args) {
    parameters_file <- tempfile("dummy-parameters", fileext = ".txt")
    withr::local_file(parameters_file)
    cat(parameters, file=parameters_file)
    train_instances_file <- tempfile("dummy-train-instances", fileext = ".txt")
    withr::local_file(train_instances_file)
    cat("1\n", file=train_instances_file)
    irace.cmdline(paste0(args, ' --debug-level 3 --parameter-file=', parameters_file,
                         ' --train-instances-dir=  --train-instances-file=', train_instances_file,
                         ' --target-runner=', target_runner_dummy))
  }
  test_that("--check", {
    expect_warning(
      run_cmdline(paste0('p_int  "--p_int " i (1, 10)\n',
                         'p_real "--p_real " r (1, 10)\n'),
                  '--check --max-experiments 500'),
      "No scenario file given")
  })
  test_that("--max-time", {
    expect_warning(
    expect_warning(
      run_cmdline(paste0('p_int  "--p_int " i (1, 10)\n',
                         'p_real "--p_real " r (1, 10)\n',
                         'time   "--time "  c (1)\n'),
                  '--max-time 2500'),
        "With the current settings and estimated time per run"),
      "No scenario file given")      
  })
  test_that("--capping", {
    expect_warning(
      expect_warning(
        run_cmdline(paste0('p_int   "--p_int "    i (1, 10)\n',
                           'p_real  "--p_real "   r (1, 10)\n',
                           'time    "--time "     c (1)\n',
                           'capping "--opt-time " c (1)\n'),
                    '--max-time 100 --bound-max 1 --capping 1 '),
        "With the current settings and estimated time per run"),
      "No scenario file given")      
  })
  test_that("Error cost time", {
    expect_error(
      expect_warning(
        run_cmdline(paste0('p_int   "--p_int "    i (1, 10)\n',
                           'p_real  "--p_real "   r (1, 10)\n'),
                    '--max-time 100 '),
        "No scenario file given"),
      "The output of targetRunner must be two numbers 'cost time'",
      fixed = TRUE)
  })
  test_that("low --max-experiments", {
    expect_warning(
    expect_error(
      run_cmdline(paste0('p_int  "--p_int " i (1, 10)\n',
                         'p_real "--p_real " r (1, 10)\n',
                         'time   "--time "  c (1)\n'),
                  '--max-experiments 50'),
        "With the current settings"),
      "No scenario file given")
  })
  test_that("--min-experiments", {
    expect_no_warning(
    expect_warning(
      run_cmdline(paste0('p_int  "--p_int " i (1, 10)\n',
                         'p_real "--p_real " r (1, 10)\n',
                         'time   "--time "  c (1)\n'),
                  '--min-experiments 50'),
      "No scenario file given"))
  })
  
}) # withr::with_output_sink()
