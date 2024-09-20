withr::with_output_sink("test-target-runner-dummy.Rout", {
  skip_on_cran()

  get_executable <- function(filename) {
    filename <- paste0(filename, if (system_os_is_windows()) ".exe" else "")
    p <- if (.Platform$r_arch == "") file.path( "bin", filename) else file.path("bin", .Platform$r_arch, filename)
    system.file(p, package="irace", mustWork = FALSE)
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
    iraceexe <- get_executable("irace")
    skip_if_not(nzchar(iraceexe), "Not run because 'irace' is not installed")
    expect_true(file.exists(iraceexe))
    # FIXME: For some reason, this does not generate any output on Windows
    output <- expect_silent(system2(iraceexe, "--help", stdout = TRUE, stderr = TRUE))
    skip_on_os("windows")
    expect_match(paste(collapse="", output),
                 "irace: An implementation in R of.*called with: --help")
  })

  test_that("ablation exe works", {
    ablationexe <- get_executable("ablation")
    skip_if_not(nzchar(ablationexe), "Not run because 'ablation' is not installed")
    expect_true(file.exists(ablationexe))
    # FIXME: For some reason, this does not generate any output on Windows
    output <- expect_silent(system2(ablationexe, "--help", stdout = TRUE, stderr = TRUE))
    skip_on_os("windows")
    expect_match(paste(collapse="", output),
      "ablation: An implementation in R of Ablation Analysis.*called with: --help")
  })

  run_cmdline <- function(parameters, args) {
    parameters_file <- tempfile("dummy-parameters", fileext = ".txt")
    withr::local_file(parameters_file)
    cat(parameters, file=parameters_file)
    train_instances_file <- tempfile("dummy-train-instances", fileext = ".txt")
    withr::local_file(train_instances_file)
    cat("1\n", file=train_instances_file)
    irace_cmdline(paste0(args, ' --debug-level 3 --parameter-file=', parameters_file,
                         ' --train-instances-dir=  --train-instances-file=', train_instances_file,
                         ' --target-runner=', target_runner_dummy))
  }

  target_runner_dummy <- get_executable("target-runner-dummy")
  skip_if_not(nzchar(target_runner_dummy), "Not run because 'target-runner-dummy' is not installed")
  expect_true(file.exists(target_runner_dummy))
  
  test_that("--check", {
    expect_warning(
      run_cmdline(paste0('p_int  "--p_int " i (1, 10)\n',
                         'p_real "--p_real " r (1, 10)\n'),
                  '--check --max-experiments 500'),
      "No scenario file given")
  })
  test_that("--max-time", {
    expect_warning(
      run_cmdline(paste0('p_int  "--p_int " c (1)\n',
        'p_real "--p_real " r (0, 1)\n',
        'dummy1 "--dummy1 " r (0, 1)\n',
        'time   "--time "  c (1)\n'),
        '--max-time 2500'),
      "No scenario file given")
  })
  test_that("boundMax is too large", {
    expect_warning(
      expect_warning(
        run_cmdline(paste0('p_int   "--p_int "    i (1, 10)\n',
                           'p_real  "--p_real "   r (0, 1)\n',
                           'time    "--time "     c (1)\n',
                           'capping "--opt-time " c (1)\n'),
                    '--max-time 1000 --bound-max 100 --capping 1'),
        "is too large"),
      "No scenario file given")      
  })
  test_that("--capping", {
    expect_warning(
      expect_warning(
        run_cmdline(paste0('p_int   "--p_int "    i (1, 10)\n',
                           'p_real  "--p_real "   r (1, 10)\n',
                           'dummy1 "--dummy1 " r (1, 10)\n',
                           'dummy2 "--dummy2 " r (1, 10)\n',
                           'dummy3 "--dummy3 " r (1, 10)\n',
                           'dummy4 "--dummy4 " r (1, 10)\n',
                           'dummy5 "--dummy5 " r (1, 10)\n',
                           'dummy6 "--dummy6 " r (1, 10)\n',
                           'dummy7 "--dummy7 " r (1, 10)\n',
                           'dummy8 "--dummy8 " r (1, 10)\n',
                           'dummy9 "--dummy9 " r (1, 10)\n',
                           'dummy11 "--dummy11 " r (1, 10)\n',
                           'dummy12 "--dummy12 " r (1, 10)\n',
                           'dummy13 "--dummy13 " r (1, 10)\n',
                           'dummy14 "--dummy14 " r (1, 10)\n',
                           'dummy15 "--dummy15 " r (1, 10)\n',
                           'dummy16 "--dummy16 " r (1, 10)\n',
                           'dummy17 "--dummy17 " r (1, 10)\n',
                           'dummy18 "--dummy18 " r (1, 10)\n',
                           'dummy19 "--dummy19 " r (1, 10)\n',
                           'time    "--time "     c (1)\n',
                           'capping "--opt-time " c (1)\n'),
                    '--max-time 500 --bound-max 1 --capping 1 '),
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
