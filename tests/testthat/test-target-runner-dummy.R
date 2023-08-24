withr::with_output_sink("test-target-runner-dummy.Rout", {
  skip_on_cran()
  get_target_runner_dummy <- function() {
    exe <- paste0("target-runner-dummy", if (system_os_is_windows()) ".exe" else "")
    p <- file.path(if (.Platform$r_arch == "") "bin" else file.path("bin", .Platform$r_arch), exe)
    res <- system.file(package="irace", mustWork = FALSE, p)
    if (res == "") {
      res <- test_path(file.path("../../src/dummy", exe))
    }
    res
  }
  target_runner_dummy <- get_target_runner_dummy()
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
}) # withr::with_output_sink()
