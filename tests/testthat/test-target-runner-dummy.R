withr::with_output_sink("test-target-runner-dummy.Rout", {
  skip_on_cran()
  get_target_runner_dummy <- function() {
    p <- file.path(if (.Platform$r_arch == "") "bin" else file.path("bin", .Platform$r_arch),
                   paste0("target-runner-dummy", if (system_os_is_windows()) ".exe" else ""))
    system.file(package="irace", mustWork = TRUE, p)
  }
  target_runner_dummy <- get_target_runner_dummy()
  
  run_cmdline <- function(parameters, args) {
    parameters_file <- "dummy-parameters.txt"
    on.exit(file.remove(parameters_file), add=TRUE)
    cat(parameters, file=parameters_file)
    train_instances_file <- "dummy-train-instances.txt"
    on.exit(file.remove(train_instances_file), add=TRUE)
    cat("1\n", file=train_instances_file)
    irace.cmdline(paste0(args, ' --debug-level 3 --parameter-file=', parameters_file,
                         ' --train-instances-dir=  --train-instances-file=', train_instances_file,
                         ' --target-runner=', target_runner_dummy))
  }
  test_that("--check", {
    run_cmdline(paste0('p_int  "--p_int " i (1, 10)\n',
                       'p_real "--p_real " r (1, 10)\n'),
                '--check --max-experiments 500')
  })
  test_that("--max-time", {
    run_cmdline(paste0('p_int  "--p_int " i (1, 10)\n',
                       'p_real "--p_real " r (1, 10)\n',
                       'time   "--time "  c (1)\n'),
                '--max-time 2500')
  })
  test_that("--capping", {
    expect_warning(
      run_cmdline(paste0('p_int   "--p_int "    i (1, 10)\n',
                         'p_real  "--p_real "   r (1, 10)\n',
                         'time    "--time "     c (1)\n',
                         'capping "--opt-time " c (1)\n'),
                  '--max-time 100 --bound-max 1 --capping 1 '))
  })
  test_that("Error cost time", {
    expect_error(
      run_cmdline(paste0('p_int   "--p_int "    i (1, 10)\n',
                         'p_real  "--p_real "   r (1, 10)\n'),
                  '--max-time 100 '),
      "The output of targetRunner must be two numbers 'cost time'",
      fixed = TRUE)
  })
}) # withr::with_output_sink()
