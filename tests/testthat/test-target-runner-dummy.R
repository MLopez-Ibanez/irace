withr::with_output_sink("test-target-runner-dummy.Rout", {

  target_runner_dummy <- system.file(package="irace",
                                     file.path("bin", paste0("target-runner-dummy",
                                                             if (system_os_is_windows()) ".exe" else "")))

  run_cmdline <- function(parameters, args) {
    parameters_file <- "dummy-parameters.txt"
    on.exit(file.remove(parameters_file), add=TRUE)
    cat(parameters, file=parameters_file)
    train_instances_file <- "dummy-train-instances.txt"
    on.exit(file.remove(train_instances_file), add=TRUE)
    cat("1\n", file=train_instances_file)
    irace.cmdline(paste0(args, ' --parameter-file=', parameters_file,
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
                '--max-time 50')
  })
}) # withr::with_output_sink()
