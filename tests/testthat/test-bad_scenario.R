withr::with_output_sink("test-bad_scenario.Rout", {
  test_that("bad scenario: unknown variables", {
    skip_on_cran()
    bad_scenario <- test_path("bad_scenario.txt")
    expect_error(irace_cmdline(paste0("--scenario ", bad_scenario)), "unknown variables")
    good_scenario <- test_path("good_scenario.txt")
    expect_error(irace_cmdline(paste0("--scenario ", good_scenario, " --unknown")), "Unknown command-line options: --unknown")
  })
})
