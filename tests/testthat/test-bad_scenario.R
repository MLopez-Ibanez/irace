withr::with_output_sink("test-bad_scenario.Rout", {
  test_that("bad scenario: unknown variables", {
    skip_on_cran()
    expect_error(irace.cmdline("--scenario bad_scenario.txt"), "unknown variables")
    expect_error(irace.cmdline("--scenario good_scenario.txt --unknown"), "Unknown command-line options: --unknown")
  })
})
