context("bug")
skip_on_cran()
withr::with_output_sink("test-bad_scenario.Rout", {
  expect_error(irace.cmdline("--scenario bad_scenario.txt"), "unknown variables")
})

