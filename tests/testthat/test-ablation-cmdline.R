withr::with_output_sink("test-ablation-cmdline.Rout", {
  skip_on_cran()
  
  test_that("--help", {
    expect_no_warning(ablation_cmdline("--help"))
  })
  
  test_that("--log-file=sann.rda", {
    logfile <- system.file(package="irace", mustWork = TRUE, file.path("exdata","sann.rda"))
    outfile <- tempfile(pattern = "log-ablation", fileext = ".Rdata")
    plotfile <- tempfile(pattern = "ablation", fileext = ".pdf")
    expect_no_warning(
      ablation_cmdline(paste0("--log-file=", logfile, " -o ", outfile, " -p ", plotfile)))
  })
}) # withr::with_output_sink()
