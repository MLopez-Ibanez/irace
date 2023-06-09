withr::with_output_sink("test-ablation-cmdline.Rout", {
  skip_on_cran()
  
  test_that("--help", {
    ablation_cmdline("--help")
  })
  
  test_that("--log-file=sann.rda", {
    logfile <- system.file(package="irace", mustWork = TRUE, file.path("exdata","sann.rda"))
    ablation_cmdline(paste0("--log-file=", logfile))
  })
}) # withr::with_output_sink()
