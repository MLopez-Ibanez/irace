withr::with_output_sink("test-ablation.Rout", {
  skip_on_cran()
  withr::local_options(warn=2)
  logfile <- system.file(package="irace", mustWork = TRUE, "exdata", "sann.rda")
  outfile <- withr::local_tempfile(pattern = "log-ablation", fileext = ".Rdata")
    
  test_that("ablation('sann.rda')", {
    res <- ablation(logfile, ablationLogFile = outfile)
    expect_true(res$complete)
  })
  
  test_that("ablation('sann.rda') src,target as character", {
    src_file <- withr::local_tempfile(pattern="src", fileext=".txt", lines=c("tmax temp", "1 1"))
    target_file <- withr::local_tempfile(pattern="target", fileext=".txt", lines=c("tmax temp", "100 100"))
    res <- ablation(logfile, ablationLogFile = outfile, src = src_file, target = target_file)
    expect_true(res$complete)
  })

  test_that("--help", {
    expect_output(ablation_cmdline("--help"))
  })
  
  test_that("--log-file=sann.rda", {
    plotfile <- withr::local_tempfile(pattern = "ablation", fileext = ".pdf")
    res <- ablation_cmdline(paste0("--log-file=", logfile, " -o ", outfile, " -p ", plotfile))
    expect_true(res$complete)
  })

}) # withr::with_output_sink()
