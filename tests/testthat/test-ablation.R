withr::with_output_sink("test-ablation.Rout", {
  skip_on_cran()
  
  test_that("ablation('sann.rda')", {
    logfile <- system.file(package="irace", mustWork = TRUE, file.path("exdata","sann.rda"))
    outfile <- withr::local_tempfile(pattern = "log-ablation", fileext = ".Rdata")
    expect_no_warning(res <- ablation(logfile, ablationLogFile = outfile))
  })
  test_that("ablation('sann.rda') src,target as character", {
    logfile <- system.file(package="irace", mustWork = TRUE, file.path("exdata","sann.rda"))
    outfile <- withr::local_tempfile(pattern = "log-ablation", fileext = ".Rdata")
    src_file <- withr::local_tempfile(pattern="src", fileext=".txt", lines=c("tmax temp", "1 1"))
    target_file <- withr::local_tempfile(pattern="target", fileext=".txt", lines=c("tmax temp", "100 100"))
    expect_no_warning(res <- ablation(logfile, ablationLogFile = outfile, src = src_file, target = target_file))
  })
}) # withr::with_output_sink()
