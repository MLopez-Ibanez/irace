withr::with_output_sink("test-get-functions.Rout", {

  test_that("getConfigurationById()", {
    log <- read_logfile(system.file(package="irace", "exdata", "irace-acotsp.Rdata", mustWork=TRUE))
    ids <- sample(log$allConfigurations[[".ID."]], size=3)
    ids <- c(ids, rev(ids))
    sel_ids <- getConfigurationById(log, ids = ids)[[".ID."]]
    expect_identical(sel_ids, ids)
})

})
