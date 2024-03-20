#!/usr/bin/env Rscript
# Check reverse dependencies
remotes::install_github('r-lib/revdepcheck', upgrade="never")
require("revdepcheck")
#revdep_check(num_workers = 2L, bioc=FALSE)
revdep_check(num_workers = 1L, quiet = FALSE, bioc=FALSE)
revdep_report()
cat("*** Summary ***\n")
revdep_summary()
