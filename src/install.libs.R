execs <- file.path("iracebin", c("irace","ablation"))
if (WINDOWS) execs <- paste0(execs, ".exe")
if (any(file.exists(execs))) {
  dest <- file.path(R_PACKAGE_DIR,  paste0('bin', R_ARCH))
  #print(dest)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(execs, dest, overwrite = TRUE)
} else {
  stop(execs, " not found!")
}
