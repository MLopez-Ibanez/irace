files <- c(file.path("iracebin", c("irace","ablation")),
  file.path("dummy", "target-runner-dummy"))

file_move <- function(from, to) {
  file.copy(from = from, to = to, overwrite = TRUE)
  file.remove(from)
}

if (WINDOWS) files <- paste0(files, ".exe")
if (any(file.exists(files))) {
  dest <- file.path(R_PACKAGE_DIR,  paste0('bin', R_ARCH))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file_move(files, dest)
} else {
  stop(files, " not found!")
}
