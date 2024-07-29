update_package_version <- function()
{
  dcf <- read.dcf(file = "DESCRIPTION", fields=c("Version", "RemoteSha"))
  if (NROW(dcf) < 1L)
    return(invisible())
  dcf <- as.list(dcf[1L, ])
  git_rev <- dcf$RemoteSha
  if (is.na(git_rev))
    git_rev <- "unknown"
  git <- Sys.which("git")
  if (nchar(git) > 0L && fs::file_exists(".git")
    && grepl("[0-9a-z]+$", system2(git, "describe --first-parent --always", stdout = TRUE), perl=TRUE)) {
    git_rev <- system2(git, "describe --dirty --first-parent --always --exclude '*'", stdout = TRUE)
  }
  if (git_rev != "unknown") {
    realversion <- paste0(dcf$Version, ".", git_rev)
    cat(file='./R/version.R', sep='',
"#' A character string containing the version of `irace` including git SHA.\n#' @export\nirace_version <- '", realversion, "'\n")
  }
  invisible()
}
update_package_version()
# We define this tentatively to avoid: undefined exports: irace_version
irace_version <- "unknown"
