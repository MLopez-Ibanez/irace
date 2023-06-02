update_package_version <- function()
{
  dcf <- read.dcf(file = "DESCRIPTION")
  if (NROW(dcf) < 1L) return(NULL)
  dcf <- as.list(dcf[1L, ])
  packageversion <- dcf$Version
  git_rev <- dcf$RemoteSha
  if (is.null(git_rev))
    git_rev <- "unknown"
  git <- Sys.which("git")
  if (file.exists(".git") && length(git) > 0L
      && grepl("[0-9a-z]+$", system2(git, "describe --first-parent --always", stdout = TRUE), perl=TRUE)) {
    git_rev <- system2(git, "describe --dirty --first-parent --always --exclude '*'", stdout = TRUE)
  }
  if (git_rev != "unknown") {
    realversion <- paste0(packageversion, ".", git_rev)
    cat(file='./R/version.R', sep='',
"#' A character string containing the version of `irace` including git SHA.
#' @export
irace.version <- '", realversion, "'\n")
  }
  invisible()
}
update_package_version()
# We define this tentatively to avoid: undefined exports: irace.version
irace.version <- "unknown"
