library(cli)
library(openssl)
library(fs)
library(withr)

# We have to do all this mess to get the PDF vignette from the package just
# build. Is there a better way?

# Most of it copied from: https://github.com/r-lib/pkgdown/blob/master/R/deploy-site.R
git <- function(..., echo_cmd = TRUE, echo = TRUE, error_on_status = TRUE) {
  invisible(processx::run("git", c(...), echo_cmd = echo_cmd, echo = echo, error_on_status = error_on_status))
}

github_worktree_add <- function(dir, remote, branch) {
  rule("Adding worktree", line = 1)
  git("worktree",
    "add",
    "--track", "-B", branch,
    dir,
    paste0(remote, "/", branch)
  )
}

github_worktree_remove <- function(dir) {
  rule("Removing worktree", line = 1)
  git("worktree", "remove", dir)
}

github_push <- function(dir, commit_message, remote, branch) {
  # force execution before changing working directory
  force(commit_message)

  rule("Commiting updated site", line = 1)

  withr::with_dir(dir, {
    git("add", "-A", ".")
    git("commit", "--allow-empty", "-m", commit_message)

    rule("Deploying to GitHub Pages", line = 1)
    git("remote", "-v")
    git("push", "--force", remote, paste0("HEAD:", branch))
  })
}

write_lines <- function(text, path) {
  base::writeLines(enc2utf8(text), path, useBytes = TRUE)
}

tarball = Sys.getenv("PKG_TARBALL")
ssh_id = Sys.getenv("id_rsa")
host = "github.com"
repo_slug = Sys.getenv("TRAVIS_REPO_SLUG")
ssh_id_file = "~/.ssh/id_rsa"
travis_build_number = Sys.getenv("TRAVIS_BUILD_NUMBER")
commit_message = sprintf("Regenerate user guide (build: %s) [skip ci].",
                         travis_build_number)

rule("Setting up SSH id", line = 1)
cat_line("Copying private key to: ", ssh_id_file)
write_lines(rawToChar(openssl::base64_decode(ssh_id)), ssh_id_file)
cat_line("Setting private key permissions to 0600")
fs::file_chmod(ssh_id_file, "0600")

cat_line("Setting remote to use the ssh url")

git("remote", "set-url", "origin", sprintf("git@%s:%s.git", host, repo_slug))

dest_dir <- fs::dir_create(fs::file_temp())

branch = "gh-pages"
remote = "origin"
# Explicitly set the branches tracked by the origin remote.
# Needed if we are using a shallow clone, such as on travis-CI
git("remote", "set-branches", remote, branch)
git("fetch", remote, branch)

github_worktree_add(dest_dir, remote, branch)

# Here we extract the vignette.
cat_line("Extracting irace-package.pdf")
untar(tarball, files = "irace/inst/doc/irace-package.pdf", verbose=TRUE,
      extras ="--strip-components=3", exdir=dest_dir)

github_push(dest_dir, commit_message, remote, branch)
rule("Deploy completed", line = 2)
github_worktree_remove(dest_dir)

#devtools::install()
install.packages("pkgdown")
pkgdown::deploy_site_github(run_dont_run = TRUE)

