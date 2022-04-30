#!/bin/sh
# The purpose of this script is to record the git SHA in the version
# number. Unfortunately, there does not seem to exist an easy way to run code
# during the build/install of an R package. Using a configure script fails when
# building in Windows. R sucks sometimes.

# A possible solution is to call this script during a github action that update and
# commits R/version.R.  This is problematic because this file produces
# conflicts and extra commits. It should not exist in the github repo.

# The current solution is to run this script manually when building the package
# on behalf of someone else. This is what the Makefile does.

# The ideal solution would be that R CMD build/install runs this script (or
# something equivalent in R) and places the R version file (or updates the
# DESCRIPTION) only in the built/installed package. I don't know how to do
# this.
LC_ALL=C
packageversion=`grep -F "Version: " DESCRIPTION | cut -f2 -d" "`
git_rev=`grep -q -F "RemoteSha: " DESCRIPTION | cut -f2 -d" "`

if test "$git_rev" = ""; then 
    git_rev="unknown"
fi

if test -e .git && (which git 1> /dev/null 2>&1) && (git describe --first-parent --always | grep -q "[0-9a-z]\+$"); then
    git checkout --quiet R/version.R  # Ignore changes to R/version.R
    git_rev=`git describe --dirty --first-parent --always --exclude "*"`
fi

if (test "$git_rev" != "unknown") || (test ! -r R/version.R) 2>/dev/null; then
    realversion=${packageversion}.${git_rev}
    R --slave -e "cat(file='./R/version.R', sep='', \"#' irace.version\n#'\n#' A character string containing the version of \`irace\`.\n#' @export\nirace.version <- '${realversion}'\n\")"
fi
