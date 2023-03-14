README file for developers
===========================

Git commands
------------
```
# See differences in your local copy
git diff
# Commit your changes to the local repository
git commit -m "Explain your changes"
# Update your local copy with changes from remote repository (github)
git pull --rebase
# Send your changes to the remote repository
git push
# See differences between branches
git diff master
# Merge changes from master into your branch
git rebase -i master
# Get help
git help
```

Developing irace
----------------

* make build

  Build irace

* make install

  Build and install the package
  
* make cran

  Check that the build is acceptable by CRAN

* make check

  More thoroughly test the build

* make quick-install

  Do a minimal build and install that has as few requirements as
  possible. Designed for installing on machines that are not used for
  development, like IRIDIA's cluster.
  
  

CHECKING:
---------

* --as-cran messages of the type: 

  Undefined global functions or variables:
     median p.adjust pchisq qt read.table recover rexp rnorm runif t.test
     wilcox.test 

  can be fixed by running:
```R
imports_for_undefined_globals <-
function(txt, lst, selective = TRUE)
{
    if(!missing(txt))
        lst <- scan(what = character(), text = txt, quiet = TRUE)
    nms <- lapply(lst, find)
    ind <- sapply(nms, length) > 0L
    imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
    if(selective) {
        sprintf("importFrom(%s)",
                vapply(Map(c, names(imp), imp),
                       function(e)
                           paste0("\"", e, "\"", collapse = ", "),
                       ""))
    } else {
        sprintf("import(\"%s\")", names(imp))
    }
} 
```
```
R> txt <- "median p.adjust pchisq qt read.table recover rexp rnorm runif t.test
     wilcox.test" 
R> writeLines(imports_for_undefined_globals(txt, selective = FALSE))
```
And adding the result to NAMESPACE. Namespaces newly imported from also need to
be listed in the Imports: field of DESCRIPTION.


RELEASE Process:
----------------

TODO: See useful release steps here: https://github.com/tidyverse/ggplot2/issues/4965

1. git status # make sure you are up to date and clean

2. make check # passes

3. make releasecheck 

4. make examples # Takes a few hours. Inspect the examples in the vignette.

5. make revdepcheck # Takes a few hours

6. make releasebuild # Inspect the output for strange files!

7. make closeversion

8. Update `cran-comments.md`

9. make submit

11.a IF the package requires further changes:

  * Make the changes.

  * Repeat the whole RELEASE process above without bumping the version number.


11.b IF the package is released in CRAN:

  * Bump the version number in DESCRIPTION and NEWS.md.

  * make build # To update other files with the new version."

  * git ci -a -m " * Bump development version to $NEW_VERSION"

12. Announce the release in the Google group:

    https://groups.google.com/d/forum/irace-package


Announce email
==============

Dear irace users,

Version 2.2 of irace is available from CRAN:
https://cran.r-project.org/package=irace


There are significant bugfixes and only very minor interface changes so we recommend all users to upgrade:

NEWS entry

Best wishes,

    Manuel López-Ibáñez.
     

