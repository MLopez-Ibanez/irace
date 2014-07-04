

# An internal function to reload irace and set options for debugging
# errors. It may also be used to reload other packages.
# FIXME: Reload dynamic libraries? See ?dyn.load
irace.reload.debug <- function(package = "irace")
{
  pkg <- paste("package:", package, sep ="")
  try(detach(pkg, character.only = TRUE, unload = TRUE))
  library(package, character.only = TRUE)
  options(error=recover)
}

.irace.prefix <- "== irace == "

.irace.bug.report <-
  paste("An unexpected condition occurred.",
        "Please report this bug to the authors of the irace package <http://iridia.ulb.ac.be/irace>")

# Print a user-level fatal error message, when the calling context
# cannot help the user to understand why the program failed.
## FIXME: rename this function to irace.error
tunerError <- function(...)
{
  # The default is only 1000, which is too small. 8170 was the maximum
  # value allowed in R 2.14.1
  op <- options(warning.length = 8170)
  on.exit(options(op))
  stop (.irace.prefix, ..., call. = FALSE)
}

irace.assert <- function(exp)
{
  if (exp) return(invisible())
  mc <- match.call()[[2]]
  msg <- paste(deparse(mc), " is not TRUE\n", .irace.bug.report, sep = "")
  # FIXME: It would be great if we could save into a file the state of
  # the function that called this one.
  stop (msg)
  invisible()
}

file.check <- function (file, executable = FALSE, readable = executable,
                        isdir = FALSE, notempty = FALSE, text = NULL)
{
  EXEC <- 1 # See documentation of the function file.access()
  READ <- 4

  if (!is.character(file) || is.null.or.empty(file)) {
    tunerError (text, " ", shQuote(file), " is not a vaild filename")
  }
  ## Remove trailing slash if present for windows OS compatibility
  if (substring(file, nchar(file), nchar(file)) %in% c("/", "\\"))
    file <- substring(file, 1, nchar(file) - 1)
  
  if (!file.exists(file)) {
    tunerError (text, " '", file, "' does not exist")
    return(FALSE)
  }
  if (readable && (file.access(file, mode = READ) != 0)) {
    tunerError(text, " '", file, "' is not readable")
    return (FALSE)
  }
  if (executable && file.access(file, mode = EXEC) != 0) {
    tunerError(text, " '", file, "' is not executable")
    return (FALSE)
  }

  if (isdir) {
    if (!file.info(file)$isdir) {
      tunerError(text, " '", file, "' is not a directory")
      return (FALSE)
    }
    if (notempty && length(list.files (file, recursive=TRUE)) == 0) {
      tunerError(text, " '", file, "' does not contain any file")
      return (FALSE)
    }
  } else if (file.info(file)$isdir) {
    tunerError(text, " '", file, "' is a directory, not a file")
    return (FALSE)
  }
  return (TRUE)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
  { abs(x - round(x)) < tol }

is.null.or.na <- function(x)
{
  is.null(x) || (length(x) == 1 && suppressWarnings(is.na(x)))
}

is.null.or.empty <- function(x)
{
  is.null(x) || (length(x) == 1 && x == "")
}

strcat <- function(..., collapse = NULL)
  do.call(paste, args=list(..., sep="", collapse = collapse))

# FIXME: Isn't a R function to do this? More portable?
canonical.dirname <- function(dirname = stop("required parameter"))
{
  return (sub ("([^/])$", "\\1/", dirname))
}

# Function to convert a relative to an absolute path. CWD is the
# working directory to complete relative paths. It tries really hard
# to create canonical paths.
## FIXME: This needs to be tested on Windows.
# The following code can be used to test this function.
## test <- function(testcases) {
##   cwd <- getwd()
##   on.exit(setwd(cwd))
##   setwd("/tmp")
##   testcases <- read.table(text='
## "."                          "/tmp"
## ".."                         "/"
## "../"                        "/"
## "../."                       "/"
## "../.."                     "/"
## "../../"                     "/"
## "../../x.r"                  "/x.r"
## "../leslie/"                 "/leslie"
## "../leslie/x.r"              "/leslie/x.r"
## "../x.r"                     "/x.r"
## "..irace"                    "/tmp/..irace"
## "./"                         "/tmp"
## "./."                        "/tmp"
## "././x.r"                    "/tmp/x.r"
## "./irace/../x.r"             "/tmp/x.r"
## "./x.r"                      "/tmp/x.r"
## ".x.R"                       "/tmp/.x.R"
## "/./x.r"                     "/x.r"
## "/home"                      "/home"
## "/home/leslie/././x.r"       "/home/leslie/x.r"
## "/home/leslie/~/x.r"         "/home/leslie/~/x.r"
## "/~/x.r"                     "/~/x.r"
## "e:/home/leslie/x.r"         "e:/home/leslie/x.r"
## "leslie/leslie/../../irace"  "/tmp/irace"
## "x.r"                        "/tmp/x.r"
## "~/../x.r"                   "/home/x.r"
## "~/irace/../../x.r"          "/home/x.r"
## "~/x.r"                      "~/x.r"
## ', stringsAsFactors=FALSE)
##   for(i in 1:nrow(testcases)) {
##     orig <- testcases[i,1]
##     res <- path.rel2abs(testcases[i,1])
##     exp <- path.expand(testcases[i,2])
##     if (res == exp) {
##       cat("[OK] ", orig, " -> ", res, "\n", sep="")
##     } else {
##       stop("[FAILED] ", orig, " -> ", res, " but expected: ", exp, "\n")
##     }
##   }
## }
## options(error=dump.frames)
## test(testcases)
path.rel2abs <- function (path, cwd = getwd())
{
  if (is.null.or.na(path)) {
    return (NULL)
  } else if (path == "") {
    return ("")
  }

  # Possibly expand ~/path to /home/user/path.
  path <- path.expand(path)

  filename <- basename(path)
  path <- dirname(path)
  s <- .Platform$file.sep
  if (path == ".") {
    if (filename == ".") { # This is the current directory
      return (cwd)
      # We handle the case ".." later.
    } else if (filename != "..") { # This is a file in the current directory
      return(strcat(cwd, s, filename))
    }
  }
  # else
  
  # Prefix the current cwd to the path if it doesn't start with "c:\" or /
  reg.exp <- strcat("^", s, "|^[A-Za-z]:", s)
  if (!grepl(reg.exp, path))
    path <- strcat(cwd, s, path)

  # Change "/./" to "/" to get a canonical form 
  path <- gsub(strcat(s, ".", s), s, path, fixed = TRUE)

  # Change "/.$" to "/" to get a canonical form 
  path <- sub(strcat(s, "\\.$"), s, path)

  # Change "/x/../" to "/" to get a canonical form 
  prevdir.regex <- strcat(s, "[^", s,"]+", s, "\\.\\.")
  repeat {
    # We need to do it one by one so "a/b/c/../../../" is not converted to "a/b/../"
    tmp <- sub(strcat(prevdir.regex, s), s, path)
    if (tmp == path) break
    path <- tmp
  }
  # Handle "/something/..$" to "/" that is, when ".." is the last thing in the path.
  path <- sub(strcat(prevdir.regex, "$"), s, path)

  # Handle "^/../../.." to "/" that is, going up at the root just returns the root.
  repeat {
    # We need to do it one by one so "a/b/c/../../../" is not converted to "a/b/../"
    tmp <- sub(strcat("^", s, "\\.\\.", s), s, path)
    if (tmp == path) break
    path <- tmp
  }
  # Handle "^/..$" to "/" that is, when ".." is the last thing in the path.
  path <- sub(strcat("^", s, "\\.\\.$"), s, path)

  # It may happen that path ends in "/", for example, for "/x". Do
  # not add another "/"
  if (filename != ".") {
    last <- substr(path, nchar(path), nchar(path))
    if (last == s) {
      path <- strcat(path, filename)
    } else {
      path <- strcat(path, s, filename)
    }
  }
  # We use normalizePath, which will further simplify the path if
  # the path exists.
  return (suppressWarnings(normalizePath(path, mustWork = NA)))
}

is.function.name <- function(FUN)
{
  # FIXME: Is there a simpler way to do this check?
  is.function(FUN) ||
  (!is.null(FUN) && !is.na(FUN) && as.character(FUN) != "" &&
   !is.null(mget(as.character(FUN), envir = as.environment(-1),
                 mode = "function", ifnotfound = list(NULL),
                 inherits = TRUE)[[1]]))
}

trim.leading <- function(str)
{
  return (sub('^[[:space:]]+', '', str)) ## white space, POSIX-style
}
trim.trailing <- function(str)
{
  return (sub('[[:space:]]+$', '', str)) ## white space, POSIX-style
}
# remove leading and trailing white space characters
trim <- function(str)
{
  return (trim.trailing(trim.leading(str)))
}

isFixed <- function (paramName, parameters)
{
  return (as.logical(parameters$isFixed[paramName]))
}

oneParamBoundary <- function (paramName, parameters)
{
  return (parameters$boundary[[paramName]])
}

oneParamLowerBound <- function (paramName, parameters)
{
  return (as.numeric(parameters$boundary[[paramName]][1]))
}

oneParamUpperBound <- function (paramName, parameters)
{
  return (as.numeric(parameters$boundary[[paramName]][2]))
}

nbParam <- function (parameters)
{
  return (length(parameters$names))
}


## extractElites
# Input: the candidates with the .RANK. field filled.
#        the number of elites wished
# Output: nbElites elites, sorted by ranks, with the weights assigned.
extractElites <- function(candidates, nbElites)
{
  if (nbElites < 1) {
    ## FIXME: Should this be an error or should we handle it in some other way?
    stop("nbElites is lower or equal to zero.") 
  }
  # Sort by rank.
  elites <- candidates[order(as.numeric(candidates$.RANK.)), , drop = FALSE]
  elites <- elites[1:nbElites, , drop = FALSE]
  elites[, ".WEIGHT."] <- ((nbElites - (1:nbElites) + 1)
                           / (nbElites * (nbElites + 1) / 2))
  return (elites)
}

## Keep only parameters values
removeCandidatesMetaData <- function(candidates)
{
  # Meta-data colnames begin with "."
  return (candidates[, grep("^\\.", colnames(candidates), invert=TRUE),
                     drop = FALSE])
}

candidates.print <- function(cand, metadata = FALSE)
{
  rownames(cand) <- cand$.ID.
  if (!metadata) {
    cand <- removeCandidatesMetaData(cand)
  } 
  print(as.data.frame(cand, stringsAsFactors = FALSE))
}

candidates.print.command <- function(cand, parameters)
{
  if (nrow(cand) <= 0) return(invisible())
  rownames(cand) <- cand$.ID.
  cand <- removeCandidatesMetaData(cand)
  print(data.frame(command =
                   apply(cand[,unlist(parameters$names), drop = FALSE],
                         1, buildCommandLine, switches = parameters$switches),
                   stringsAsFactors = FALSE))
}


# FIXME: This may not work when working interactively. For example,
# one cannot change the number of slaves.  A more robust function
# would try to close any open slaves, and then re-spawn a different
# number.
mpiInit <- function(nslaves, debugLevel = 0)
{
  # Load the Rmpi package if it is not already loaded.
  if (!is.loaded("mpi_initialize")) {
    if (! require("Rmpi", quietly = TRUE))
      tunerError("The `Rmpi' package is required for using MPI.")

    # FIXME: We should do this when irace finalizes.
    # When R exits, finalize MPI.
    reg.finalizer(environment(Rmpi::mpi.exit), function(e) {
      # Rmpi already prints a message, so we don't need this.
      # cat("# Finalize MPI...\n")
      if (Rmpi::mpi.comm.size(1) > 0)
        # FIXME: dellog == TRUE tries to delete log files, but it does
        # not take into account that we may have change directory and
        # it does not fails gracefully but produces an annoying:
        # Warning message: running command 'ls *.30577+1.*.log 2>/dev/null' had status 2
        Rmpi::mpi.close.Rslaves(dellog = FALSE)
      # FIXME: How to avoid the message
      # "Rmpi cannot be used unless relaunching R" ?
      Rmpi::mpi.exit()
    }, onexit = TRUE)

    # Create slaves
    Rmpi::mpi.spawn.Rslaves(nslaves = nslaves, quiet = (debugLevel == 0))
  }
}
