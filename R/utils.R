# An internal function to reload irace and set options for debugging
# errors. It may also be used to reload other packages.
# FIXME: Reload dynamic libraries? See ?dyn.load
irace.reload.debug <- function(package = "irace")
{
  pkg <- paste("package:", package, sep ="")
  try(detach(pkg, character.only = TRUE, unload = TRUE))
  library(package, character.only = TRUE)
  options(error = if (interactive()) utils::recover else
          quote(utils::dump.frames("iracedump", TRUE)))
}

.irace.prefix <- "== irace == "

.irace.bug.report <-
  paste("An unexpected condition occurred.",
        "Please report this bug to the authors of the irace package <http://iridia.ulb.ac.be/irace>")

irace.print.memUsed <- function(objects)
{
  envir <- parent.frame()
  if (missing(objects)) {
    objects <- ls(envir = envir, all.names = TRUE)
  }
  x <- sapply(objects, function(name)
              object.size(get(name, envir = envir)) / 1024)

  objects <- ls(envir = .irace)
  y <- sapply(objects, function(name)
              object.size(get(name, envir = .irace)) / 1024)
  names(y) <- paste0(".irace$", names(y))
  x <- c(x, y)

  # Do not print anything that is smaller than 32 Kb
  x <- x[x > 32]
  cat(sep="", sprintf("%30s : %17.1f Kb\n", names(x), x))
  cat(sep="", sprintf("%30s : %17.1f Mb\n", "Total", sum(x) / 1024))
}

# Print a user-level fatal error message, when the calling context
# cannot help the user to understand why the program failed.
irace.error <- function(...)
{
  # The default is only 1000, which is too small. 8170 is the maximum
  # value allowed up to R 3.0.2
  op <- options(warning.length = 8170)
  on.exit(options(op))
  stop (.irace.prefix, ..., call. = FALSE)
}

## utils::dump.frames is broken and cannot be used with bquote, so we need a wrapper.
## See help(dump.frames)
irace.dump.frames <- function()
{
  execDir <- getOption(".irace.execdir")
  if (!is.null(execDir)) {
    cwd <- setwd(execDir)
    on.exit(setwd(cwd), add = TRUE)
  }
  ## Only a very recent R version allows saving GlovalEnv:
  ## https://stat.ethz.ch/pipermail/r-devel/2016-November/073378.html
  # utils::dump.frames(dumpto = "iracedump", to.file = TRUE, include.GlobalEnv = TRUE)
  ## For now, we use the following work-around:
  ## http://stackoverflow.com/questions/40421552/r-how-make-dump-frames-include-all-variables-for-later-post-mortem-debugging
  utils::dump.frames(dumpto = "iracedump")
  save.image(file = "iracedump.rda")
}

irace.assert <- function(exp)
{
  if (exp) return(invisible())
  mc <- match.call()[[2]]
  msg <- paste(deparse(mc), " is not TRUE\n", .irace.bug.report, sep = "")
  # FIXME: It would be great if we could save into a file the state of
  # the function that called this one.
  traceback(1)
  op <- options(warning.length = 8170,
                error = if (interactive()) utils::recover
                        else irace.dump.frames)
  on.exit(options(op))
  stop (msg)
  invisible()
}

irace.note <- function(...)
{
  cat ("# ", format(Sys.time(), usetz=TRUE), ": ",
       paste(..., sep = "", collapse = ""), sep = "")
}

file.check <- function (file, executable = FALSE, readable = executable,
                        isdir = FALSE, notempty = FALSE, text = NULL)
{
  EXEC <- 1 # See documentation of the function file.access()
  READ <- 4

  if (!is.character(file) || is.null.or.empty(file)) {
    irace.error (text, " ", shQuote(file), " is not a vaild filename")
  }
  ## Remove trailing slash if present for windows OS compatibility
  if (substring(file, nchar(file), nchar(file)) %in% c("/", "\\"))
    file <- substring(file, 1, nchar(file) - 1)
  
  if (!file.exists(file)) {
    irace.error (text, " '", file, "' does not exist")
    return(FALSE)
  }
  if (readable && (file.access(file, mode = READ) != 0)) {
    irace.error(text, " '", file, "' is not readable")
    return (FALSE)
  }
  if (executable && file.access(file, mode = EXEC) != 0) {
    irace.error(text, " '", file, "' is not executable")
    return (FALSE)
  }

  if (isdir) {
    if (!file.info(file)$isdir) {
      irace.error(text, " '", file, "' is not a directory")
      return (FALSE)
    }
    if (notempty && length(list.files (file, recursive=TRUE)) == 0) {
      irace.error(text, " '", file, "' does not contain any file")
      return (FALSE)
    }
  } else if (file.info(file)$isdir) {
    irace.error(text, " '", file, "' is a directory, not a file")
    return (FALSE)
  }
  return (TRUE)
}

# Returns the smallest multiple of d that is higher than or equal to x.
round.to.next.multiple <- function(x, d)
  return(x + d - 1 - (x - 1) %% d)

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
{
  abs(x - round(x)) < tol
}

is.null.or.na <- function(x)
{
  is.null(x) || (length(x) == 1 && suppressWarnings(is.na(x)))
}

is.null.or.empty <- function(x)
{
  is.null(x) || (length(x) == 1 && is.character(x) && x == "")
}

strcat <- function(...)
{
  do.call(paste0, args = list(..., collapse = NULL))
}

# FIXME: Isn't there an R function to do this? More portable?
canonical.dirname <- function(dirname)
{
  if (missing(dirname))
    stop ("argument 'dirname' is required")
  ## FIXME: Perhaps this is better?
  # s <- .Platform$file.sep
  # return(sub(paste0(s,"?$"), s, dirname))
  return (sub ("([^/])$", "\\1/", dirname))
}

# Function to convert a relative to an absolute path. CWD is the
# working directory to complete relative paths. It tries really hard
# to create canonical paths.
## FIXME: This needs to be tested on Windows.
path.rel2abs <- function (path, cwd = getwd())
{
  if (is.null.or.na(path)) {
    return (NULL)
  } else if (path == "") {
    return ("")
  }

  s <- .Platform$file.sep

  # Possibly expand ~/path to /home/user/path.
  path <- path.expand(path)
  filename <- basename(path)
  path <- dirname(path)

  # Prefix the current cwd to the path if it doesn't start with "c:\" or /
  reg.exp <- strcat("^", s, "|^[A-Za-z]:", s)
  if (path == "." || !grepl(reg.exp, path)) {
    # There is no need to normalize cwd if it was returned by getwd()
    if (!missing(cwd)) {
      # Change "//" to "/" to get a canonical form
      cwd <- gsub(strcat(s, s, "+"), s, cwd)
      # Drop final '/' if any
      cwd <- sub(strcat(s, "$"), "", cwd)
      if (substr(cwd, 0, 1) == ".") {
        # Recurse to get absolute cwd
        cwd <- path.rel2abs(cwd)
      }
    }
    if (path == ".") {
      if (filename == ".") { # This is the current directory
        return (suppressWarnings(normalizePath(cwd, mustWork = NA)))
        # We handle the case ".." later.
      } else if (filename != "..") { # This is a file in the current directory
        path <- strcat(cwd, s, filename)
        return(suppressWarnings(normalizePath(path, mustWork = NA)))
      }
    }
    path <- strcat(cwd, s, path)
  }
  # else

  # Change "//" to "/" to get a canonical form 
  path <- gsub(strcat(s, s, "+"), s, path)

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
    # Drop final '/' if any
    path <- sub(strcat(s, "?$"), strcat(s, filename), path)
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

# This function is used to trim potentially large strings for printing, since
# the maximum error/warning length is 8170 characters (R 3.0.2)
strlimit <- function(str, limit = 5000)
{
  if (nchar(str) > limit) {
    return(paste0(substr(str, 1, limit - 3), "..."))
  }
  return(str)
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

paramDomain <- function (paramName, parameters)
{
  return (parameters$domain[[paramName]])
}

paramLowerBound <- function (paramName, parameters)
{
  return (as.numeric(parameters$domain[[paramName]][1]))
}

paramUpperBound <- function (paramName, parameters)
{
  return (as.numeric(parameters$domain[[paramName]][2]))
}

nbParam <- function (parameters)
{
  return (length(parameters$names))
}

## This function takes two matrices x and y and merges them such that the
## resulting matrix z has:
# rownames(z) <- setunion(rownames(x), rownames(y)) and 
# rownames(z) <- setunion(rownames(x), rownames(y)) and
# z[rownames(x), colnames(x)] <- x and z[rownames(y), colnames(y)] <- y, and
# z[i, j] <- NA for all i,j not in x nor y.
merge.matrix <- function(x, y)
{
  new.cols <- setdiff(colnames(y), colnames(x))
  new.rows <- setdiff(rownames(y), rownames(x))

  if (is.null(rownames(x)) || is.null(colnames(x)))
    return(y)

  if (is.null(rownames(y)) || is.null(colnames(y)))
    return(x)

  # Add columns
  x <- cbind(x,
             matrix(NA, ncol = length(new.cols), nrow = nrow(x),
                    dimnames = list(rownames(x), new.cols)))
  # Add rows
  x <- rbind(x,
             matrix(NA, ncol = ncol(x), nrow = length(new.rows),
                    dimnames = list(new.rows, colnames(x))))
  # Update
  x[rownames(y), colnames(y)] <- y
  return(x)
}

## extractElites
# Input: the configurations with the .RANK. field filled.
#        the number of elites wished
# Output: nbElites elites, sorted by ranks, with the weights assigned.
extractElites <- function(configurations, nbElites)
{
  if (nbElites < 1) {
    ## FIXME: Should this be an error or should we handle it in some other way?
    irace.error("nbElites is lower or equal to zero.") 
  }
  # Sort by rank.
  elites <- configurations[order(as.numeric(configurations$.RANK.)), , drop = FALSE]
  elites <- elites[1:nbElites, , drop = FALSE]
  elites[, ".WEIGHT."] <- ((nbElites - (1:nbElites) + 1)
                           / (nbElites * (nbElites + 1) / 2))
  return (elites)
}

## Keep only parameters values
removeConfigurationsMetaData <- function(configurations)
{
  # Meta-data colnames begin with "."
  return (configurations[, grep("^\\.", colnames(configurations), invert = TRUE),
                     drop = FALSE])
}

configurations.print <- function(configuration, metadata = FALSE)
{
  rownames(configuration) <- configuration$.ID.
  if (!metadata) {
    configuration <- removeConfigurationsMetaData(configuration)
  } 
  print(as.data.frame(configuration, stringsAsFactors = FALSE))
}

configurations.print.command <- function(configuration, parameters)
{
  if (nrow(configuration) <= 0) return(invisible())
  ids <- as.numeric(configuration$.ID.)
  configuration <- removeConfigurationsMetaData(configuration)
  # Re-sort the columns
  configuration <- configuration[, parameters$names, drop = FALSE]
  # A better way to do this? We cannot use apply() because that coerces
  # to a character matrix thus messing up numerical values.
  len <- nchar(max(ids))
  for (i in seq_len (nrow(configuration))) {
    cat(sprintf("%-*d %s\n", len, ids[i],
                buildCommandLine(configuration[i, , drop=FALSE], parameters$switches)))
  }
}


# FIXME: This may not work when working interactively. For example,
# one cannot change the number of slaves.  A more robust function
# would try to close any open slaves, and then re-spawn a different
# number.
##
# FIXME2: Slaves will load the irace namespace independently of how the master
# loaded it. This can be seen by doing in the slaves:
#
## print(loadedNamespaces())
## try(print(as.list(get(".__NAMESPACE__.", envir = asNamespace("irace", base.OK = FALSE),
##                       inherits = FALSE))$path))
## try(print(path.package("irace")))     
#
# That is, neither R_LIBS, .libPaths or whether library was called with lib.loc
# will affect the slaves. It also happens before we can set those variables on
# the slaves. Thus, one may end up running a different version of irace on the
# slaves than on the master. I wasted more than 12 hours trying to find a
# work-around but nothing seems to work.
mpiInit <- function(nslaves, debugLevel = 0)
{
  # Load the Rmpi package if it is not already loaded.
  if (! ("Rmpi" %in% loadedNamespaces())) {
    if (! suppressPackageStartupMessages
        (requireNamespace("Rmpi", quietly = TRUE)))
      irace.error("The 'Rmpi' package is required for using MPI")
    
    # When R exits, finalize MPI.
    reg.finalizer(environment(Rmpi::mpi.exit), function(e) {
      # Rmpi already prints a message, so we don't need this.
      # cat("# Finalize MPI...\n")
      if (Rmpi::mpi.comm.size(1) > 0)
        # FIXME: dellog == TRUE tries to delete log files, but it does
        # not take into account that we may have changed directory and
        # it does not fail gracefully but produces an annoying:
        # Warning message: running command 'ls *.30577+1.*.log 2>/dev/null' had status 2
        Rmpi::mpi.close.Rslaves(dellog = FALSE)
      # We would like to use .Call("mpi_finalize", PACKAGE = "Rmpi"), which is
      # what mpi.finalize does, minus the annoying message: "Exiting Rmpi. Rmpi
      # cannot be used unless relaunching R", which we do not care about
      # because this finalizer should only be called when exiting R.
      capture.output(Rmpi::mpi.finalize(),
                     file = if (.Platform$OS.type == 'windows') 'NUL' else '/dev/null')
    }, onexit = TRUE)

    # Create slaves
    #  needlog: a logical. If TRUE, R BATCH outputs will be saved in log
    #           files.  If FALSE, the outputs will send to /dev/null.
    #  quiet: a logical. If TRUE, do not print anything unless an error occurs.
    #         If FALSE, prints to stdio how many slaves are successfully
    #         spawned and where they are running.
    Rmpi::mpi.spawn.Rslaves(nslaves = nslaves, quiet = (debugLevel < 2),
                            needlog = (debugLevel > 0))
  }
}

## FIXME: Move this to the manual page.
# Computes:
# * Kendall's W (also known as Kendall's coefficient of concordance)
#   If 1, all configurations have ranked in the same order in all instances.
#   If 0, the ranking of each configuration on each instance is essentially random.
#   W = Friedman / (m * (k-1))
#
# * Spearman's rho: average (Spearman) correlation coefficient computed on the
#   ranks of all pairs of raters. If there are no repeated data values, a
#   perfect Spearman correlation of +1 or −1 occurs when each of the variables
#   is a perfect monotone function of the other.

# data: matrix with the data, instances in rows (judges), configurations in
# columns.
concordance <- function(data)
{
  irace.assert (is.matrix(data) && is.numeric(data))

  n <- nrow(data) #judges
  k <- ncol(data) #objects
  if (n <= 1 || k <= 1)
    return(list(kendall.w = NA, spearman.rho = NA))

  # Get rankings by rows (per instance)
  r <- t(apply(data, 1L, rank))
  R <- colSums(r)
  TIES <- tapply(r, row(r), table)
  # If everything is tied, then W=1, perfect homogeneity.
  if (all(unlist(TIES) == ncol(data))) {
    W <- 1
  } else {
    # FIXME: This formula seems slightly different from the one in
    # friedman.test. Why?
    T <- sum(unlist(lapply(TIES, function (u) {u^3 - u})))
    W <- ((12 * sum((R - n * (k + 1) / 2)^2)) /
          ((n^2 * (k^3 - k)) - (n * T)))
  }
  
  # Spearman's rho
  rho <- (n * W - 1) / (n - 1)

  ## Same as in friedman test
  #STATISTIC <- n * (k - 1) * W
  #PARAMETER <- k - 1
  #pvalue <- pchisq(PARAMETER, df = PARAMETER, lower.tail = FALSE)
  return(list(kendall.w = W, spearman.rho = rho))
} 

## FIXME: Move this to the manual page.
## FIXME: Reference! Explain a bit what is computed!
# Calculates Performance similarity of instances  
#       data: matrix with the data, instances in rows (judges), configurations
#             in columns.
# Returns: variance value [0,1], where 0 is a homogeneous set of instances and 
#          1 is a heterogeneous set. 
dataVariance <- function(data)
{
  irace.assert (is.matrix(data) && is.numeric(data))
  # LESLIE: should we rank data??
  # MANUEL: Why?
  if (nrow(data) <= 1 || ncol(data) <= 1) return(NA)
  
  # Normalize
  #datamin <- apply(data,1,min,na.rm=TRUE)
  #datamax <- apply(data,1,max,na.rm=TRUE)
  #normdata <- (data - datamin) / (datamax-datamin) 
  
  #standardize
  meandata <- rowMeans(data)
  stddata  <- apply(data, 1L, sd)
  # If stddata == 0, then data is constant and it doesn't matter as long as it
  # is non-zero.
  stddata[stddata == 0] <- 1
  zscoredata <- (data - meandata) / stddata 
  
  # We could log-tranform if needed

  # Variance of configurations
  qvar <- mean(apply(zscoredata, 2L, var))
 
  return(qvar) 
}

runcommand <- function(command, args, id, debugLevel)
{
  if (debugLevel >= 2) {
    irace.note (command, " ", args, "\n")
    elapsed <- proc.time()["elapsed"]
  }
  err <- NULL
  output <- withCallingHandlers(
    tryCatch(system2(command, args, stdout = TRUE, stderr = TRUE),
             error = function(e) {
               err <<- c(err, paste(conditionMessage(e), collapse="\n"))
               NULL
             }), warning = function(w) {
               err <<- c(err, paste(conditionMessage(w), collapse="\n"))
               invokeRestart("muffleWarning")
             })
  # If the command could not be run an R error is generated.  If ‘command’
  # runs but gives a non-zero exit status this will be reported with a
  # warning and in the attribute ‘"status"’ of the result: an attribute
  # ‘"errmsg"’ may also be available.
  if (!is.null(err)) {
    err <- paste(err, collapse ="\n")
    if (!is.null(attr(output, "errmsg")))
      output <- paste(sep = "\n", attr(output, "errmsg"))
    if (debugLevel >= 2)
      irace.note ("ERROR (", id, "): ", err, "\n")
    return(list(output = output, error = err))
  }
  if (debugLevel >= 2) {
    irace.note ("DONE (", id, ") Elapsed: ",
                formatC(proc.time()["elapsed"] - elapsed,
                        format = "f", digits = 2), "\n")
  }
  return(list(output = output, error = NULL))
}

# Safe sampling of vector: 
resample <- function(x, ...) x[sample.int(length(x), ...)]
