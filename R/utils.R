# Print a user-level warning message, when the calling context
# cannot help the user to understand why the program failed.
irace.warning <- function(...)
{
  if (getOption(".irace.quiet", default=FALSE)) return()
  warning(paste0(.irace_msg_prefix, ..., collapse=""),
          call. = FALSE, immediate. = TRUE)
}

# Print a user-level fatal error message, when the calling context
# cannot help the user to understand why the program failed.
irace.error <- function(...)
{
  # The default is only 1000, which is too small. 8170 is the maximum
  # value allowed up to R 3.0.2
  withr::local_options(list(warning.length = 8170))
  stop (.irace_msg_prefix, ..., call. = FALSE)
}

## When irace crashes, it generates a file "iracedump.rda". To debug the crash use:
## R> load("iracedump.rda")
## R> debugger(iracedump)
##
## See help(dump.frames) for more details.
irace.dump.frames <- function()
{
  execDir <- getOption(".irace.execdir")
  if (!is.null(execDir)) cwd <- setwd(execDir)
  utils::dump.frames(dumpto = "iracedump", to.file = TRUE, include.GlobalEnv = TRUE)
  # FIXME: We want to use on.exit(setwd(cwd)) but q() does not run on.exit.
  if (!is.null(execDir)) setwd(cwd)
  # We need this to signal an error in R CMD check. See help(dump.frames)
  if (!interactive()) quit("no", status = 1)
}

# Print an internal fatal error message that signals a bug in irace.
irace.internal.error <- function(...)
{
  .irace.bug.report <-
    paste0(.irace_msg_prefix, "An unexpected condition occurred. ",
           "Please report this bug to the authors of the irace package <https://github.com/MLopez-Ibanez/irace/issues>")

  op <- options(warning.length = 8170)
  if (!base::interactive()) options(error = irace.dump.frames)
  on.exit(options(op))
  # 6 to not show anything below irace.assert()
  bt <- capture.output(traceback(5))
  warnings()
  stop (.irace_msg_prefix, paste0(..., collapse = "\n"), "\n",
        paste0(bt, collapse= "\n"), "\n",
        .irace.bug.report, call. = FALSE)
  invisible()
}

irace.assert <- function(exp, eval_after = NULL)
{
  # FIXME: It would be great if we could save into a file the state of
  # the function that called this one.
  if (isTRUE(as.logical(exp))) return(invisible())
  mc <- sys.call()[[2L]]
  msg <- paste0("'", deparse(mc), "' is not TRUE")
  if (!is.null(eval_after)) {
    msg_after <- eval.parent(capture.output(eval_after))
    msg <- paste0(msg, "\n", paste0(msg_after, collapse="\n"))
  }
  irace.internal.error(msg)
  invisible()
}

irace.note <- function(...)
{
  # FIXME: If this was a function within an irace object, we could replace it
  # when using quiet.
  if (getOption(".irace.quiet", default=FALSE)) return()
  cat ("# ", format(Sys.time(), usetz=TRUE), ": ",
       paste0(..., collapse = ""), sep = "")
}

file.check <- function (file, executable = FALSE, readable = executable,
                        writeable = FALSE,
                        isdir = FALSE, notempty = FALSE, text = NULL)
{
  EXEC <- 1 # See documentation of the function file.access()
  WRITE <- 2
  READ <- 4

  if (!is.character(file) || is.null.or.empty(file)) {
    irace.error (text, " ", shQuote(file), " is not a vaild filename")
  }
  file <- path_rel2abs(file)
  ## The above should remove the trailing separator if present for windows OS
  ## compatibility, except when we have just C:/, where the trailing separator
  ## must remain.
  
  if (!file.exists(file)) {
    if (writeable) {
      if (tryCatch({ suppressWarnings(file.create(file) && file.remove(file)) },
                   error=function(e) FALSE))
        return(TRUE)
      irace.error("cannot create ", text, " ", shQuote(file))
      return (FALSE)
    }
    irace.error (text, " '", file, "' does not exist")
    return(FALSE)
  }

  if (writeable && (file.access(file, mode = WRITE) != 0)) {
    irace.error(text, " '", file, "' cannot be written into")
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
round_to_next_multiple <- function(x, d) (x + d - 1L - (x - 1L) %% d)

# This returns FALSE for Inf/-Inf/NA
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
  is.finite(x) & (abs(x - round(x)) < tol)


is_na_nowarn <- function(x)
  length(x) == 1L && suppressWarnings(is.na(x))

is_na_or_empty <- function(x)
  (length(x) == 0L) || is_na_nowarn(x)

is.null.or.na <- function(x)
  is.null(x) || is_na_nowarn(x)

is.null.or.empty <- function(x)
  (length(x) == 0L) || (length(x) == 1L && !suppressWarnings(is.na(x)) && is.character(x) && x == "")

is_null_or_empty_or_na <- function(x)
  (length(x) == 0L) || is_na_nowarn(x) || (length(x) == 1L && !suppressWarnings(is.na(x)) && is.character(x) && x == "")

is.function.name <- function(FUN)
{
  # FIXME: Is there a simpler way to do this check?
  is.function(FUN) ||
    (!is.null(FUN) && !is.na(FUN) && as.character(FUN) != "" &&
     !is.null(get.function(FUN)))
}

get.function <- function(FUN)
{
  if (is.function(FUN)) return(FUN)
  FUN <- dynGet(as.character(FUN), ifnotfound = NULL, inherits = TRUE)
  if (is.function(FUN)) return(FUN)
  NULL
}

is.bytecode <- function(x) typeof(x) == "bytecode"

bytecompile <- function(x)
{
  if (is.bytecode(x)) return(x)
  compiler::cmpfun(x)
}

# This function is used to trim potentially large strings for printing, since
# the maximum error/warning length is 8170 characters (R 3.0.2)
strlimit <- function(s, limit = 5000L)
{
  if (nchar(s) <= limit) return(s)
  paste0(substr(s, 1L, limit - 3L), "...")
}

#' Update filesystem paths of a scenario consistently.
#'
#' This function should be used to change the filesystem paths stored in a
#' scenario object. Useful when moving a scenario from one computer to another.
#'
#' @inheritParams defaultScenario
#' @param from `character(1)`\cr Character string containing a regular expression (or character
#'   string for `fixed = TRUE`) to be matched.
#' @param to `character(1)`\cr  The replacement string.character string. For `fixed = FALSE`
#'   this can include backreferences `"\1"` to `"\9"` to
#'   parenthesized subexpressions of `from`.
#' @param fixed `logical(1)`\cr If `TRUE`, `from` is a string to be matched
#'   as is.
#' @return The updated scenario
#' @examples
#' \dontrun{
#' scenario <- readScenario(filename = "scenario.txt")
#' scenario <- scenario_update_paths(scenario, from = "/home/manuel/", to = "/home/leslie")
#' }
#' @seealso [base::grep()]
#' @export
scenario_update_paths <- function(scenario, from, to, fixed = TRUE)
{
  pathParams <- .irace.params.def[.irace.params.def[, "type"] == "p", "name"]
  # Only consider the ones that actually appear in scenario.
  pathParams <- intersect(pathParams, names(scenario))
  scenario[pathParams] <- lapply(scenario[pathParams], sub, pattern = from, replacement = to, fixed = fixed)
  scenario
}

test.type.order.str <- function(test.type)
  switch(test.type,
         friedman = "sum of ranks",
         t.none =, # Fall-throught
         t.holm =, # Fall-throught
         t.bonferroni = "mean value",
         irace.internal.error ("test.type.order.str() Invalid value '",
                               test.type, "' of test.type"))


trim_leading <- function(str)
  sub('^[[:space:]]+', '', str, perl = TRUE) ## white space, POSIX-style

trim_trailing <- function(str)
  sub('[[:space:]]+$', '', str, perl = TRUE) ## white space, POSIX-style

# remove leading and trailing white space characters
trim <- function(str) trim_trailing(trim_leading(str))

## This function takes two matrices x and y and merges them such that the
## resulting matrix z has:
# rownames(z) <- setunion(rownames(x), rownames(y)) and 
# rownames(z) <- setunion(rownames(x), rownames(y)) and
# z[rownames(x), colnames(x)] <- x and z[rownames(y), colnames(y)] <- y, and
# z[i, j] <- NA for all i,j not in x nor y.
merge_matrix <- function(x, y)
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
  # There must be a non-NA entry for each instance.
  irace.assert(all(rowAnys(!is.na(x))))
  return(x)
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
  if ("Rmpi" %not_in% loadedNamespaces()) {
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
## FIXME: Export this function.
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
#
# data: matrix with the data, instances in rows (judges), configurations in
# columns.
concordance <- function(data)
{
  irace.assert (is.matrix(data) && is.numeric(data))

  n <- nrow(data) #judges
  k <- ncol(data) #objects
  if (n <= 1L || k <= 1L)
    return(list(kendall.w = NA, spearman.rho = NA))

  # Get rankings by rows (per instance)
  r <- rowRanks(data, ties.method = "average")
  R <- colSums2(r)
  TIES <- c(table(r,row(r)))
  # If everything is tied, then W=1, perfect homogeneity.
  if (all(TIES == k)) {
    W <- 1
  } else {
    # FIXME: This formula seems slightly different from the one in
    # friedman.test. Why?
    TIES <- sum(TIES^3 - TIES)
    W <- ((12 * sum((R - n * (k + 1) / 2)^2)) /
            ((n^2 * (k^3 - k)) - (n * TIES)))
  }
  # Spearman's rho
  rho <- (n * W - 1) / (n - 1)

  ## Same as in friedman test
  #STATISTIC <- n * (k - 1) * W
  #PARAMETER <- k - 1
  #pvalue <- pchisq(PARAMETER, df = PARAMETER, lower.tail = FALSE)
  list(kendall.w = W, spearman.rho = rho)
} 

## FIXME: Move this to the manual page.
## FIXME: Reference! Explain a bit what is computed!
# Calculates Performance similarity of instances  
#       data: matrix with the data, instances in rows (judges), configurations
#             in columns.
# Returns: variance value [0,1], where 0 is a homogeneous set of instances and 
#          1 is a heterogeneous set.
# FIXME: How to handle missing values?
dataVariance <- function(data)
{
  irace.assert (is.matrix(data) && is.numeric(data))
  # LESLIE: should we rank data??
  # MANUEL: We should add the option.
  if (nrow(data) <= 1L || ncol(data) <= 1L) return(NA)
  
  # Normalize
  #datamin <- apply(data,1,min,na.rm=TRUE)
  #datamax <- apply(data,1,max,na.rm=TRUE)
  #normdata <- (data - datamin) / (datamax-datamin) 
  
  #standardize
  meandata <- rowMeans2(data)
  stddata  <- rowSds(data)
  # If stddata == 0, then data is constant and it doesn't matter as long as it
  # is non-zero.
  stddata[stddata == 0] <- 1
  zscoredata <- (data - meandata) / stddata 
  
  # FIXME: We could log-tranform if needed
  # Variance of configurations
  mean(colVars(zscoredata))
}

runcommand <- function(command, args, id, debugLevel, timeout = 0)
{
  if (debugLevel >= 2L) {
    irace.note (command, " ", args, "\n")
    elapsed <- proc.time()["elapsed"]
  }
  err <- NULL
  output <- withCallingHandlers(
    tryCatch(system2(command, args, stdout = TRUE, stderr = TRUE, timeout = timeout),
             error = function(e) {
               err <<- c(err, paste(conditionMessage(e), collapse="\n"))
               NULL
             }), warning = function(w) {
               err <<- c(err, paste(conditionMessage(w), collapse="\n"))
               invokeRestart("muffleWarning")
             })
  if (is.null(output))
    output <- ""
  # If the command could not be run an R error is generated.  If ‘command’
  # runs but gives a non-zero exit status this will be reported with a
  # warning and in the attribute ‘"status"’ of the result: an attribute
  # ‘"errmsg"’ may also be available.
  if (!is.null(err)) {
    err <- paste(err, collapse = "\n")
    if (!is.null(attr(output, "errmsg")))
      err <- paste(sep = "\n", err, attr(output, "errmsg"))
    if (debugLevel >= 2L)
      irace.note ("ERROR (", id, "): ", err, "\n")
    return(list(output = output, error = err))
  }
  if (debugLevel >= 2L) {
    irace.note ("DONE (", id, ") Elapsed wall-clock seconds: ",
                formatC(proc.time()["elapsed"] - elapsed,
                        format = "f", digits = 2), "\n")
  }
  # TODO: Return elapsed time so that we can report at the end the total
  # elapsed time taken by irace vs the time taken by the target-runner.
  list(output = output, error = NULL)
}

# Safe sampling of vector: 
resample <- function(x, ...) x[sample.int(length(x), ...)]

# Rounds up the number x to the specified number of decimal places 'digits'.
ceiling_digits <- function(x, digits)
{
   multiple <- 10^-digits
   div <- x / multiple
   int_div <- trunc(div)
   int_div * multiple + ceiling(div - int_div) * multiple
}

# ceil.decimal <- function(x, d) { 
  # # get the significant digits in the integer part.
  # ssd <- x * 10^(d)
  # # get the non significant digits
  # nsd <- ssd - floor(ssd)

  # ssd <- trunc(ssd)
  # sel <- nsd > 0 | ssd==0
  # ssd[sel] <- ssd[sel] + 1
  # x2 <- ssd/10^(d)
  # return(x2)
# }

is.file.extension <- function(filename, ext)
  substring(filename, nchar(filename) + 1L - nchar(ext)) == ext

is.sub.path <- function(x, dir, n = nchar(dir)) substr(x, 1L, n) == dir

# Same as !(x %in% table). Package data.table has %notin%.
"%not_in%" <- function(x, table) is.na(match(x, table))

irace_save_logfile <- function(iraceResults, scenario)
{
  # FIXME: Raul Santa Maria proposed to only save if sufficient times (>= 1
  # minute) has passed since the last save. We would need an option to force
  # saving the last one.
  if (is.null.or.empty(scenario$logFile)) return(invisible())
  # Files produced by `saveRDS` (or `serialize` to a file connection) are not
  # suitable as an interchange format between machines, for example to download
  # from a website. The files produced by `save` have a header identifying the
  # file type and so are better protected against erroneous use.
  save(iraceResults, file = scenario$logFile, version = 3L)
}

valid_iracelog <- function(x)
{
  is.list(x) && ("scenario" %in% names(x))
}

#' Read the log file produced by irace (`irace.Rdata`).
#'
#' @param filename Filename that contains the log file saved by irace. Example: `irace.Rdata`.
#'
#' @param name Optional argument that allows overriding the default name of the object in the file.
#' 
#' @return (`list()`)
#' @examples
#' irace_results <- read_logfile(system.file("exdata/irace-acotsp.Rdata", package="irace",
#'                                           mustWork=TRUE))
#' str(irace_results)
#' @concept analysis
#' @export
read_logfile <- function(filename, name = "iraceResults")
{
  if (is_na_or_empty(filename))
    irace.error("read_logfile: 'filename' is NULL or NA.")
  # If filename is already the iraceResults object, just return it.
  if (valid_iracelog(filename)) return(filename)

  if (file.access(filename, mode = 4) != 0)
    irace.error("read_logfile: Cannot read file '", filename, "'.")
  
  load(filename)
  iraceResults <- get0(name, inherits=FALSE)
  if (!valid_iracelog(iraceResults))
    irace.error("read_logfile: The file '", filename, "' does not contain the '", name, "' object.")
  
  iraceResults
}

get_log_clean_version <- function(iraceResults)
{
  log_version <- iraceResults$irace_version
  if (is.null(log_version))
    log_version <- iraceResults$irace.version
  if (is.null(log_version))
    return(package_version("0"))
  if (length(gregexpr("\\.", log_version)[[1L]]) > 3L
    || grepl("[a-z]", log_version))
    log_version <- sub("\\.[^.]*$", "", log_version)
  package_version(log_version)
}

#' Check if the results object generated by irace has data about the testing phase.
#'
#' @param iraceResults `list()`|`character(1)`\cr Object created by \pkg{irace} and typically saved in the log file `irace.Rdata`. If a character string is given, then it is interpreted as the path to the log file from which the `iraceResults` object will be loaded.
#'
#' @return `logical(1)`
#' @examples
#' irace_results <- read_logfile(system.file("exdata/irace-acotsp.Rdata", package="irace",
#'                                           mustWork=TRUE))
#' print(has_testing_data(irace_results))
#' @export
has_testing_data <- function(iraceResults)
{
  ins <- iraceResults$scenario$testInstances
  exp <- iraceResults$testing$experiments
  !(length(ins) == 0L ||
    (length(ins) == 1L && (is.na(ins) || nchar(ins) == 0L)) ||
    length(exp) == 0L || !(is.matrix(exp) || is.data.frame(exp)))
}

do_nothing <- function(...) invisible()

.irace_tolerance <- sqrt(.Machine$double.eps)

seq_nrow <- function(x) seq_len(nrow(x))

clamp <- function(x, lower, upper) pmax.int(lower, pmin.int(x, upper))

truncate_rows <- function(x, n)
{
  nx <- nrow(x)
  if (nx <= n) return(x)
  x[-seq.int(n + 1L, nx), ]
}


vlast <- function(x)
{
  stopifnot(is.null(dim(x)))
  lx <- length(x)
  if (!lx)
    x
  else
    x[[lx]]
}

# 2147483647 is the maximum value for a 32-bit signed integer.
# We use replace = TRUE, because replace = FALSE allocates memory for each possible number.
# Do not use .Machine$integer.max unless it is smaller, to minimize differences
# between machines.
runif_integer <- function(size)
  sample.int(min(2147483647L, .Machine$integer.max), size = size, replace = TRUE)
