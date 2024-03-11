# R6 Class for parsing command-line arguments
CommandArgsParser <- R6::R6Class("CommandArgsParser", cloneable = FALSE, lock_class = TRUE, list(
  argv = NULL,
  argsdef = NULL,
  initialize = function(argv, argsdef) {
    # Handle the case where we are given a single character string like a
    # command-line.
    if (!missing(argv) && length(argv) == 1) {
      # strsplit does not respect quoted strings.
      argv <- scan(text=argv, what='character', quiet=TRUE)
    }
    self$argv <- argv
    required_colnames <- c("name", "short", "long", "type", "default")
    if (any(required_colnames %not_in% colnames(argsdef))) {
      stop("argsdef must contain the column names: ", paste0(required_colnames, collapse=", "))
    }
    self$argsdef <- argsdef
    rownames(self$argsdef) <- argsdef$name
    self
  },
  readCmdLineParameter = function (paramName, default = NULL) {
    short <- self$argsdef[paramName, "short"]
    long <- self$argsdef[paramName,"long"]
    value <- self$readArg(short = short, long = long)
    if (is.null(value)) {
      value <- if (is.null(default)) self$argsdef[paramName, "default"] else default
    } else if (is.na(value) && self$argsdef[paramName,"type"] != 'x') {
      stop("option '", long, "' requires an argument", call. = FALSE)
    }
    return(value)
  },
  # Function to read command-line arguments.
  ## FIXME: This function always consumes two arguments. This is problematic
  ## for flags that have no arguments, like --check.
  readArg = function(short = "", long = "") {
    if (length(short) == 0) short <- ""
    if (length(long) == 0) long <- ""
    if (short == "" && long == "") return(NULL)
    argv <- self$argv
    pos <- c()
    pattern <- ""
    if (short != "") {
      pattern_equal <- paste0("^", short, "=")
      pattern <- paste0("^", short, "$|", pattern_equal)
    }
    if (long != "") {
      pattern_long_equal <- paste0("^", long, "=")
      pattern_long <- paste0("^", long, "$|", pattern_long_equal)
      if (short != "") {
        pattern <-  paste0(pattern, "|", pattern_long)
        pattern_equal <- paste0(pattern_equal, "|", pattern_long_equal)
      } else {
        pattern <- pattern_long
        pattern_equal <- pattern_long_equal
      }
    }
    pos <- grep(pattern, argv)
    if (length(pos) == 0) {
      return(NULL) # Not found
    } else if (length(pos) > 0) {
      # Allow repeated parameters
      pos <- max(pos)
    }
    if (grepl(pattern_equal, argv[pos])) {
      value <- unlist(strsplit(argv[pos], '=', fixed = TRUE))[2]
      if (is.null (value) || is.na(value))
        value <- ""
    } else {
      value <- argv[pos + 1]
      self$argv <- argv[-(pos + 1)]
    }
    self$argv <- self$argv[-pos]
    return (value)
  },
  readAll = function() {
    params <- list()
    for (param in self$argsdef$name[self$argsdef$type != 'x']) {
      value <- self$readCmdLineParameter(paramName = param)
      if (is.na(value) || (length(value) > 0 && value == "")) value <- NULL
      params[[param]] <- value
    }
    params
  },
  cmdline_usage = function() {
    cmdline_usage(self$argsdef)
  })
)

# `cmdline_usage()` prints the output of `--help`
#
# @param cmdline_args Definition of the command-line arguments.
# 
# @export
cmdline_usage <- function(cmdline_args)
{
  for (i in seq_len(nrow(cmdline_args))) {
    short <- cmdline_args[i,"short"]
    long <- cmdline_args[i,"long"]
    desc <- cmdline_args[i,"description"]
    if (desc == "" || (short == "" && long == "")) next
    if (short != "") short <- paste0(short,",")
    default <- cmdline_args[i,"default"]
    if (!is_null_or_empty_or_na(default)) {
      desc <- paste0(desc, " Default: ", default, ".")
    }
    cat(sep = "\n", strwrap(desc, width = 80,
                            initial = sprintf("%3s%-20s  ", short, long),
                            exdent = 25))
  }
}
