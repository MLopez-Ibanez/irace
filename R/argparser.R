#' R6 Class for parsing command-line arguments
#' 
#' @export
CommandArgsParser <- R6::R6Class("CommandArgsParser", cloneable = FALSE, list(
  argv = NULL,
  argsdef = NULL,
  initialize = function(argv, argsdef) {
    # Handle the case where we are given a single character string like a
    # command-line.
    if (!missing(argv) && length(argv) == 1) {
      argv <- strsplit(trim(argv), " +")[[1]]
    }
    self$argv <- argv
    required_colnames <- c("name", "short", "long", "type", "default")
    if (any(required_colnames %!in% colnames(argsdef))) {
      stop("argsdef must contain the column names: ", paste0(required_colnames, collapse=", "))
    }
    self$argsdef <- argsdef
    rownames(self$argsdef) <- argsdef$name
    self
  },
  readCmdLineParameter = function (paramName, default = NULL) {
    value <- self$readArg(short = self$argsdef[paramName, "short"],
                          long  = self$argsdef[paramName,"long"])
    if (is.null(value)) {
      value <- if (is.null(default)) self$argsdef[paramName, "default"] else default
    } else if (is.na(value) && self$argsdef[paramName,"type"] != 'x') {
      stop("option '", self$argsdef[paramName,"long"],"' requires an argument", call. = FALSE)
    }
    return(value)
  },
  # Function to read command-line arguments.
  ## FIXME: This function always consumes two arguments. This is problematic
  ## for flags that have no arguments, like --check.
  readArg = function(short = "", long = "") {
    argv <- self$argv
    pos <- c()
    if (length(short) > 0) {
      # FIXME: use match()
      pos <- grep(paste0("^", short, "$"), argv)
      if (length(pos) == 0) {
        # FIXME: use pmatch()
        pos <- grep(paste0("^", short, "="), argv)
      }
    }
    if (length(long) > 0 && length(pos) == 0)  {
      pos <- grep(paste0("^", long, "$"), argv)
      if (length(pos) == 0) {
        pos <- grep(paste0("^", long, "="), argv)
      }
    }
    if (length(pos) == 0) {
      return(NULL)
    } else if(length(pos) > 0) {
      # Allow repeated parameters
      pos <- max(pos)
    }
  
    value <- unlist(strsplit(argv[pos], '=', fixed = TRUE))[2]
    if (is.null (value) || is.na(value)) {
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
  cmdline_usage = function(){
    irace::cmdline_usage(self$argsdef)
  })
)

#' `cmdline_usage()` prints the output of `--help`
#'
#' @param cmdline_args Definition of the command-line arguments.
#' 
#' @rdname CommandArgsParser
#' @export
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
