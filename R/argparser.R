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
    self$argsdef <- argsdef
  },
  readCmdLineParameter = function (paramName, default = NULL) {
    value <- self$readArg (short = self$argsdef[paramName, "short"],
                           long  = self$argsdef[paramName,"long"])
    if (is.null(value)) {
      value <- if (is.null(default)) self$argsdef[paramName, "default"] else default
    } else if (is.na(value) && self$argsdef[paramName,"type"] != 'x' ) {
      irace.error ("option '", self$argsdef[paramName,"long"],
                   "' requires an argument\n")
    }
    return(value)
  },
  # Function to read command-line arguments.
  ## FIXME: This function always consumes two arguments. This is problematic
  ## for flags that have no arguments, like --check.
  readArg = function(short = "", long = "") {
    argv <- self$argv
    pos <- c()
    if (length (short) > 0) {
      pos <- grep (paste0("^", short, "$"), argv)
      if (length (pos) == 0) {
        pos <- grep (paste0("^", short, "="), argv)
      }
    }
    if (length (long) > 0 && length (pos) == 0)  {
      pos <- grep (paste0("^", long, "$"), argv)
      if (length (pos) == 0) {
        pos <- grep (paste0("^", long, "="), argv)
      }
    }
    if (length (pos) == 0) {
      return (NULL)
    } else if (length(pos) > 0) {
      # Allow repeated parameters
      pos <- max(pos)
    }
  
    value <- unlist(strsplit (argv[pos], '=', fixed = TRUE))[2]
    if (is.null (value) || is.na(value)) {
      value <- argv[pos + 1]
      self$argv <- argv[-(pos + 1)]
    }
    self$argv <- self$argv[-pos]
    return (value)
  })
  )
