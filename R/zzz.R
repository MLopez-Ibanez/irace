# Uses 10 decimal places at most so that there is space for '-', '.', and
# 'e-NN' within the 16 spaces.
.irace.format.perf <- "%#16.10g"

.onLoad <- function(libname, pkgname) {
  # FIXME:  We would like to use %#16.10g but this causes problems with
  # https://github.com/oracle/fastr/issues/191
  R_engine <- R.version$engine
  if (!is.null(R_engine) && R_engine == "FastR")
    .irace.format.perf <<- "%16.10g"  
  invisible()
}
