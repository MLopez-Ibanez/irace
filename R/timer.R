Timer <- R6::R6Class("Timer", lock_class = TRUE, list(
  start = NULL,
  initialize = function() {
    self$start <- proc.time()
  },
  elapsed = function() {
    x <- proc.time() - self$start
    if (!is.na(x[4L])) 
        x[1L] <- x[1L] + x[4L]
    if (!is.na(x[5L])) 
        x[2L] <- x[2L] + x[5L]
    x <- x[1L:3L]
    names(x) <- c("user", "system", "wallclock")
    x
  })
)

