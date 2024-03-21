Timer <- R6::R6Class("Timer", cloneable = TRUE, lock_class = TRUE, lock_objects = TRUE, public = list(
  start = NULL,
  initialize = function() {
    self$start <- proc.time()
  },
  elapsed = function() {
    x <- proc.time() - self$start
    if (!is.na(x[[4L]])) 
        x[[1L]] <- x[[1L]] + x[[4L]]
    if (!is.na(x[[5L]])) 
        x[[2L]] <- x[[2L]] + x[[5L]]
    c(user=x[[1L]], system=x[[2L]], wallclock=x[[3L]])
  })
)

