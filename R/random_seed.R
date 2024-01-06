#' Get, set and restore the state of the random number generator state.
#'
#' @details These functions originate from the [`withr`][withr::with_seed()] package.
#' 
#' @param seed (`list()`|`integer(1)`)\cr Either an integer or the list returned gy [get_random_seed()].

#' @return [get_random_seed()] returns a list with two components `random_seed` and `rng_kind` or NULL if no seed was set; [set_random_seed()] and [restore_random_seed()] do not return anything.
#' @name random_seed
#' @examples
#' old_seed <- get_random_seed()
#' on.exit(restore_random_seed(old_seed))
#' set_random_seed(42)
#' value1 <- runif(1)
#' set_random_seed(42)
#' value2 <- runif(1)
#' stopifnot(all.equal(value1,value2))
#' 
#' @export
get_random_seed <- function()
{
  if (has_random_seed()) {
    return(list(random_seed = get(".Random.seed", globalenv(), mode = "integer", 
                                  inherits = FALSE), rng_kind = RNGkind()))
  }
  NULL
}

#' @rdname random_seed
#' @export
set_random_seed <- function(seed)
{
  if (is.list(seed)) {
    RNGkind(seed$rng_kind[[1L]], normal.kind = seed$rng_kind[[2L]],
            sample.kind = seed$rng_kind[[3L]])
    assign(".Random.seed", seed$random_seed, globalenv())
  } else {
    set.seed(seed)
  }
}

#' @rdname random_seed
#' @export
restore_random_seed <- function(seed)
{
  if (is.null(seed)) {
    rm_random_seed()
  } else {
    set_random_seed(seed)
  }
}

rm_random_seed <- function ()
{
  if (has_random_seed()) {
    set.seed(seed = NULL)
    rm(".Random.seed", envir = globalenv())
  }
}

has_random_seed <- function() 
{
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

# @return [gen_random_seeds()] returns a list of `n` seeds that were generated from the `global_seed`.
# The generated seeds can then e.g. be used to seed thread-local RNGs.
gen_random_seeds <- function(n, global_seed = NULL)
{
  # Use a random global seed if not set.
  if (!is.null(global_seed))
    set_random_seed(global_seed)

  # Generate 'n' seeds using the 'global_seed'.
  trunc(runif(n, 1, .Machine$integer.max))
}
