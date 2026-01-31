# Get, set and restore the state of the random number generator state.

Get, set and restore the state of the random number generator state.

## Usage

``` r
get_random_seed()

set_random_seed(seed)

restore_random_seed(seed)
```

## Arguments

- seed:

  ([`list()`](https://rdrr.io/r/base/list.html)\|`integer(1)`)  
  Either an integer or the list returned gy `get_random_seed()`.

## Value

`get_random_seed()` returns a list with two components `random_seed` and
`rng_kind` or NULL if no seed was set; `set_random_seed()` and
`restore_random_seed()` do not return anything.

## Details

These functions originate from the
[`withr`](https://withr.r-lib.org/reference/with_seed.html) package.

## Examples

``` r
old_seed <- get_random_seed()
on.exit(restore_random_seed(old_seed))
set_random_seed(42)
value1 <- runif(1)
set_random_seed(42)
value2 <- runif(1)
stopifnot(all.equal(value1,value2))
```
