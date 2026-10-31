# Converts a relative path to an absolute path.

If the path passed corresponds to an executable, it tries to find its
path using [`Sys.which()`](https://rdrr.io/r/base/Sys.which.html).
Expansion of `'~'` in Windows follows the definition of
[`fs::path_expand()`](https://fs.r-lib.org/reference/path_expand.html)
rather than
[`base::path.expand()`](https://rdrr.io/r/base/path.expand.html). This
function tries really hard to create canonical paths.

## Usage

``` r
path_rel2abs(path, cwd = getwd())
```

## Arguments

- path:

  (`character(1)`) Character string representing a relative path.

- cwd:

  (`character(1)`) Current working directory.

## Value

(`character(1)`) Character string representing the absolute path

## Examples

``` r
path_rel2abs("..")
#> [1] "/home/runner/work/irace/irace/docs"
```
