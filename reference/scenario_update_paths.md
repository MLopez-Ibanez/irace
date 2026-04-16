# Update filesystem paths of a scenario consistently.

This function should be used to change the filesystem paths stored in a
scenario object. Useful when moving a scenario from one computer to
another.

## Usage

``` r
scenario_update_paths(scenario, from, to, fixed = TRUE)
```

## Arguments

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or
  [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md).

- from:

  `character(1)`  
  Character string containing a regular expression (or character string
  for `fixed = TRUE`) to be matched.

- to:

  `character(1)`  
  The replacement string.character string. For `fixed = FALSE` this can
  include backreferences `"\1"` to `"\9"` to parenthesized
  subexpressions of `from`.

- fixed:

  `logical(1)`  
  If `TRUE`, `from` is a string to be matched as is.

## Value

The updated scenario

## See also

[`base::grep()`](https://rdrr.io/r/base/grep.html)

## Examples

``` r
if (FALSE) { # \dontrun{
scenario <- readScenario(filename = "scenario.txt")
scenario <- scenario_update_paths(scenario, from = "/home/manuel/", to = "/home/leslie")
} # }
```
