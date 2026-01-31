# Read parameters in PCS (AClib) format and write them in irace format.

Read parameters in PCS (AClib) format and write them in irace format.

## Usage

``` r
read_pcs_file(file, digits = 4L, debugLevel = 0L, text)
```

## Arguments

- file:

  `character(1)`  
  Filename containing the definitions of the parameters to be tuned.

- digits:

  `integer(1)`  
  The number of decimal places to be considered for real-valued
  parameters.

- debugLevel:

  `integer(1)`  
  Larger values produce more verbose output.

- text:

  `character(1)`  
  If `file` is not supplied and this is, then parameters are read from
  the value of `text` via a text connection.

## Value

A string representing the parameters in irace format.

## Details

Either `file` or `text` must be given. If `file` is given, the
parameters are read from the file `file`. If `text` is given instead,
the parameters are read directly from the `text` character string. In
both cases, the parameters must be given (in `text` or in the file whose
name is `file`) in the expected form. See the documentation for details.
If none of these parameters is given, irace will stop with an error.

**FIXME:** Multiple conditions and default configuration are currently
ignored. See <https://github.com/MLopez-Ibanez/irace/issues/31>

## References

Frank Hutter, Manuel López-Ibáñez, Chris Fawcett, Marius Thomas
Lindauer, Holger H. Hoos, Kevin Leyton-Brown, and Thomas Stützle.
**AClib: A Benchmark Library for Algorithm Configuration**. In P. M.
Pardalos, M. G. C. Resende, C. Vogiatzis, and J. L. Walteros, editors,
*Learning and Intelligent Optimization, 8th International Conference,
LION 8*, volume 8426 of Lecture Notes in Computer Science, pages 36–40.
Springer, Heidelberg, 2014.

## See also

[`readParameters()`](https://mlopez-ibanez.github.io/irace/reference/readParameters.md)

## Author

Manuel López-Ibáñez

## Examples

``` r
 ## Read the parameters directly from text
 pcs_table <- '
 # name       domain
 algorithm    {as,mmas,eas,ras,acs}[as]
 localsearch  {0, 1, 2, 3}[0]
 alpha        [0.00, 5.00][1]
 beta         [0.00, 10.00][1]
 rho          [0.01, 1.00][0.95]
 ants         [1, 100][10]il
 q0           [0.0, 1.0][0]
 rasrank      [1, 100][1]i
 elitistants  [1, 750][1]i
 nnls         [5, 50][5]i
 dlb          {0, 1}[1]
 Conditionals:
 q0 | algorithm in {acs}
 rasrank | algorithm in {ras}
 elitistants | algorithm in {eas}
 nnls | localsearch in {1,2,3}
 dlb | localsearch in {1,2,3}
 {alpha=0, beta=0}'
 parameters_table <- read_pcs_file(text=pcs_table)
 cat(parameters_table)
#> 
#> # name       domain
#> algorithm "algorithm" c (as,mmas,eas,ras,acs)
#> localsearch "localsearch" c (0, 1, 2, 3)
#> alpha "alpha" r (0.00, 5.00)
#> beta "beta" r (0.00, 10.00)
#> rho "rho" r (0.01, 1.00)
#> ants "ants" i,log (1, 100)
#> q0 "q0" r (0.0, 1.0) | algorithm == "acs"
#> rasrank "rasrank" i (1, 100) | algorithm == "ras"
#> elitistants "elitistants" i (1, 750) | algorithm == "eas"
#> nnls "nnls" i (5, 50) | localsearch %in% c("1","2","3")
#> dlb "dlb" c (0, 1) | localsearch %in% c("1","2","3")
#> 
#> [forbidden]
#> (alpha == 0) & (beta == 0)
 parameters <- readParameters(text=parameters_table)
#> # 2026-01-31 15:02:04 UTC: 1 expression(s) specifying forbidden configurations read.
 str(parameters)
#> Classes 'ParameterSpace', 'R6' <ParameterSpace>
#>   Public:
#>     .params: list
#>     as_character: function () 
#>     clone: function (deep = FALSE) 
#>     conditions: list
#>     depends: list
#>     domains: list
#>     forbid_configurations: function (x) 
#>     forbidden: list
#>     get: function (x) 
#>     get_ordered: function () 
#>     hierarchy: 1 1 1 1 1 1 2 2 2 2 2
#>     initialize: function (..., forbidden = NULL, verbose = 0L) 
#>     isFixed: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FA ...
#>     names: algorithm localsearch alpha beta rho ants q0 rasrank eli ...
#>     names_fixed: 
#>     names_numeric: alpha beta rho ants q0 rasrank elitistants nnls
#>     names_variable: algorithm localsearch alpha beta rho ants q0 rasrank eli ...
#>     nbFixed: 0
#>     nbParameters: 11
#>     nbVariable: 11
#>     switches: algorithm localsearch alpha beta rho ants q0 rasrank eli ...
#>     types: c c r r r i r i i i c 
```
