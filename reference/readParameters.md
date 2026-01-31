# Reads the parameters to be tuned by irace from a file or from a character string.

Reads the parameters to be tuned by irace from a file or from a
character string.

## Usage

``` r
readParameters(file, digits = 4L, debugLevel = 0L, text)
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

A list containing the definitions of the parameters read. The list is
structured as follows:

- `names`:

  Vector that contains the names of the parameters.

- `types`:

  Vector that contains the type of each parameter 'i', 'c', 'r', 'o'.
  Numerical parameters can be sampled in a log-scale with 'i,log' and
  'r,log' (no spaces).

- `switches`:

  Vector that contains the switches to be used for the parameters on the
  command line.

- `domain`:

  List of vectors, where each vector may contain two values (minimum,
  maximum) for real and integer parameters, or possibly more for
  categorical parameters.

- `conditions`:

  List of R logical expressions, with variables corresponding to
  parameter names.

- `isFixed`:

  Logical vector that specifies which parameter is fixed and, thus, it
  does not need to be tuned.

- `nbParameters`:

  An integer, the total number of parameters.

- `nbFixed`:

  An integer, the number of parameters with a fixed value.

- `nbVariable`:

  Number of variable (to be tuned) parameters.

- `depends`:

  List of character vectors, each vector specifies which parameters
  depend on this one.

- `is_dependent`:

  Logical vector that specifies which parameter has a dependent domain.

- `digits`:

  Integer vector that specifies the number of digits per parameter.

- `forbidden`:

  List of expressions that define which parameter configurations are
  forbidden.

## Details

Either `file` or `text` must be given. If `file` is given, the
parameters are read from the file `file`. If `text` is given instead,
the parameters are read directly from the `text` character string. In
both cases, the parameters must be given (in `text` or in the file whose
name is `file`) in the expected form. See the documentation for details.
If none of these parameters is given, irace will stop with an error.

A fixed parameter is a parameter that should not be sampled but instead
should be always set to the only value of its domain. In this function
we set `isFixed` to TRUE only if the parameter is a categorical and has
only one possible value. If it is an integer and the minimum and maximum
are equal, or it is a real and the minimum and maximum values satisfy
`round(minimum, digits) == round(maximum, digits)`, then the parameter
description is rejected as invalid to identify potential user errors.

The order of the parameters determines the order in which parameters are
given to `targetRunner`. Changing the order may also change the results
produced by `irace`, even with the same random seed.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste

## Examples

``` r
 ## Read the parameters directly from text
 parameters_table <- '
 # name       switch           type  values               [conditions (using R syntax)]
 algorithm    "--"             c     (as,mmas,eas,ras,acs)
 localsearch  "--localsearch " o     (0, 1, 2, 3)
 alpha        "--alpha "       r     (0.00, 5.00)
 beta         "--beta "        r     (0.00, 10.00)
 rho          "--rho  "        r     (0.01, 1.00)
 ants         "--ants "        i,log (5, 100)
 q0           "--q0 "          r     (0.0, 1.0)           | algorithm == "acs"
 rasrank      "--rasranks "    i     (1, "min(ants, 10)") | algorithm == "ras"
 elitistants  "--elitistants " i     (1, ants)            | algorithm == "eas"
 nnls         "--nnls "        i     (5, 50)              | localsearch %in% c(1,2,3)
 dlb          "--dlb "         c     (0, 1)               | localsearch %in% c(1,2,3)

 [forbidden]
 (alpha == 0.0) & (beta == 0.0)
 [global]
 digits = 4
 '
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
#>     switches: -- --localsearch  --alpha  --beta  --rho   --ants  --q0  ...
#>     types: c o r r r i r i i i c 
```
