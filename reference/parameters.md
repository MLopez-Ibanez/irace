# Create a parameter space to be tuned.

- `param_cat()` creates a categorical parameter.

- `param_ord()` creates an ordinal parameter.

- `param_real()` creates a real-valued parameter.

- `param_int()` creates an integer parameter.

## Usage

``` r
parametersNew(..., forbidden = NULL, debugLevel = 0L)

param_cat(name = name, values, label = "", condition = TRUE)

param_ord(name, values, label = "", condition = TRUE)

param_real(
  name,
  lower,
  upper,
  label = "",
  condition = TRUE,
  transf = "",
  digits = 15L
)

param_int(name, lower, upper, label = "", condition = TRUE, transf = "")
```

## Arguments

- ...:

  one or more parameters created by `param_int()`, `param_real()`,
  `param_ord()`, or `param_cat()`.

- forbidden:

  `expression()|character(1)`  
  String or list of expressions that define forbidden parameter
  configurations.

- debugLevel:

  `integer(1)`  
  Larger values produce more verbose output.

- name:

  `character(1)`  
  Parameter name (must be alphanumeric).

- values:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Domain as a vector of strings.

- label:

  `character(1)`  
  Label associated to the parameter. Often used to encode a command-line
  switch that activates the parameter.

- condition:

  `expression(1)|character(1)`  
  Expression that defines when the parameter is active according to the
  value of other parameters.

- lower, upper:

  Lower and upper limits of the valid domain.

- transf:

  `character(1)`  
  If `"log"`, then the parameter is sampled in a logarithmic scale.

- digits:

  `integer(1)`  
  The number of decimal places to be considered for real-valued
  parameters.

## Value

`ParameterSpace`

## Examples

``` r
digits <- 4L
parametersNew(
   param_cat(name = "algorithm", values = c("as", "mmas", "eas", "ras", "acs"), label = "--"),
   param_ord(name = "localsearch", values = c("0", "1", "2", "3"), label = "--localsearch "),
   param_real(name = "alpha", lower = 0.0, upper=5.0, label = "--alpha ", digits = digits),
   param_real(name = "beta", lower = 0.0, upper = 10.0, label = "--beta ", digits = digits),
   param_real(name = "rho", lower = 0.01, upper = 1.00, label = "--rho ", digits = digits),
   param_int(name = "ants", lower = 5, upper = 100, transf = "log", label = "--ants "),
   param_real(name = "q0", label = "--q0 ", lower=0.0, upper=1.0,
              condition = expression(algorithm == "acs")),
   param_int(name = "rasrank", label = "--rasranks ", lower=1, upper=quote(min(ants, 10)),
             condition = 'algorithm == "ras"'),
   param_int(name = "elitistants", label = "--elitistants ", lower=1, upper=expression(ants),
             condition = 'algorithm == "eas"'),
   param_int(name = "nnls", label = "--nnls ", lower = 5, upper = 50,
             condition = expression(localsearch %in% c(1,2,3))),
   param_cat(name = "dlb",  label = "--dlb ", values = c(0,1),
             condition = "localsearch %in% c(1,2,3)"),
   forbidden = "(alpha == 0) & (beta == 0)")
#> List of 11
#>  $ algorithm  :List of 8
#>   ..$ name        : chr "algorithm"
#>   ..$ type        : chr "c"
#>   ..$ domain      : chr [1:5] "as" "mmas" "eas" "ras" ...
#>   ..$ label       : chr "--"
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   : logi TRUE
#>   ..- attr(*, "class")= chr [1:3] "ParamCat" "Parameter" "list"
#>  $ localsearch:List of 8
#>   ..$ name        : chr "localsearch"
#>   ..$ type        : chr "o"
#>   ..$ domain      : chr [1:4] "0" "1" "2" "3"
#>   ..$ label       : chr "--localsearch "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   : logi TRUE
#>   ..- attr(*, "class")= chr [1:3] "ParamOrd" "Parameter" "list"
#>  $ alpha      :List of 9
#>   ..$ name        : chr "alpha"
#>   ..$ type        : chr "r"
#>   ..$ domain      : num [1:2] 0 5
#>   ..$ label       : chr "--alpha "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   : logi TRUE
#>   ..$ digits      : int 4
#>   ..- attr(*, "class")= chr [1:3] "ParamReal" "Parameter" "list"
#>  $ beta       :List of 9
#>   ..$ name        : chr "beta"
#>   ..$ type        : chr "r"
#>   ..$ domain      : num [1:2] 0 10
#>   ..$ label       : chr "--beta "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   : logi TRUE
#>   ..$ digits      : int 4
#>   ..- attr(*, "class")= chr [1:3] "ParamReal" "Parameter" "list"
#>  $ rho        :List of 9
#>   ..$ name        : chr "rho"
#>   ..$ type        : chr "r"
#>   ..$ domain      : num [1:2] 0.01 1
#>   ..$ label       : chr "--rho "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   : logi TRUE
#>   ..$ digits      : int 4
#>   ..- attr(*, "class")= chr [1:3] "ParamReal" "Parameter" "list"
#>  $ ants       :List of 8
#>   ..$ name        : chr "ants"
#>   ..$ type        : chr "i"
#>   ..$ domain      : int [1:2] 5 100
#>   ..$ label       : chr "--ants "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr "log"
#>   .. ..- attr(*, "lower")= num 1.6094379124341
#>   .. ..- attr(*, "upper")= num 4.61512051684126
#>   ..$ condition   : logi TRUE
#>   ..- attr(*, "class")= chr [1:3] "ParamInt" "Parameter" "list"
#>  $ q0         :List of 9
#>   ..$ name        : chr "q0"
#>   ..$ type        : chr "r"
#>   ..$ domain      : num [1:2] 0 1
#>   ..$ label       : chr "--q0 "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   :  expression(algorithm == "acs")
#>   ..$ digits      : int 15
#>   ..- attr(*, "class")= chr [1:3] "ParamReal" "Parameter" "list"
#>  $ rasrank    :List of 8
#>   ..$ name        : chr "rasrank"
#>   ..$ type        : chr "i"
#>   ..$ domain      :  expression(1L, min(ants, 10))
#>   ..$ label       : chr "--rasranks "
#>   ..$ is_dependent: logi TRUE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   :  expression(algorithm == "ras")
#>   ..- attr(*, "class")= chr [1:3] "ParamInt" "Parameter" "list"
#>  $ elitistants:List of 8
#>   ..$ name        : chr "elitistants"
#>   ..$ type        : chr "i"
#>   ..$ domain      :  expression(1L, ants)
#>   ..$ label       : chr "--elitistants "
#>   ..$ is_dependent: logi TRUE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   :  expression(algorithm == "eas")
#>   ..- attr(*, "class")= chr [1:3] "ParamInt" "Parameter" "list"
#>  $ nnls       :List of 8
#>   ..$ name        : chr "nnls"
#>   ..$ type        : chr "i"
#>   ..$ domain      : int [1:2] 5 50
#>   ..$ label       : chr "--nnls "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   :  expression(localsearch %in% c(1, 2, 3))
#>   ..- attr(*, "class")= chr [1:3] "ParamInt" "Parameter" "list"
#>  $ dlb        :List of 8
#>   ..$ name        : chr "dlb"
#>   ..$ type        : chr "c"
#>   ..$ domain      : chr [1:2] "0" "1"
#>   ..$ label       : chr "--dlb "
#>   ..$ is_dependent: logi FALSE
#>   ..$ isFixed     : logi FALSE
#>   ..$ transform   : chr ""
#>   ..$ condition   :  expression(localsearch %in% c(1, 2, 3))
#>   ..- attr(*, "class")= chr [1:3] "ParamCat" "Parameter" "list"
#> Forbidden:
#> [[1]]
#> <bytecode: 0x556344f7e408>
#> attr(,"source")
#> [1] "(alpha == 0) & (beta == 0)"
#> 
```
