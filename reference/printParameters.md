# Print parameter space in the textual format accepted by irace.

Print parameter space in the textual format accepted by irace.

## Usage

``` r
printParameters(parameters)
```

## Arguments

- parameters:

  `ParameterSpace`  
  Data structure containing the parameter space definition. The data
  structure has to similar to the one returned by the function
  [`readParameters`](https://mlopez-ibanez.github.io/irace/reference/readParameters.md).

## Value

[`character()`](https://rdrr.io/r/base/character.html)

## See also

[`readParameters()`](https://mlopez-ibanez.github.io/irace/reference/readParameters.md)

## Examples

``` r
parameters_table <- '
 # name       switch           type  values               [conditions (using R syntax)]
 algorithm    "--"             c     (as,mmas,eas,ras,acs)
 localsearch  "--localsearch " c     (0, 1, 2, 3)
 alpha        "--alpha "       r     (0.00, 5.00)
 beta         "--beta "        r     (0.00, 10.00)
 rho          "--rho  "        r     (0.01, 1.00)
 ants         "--ants "        i,log (5, 100)
 q0           "--q0 "          r     (0.0, 1.0)           | algorithm == "acs"
 q0dep       "--q0 "           r     (0.0, q0)            | algorithm != "acs"
 rasrank      "--rasranks "    i     (1, "min(ants, 10)") | algorithm == "ras"
 elitistants  "--elitistants " i     (1, ants)            | algorithm == "eas"
 nnls         "--nnls "        i     (5, 50)              | localsearch %in% c(1,2,3)
 dlb          "--dlb "         c     (0, 1)               | localsearch %in% c(1,2,3)

 [forbidden]
 (alpha == 0.0) & (beta == 0.0)
'
parameters <- readParameters(text=parameters_table)
#> # 2026-01-31 15:02:00 UTC: 1 expression(s) specifying forbidden configurations read.
printParameters(parameters)
#> algorithm   "--"             c (as,mmas,eas,ras,acs)
#> localsearch "--localsearch " c (0,1,2,3)      
#> alpha       "--alpha "       r (0,5)          
#> beta        "--beta "        r (0,10)         
#> rho         "--rho  "        r (0.01,1)       
#> ants        "--ants "        i,log (5,100)        
#> q0          "--q0 "          r (0,1)           | algorithm == "acs"
#> q0dep       "--q0 "          r (0,"q0")        | algorithm != "acs"
#> rasrank     "--rasranks "    i (1,"min(ants, 10)") | algorithm == "ras"
#> elitistants "--elitistants " i (1,"ants")      | algorithm == "eas"
#> nnls        "--nnls "        i (5,50)          | localsearch %in% c(1, 2, 3)
#> dlb         "--dlb "         c (0,1)           | localsearch %in% c(1, 2, 3)
#> 
#> [forbidden]
#> (alpha == 0) & (beta == 0)
#> 
#> [global]
#> digits = 4
```
