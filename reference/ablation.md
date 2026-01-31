# Performs ablation between two configurations (from source to target).

Ablation is a method for analyzing the differences between two
configurations.

## Usage

``` r
ablation(
  iraceResults,
  src = 1L,
  target = NULL,
  ab_params = NULL,
  type = c("full", "racing"),
  nrep = 1L,
  seed = 1234567L,
  ablationLogFile = "log-ablation.Rdata",
  instancesFile = "train",
  ...
)
```

## Arguments

- iraceResults:

  [`list()`](https://rdrr.io/r/base/list.html)\|`character(1)`  
  Object created by irace and typically saved in the log file
  `irace.Rdata`. If a character string is given, then it is interpreted
  as the path to the log file from which the `iraceResults` object will
  be loaded.

- src, target:

  `integer(1)|character(1)`  
  Source and target configuration IDs. By default, the first
  configuration ever evaluated (ID 1) is used as `src` and the best
  configuration found by irace is used as target. If the argument is a
  string, it is interpreted as the path to a file, with the format
  specified by
  [`readConfigurationsFile()`](https://mlopez-ibanez.github.io/irace/reference/readConfigurationsFile.md),
  that contains the configuration.

- ab_params:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Specific parameter names to be used for the ablation. They must be in
  `parameters$names`. By default, use all parameters.

- type:

  `"full"|"racing"`  
  Type of ablation to perform: `"full"` will execute each configuration
  on all `n_instances` to determine the best-performing one; `"racing"`
  will apply racing to find the best configurations.

- nrep:

  `integer(1)`  
  Number of replications per instance used in `"full"` ablation. When
  `nrep > 1`, each configuration will be executed `nrep` times on each
  instance with different random seeds.

- seed:

  `integer(1)`  
  Integer value to use as seed for the random number generation.

- ablationLogFile:

  `character(1)`  
  Log file to save the ablation log. If `NULL`, the results are not
  saved to a file.

- instancesFile:

  `character(1)`  
  Instances file used for ablation: `'train'`, `'test'` or a filename
  containing the list of instances.

- ...:

  Further arguments to override scenario settings, e.g., `debugLevel`,
  `parallel`, etc.

## Value

A list containing the following elements:

- allConfigurations:

  Configurations tested in the ablation.

- state:

  State of the ablation process.

- experiments:

  A matrix with the results of the experiments (columns are
  configurations, rows are instances).

- scenario:

  Scenario object with the settings used for the experiments.

- trajectory:

  IDs of the best configurations at each step of the ablation.

- best:

  Best configuration found in the experiments.

- complete:

  `TRUE` if the ablation process was completed.

## References

C. Fawcett and H. H. Hoos. Analysing differences between algorithm
configurations through ablation. Journal of Heuristics, 22(4):431–458,
2016.

## See also

[`plotAblation()`](https://mlopez-ibanez.github.io/irace/reference/plotAblation.md)
[`ablation_cmdline()`](https://mlopez-ibanez.github.io/irace/reference/ablation_cmdline.md)

## Author

Leslie Pérez Cáceres and Manuel López-Ibáñez

## Examples

``` r
# \donttest{
logfile <- system.file(package="irace", "exdata", "sann.rda")
# Execute ablation between the first and the best configuration found by irace.
ablog <- ablation(logfile, ablationLogFile = NULL)
#> # Using 'train' instances:
#> 0.926512248916727
#> 0.881447620781588
#> 0.909592610129919
#> 0.912459277265424
#> 0.886530077346204
#> 0.887924070489102
#> 0.883605380578211
#> 0.912413023003742
#> 0.878111345700819
#> 0.865791411063307
#> 0.903694133506134
#> 0.879149929442663
#> 0.886400217755539
#> 0.895989119241964
#> 0.909120245439193
#> 0.881979750732162
#> 0.919753681601691
#> 0.92424528483988
#> 0.896670180363852
#> 0.903829982577083
#> 0.852429177369978
#> 0.918108622913176
#> 0.899288307951119
#> 0.922136158744433
#> 0.896610086916287
#> 0.845487414580054
#> 0.93114129451634
#> 0.900461853869059
#> 0.909815497064983
#> 0.909680861020661
#> 0.865980971456221
#> 0.8855757532444
#> 0.883735413827381
#> 0.890170585764637
#> 0.91468809951868
#> 0.924272468739532
#> 0.897463392048484
#> 0.916174446291724
#> 0.915688611712287
#> 0.873002012496727
#> 0.905509184543295
#> 0.85078719203226
#> 0.914945691195527
#> 0.945176292475208
#> 0.889168401237741
#> 0.884740546827437
#> 0.906616996817406
#> 0.893070757362213
#> 0.941981446903129
#> 0.873476031894195
#> # 2026-01-31 15:01:43 UTC: Starting ablation from 1 to 125
#> # Seed: 1234567
#> # Source configuration (row number is ID):
#>   tmax    temp
#> 1 4927 23.4499
#> # Target configuration (row number is ID):
#>     tmax    temp
#> 125 1352 42.4881
#> # 2026-01-31 15:01:43 UTC: Executing source and target configurations on the given instances * nrep (50)...
#> # Generating configurations (row number is ID): tmax temp 
#>   tmax    temp
#> 3 1352 23.4499
#> 4 4927 42.4881
#> # 2026-01-31 15:01:46 UTC: Ablation (full) of 2 configurations on 50 instances (this may take a while ...).
#> # Best changed parameters:
#> # tmax : 4927 -> 1352 
#> # 2026-01-31 15:01:48 UTC: Final best configuration:
#>   tmax    temp
#> 2 1352 42.4881
plotAblation(ablog)

# Execute ablation between two selected configurations, and selecting only a
# subset of parameters, directly reading the setup from the irace log file.
ablog <- ablation(logfile, src = 1, target = 10,
                  ab_params = c("temp"), ablationLogFile = NULL)
#> # Using 'train' instances:
#> 0.926512248916727
#> 0.881447620781588
#> 0.909592610129919
#> 0.912459277265424
#> 0.886530077346204
#> 0.887924070489102
#> 0.883605380578211
#> 0.912413023003742
#> 0.878111345700819
#> 0.865791411063307
#> 0.903694133506134
#> 0.879149929442663
#> 0.886400217755539
#> 0.895989119241964
#> 0.909120245439193
#> 0.881979750732162
#> 0.919753681601691
#> 0.92424528483988
#> 0.896670180363852
#> 0.903829982577083
#> 0.852429177369978
#> 0.918108622913176
#> 0.899288307951119
#> 0.922136158744433
#> 0.896610086916287
#> 0.845487414580054
#> 0.93114129451634
#> 0.900461853869059
#> 0.909815497064983
#> 0.909680861020661
#> 0.865980971456221
#> 0.8855757532444
#> 0.883735413827381
#> 0.890170585764637
#> 0.91468809951868
#> 0.924272468739532
#> 0.897463392048484
#> 0.916174446291724
#> 0.915688611712287
#> 0.873002012496727
#> 0.905509184543295
#> 0.85078719203226
#> 0.914945691195527
#> 0.945176292475208
#> 0.889168401237741
#> 0.884740546827437
#> 0.906616996817406
#> 0.893070757362213
#> 0.941981446903129
#> 0.873476031894195
#> # 2026-01-31 15:01:48 UTC: Starting ablation from 1 to 10
#> # Seed: 1234567
#> # Source configuration (row number is ID):
#>   tmax    temp
#> 1 4927 23.4499
#> # Target configuration (row number is ID):
#>    tmax    temp
#> 10 4614 79.6999
#> # 2026-01-31 15:01:48 UTC: Executing source and target configurations on the given instances * nrep (50)...
#> # 2026-01-31 15:01:50 UTC: Final best configuration:
#>   tmax    temp
#> 1 4927 23.4499
plotAblation(ablog, type = "mean")

# }
```
