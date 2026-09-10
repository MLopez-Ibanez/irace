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
#> 0.903490123232622
#> 0.886919530870126
#> 0.911157291727384
#> 0.878645030899785
#> 0.895200958226039
#> 0.896395251667589
#> 0.867963506461925
#> 0.858688725965214
#> 0.899676503444111
#> 0.914923551813781
#> 0.922645860890715
#> 0.871984746963417
#> 0.912899262264456
#> 0.931927267035091
#> 0.860225544917858
#> 0.901624463888034
#> 0.938511057040748
#> 0.916873883405421
#> 0.904684543791452
#> 0.932400306407905
#> 0.883528302024449
#> 0.86170258988522
#> 0.885463734539537
#> 0.867467741414538
#> 0.886458027294953
#> 0.930551736718722
#> 0.891181644624557
#> 0.882829494432392
#> 0.898682958196758
#> 0.873757169360118
#> 0.913695852079188
#> 0.902369831523824
#> 0.907313192386764
#> 0.883763259454156
#> 0.875931854428166
#> 0.906257296119653
#> 0.895522357450633
#> 0.900283903606566
#> 0.881257883460408
#> 0.941790219496014
#> 0.889119135522115
#> 0.8630278636287
#> 0.872988456596976
#> 0.894555931300127
#> 0.889517160931154
#> 0.901767689717941
#> 0.910651127809108
#> 0.913746982694974
#> 0.894082996027343
#> 0.888773677763936
#> # 2026-09-10 08:26:47 UTC: Starting ablation from 1 to 124
#> # Seed: 1234567
#> # Source configuration (row number is ID):
#>   tmax    temp
#> 1  972 35.9569
#> # Target configuration (row number is ID):
#>     tmax   temp
#> 124 2739 0.7383
#> # 2026-09-10 08:26:47 UTC: Executing source and target configurations on the given instances * nrep (50)...
#> # Generating configurations (row number is ID): tmax temp 
#>   tmax    temp
#> 3 2739 35.9569
#> 4  972  0.7383
#> # 2026-09-10 08:26:48 UTC: Ablation (full) of 2 configurations on 50 instances (this may take a while ...).
#> # Best changed parameters:
#> # temp : 35.9569 -> 0.7383 
#> # 2026-09-10 08:26:50 UTC: Final best configuration:
#>   tmax   temp
#> 2 2739 0.7383
plotAblation(ablog)

# Execute ablation between two selected configurations, and selecting only a
# subset of parameters, directly reading the setup from the irace log file.
ablog <- ablation(logfile, src = 1, target = 10,
                  ab_params = c("temp"), ablationLogFile = NULL)
#> # Using 'train' instances:
#> 0.903490123232622
#> 0.886919530870126
#> 0.911157291727384
#> 0.878645030899785
#> 0.895200958226039
#> 0.896395251667589
#> 0.867963506461925
#> 0.858688725965214
#> 0.899676503444111
#> 0.914923551813781
#> 0.922645860890715
#> 0.871984746963417
#> 0.912899262264456
#> 0.931927267035091
#> 0.860225544917858
#> 0.901624463888034
#> 0.938511057040748
#> 0.916873883405421
#> 0.904684543791452
#> 0.932400306407905
#> 0.883528302024449
#> 0.86170258988522
#> 0.885463734539537
#> 0.867467741414538
#> 0.886458027294953
#> 0.930551736718722
#> 0.891181644624557
#> 0.882829494432392
#> 0.898682958196758
#> 0.873757169360118
#> 0.913695852079188
#> 0.902369831523824
#> 0.907313192386764
#> 0.883763259454156
#> 0.875931854428166
#> 0.906257296119653
#> 0.895522357450633
#> 0.900283903606566
#> 0.881257883460408
#> 0.941790219496014
#> 0.889119135522115
#> 0.8630278636287
#> 0.872988456596976
#> 0.894555931300127
#> 0.889517160931154
#> 0.901767689717941
#> 0.910651127809108
#> 0.913746982694974
#> 0.894082996027343
#> 0.888773677763936
#> # 2026-09-10 08:26:50 UTC: Starting ablation from 1 to 10
#> # Seed: 1234567
#> # Source configuration (row number is ID):
#>   tmax    temp
#> 1  972 35.9569
#> # Target configuration (row number is ID):
#>    tmax    temp
#> 10  660 67.2069
#> # 2026-09-10 08:26:50 UTC: Executing source and target configurations on the given instances * nrep (50)...
#> # 2026-09-10 08:26:52 UTC: Final best configuration:
#>   tmax    temp
#> 1  972 35.9569
plotAblation(ablog, type = "mean")

# }
```
