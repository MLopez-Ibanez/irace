# Read the log file produced by irace (`irace.Rdata`).

Read the log file produced by irace (`irace.Rdata`).

## Usage

``` r
read_logfile(filename, name = "iraceResults")
```

## Arguments

- filename:

  Filename that contains the log file saved by irace. Example:
  `irace.Rdata`.

- name:

  Optional argument that allows overriding the default name of the
  object in the file.

## Value

([`list()`](https://rdrr.io/r/base/list.html))

## Examples

``` r
irace_results <- read_logfile(system.file("exdata/irace-acotsp.Rdata", package="irace",
                                          mustWork=TRUE))
str(irace_results)
#> List of 10
#>  $ scenario         :List of 59
#>   ..$ scenarioFile             : chr "/home/manu/work/irace/git/devel-examples/vignette-example/scenario.txt"
#>   ..$ configurationsFile       : chr "/home/manu/work/irace/git/devel-examples/vignette-example/default.txt"
#>   ..$ logFile                  : chr "./irace-acotsp.Rdata"
#>   ..$ trainInstancesDir        : chr "./instances"
#>   ..$ trainInstancesFile       : chr "./tsp-2000-50-training.txt"
#>   ..$ testInstancesDir         : chr "./instances"
#>   ..$ testInstancesFile        : chr "./tsp-2000-50-testing.txt"
#>   ..$ testNbElites             : int 5
#>   ..$ testIterationElites      : logi TRUE
#>   ..$ maxExperiments           : int 1000
#>   ..$ seed                     : int 687542627
#>   ..$ execDir                  : chr "./"
#>   ..$ parameterFile            : chr "./parameters.txt"
#>   ..$ parameters               :Classes 'ParameterSpace', 'R6' <ParameterSpace>
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
#>     hierarchy: 1 1 1 1 1 1 2 2 2 2 2 1
#>     initialize: function (..., forbidden = NULL, verbose = 0L) 
#>     isFixed: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FA ...
#>     names: algorithm localsearch alpha beta rho ants nnls q0 dlb ra ...
#>     names_fixed: time
#>     names_numeric: alpha beta rho ants nnls q0 rasrank elitistants
#>     names_variable: algorithm localsearch alpha beta rho ants nnls q0 dlb ra ...
#>     nbFixed: 1
#>     nbParameters: 12
#>     nbVariable: 11
#>     switches: -- --localsearch  --alpha  --beta  --rho   --ants  --nnl ...
#>     types: c c r r r i i r c i i c 
#>   ..$ recoveryFile             : chr ""
#>   ..$ instances                : chr [1:50] "./instances/2000-511.tsp" "./instances/2000-512.tsp" "./instances/2000-513.tsp" "./instances/2000-514.tsp" ...
#>   ..$ sampleInstances          : logi TRUE
#>   ..$ testInstances            : chr [1:50] "./instances/2000-101.tsp" "./instances/2000-102.tsp" "./instances/2000-103.tsp" "./instances/2000-104.tsp" ...
#>   ..$ testType                 : chr "friedman"
#>   ..$ firstTest                : int 5
#>   ..$ blockSize                : int 1
#>   ..$ eachTest                 : int 1
#>   ..$ targetRunner             : chr "./target-runner"
#>   ..$ targetRunnerLauncher     : chr ""
#>   ..$ targetCmdline            : chr "{configurationID} {instanceID} {seed} {instance} {bound} {targetRunnerArgs}"
#>   ..$ targetRunnerRetries      : int 0
#>   ..$ targetRunnerTimeout      : int 0
#>   ..$ targetRunnerData         : chr ""
#>   ..$ deterministic            : logi FALSE
#>   ..$ minExperiments           : chr NA
#>   ..$ maxTime                  : int 0
#>   ..$ budgetEstimation         : num 0.05
#>   ..$ minMeasurableTime        : num 0.01
#>   ..$ parallel                 : int 2
#>   ..$ loadBalancing            : logi TRUE
#>   ..$ mpi                      : logi FALSE
#>   ..$ batchmode                : chr "0"
#>   ..$ quiet                    : logi FALSE
#>   ..$ debugLevel               : int 0
#>   ..$ softRestart              : logi TRUE
#>   ..$ softRestartThreshold     : num 1e-04
#>   ..$ elitist                  : logi TRUE
#>   ..$ elitistNewInstances      : int 1
#>   ..$ elitistLimit             : int 0
#>   ..$ capping                  : logi FALSE
#>   ..$ cappingAfterFirstTest    : logi FALSE
#>   ..$ cappingType              : chr "median"
#>   ..$ boundType                : chr "candidate"
#>   ..$ boundDigits              : int 0
#>   ..$ boundPar                 : int 1
#>   ..$ boundAsTimeout           : logi TRUE
#>   ..$ postselection            : logi TRUE
#>   ..$ aclib                    : logi FALSE
#>   ..$ nbIterations             : int 0
#>   ..$ nbExperimentsPerIteration: int 0
#>   ..$ minNbSurvival            : int 0
#>   ..$ nbConfigurations         : int 0
#>   ..$ mu                       : int 5
#>   ..$ confidence               : num 0.95
#>  $ irace_version    : chr "4.4.4.ae27aa1"
#>  $ iterationElites  : int [1:8] 31 31 31 98 120 136 167 167
#>  $ allElites        :List of 8
#>   ..$ : int [1:2] 31 9
#>   ..$ : int [1:3] 31 9 49
#>   ..$ : int [1:4] 31 9 49 72
#>   ..$ : int [1:5] 98 31 96 49 103
#>   ..$ : int [1:5] 120 136 150 134 98
#>   ..$ : int [1:5] 136 150 120 166 134
#>   ..$ : int [1:5] 167 136 150 120 166
#>   ..$ : int [1:5] 167 120 136 150 166
#>  $ experiments      : num [1:15, 1:168] 42220233 39533155 41303115 40251195 39741584 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:15] "1" "2" "3" "4" ...
#>   .. ..$ : chr [1:168] "1" "2" "3" "4" ...
#>  $ allConfigurations:'data.frame':   168 obs. of  14 variables:
#>   ..$ .ID.       : int [1:168] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ algorithm  : chr [1:168] "as" "mmas" "ras" "acs" ...
#>   ..$ localsearch: chr [1:168] "0" "2" "0" "3" ...
#>   ..$ alpha      : num [1:168] 1 0.365 2.865 1.615 4.115 ...
#>   ..$ beta       : num [1:168] 1 6.22 1.22 8.72 3.72 ...
#>   ..$ rho        : num [1:168] 0.95 0.967 0.472 0.225 0.72 ...
#>   ..$ ants       : num [1:168] 10 19 86 41 9 6 28 59 13 5 ...
#>   ..$ nnls       : num [1:168] NA 50 NA 39 16 NA 44 10 33 47 ...
#>   ..$ q0         : num [1:168] NA NA NA 0.762 NA ...
#>   ..$ dlb        : chr [1:168] NA "1" NA "0" ...
#>   ..$ rasrank    : num [1:168] NA NA 42 NA NA NA NA NA NA NA ...
#>   ..$ elitistants: num [1:168] NA NA NA NA 602 NA 508 NA NA 649 ...
#>   ..$ time       : chr [1:168] "5" "5" "5" "5" ...
#>   ..$ .PARENT.   : int [1:168] NA NA NA NA NA NA NA NA NA NA ...
#>  $ softRestart      : logi [1:7] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ state            :Classes 'RaceState', 'R6' <RaceState>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     cluster: NULL
#>     completed: Not enough budget to race more than the minimum configur ...
#>     elapsed: 5385.684 72.348 2821.706
#>     elapsed_recovered: 5353.846 71.943 2805.523
#>     elitist_new_instances: 0
#>     experiment_log: data.table, data.frame
#>     initialize: function (scenario, new = TRUE, recover = FALSE) 
#>     instances_log: data.table, data.frame
#>     minSurvival: 5
#>     next_instance: 15
#>     print_mem_used: function (objects) 
#>     race_experiment_log: NULL
#>     recover_output: function (instance_idx, configuration_id) 
#>     recovery_info: NULL
#>     recovery_mode: FALSE
#>     rejected_ids: NULL
#>     reset_race_experiment_log: function () 
#>     rng: list
#>     save_recovery: function (iraceResults, logfile) 
#>     seed: 687542627
#>     session_info: sessionInfo
#>     start_parallel: function (scenario) 
#>     stop_parallel: function () 
#>     target_evaluator: NULL
#>     target_runner: function (experiment, scenario) 
#>     timeUsed: 0
#>     time_elapsed: function () 
#>     time_next_save: 2810.927
#>     timer: Timer, R6
#>     update_experiment_log: function (output, instances, scenario) 
#>     update_race_experiment_log: function (experiment_log, scenario) 
#>     update_rejected: function (rejected_ids, configurations)  
#>  $ psrace_log       :List of 5
#>   ..$ configurations :'data.frame':  5 obs. of  16 variables:
#>   .. ..$ .ID.       : int [1:5] 167 120 136 150 166
#>   .. ..$ algorithm  : chr [1:5] "as" "as" "as" "as" ...
#>   .. ..$ localsearch: chr [1:5] "3" "3" "3" "3" ...
#>   .. ..$ alpha      : num [1:5] 4.03 3.9 3.92 3.75 4.03
#>   .. ..$ beta       : num [1:5] 1.608 2.366 0.377 2.567 1.47
#>   .. ..$ rho        : num [1:5] 0.961 0.822 0.989 0.766 0.895
#>   .. ..$ ants       : num [1:5] 13 13 8 15 20
#>   .. ..$ nnls       : num [1:5] 17 16 22 15 13
#>   .. ..$ q0         : num [1:5] NA NA NA NA NA
#>   .. ..$ dlb        : chr [1:5] "1" "1" "1" "1" ...
#>   .. ..$ rasrank    : num [1:5] NA NA NA NA NA
#>   .. ..$ elitistants: num [1:5] NA NA NA NA NA
#>   .. ..$ time       : chr [1:5] "5" "5" "5" "5" ...
#>   .. ..$ .PARENT.   : int [1:5] 150 98 98 98 120
#>   .. ..$ .RANK.     : num [1:5] 46 52 53 53 53
#>   .. ..$ .WEIGHT.   : num [1:5] 0.3333 0.2667 0.2 0.1333 0.0667
#>   ..$ instances      :Classes ‘data.table’ and 'data.frame': 15 obs. of  2 variables:
#>   .. ..$ instanceID: int [1:15] 48 49 9 33 31 2 25 39 22 36 ...
#>   .. ..$ seed      : int [1:15] 1498426593 1324006684 156117387 2123556176 975149182 657774990 1688886839 1722597766 545710096 685987118 ...
#>   .. ..- attr(*, ".internal.selfref")=<pointer: (nil)> 
#>   ..$ max_experiments: int 6
#>   ..$ experiments    : num [1:15, 1:6] 32816824 32916573 32754732 32672412 32736382 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : chr [1:15] "8" "7" "12" "14" ...
#>   .. .. ..$ : chr [1:6] "120" "134" "136" "150" ...
#>   ..$ elites         : int [1:5] 167 120 136 150 166
#>  $ testing          :List of 2
#>   ..$ experiments: num [1:50, 1:13] 32749905 32710952 33131706 32773687 32805937 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : chr [1:50] "1t" "2t" "3t" "4t" ...
#>   .. .. ..$ : chr [1:13] "31" "9" "49" "72" ...
#>   ..$ seeds      : Named int [1:50] 1987246585 415084885 310098509 1191891037 1992920706 1556635314 660937021 627123935 421925618 1440918674 ...
#>   .. ..- attr(*, "names")= chr [1:50] "1t" "2t" "3t" "4t" ...
```
