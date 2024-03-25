RaceState <- R6Class("RaceState", lock_class = TRUE,
 public = list(
   # This may be dynamically adjusted
   cluster = NULL,
   completed = "Incomplete",
   elapsed = 0L,
   elapsed_recovered = 0L,
   eliteConfigurations = NULL,
   elitist_new_instances = 0L,
   instancesList = NULL,
   model = NULL,
   next_instance = -1L,
   recovery_info = NULL,
   rejectedIDs = NULL,
   rng = NULL,
   seed = NULL,
   sessionInfo = NULL,
   target_evaluator = NULL,
   target_runner = NULL,
   timer = NULL,
   # Methods.
   initialize = function(scenario) {
     self$timer <- Timer$new()
     self$target_runner <- if (is.function(scenario$targetRunner)) 
                             bytecompile(scenario$targetRunner)
                           else if (scenario$aclib)
                             target_runner_aclib
                           else target_runner_default

     if (!is.null(scenario$targetEvaluator)) {
       self$target_evaluator <- if (is.function(scenario$targetEvaluator))
                                  bytecompile(scenario$targetEvaluator)
                                else
                                  target_evaluator_default
     }
     seed <- scenario$seed
     if (is.na(seed)) {
       seed <- trunc(runif(1, 1, .Machine$integer.max))
     }
     set_random_seed(seed)
     self$seed <- seed
     self$rng <- get_random_seed()
     if (scenario$debugLevel > 2L) {
       irace.note("RNGkind: ", paste0(self$rng$rng_kind, collapse = " "), "\n",
                  "# .Random.seed: ", paste0(self$rng$random_seed, collapse = ", "), "\n")
     }
     # We do this here it is available even if we crash.
     self$sessionInfo <- sessionInfo()
     invisible(self)
   },
   
   save_recovery = function(rejectedIDs, eliteConfigurations, model, ...) {
     self$time_elapsed()
     self$rng <- get_random_seed()
     self$eliteConfigurations <- eliteConfigurations
     self$rejectedIDs <- rejectedIDs
     self$model <- model
     self$recovery_info <- list(...)
   },
   
   recover = function(scenario) {
     self$elapsed_recovered <- self$elapsed
     self$initialize(scenario)
     restore_random_seed(self$rng)
     envir <- parent.frame()
     # FIXME: This is a bit annoying, it would be better to keep these within RaceState all the time.
     for (name in setdiff(names(formals(self$save_recovery)), "..."))
       assign(name, self[[name]], envir = envir)
     for (name in names(self$recovery_info))
       assign(name, self$recovery_info[[name]], envir = envir)
     self$stop_parallel()
     invisible(self)
   },

   time_elapsed = function() {
     self$elapsed <- self$timer$elapsed() + self$elapsed_recovered
     self$elapsed
   },
   
   start_parallel = function(scenario) {
     parallel <- scenario$parallel
     data.table::setDTthreads(if (parallel <= 1L) 1L else min(4L, parallel))
     if (!is.null(scenario$targetRunnerParallel) || parallel <= 1L)
       return(invisible(self))

     # Starting the parallel environment may set some logs to the current
     # directory, so switch to execDir momentarily.
     withr::local_dir(scenario$execDir) # setwd()
     if (scenario$mpi) {
       mpiInit(parallel, scenario$debugLevel)
     } else {
       requireNamespace("parallel", quietly = TRUE)
       if (.Platform$OS.type == 'windows' && is.null(self$cluster)) {
         # FIXME: makeCluster does not print the output generated by the workers
         # on Windows. We need to use the future package for that:
         # https://stackoverflow.com/questions/56501937/how-to-print-from-clusterapply
         self$cluster <- parallel::makeCluster(parallel)
         if (scenario$debugLevel >= 1L) irace.note("makeCluster initialized for ", parallel, " jobs.")
         # We export the global environment because the user may have defined
         # stuff there. There must be a better way to do this, but I cannot
         # figure it out. R sucks sometimes.
         parallel::clusterExport(self$cluster, ls(envir=.GlobalEnv))
         # In Windows, this needs to be exported, or we get:
         ## Error in checkForRemoteErrors(val) : 
         ##  2 nodes produced errors; first error: could not find function "target.runner"
         parallel::clusterExport(self$cluster, list("target_runner"), envir=self)
         # parallel::clusterExport(self$cluster, ls(environment(startParallel)), envir=environment(startParallel))
       }
     }
     invisible(self)
   },

   stop_parallel = function() {
     if (!is.null(self$cluster)) {
       try(parallel::stopCluster(self$cluster), silent=TRUE)
       self$cluster <- NULL
     }
     invisible(self)
   },

   print_mem_used = function(objects) {
     object_size_kb <- function (name, envir)
       object.size(get(name, envir = envir)) / 1024
     
     envir <- parent.frame()
     if (missing(objects))
       objects <- ls(envir = envir, all.names = TRUE)
     
     x <- sapply(objects, object_size_kb, envir = envir)
     y <- sapply(names(get(class(self)[1L])$public_fields), object_size_kb, envir = self)
     names(y) <- paste0("RaceState$", names(y))
     x <- c(x, y)
     # Do not print anything that is smaller than 32 Kb
     x <- x[x > 32]
     cat(sep="", sprintf("%30s : %17.1f Kb\n", names(x), x),
       sprintf("%30s : %17.1f Mb\n", "Total", sum(x) / 1024),
       # This does garbage collection and also prints memory used by R.
       sprintf("%30s : %17.1f Mb\n", "gc", sum(gc()[,2L])))
     invisible(self)
   },

   no_elitrace_init_instances = function(deterministic) {
     max_instances <- nrow(self$instancesList)
     # if next.instance == 1 then this is the first iteration.
     # If deterministic consider all (do not resample).
     if (self$next_instance == 1L || deterministic) return(seq_len(max_instances))
     irace.assert(self$next_instance < max_instances)
     self$next_instance : max_instances
   },
   
   elitrace_init_instances = function(deterministic, sampleInstances) {
     max_instances <- nrow(self$instancesList)
     # if next_instance == 1 then this is the first iteration.
     next_instance <- self$next_instance
     if (next_instance == 1L) return(seq_len(max_instances)) # Consider all
     
     new_instances <- NULL
     last_new <- next_instance - 1L + self$elitist_new_instances
     # Do we need to add new instances?
     if (self$elitist_new_instances > 0L) {
       if (last_new > max_instances) {
         # This may happen if the scenario is deterministic and we would need
         # more instances than what we have.
         irace.assert(deterministic)
         if (next_instance <= max_instances) {
           # Add all instances that we have not seen yet as new ones.
           last_new <- max_instances
           new_instances <- next_instance : last_new
         } # else new_instances remains NULL and last_new remains > number of instances.
         # We need to update this because the value is used below and now there
         # may be fewer than expected, even zero.
         self$elitist_new_instances <- length(new_instances)
       } else {
         new_instances <- next_instance : last_new
       }
     }
     # FIXME: we should sample taking into account the block-size, so we sample blocks, not instances.
     past_instances <- if (sampleInstances) sample.int(next_instance - 1L)
                       else seq_len(next_instance - 1L)
 
     # new_instances + past.instances + future_instances
     if ((last_new + 1L) <= max_instances) {
       future_instances <- (last_new + 1L) : max_instances
       return(c(new_instances, past_instances, future_instances))
     }
     c(new_instances, past_instances)
   }

))