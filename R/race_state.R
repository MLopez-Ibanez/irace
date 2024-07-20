RaceState <- R6Class("RaceState", lock_class = TRUE,
 public = list(
   # This may be dynamically adjusted
   cluster = NULL,
   completed = "Incomplete",
   elapsed = 0L,
   elapsed_recovered = 0L,
   elite_configurations = NULL,
   elitist_new_instances = 0L,
   experiment_log = NULL,
   instances_log = NULL,
   model = NULL,
   next_instance = -1L,
   recovery_info = NULL,
   rejected_ids = NULL,
   rng = NULL,
   seed = NULL,
   session_info = NULL,
   target_evaluator = NULL,
   target_runner = NULL,
   timer = NULL,
   # Methods.
   initialize = function(scenario, new = TRUE) {
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
     if (is.null(self$experiment_log)) {
       self$experiment_log <- data.table(iteration=integer(0), instance=integer(0),
         configuration=integer(0), time=numeric(0), bound=numeric(0))
     }

     if (new) {
       seed <- scenario$seed
       if (is.na(seed))
         seed <- trunc(runif(1, 1, .Machine$integer.max))
       set_random_seed(seed)
       self$seed <- seed
       self$rng <- get_random_seed()
       self$elitist_new_instances <- round_to_next_multiple(scenario$elitistNewInstances, scenario$blockSize)
     } else {
       self$elapsed_recovered <- self$elapsed
       restore_random_seed(self$rng)
     }
     if (scenario$debugLevel >= 3L) {
       irace.note("RNGkind: ", paste0(self$rng$rng_kind, collapse = " "), "\n",
                  "# .Random.seed: ", paste0(self$rng$random_seed, collapse = ", "), "\n")
     }
     # We do this here it is available even if we crash.
     self$session_info <- sessionInfo()
     invisible(self)
   },

   update_experiment_log = function(output, instances, configurations_id, scenario, iteration) {
     # Extract results
     times <- c()
     costs <- c()
     # FIXME: Convert output to a matrix so that we can skip this loop
     for (j in instances) {
       vcost <- unlist(lapply(output[[j]], "[[", "cost"))
       costs <- c(costs, vcost)
       vtimes <- unlist(lapply(output[[j]], "[[", "time"))
       irace.assert(!any(is.null(vtimes)))
       times <- c(times, vtimes)
     }
     if (scenario$capping)
       costs <- applyPAR(costs, boundMax = scenario$boundMax, boundPar = scenario$boundPar)

     self$experiment_log <- rbind(self$experiment_log,
       data.table(iteration = iteration, instance = rep(instances, each = length(configurations_id)),
         configuration = rep(configurations_id, times = length(instances)),
         time = times, bound = if (is.null(scenario$boundMax)) NA else scenario$boundMax))
     
     matrix(costs, nrow = length(instances), ncol = length(configurations_id),
       byrow = TRUE,
       dimnames = list(instances, as.character(configurations_id)))
   },

   save_recovery = function(elite_configurations, model, ...) {
     self$time_elapsed()
     self$rng <- get_random_seed()
     self$elite_configurations <- elite_configurations
     self$model <- model
     self$recovery_info <- list(...)
   },

   update_rejected = function(rejected_ids, configurations) {
     if (length(rejected_ids) == 0L) return(NULL)
     self$rejected_ids <- c(self$rejected_ids, rejected_ids) 
     configurations[configurations[[".ID."]] %in% rejected_ids, , drop = FALSE]
   },
   
   recover = function(scenario) {
     self$initialize(scenario, new = FALSE)
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
   }
))

no_elitist_init_instances <- function(self, deterministic)
{
  max_instances <- nrow(self$instances_log)
  # if next.instance == 1 then this is the first iteration.
  # If deterministic consider all (do not resample).
  if (self$next_instance == 1L || deterministic) return(seq_len(max_instances))
  irace.assert(self$next_instance < max_instances)
  self$next_instance : max_instances
}
   
elitist_init_instances <- function(self, deterministic, sampleInstances, elitist_new_instances, block_size)
{
  max_instances <- nrow(self$instances_log)
  # if next_instance == 1 then this is the first iteration.
  next_instance <- self$next_instance
  if (next_instance == 1L) return(seq_len(max_instances)) # Consider all
  
  new_instances <- NULL
  last_new <- next_instance - 1L + elitist_new_instances
  # Do we need to add new instances?
  if (elitist_new_instances > 0L) {
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
  irace.assert((next_instance - 1L) %% block_size == 0,
    eval_after={cat("next_instance:", next_instance, ", block_size:", block_size, "\n")})
  past_instances <- if (sampleInstances)
                      sample.int(next_instance - 1L) else seq_len(next_instance - 1L)
  
  # new_instances + past_instances + future_instances
  if (last_new + 1L <= max_instances) {
    future_instances <- (last_new + 1L) : max_instances
    return(c(new_instances, past_instances, future_instances))
  }
  c(new_instances, past_instances)
}
