RaceState <- R6Class("RaceState", lock_class = TRUE,
 public = list(
   # This may be dynamically adjusted
   cluster = NULL,
   completed = "Incomplete",
   elapsed = 0L,
   elapsed_recovered = 0L,
   elitist_new_instances = 0L,
   experiment_log = NULL,
   instances_log = NULL,
   minSurvival = NULL,
   next_instance = -1L,
   race_experiment_log = NULL,
   recovery_info = NULL,
   recovery_mode = FALSE,
   rejected_ids = NULL,
   rng = NULL,
   seed = NULL,
   session_info = NULL,
   target_evaluator = NULL,
   target_runner = NULL,
   timeUsed = 0,
   time_next_save = 0,
   timer = NULL,
   # Methods.
   initialize = function(scenario, new = TRUE, recover = FALSE) {
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
     irace_assert(new || !recover)
     if (new) {
       # elitist_new_instances must be a multiple of blockSize.
       self$elitist_new_instances <- scenario$elitistNewInstances * scenario$blockSize
       # We cannot recover if we did not get to initialize self$rng.
       if (recover && !is.null(self$rng)) {
         restore_random_seed(self$rng)
         self$recovery_mode <- TRUE
         set(self$experiment_log, j = "iteration", value = NULL)
         self$recovery_info <- rbindlist(c(list(self$experiment_log), self$race_experiment_log), use.names = TRUE)
         # Reinitialize some state.
         self$completed = "Incomplete"
         self$elapsed = 0L
         self$elapsed_recovered = 0L
         self$experiment_log = NULL
         self$next_instance = -1L
         self$race_experiment_log = NULL
         self$rejected_ids = NULL
         self$timeUsed = 0
         self$time_next_save = 0
         # Just in case anything is still running.
         self$stop_parallel()
       } else {
         seed <- scenario$seed
         if (is.na(seed))
           seed <- trunc(runif(1, 1, .Machine$integer.max))
         set_random_seed(seed)
         self$seed <- seed
         self$rng <- get_random_seed()
       }
     } else { # !new
       self$elapsed_recovered <- self$elapsed
       restore_random_seed(self$rng)
     }

     if (is.null(self$experiment_log)) {
       self$experiment_log <- data.table(iteration=integer(0), instance=integer(0),
         configuration=integer(0), cost = numeric(0), time = numeric(0),
         bound = if (is.null(scenario$boundMax)) NULL else numeric(0))
     }

     if (scenario$debugLevel >= 3L) {
       irace_note("RNGkind: ", paste0(self$rng$rng_kind, collapse = " "), "\n",
                  "# .Random.seed: ", paste0(self$rng$random_seed, collapse = ", "), "\n")
     }
     # We do this here, so it is available even if we crash.
     self$session_info <- utils::sessionInfo()
     invisible(self)
   },

   update_experiment_log = function(output, instances, scenario) {
     # FIXME: The instances parameter is not needed.
     irace_assert(all.equal(rep(instances, each = length(unique(output[["configuration"]]))), output$instance))
     # Extract results
     self$experiment_log <- rbindlist(list(self$experiment_log, output), use.names=TRUE)
     experiments_output_to_matrix(output, scenario)
   },

   save_recovery = function(iraceResults, logfile) {
     now <- self$timer$wallclock()
     # Do not save to disk too frequently.
     if (now >= self$time_next_save) {
       # irace_note("Saving recovery info.\n")
       iraceResults$state <- self
       save_irace_logfile(iraceResults, logfile)
       self$time_next_save <- now + .irace_minimum_saving_time
     }
   },

   update_race_experiment_log = function(experiment_log, scenario) {
     self$race_experiment_log <- c(self$race_experiment_log, list(experiment_log))
     now <- self$timer$wallclock()
     # Do not save to disk too frequently.
     if (now >= self$time_next_save) {
       # irace_note("Saving recovery info.\n")
       iraceResults <- list(
         scenario = scenario,
         irace_version = irace_version,
         state = self)
       save_irace_logfile(iraceResults, logfile = scenario$logFile)
       self$time_next_save <- now + .irace_minimum_saving_time
     }
     invisible()
   },

   reset_race_experiment_log = function() {
     res <- rbindlist(self$race_experiment_log, use.names=TRUE)
     self$race_experiment_log <- NULL
     res
   },

   recover_output = function(instance_idx, configuration_id) {
     search <- data.table(instance = instance_idx, configuration = configuration_id)
     res <- self$recovery_info[search, on = .(instance,configuration), mult="first", nomatch=NULL, which=TRUE]
     irace_assert(length(res) == 0L || length(res) == nrow(search))
     if (length(res) == 0L) {
       irace_note("Cannot find the following in recovery info:")
       print(search[!self$recovery_info, on = .(instance,configuration)])
       irace_error("Recovery terminated.")
     }
     # Get the rows.
     output <- self$recovery_info[res]
     # Delete those rows.
     self$recovery_info <- self$recovery_info[-res]
     if (nrow(self$recovery_info) == 0L) {
       irace_note("Recovery completed.\n")
       self$recovery_mode <- FALSE
       self$recovery_info <- NULL
     }
     output
   },

   update_rejected = function(rejected_ids, configurations) {
     if (length(rejected_ids) == 0L) return(NULL)
     self$rejected_ids <- c(self$rejected_ids, rejected_ids)
     configurations[configurations[[".ID."]] %in% rejected_ids, , drop = FALSE]
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
         if (scenario$debugLevel >= 1L)
           irace_note("makeCluster initialized for ", parallel, " jobs.\n")
         # We export the global environment because the user may have defined
         # stuff there. There must be a better way to do this, but I cannot
         # figure it out. R sucks sometimes.
         parallel::clusterExport(self$cluster, ls(envir=.GlobalEnv))
         # In Windows, this needs to be exported, or we get:
         ## Error in checkForRemoteErrors(val) :
         ##  2 nodes produced errors; first error: could not find function "target_runner"
         parallel::clusterExport(self$cluster, list("target_runner"), envir=self)
         if (is.function(scenario$targetRunner)
           && !identical(environment(scenario$targetRunner), globalenv())) {
           env_target_runner <- environment(scenario$targetRunner)
           funglobs <- codetools::findGlobals(self$target_runner, merge=TRUE)
           common <- intersect(funglobs, ls(envir=env_target_runner))
           if (length(common))
             parallel::clusterExport(self$cluster, common, envir=env_target_runner)
         }
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
       utils::object.size(get(name, envir = envir)) / 1024

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
  irace_assert(self$next_instance < max_instances)
  self$next_instance : max_instances
}

elitist_init_instances <- function(self, deterministic, sampleInstances, elitist_new_instances)
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
      irace_assert(deterministic)
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
  past_instances <- if (sampleInstances) sample.int(next_instance - 1L)
                    else seq_len(next_instance - 1L)

  # new_instances + past_instances + future_instances
  if (last_new + 1L <= max_instances) {
    future_instances <- (last_new + 1L) : max_instances
    return(c(new_instances, past_instances, future_instances))
  }
  c(new_instances, past_instances)
}
