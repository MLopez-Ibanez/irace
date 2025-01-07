# ---------------------------------------- -*- mode: r; mode: font-lock -*- #
# race.R                       Racing methods for the selection of the best #
# ------------------------------------------------------------------------- #

# ========================================================================= #
# Racing methods for the selection of the best                              #
# ------------------------------------------------------------------------- #
# Copyright (C) 2003 Mauro Birattari                                        #
# ========================================================================= #
# This program is free software; you can redistribute it and/or modify it   #
# under the terms of the GNU General Public License as published by the     #
# Free Software Foundation; either version 2 of the License, or (at your    #
# option) any later version.                                                #
#                                                                           #
# This program is distributed in the hope that it will be useful, but       #
# WITHOUT ANY WARRANTY; without even the implied warranty of                #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU         #
# General Public License for more details.                                  #
#                                                                           #
# You should have received a copy of the GNU General Public License along   #
# with this program; if not, write to the Free Software Foundation, Inc.,   #
# 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.                  #
# ========================================================================= #

# ========================================================================= #
# Mauro BIRATTARI                                                           #
# IRIDIA - ULB, CP 194/6                                                    #
# Av. F. D. Roosevelt 50                                    mbiro@ulb.ac.be #
# 1050 Brussels, Belgium                     http://iridia.ulb.ac.be/~mbiro #
# ========================================================================= #
# $Id: race.R,v 1.54 2005/03/30 12:40:42 mbiro Exp $ #

createExperimentList <- function(configurations, parameters,
                                 instances, instances_ID, seeds, bounds)
{
  n_configurations <- nrow(configurations)
  configurations_id <- configurations[[".ID."]]
  pnames <- parameters$names
  configurations <- as.list(configurations[, pnames, drop=FALSE])
  # FIXME: How to do this faster?
  # - maybe purrr::transpose() ?
  # - mlr3misc::transpose_list() uses .mapply(list, configurations, list(), which is slower.
  configurations <- lapply(seq_len(n_configurations), function(i) lapply(configurations, `[`, i))
  dots <- list(id_configuration = configurations_id, configuration = configurations)
  if (!is.null(bounds)) {
    # There must be a bound for each configuration.
    # FIXME: There must be a bound for each configuration AND each instance.
    irace_assert(length(bounds) == n_configurations)
    dots$bound <- bounds
  }
  configurations <- .mapply(list, dots = dots, MoreArgs = NULL)
  instances <- instances[instances_ID]
  n_instances <- length(instances_ID)
  instances <- .mapply(list,
    dots = list(id_instance = instances_ID, seed = seeds, instance = instances),
    MoreArgs = NULL)
  .mapply(c, dots = list(
    rep(instances, each = n_configurations),
    rep(configurations, times = n_instances)), MoreArgs = NULL)
}


race_wrapper_helper <- function(race_state, configurations, instance_idx, bounds,
                         is_exe, scenario)
{
  # Experiment list to execute
  experiments <- createExperimentList(configurations, parameters = scenario$parameters,
                                      instances = scenario$instances,
                                      instances_ID = race_state$instances_log[["instanceID"]][instance_idx],
                                      seeds = race_state$instances_log[["seed"]][instance_idx],
                                      bounds = bounds)

  irace_assert(length(is_exe) == length(experiments))
  instance_idx <- rep(instance_idx, each = nrow(configurations))
  if (race_state$recovery_mode) {
    # With targetEvaluator or if everything is executed, we recover everything.
    if (!is.null(scenario$targetEvaluator) || all(is_exe)) {
      configuration_id <- unlist_element(experiments, "id_configuration")
      target_output <- race_state$recover_output(instance_idx, configuration_id)
    } else {
      irace_assert(any(is_exe))
      configuration_id <- unlist_element(experiments[is_exe], "id_configuration")
      target_output <- race_state$recover_output(instance_idx[is_exe], configuration_id)
    }
  } else { # !recovery_mode
    # We cannot let targetRunner or targetEvaluator modify our random seed, so we save it.
    withr::local_preserve_seed()
    target_output <- vector("list", length(experiments))
    # Execute experiments for which is_exe is TRUE:
    if (any(is_exe))
      target_output[is_exe] <- execute_experiments(race_state, experiments[is_exe], scenario)

    # If targetEvaluator is NULL, then target_output must contain the right
    # output already.  Otherwise, targetEvaluator considers all experiments.
    if (!is.null(scenario$targetEvaluator)) {
      target_output <- execute_evaluator(race_state$target_evaluator, experiments, scenario, target_output)
    } else if (any(!is_exe))  {
      experiments <- experiments[is_exe]
      instance_idx <- instance_idx[is_exe]
    }
    target_output <- rbindlist(target_output, fill=TRUE, use.names=TRUE)
    set(target_output, j = setdiff(colnames(target_output), c("cost", "time")), value = NULL)
    if ("time" %notin% colnames(target_output))
      set(target_output, j = "time", value = NA)
    set(target_output, j = "configuration", value = unlist_element(experiments, "id_configuration"))
    set(target_output, j = "instance", value = instance_idx)
    if (!is.null(bounds))
      set(target_output, j = "bound", value = unlist_element(experiments, "bound"))
  }
  target_output
}

## Executes a list of configurations in a particular instance
## configurations: description having the id of the configuration
## instance.idx: index of the instance,seed pair in race_state$instances_log
## bounds: execution bounds (NULL if not needed).
## is_exe: Boolean vector that determines which experiments need to executed.
race_wrapper <- function(race_state, configurations, instance_idx, bounds,
                         is_exe, scenario)
{
  target_output <- race_wrapper_helper(race_state, configurations, instance_idx,
    bounds, is_exe, scenario)
  race_state$update_race_experiment_log(target_output, scenario)
  target_output
}

experiments_output_to_matrix <- function(output, scenario)
{
  if (scenario$capping)
    output[["cost"]] <- applyPAR(output[["cost"]], boundMax = scenario$boundMax, boundPar = scenario$boundPar)
  as.matrix(dcast(output[, c("instance", "configuration", "cost")], instance ~ configuration, value.var = "cost"),
    rownames = "instance")
}

aux2.friedman <- function(y, I, alive, conf.level = 0.95)
{
  dropped.any <- FALSE
  n <- nrow(y)
  k <- length(I)
  r <- rowRanks(y, cols = I, ties.method = "average", useNames = FALSE)
  R <- colSums2(r, useNames = FALSE)
  o <- order(R)
  best <- I[o[1L]]
  TIES <- tapply(c(r), row(r), table)
  STATISTIC <- ((12 * sum((R - n * (k + 1L) / 2)^2)) /
                (n * k * (k + 1L)
                  - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                     (k - 1L))))
  PARAMETER <- k - 1L
  PVAL      <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  #names(STATISTIC) <- "Friedman chi-squared"
  #names(PARAMETER) <- "df"

  alpha <- 1 - conf.level
  if (!is.nan(PVAL) && PVAL < alpha) {
    # This formula for multiple comparisons comes from Conover, "Practical
    # Nonparametric Statistics", 1999, pages 369-371.
    A <- sum(as.vector(r)^2)
    t <- qt(1 - alpha / 2, df = (n - 1L) * (k - 1L)) *
      (2 * (n * A - sum(R^2)) / ((n - 1L) * (k - 1L)))^(1 / 2)
    J <- best
    for (j in 2L:k) {
      if (abs(R[o[j]] - R[o[1L]]) > t) {
        break
      } else {
        J <- c(J, I[o[j]])
      }
    }
    alive[-J] <- FALSE
    dropped.any <- TRUE
  }
  irace_assert(I[which.min(R)] == best)
  list(ranks = R, alive = alive, dropped.any = dropped.any, p.value = PVAL)
}

aux_friedman <- function(results, alive, which_alive, conf.level)
{
  no.alive <- length(which_alive)
  if (no.alive > 2L) {
    # If more then 2 configurations are left, use Friedman
    return(aux2.friedman(results, which_alive, alive, conf.level = conf.level))
  }

  ranks <- NULL
  dropped.any <- TRUE
  PVAL <- 0
  # If only 2 configurations are left, switch to Wilcoxon
  V1 <- results[, which_alive[1L]]
  V2 <- results[, which_alive[2L]]
  diffs <- V1 - V2
  # Avoid the test if the answer is obvious
  if (all(diffs <= 0)) {
    ranks <- c(1L,2L)
  } else if (all(diffs >= 0)) {
    ranks <- c(2L,1L)
  } else {
    ZEROES <- any(diffs == 0)
    if (ZEROES)
      diffs <- diffs[diffs != 0]
    r <- rank(abs(diffs))
    TIES <- length(r) != length(unique(r))
    diffs <- outer(diffs, diffs, "+")
    diffs <- sort(diffs[!lower.tri(diffs)])/2
    PVAL <- wilcox.test(V1, V2, paired = TRUE, exact = if (ZEROES || TIES) FALSE else NULL)$p.value
    irace_assert(!is.nan(PVAL) & !is.na(PVAL))
    if (PVAL >= 1 - conf.level) dropped.any <- FALSE
    # We use the pseudo median (see wilcox.test.default)
    ranks <- if (median(diffs) <= 0) c(1L,2L) else c(2L,1L)
  }
  if (dropped.any)
    alive[which_alive[ranks[2L]]] <- FALSE

  list(ranks = ranks, alive = alive, dropped.any = dropped.any, p.value = PVAL)
}

aux.one_ttest <- function(results, alive, which_alive, conf.level,
                          adjust = c("none","bonferroni","holm"))
{
  adjust <- match.arg(adjust)
  irace_assert(sum(alive) == length(which_alive))
  results <- results[, which_alive]
  means <- colMeans2(results)
  best <- which.min(means)
  mean_best <- means[best]
  pvals <- sapply(means, function(x) as.numeric(isTRUE(
                                       all.equal.numeric(mean_best[[1L]], x[[1L]], check.attributes = FALSE))))
  results_best <- results[, best]
  var_best <- var(results_best)
  which_test <- which(pvals < 1.0)
  for (j in which_test) {
    PVAL <- pvals[j]
    if (PVAL == 1.0) next
    results_j <- results[, j]
    # t.test may fail if the data in each group is almost constant. Hence, we
    # surround the call in a try() and we initialize p with 1 if the means are
    # equal or zero if they are different
    if (min(var(results_best), var(results_j)) < 10 * .Machine$double.eps) next
    # The t.test may fail if the data are not normal despite one configuration
    # clearly dominating the other.
    if (all(results_best <= results_j)) next
    try(PVAL <- t.test(results_best, results_j, alternative = "less", paired = TRUE)$p.value)
    irace_assert(!is.nan(PVAL) & !is.na(PVAL))
    pvals[j] <- PVAL
  }
  pvals <- p.adjust(pvals, method = adjust)
  dropj <- which_alive[pvals < 1.0 - conf.level]
  dropped_any <- length(dropj) > 0
  irace_assert(all(alive[dropj]))
  alive[dropj] <- FALSE
  list(ranks = means, alive = alive, dropped.any = dropped_any, p.value = min(pvals))
}

aux.ttest <- function(results, alive, which_alive, conf.level,
                      adjust = c("none","bonferroni","holm"))
{
  adjust <- match.arg(adjust)
  irace_assert(sum(alive) == length(which_alive))

  means <- colMeans2(results, cols = which_alive, useNames = FALSE)
  # FIXME: break ties using median or ranks?
  best <- which.min(means)
  mean_best <- means[best]
  pvals <- sapply(means, function(x) as.numeric(isTRUE(
                                       all.equal.numeric(mean_best[[1L]], x[[1L]], check.attributes = FALSE))))
  results <- results[, which_alive]
  results_best <- results[, best]
  var_best <- var(results_best)
  which_test <- which(pvals < 1.0)
  for (j in which_test) {
    PVAL <- pvals[j]
    if (PVAL == 1.0) next
    results_j <- results[, j]
    # t.test may fail if the data in each group is almost constant. Hence, we
    # surround the call in a try() and we initialize p with 1 if the means are
    # equal or 0 if they are different.
    if (min(var(results_best), var(results_j)) < 10 * .Machine$double.eps) next
    # The t.test may fail if the data are not normal despite one configuration
    # clearly dominating the other.
    if (all(results_best <= results_j)) next
    try(PVAL <- t.test(results_best, results_j, paired = TRUE)$p.value)
    irace_assert(!is.nan(PVAL) & !is.na(PVAL))
    pvals[j] <- PVAL
  }
  pvals <- p.adjust(pvals, method = adjust)
  dropj <- which_alive[pvals < 1.0 - conf.level]
  dropped_any <- length(dropj) > 0L
  irace_assert(all(alive[dropj]))
  alive[dropj] <- FALSE
  list(ranks = means, alive = alive, dropped.any = dropped_any, p.value = min(pvals))
}

table_hline <- function(widths)
  paste0(collapse="+", c("",strrep("-", widths), "\n"))

table_sprintf <- function(text, widths)
  paste0(collapse="|", c("", sprintf("%*s",widths, text), "\n"))

.nocap_table_fields_width <- as.integer(c(1, 11, 11, 11, 16, 11, 8, 5, 4, 6))
.nocap_colum_names <- c(" ", "Instance", "Alive", "Best", "Mean best", "Exp so far",
                        "W time", "rho", "KenW", "Qvar")
.capping_table_fields_width <- as.integer(c(1, 11, 8, 11, 11, 16, 11, 8, 5, 4, 6))
.capping_colum_names <- c(" ", "Instance", "Bound", "Alive", "Best", "Mean best", "Exp so far",
                        "W time", "rho", "KenW", "Qvar")
.capping_hline <- table_hline(.capping_table_fields_width)
.nocap_hline <- table_hline(.nocap_table_fields_width)
.race_common_header <-  "# Markers:
     x No test is performed.
     c Configurations are discarded only due to capping.
     - The test is performed and some configurations are discarded.
     = The test is performed but no configuration is discarded.
     ! The test is performed and configurations could be discarded but elite configurations are preserved.
     . All alive configurations are elite and nothing is discarded.\n\n"

.nocap_header <- paste0(.race_common_header, .nocap_hline,
  table_sprintf(.nocap_colum_names, .nocap_table_fields_width), .nocap_hline)
.capping_header <- paste0(.race_common_header, .capping_hline,
  table_sprintf(.capping_colum_names, .capping_table_fields_width), .capping_hline)

race_print_header_nocap <- function()
  cat(sep = "", .nocap_header)

race_print_header_cap <- function()
  cat(sep = "", .capping_header)

elapsed_wctime_str <- function(now, start) {
  if (now <= start) return("00:00:00")
  elapsed <- difftime(now, start, units = "secs")
  # FIXME: Maybe better and faster if we only print seconds?
  format(.POSIXct(elapsed, tz="GMT"), "%H:%M:%S")
}

race_print_task <- function(res.symb, Results,
                            instance,
                            current_task,
                            which_alive,
                            id_best,
                            best,
                            experiments_used,
                            start_time,
                            bound, capping)
{
  cat(sprintf("|%s|%11d|", res.symb, instance))
  if (capping) {
    if (is.null(bound))
      cat("      NA|")
    else
      cat(sprintf("%8.2f|", bound))
  }
  # FIXME: This is the mean of the best, but perhaps it should
  # be the sum of ranks in the case of test == friedman?
  mean_best <- mean(Results[, best])
  time_str <- elapsed_wctime_str(Sys.time(), start_time)
  cat(sprintf(paste0("%11d|%11d|", .irace.format.perf, "|%11d|%s"),
              length(which_alive), id_best, mean_best, experiments_used, time_str))

  if (current_task > 1L && length(which_alive) > 1L) {
    res <- Results[, which_alive, drop = FALSE]
    conc <- concordance(res)
    qvar <- dataVariance(res)
    # FIXME: We would like to use %+#4.2f but this causes problems with
    # https://github.com/oracle/fastr/issues/191
    cat(sprintf("|%+4.2f|%.2f|%.4f|\n", conc$spearman.rho, conc$kendall.w, qvar))
  } else {
    cat("|   NA|  NA|    NA|\n")
  }
}

race_print_footer <- function(bestconf, mean_best, break_msg, debug_level, capping = FALSE,
                              old_best_id)
{
  cat(sep = "",
      if (capping) .capping_hline else .nocap_hline,
      if (debug_level >= 1L) paste0("# Stopped because ", break_msg, "\n"),
      if (!is.null(old_best_id)) paste0("Best configuration for the instances in this race: ", old_best_id, "\n"),
      sprintf("Best-so-far configuration: %11d", bestconf[[".ID."]][1L]),
      "    mean value: ",
      sprintf(.irace.format.perf, mean_best), "\n",
      "Description of the best-so-far configuration:\n")
  configurations_print(bestconf, metadata = TRUE)
  cat("\n")
}

## This function calculates an execution bound
## data: matrix columns as configurations and rows instances
## type:
##   median: bound based on the configuration's mean median
##   mean:   bound based on the configuration's mean mean
##   worst:  bound based on the worst configuration's mean
##   best:  bound based on the best configurations's mean
executionBound <- function(data, type = "median")
{
  irace_assert (ncol(data) >= 1L)

  if (ncol(data) == 1L) {
    return (mean(data[,1L], na.rm = TRUE))
  }
  # This should never happen because the data used to obtain the execution
  # bound should be complete, that is, the bounding configurations should have
  # been executed on all previous instances.
  irace_assert (!anyNA(data))
  colmeans <- colMeans2(data)
  bound <- switch (type,
                   median = median(colmeans),
                   mean   = mean(colmeans),
                   worst  = max(colmeans),
                   # default:
                   min(colmeans))
  bound
}

## This function calculates an execution bound per instance
## data: array of the execution times on the current instance
## type:
##   median: bound based on the median time
##   mean:   bound based on the mean time
##   worst:  bound based on the worst time
##   best:  bound based on the best time
instanceBound <- function(data, type="median")
{
  irace_assert (!anyNA(data))
  switch (type,
          median = median(data),
          mean   = mean(data),
          worst  = max(data),
          # default:
          min(data))
}

## This function returns survivors obtained after applying the dominance elimination
## criterion.
##  results: matrix of experiments results (all configurations)
##  elites: index of elite configurations
##  alive: bolean array of alive configurations
##  eps: constant added to the bound to account for the measuring precision
dom_elim <- function(results, elites, alive, scenario, minSurvival, eps = 1e-5)
{
  which_alive <- which(alive)
  irace_assert(length(which_alive) >= minSurvival)
  cmeans <- colMeans2(results, cols = which_alive, useNames = FALSE)
  irace_assert(!all(is.na(cmeans))) # Only NA values when calculating mean for dominance.

  # When there are no protected elites left, select the best configurations to
  # calculate the bounds. This is quite aggressive and another alternative
  # would be to disable dom_elim when elites == 0.
  if (length(elites) == 0L) {
    # In the case we have only two alive configurations only one can be elite.
    if (length(which_alive) <= 2L)
      elites <- which_alive[which.min(cmeans)]
    else
      elites <- which_alive[order(cmeans, na.last = TRUE, decreasing = FALSE)[seq_len(minSurvival)]]
  }

  bound <- executionBound(results[, elites, drop = FALSE], type = scenario$cappingType)
  alive[which_alive] <- ((bound + eps) >= cmeans)
  alive
}

## This function applies PARX (X=boundPar) to all experiments
## that exceed the maximum execution time (boundMax)
applyPAR <- function(results, boundMax, boundPar)
{
  # We do not want to change Inf or -Inf because those represent rejection.
  if (boundPar != 1)
    results[is.finite(results) & results >= boundMax] <- boundMax * boundPar
  results
}

## This function calculates the execution time allowed for the executions
## of configurations based on previous execution times.
## It returns a list with two elements:
## * elite_bound : value (mean execution time or per instance execution time)
## used to calculate the maximum execution time (final_bounds) for each configuration.
## * final_bounds[i] : maximum execution time for candidate i on the current instance.
final_execution_bound <- function(experimentsTime, elites, current_task,
                                  which_alive, which_exe, scenario)
{
  boundMax <- scenario$boundMax
  final_bounds <- rep(boundMax, length(which_alive))

  # Elite candidates can have NA values due to the rejection.
  if (length(elites))
    elites <- elites[!is.na(experimentsTime[current_task,elites])]

  # Only apply bounds when there is previous data
  if (length(elites) == 0L || length(which_exe) == 0L) {
    # FIXME: should we use an adjusted boundMax ?
    return(list(final_bounds = final_bounds, elite_bound = boundMax))
  }
  minMeasurableTime <- scenario$minMeasurableTime

  # The elite membership is updated before calling this function to know
  # the configurations used for calculating the bounds we need to do this.
  # Note that some of these configurations could be not elite anymore for the
  # elimination phase, given that all their evaluations have been used.
  if (scenario$boundType == "instance") {
    elite_bound <- instanceBound(experimentsTime[current_task, elites],
      type = scenario$cappingType)
    final_bounds_exe <- min(elite_bound + minMeasurableTime, boundMax)
    final_bounds_exe <- ceiling_digits(final_bounds_exe, scenario$boundDigits)
  } else {
    elite_bound <- executionBound(experimentsTime[seq_len(current_task), elites, drop = FALSE],
      type = scenario$cappingType)
    elite_bound <- min(elite_bound, boundMax)
    total_time <- (current_task * elite_bound) + minMeasurableTime
    final_bounds_exe <- total_time - colSums2(experimentsTime, rows = seq_len(current_task), cols = which_exe, na.rm = TRUE, useNames = FALSE)
    # There are cases in which a small negative budget is used. For example:
    # assuming candidates 1 and 2 are elite:
    # maxBound <- 80
    # executionTime <- matrix(c(0.010,0.0170,0.010, 24,28,27, 0.010,0.017,NA),
    #        byrow=TRUE, ncol=3, nrow=3, dimnames=list(c(1,2,3),c(1,2,3)))
    # current_task <- 3; boundDigits = 0
    # elite_bound  <- irace:::executionBound(executionTime[1:current_task,1:2], "median")
    # total_time   <- elite_bound * current_task + 0.01
    # final_bounds_exe <- total_time - colSums2(executionTime[1:current_task,3,drop=FALSE], na.rm=TRUE)
    #
    # We set the execution time to the elite_bound, this should be enough
    # to eliminate a bad candidate for the next task.
    final_bounds_exe[final_bounds_exe <= 0] <- elite_bound
    # We round up the bounds up to the specified number of digits. This may
    # be necessary if the target-algorithm does not support higher precision.
    final_bounds_exe <- ceiling_digits(final_bounds_exe, scenario$boundDigits)
    final_bounds_exe[final_bounds_exe > boundMax] <- boundMax
    irace_assert(all(final_bounds_exe > 0))
  }
  final_bounds[which(which_alive %in% which_exe)] <- final_bounds_exe
  list(final_bounds = final_bounds, elite_bound = elite_bound)
}

get_ranks <- function(x, test)
  if (test == "friedman") colSums2(rowRanks(x, ties.method="average")) else rank(colMeans2(x))

# Recompute the best as follows. Given two configurations, the one evaluated
# on more instances is ranked better. Otherwise, break ties according to the
# criteria of the stat test.
overall_ranks <- function(x, test)
{
  if (ncol(x) == 1L) return(1L)

  ninstances <- colSums2(!is.na(x))
  uniq_ninstances <- sort(unique(ninstances), decreasing = TRUE)
  last_r <- 0L
  ranks <- rep_len(Inf, ncol(x))
  # Iterate from the largest to the lowest number of instances.
  for (k in uniq_ninstances) {
    confs <- which(ninstances == k)
    irace_assert(all(is.infinite(ranks[confs])))
    r <- 1L
    if (length(confs) > 1L) {
      # Select only non-NA rows
      y <- x[, confs, drop = FALSE]
      y <- y[complete.cases(y), , drop = FALSE]
      r <- get_ranks(y, test = test)
    }
    r <- r + last_r
    last_r <- max(r)
    ranks[confs] <- r
  }
  ranks
}

# Remove one elite count from every configuration not executed.
update_is_elite <- function(is_elite, which_exe)
{
  which_notexecuted <- setdiff(which(is_elite > 0L), which_exe)
  is_elite[which_notexecuted] <- is_elite[which_notexecuted] - 1L
  irace_assert (all(is_elite >= 0L))
  is_elite
}

update_elite_safe <- function(Results, is_elite)
{
  elites <- is_elite > 0L
  if (!any(elites)) return(0L) # All elites rejected.
  max(which(rowAnyNotNAs(Results, cols = elites)))
}

generateTimeMatrix <- function(elite_ids, experiment_log)
{
  # Silence CRAN warnings
  configuration <- bound <- NULL
  # Remove everything that we don't need.
  experiment_log <- experiment_log[(configuration %in% elite_ids) & !is.na(time),
    c("configuration", "instance", "time", "bound")]
  experiment_log[, time := pmin.int(time, bound)]
  experiment_log[, bound := NULL]
  time_matrix <- dcast(experiment_log, instance ~ configuration, value.var = "time")
  setcolorder(time_matrix, neworder = as.character(elite_ids))
  as.matrix(time_matrix, rownames = "instance")
}

race <- function(race_state, maxExp,
                 minSurvival = 1L,
                 configurations,
                 scenario)
  elitist_race(race_state = race_state, maxExp = maxExp,
               minSurvival = minSurvival,
               elite_data = NULL,
               configurations = configurations,
               scenario = scenario,
               elitist_new_instances = 0L)

elitist_race <- function(race_state, maxExp,
                 minSurvival = 1L,
                 elite_data = NULL,
                 configurations,
                 scenario,
                 elitist_new_instances,
                 firstTest = scenario$firstTest)
{
  blockSize <- scenario$blockSize
  conf.level <- scenario$confidence
  firstTest <- blockSize * firstTest
  eachTest <- blockSize * scenario$eachTest
  elitist <- scenario$elitist
  capping <- scenario$capping
  n_configurations <- nrow(configurations)
  alive <- rep_len(TRUE, n_configurations)
  is_rejected <- logical(n_configurations)

  ## FIXME: Remove argument checking. This must have been done by the caller.
  irace_assert(maxExp > 0L)

  if (n_configurations <= minSurvival) {
    irace_error("Not enough configurations (", n_configurations,
                ") for a race (minSurvival=", minSurvival, ")")
  }

  # Check argument: conf.level
  if (!missing(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level)!=1 ||
       !is.finite(conf.level) || conf.level < 0 || conf.level > 1))
    stop("conf.level must be a single number between 0 and 1")

  if (scenario$quiet) {
    print_header <- print_task <- print_footer <- do_nothing
  } else {
    print_header <- if (capping) race_print_header_cap else race_print_header_nocap
    print_task <- race_print_task
    print_footer <- race_print_footer
  }

  stat_test <- scenario$testType
  do_test <- switch(stat_test,
    friedman = function(results, alive, which_alive, conf.level = scenario$confidence)
      aux_friedman(results, alive, which_alive, conf.level = conf.level),
    t.none = function(results, alive, which_alive, conf.level = scenario$confidence)
      aux.ttest(results, alive, which_alive, conf.level = conf.level, adjust = "none"),
    t.holm = function(results, alive, which_alive, conf.level = scenario$confidence)
      aux.ttest(results, alive, which_alive, conf.level = conf.level, adjust = "holm"),
    t.bonferroni = function(results, alive, which_alive, conf.level = scenario$confidence)
      aux.ttest(results, alive, which_alive, conf.level = conf.level, adjust = "bonferroni"))
  do_test <- bytecompile(do_test)

  # Create the instance list according to the algorithm selected.
  if (elitist) {
    # FIXME: we should sample taking into account the block-size, so we sample blocks, not instances.
    irace_assert((race_state$next_instance - 1L) %% blockSize == 0,
      eval_after={cat("next_instance:", race_state$next_instance, ", block_size:", blockSize, "\n")})
    race_instances <- elitist_init_instances(race_state, deterministic = scenario$deterministic,
      sampleInstances = scenario$sampleInstances, elitist_new_instances = elitist_new_instances)
    # It may be reduced further by elitist_init_instances()
    elitist_new_instances <- min(elitist_new_instances, race_state$elitist_new_instances)
    all_elite_instances_evaluated <- function() {
      if (race_state$next_instance == 1L) return(TRUE)
      evaluated <- !is.na(Results[, alive, drop=FALSE])
      # All instances that have been previously seen have been evaluated by at
      # least one configuration
      all(rowAnys(evaluated)) &&
        # and the number of instances evaluated per configuration is a multiple
        # of eachTest (which is scenario$blockSize * scenario$eachTest).
        all(colSums2(evaluated) %% eachTest == 0L)
    }
  } else {
    race_instances <- no_elitist_init_instances(race_state, deterministic = scenario$deterministic)
    all_elite_instances_evaluated <- function() TRUE
  }

  irace_assert(!anyDuplicated(race_instances))
  irace_assert(identical(sort(race_instances), seq_along(race_instances)))
  nb_tasks <- length(race_instances)

  # Initialize some variables...
  experiments_used <- 0L

  ## FIXME: Probably, instead of this, we should keep elite_safe in the race_state.
  if (is.null(elite_data)) {
    elite_safe <- 0L
    elite_instances_ID <- NULL
  } else {
    irace_assert(race_state$next_instance - 1L == nrow(elite_data))
    # There must be a non-NA entry for each instance.
    irace_assert(all(rowAnyNotNAs(elite_data)),
                 eval_after = { print(elite_data)})
    # There must be a non-NA entry for each configuration.
    irace_assert(all(colAnyNotNAs(elite_data)),
                 eval_after = {
                   cat("elite_data:\n")
                   print(elite_data)
                   cat("experiment_log:\n")
                   print(race_state$experiment_log)
                 })

    # elite_safe: maximum instance number for which any configuration may be
    # considered elite. After evaluating this instance, no configuration is
    # elite.
    elite_safe <- elitist_new_instances + nrow(elite_data)
    elite_instances_ID <- as.character(race_instances[seq_len(elite_safe)])
  }

  configurations_ID <- as.character(configurations[[".ID."]])
  Results <- matrix(NA_real_,
                    nrow = elite_safe,
                    ncol = n_configurations,
                    dimnames = list(elite_instances_ID, configurations_ID))
  if (capping)
    experimentsTime <- matrix(NA_real_,
                              nrow = elite_safe,
                              ncol = n_configurations,
                              dimnames = list(elite_instances_ID, configurations_ID))

  if (is.null(elite_data)) {
    # is_elite[i] : number of instances to be seen in this race on which i has
    # been previously evaluated.
    is_elite <- integer(n_configurations)
  } else {
    Results[rownames(elite_data), colnames(elite_data)] <- elite_data
    irace_assert(all(colnames(elite_data) %chin% configurations_ID))
    if (capping) {
      # Temporarily use only 0 or 1, we will calculate the real value below.
      is_elite <- as.integer(configurations_ID %chin% colnames(elite_data))
      tmp <- generateTimeMatrix(elite_ids = colnames(elite_data),
                                experiment_log = race_state$experiment_log)
      experimentsTime[rownames(tmp), colnames(tmp)] <- tmp
      # Preliminary execution of elite configurations to calculate
      # the execution bound of initial configurations (capping only).
      if (elitist_new_instances > 0L) {
        irace_assert(elitist_new_instances %% blockSize == 0L)
        # FIXME: This should go into its own function.
        n_elite <- ncol(elite_data)
        which_elites <- which(is_elite > 0L, useNames=FALSE)
        irace_assert(identical(which_elites, seq_len(n_elite)))
        irace_note("Preliminary execution of ", n_elite,
          " elite configuration(s) over ", elitist_new_instances, " instance(s).\n")
        # FIXME: Launch all seq_len(elitist_new_instances) experiments in parallel.
        for (k in seq_len(elitist_new_instances)) {
          output <- race_wrapper (race_state,
            configurations = configurations[which_elites, , drop = FALSE],
            instance_idx = race_instances[k],
            bounds = rep(scenario$boundMax, n_elite),
            # which_exe values are within 1:nbConfigurations, whereas experiments
            # indices are within 1:length(which_alive). The following line converts
            # from one to the other.
            is_exe = rep_len(TRUE, n_elite), scenario = scenario)
          # Extract results:
          irace_assert(length(output[["cost"]]) == n_elite)
          Results[k, which_elites] <- applyPAR(output[["cost"]], boundMax = scenario$boundMax, boundPar = scenario$boundPar)
          irace_assert(!anyNA(output[["time"]]))
          experimentsTime[k, which_elites] <- output[["time"]] # capping is enabled
          irace_assert(all.equal(configurations[[".ID."]][which_elites], output[["configuration"]]))
          irace_assert(all.equal(output[["bound"]], rep(scenario$boundMax, n_elite)))
          irace_assert(all.equal(unique(output[["instance"]]), race_instances[k]))
          experiments_used <- experiments_used + n_elite

          # We remove elite configurations that are rejected given that
          # is not possible to calculate the bounds.
          rejected <- is.infinite(output[["cost"]])
          irace_assert(all.equal(as.vector(is.infinite(Results[k, which_elites])), rejected), eval_after={
            cat("rejected:\n")
            print(output[["cost"]])
            print(rejected)
            cat("Results:\n")
            print(Results[k, which_elites])
            print(is.infinite(Results[k, which_elites]))
          })
          is_rejected[which_elites] <- rejected
          is_elite[rejected] <- 0L
          which_elites <- which_elites[!rejected]
          n_elite <- length(which_elites)

          # If all elite are eliminated we stop execution of initial instances.
          if (n_elite == 0L) {
            irace_note ("All elite configurations are rejected. Execution of non-elites will be not bounded.\n")
            break
          }
        }
        if (any(is_rejected)) {
          irace_note ("Immediately rejected configurations: ",
            paste0(configurations[[".ID."]][is_rejected], collapse = ", ") , "\n")
          alive[is_rejected] <- FALSE
          # Calculate the maximum instance that has any non-NA value.
          # FIXME: Use update_elite_safe()
          if (n_elite > 0L)
            elite_safe <- max(which(rowAnys(!is.na(Results[, which_elites, drop=FALSE]))))
          else
            elite_safe <- 0L
          irace_assert(identical(update_elite_safe(Results, is_elite), elite_safe))
          # FIXME: If sum(alive) <= minSurvival, we stop later but we will have
          # elites that have not been evaluated in any instance, which is
          # bad. Ideally we would sample new configurations here but that is
          # too hard to do in iterated-racing.
        }
      }
    } # end if(capping)
    # Compute the elite membership.
    is_elite <- colSums2(!is.na(Results))
    # Remove rejected configurations.
    is_elite[is_rejected] <- 0L
  }


  no_elimination <- 0L # number of tasks without elimination.
  print_header()
  # Start main loop.
  break_msg <- NULL
  best <- NA_integer_
  which_alive <- which(alive)
  nb_alive    <- length(which_alive)

  for (current_task in seq_len(nb_tasks)) {
    which_exe   <- which_alive
    if (elitist && any(is_elite > 0L)) {
      # Filter configurations that do not need to be executed (elites).
      # This is valid only for previous iteration instances.
      irace_assert(current_task <= elite_safe)
      # Execute everything that is alive and not yet executed.
      which_exe <- which(alive & is.na(Results[current_task, ]))
      if (length(which_exe) == 0L) {
        is_elite <- update_is_elite(is_elite, which_exe)
        # LESLIE: This is the case in which there are only elite configurations alive
        # and we are still in the previous instances execution, but we can still
        # continue with the race. (This is only possible because the early termination
        # criterion is disabled)
        ## MANUEL: So what is the reason to not immediately terminate here? Is
        ## there a reason to continue?
        if (current_task == 1L) {
          # We may reach this point in the first iteration so we need to calculate best.
          if (nb_alive == 1L) {
            best <- which_alive
          } else  {
            tmpResults <- Results[1L, which_alive, drop = FALSE]
            irace_assert(!anyNA(tmpResults))
            # which.min returns only the first minimum.
            best <- which_alive[which.min(get_ranks(tmpResults, test = stat_test))]
          }
        }
        if (is.na(best)) {
          utils::dump.frames(dumpto = "best_crash", to.file = TRUE,
                      include.GlobalEnv = TRUE)
          irace_assert(!is.na(best))
        }
        id_best <- configurations[[".ID."]][best]
        print_task(".", Results[seq_len(current_task), , drop = FALSE],
                   race_instances[current_task],
                   current_task, which_alive = which_alive,
                   id_best = id_best,
                   best = best, experiments_used, start_time = Sys.time(),
                   # FIXME: Why do we pass NA as bound? Why not pass the actual bound if any?
                   bound = NA_real_, capping = capping)
        next
      }
    }

    if (all_elite_instances_evaluated()) {
      # We stop when we have less configurations than required.
      if (nb_alive <= minSurvival) {
        # Stop race if we have less or equal than the minimum number of
        # configurations.
        break_msg <- paste0("number of alive configurations (", nb_alive,
          ") <= minimum number of configurations (", minSurvival, ")")
        break
      }
      ## We continue running if (1) we have not reached the firstTest or (2)
      ## there are instances previously seen that have not been evaluated on any
      ## alive configuration.  If we just did a test, check that we have enough
      ## budget to reach the next test.
      # FIXME: In post-selection racing, we want to consume all budget, so we
      # should discard configurations until we have 2.
      if (current_task > firstTest && ( (current_task - 1L) %% eachTest) == 0L
        && experiments_used + length(which_exe) * eachTest > maxExp) {
        break_msg <- paste0("experiments for next test (",
          experiments_used + length(which_exe) * eachTest,
          ") > max experiments (", maxExp, ")")
        break
      }

      if (elitist && scenario$elitistLimit != 0L && no_elimination >= scenario$elitistLimit) {
        break_msg <- paste0("tests without elimination (", no_elimination,
          ") >= elitistLimit (", scenario$elitistLimit, ")")
        break
      }
    }

    if (nrow(Results) < current_task) {
      Results <- rbind(Results, rep_len(NA_real_, ncol(Results)))
      rownames(Results) <- race_instances[seq_nrow(Results)]
      if (capping) {
        experimentsTime <- rbind(experimentsTime, rep_len(NA_real_, ncol(experimentsTime)))
        rownames(experimentsTime) <- race_instances[seq_nrow(experimentsTime)]
      }
    }

    start_time <- Sys.time()

    # Calculate bounds for executing if needed.
    which_elite_exe <- intersect(which_exe, which(is_elite > 0L))
    irace_assert(setequal(which_elite_exe, which(is_elite & is.na(Results[current_task,]))))
    if (capping) {
      # Pre-execute elite configurations that are not yet executed in the current instance.
      if (length(which_elite_exe)) {
        # FIXME: This should go into its own function
        output <- race_wrapper(race_state,
          configurations = configurations[which_elite_exe, , drop = FALSE],
          instance_idx = race_instances[current_task],
          # FIXME: What if we already have a bound for this instance?
          bounds = rep(scenario$boundMax, length(which_elite_exe)),
          is_exe = rep_len(TRUE, length(which_elite_exe)), scenario = scenario)
        # Extract results
        irace_assert(length(output[["cost"]]) == length(which_elite_exe))
        Results[current_task, which_elite_exe] <- applyPAR(output[["cost"]], boundMax = scenario$boundMax, boundPar = scenario$boundPar)
        irace_assert(!anyNA(output[["time"]]))
        irace_assert(all.equal(configurations[which_elite_exe, ".ID."], output[["configuration"]]))
        experimentsTime[current_task, which_elite_exe] <- output[["time"]]
        irace_assert(all.equal(unique(output[["instance"]]), race_instances[current_task]))
        experiments_used <- experiments_used + length(which_elite_exe)

        # We remove elite configurations that are rejected given that
        # is not possible to calculate the bounds
        rejected <- is.infinite(output[["cost"]])
        if (any(rejected)) {
          irace_note("Immediately rejected configurations: ",
            paste0(configurations[[".ID."]][which_elite_exe[rejected]],
              collapse = ", ") , "\n")
          is_rejected[which_elite_exe] <- rejected
          is_elite[is_rejected] <- 0L
          alive[which_elite_exe] <- !rejected
          if (!any(alive)) {
            # FIXME: Only report this error if (all(is_rejected)); otherwise
            # restore non-rejected non-alive ones. Restoring a non-alive
            # configuration is difficult. We need to evaluate it in all the
            # instances that it has missed.
            irace_error("All configurations have been immediately rejected (all of them returned Inf) !")
          }
          which_alive <- which(alive)
          nb_alive    <- length(which_alive)
          elite_safe <- update_elite_safe(Results, is_elite)
        }
        which_exe <- setdiff(which_exe, which_elite_exe)
        # FIXME: There is similar code above. Can we merge these code paths?
        if (length(which_exe) == 0L) {
          is_elite <- update_is_elite(is_elite, which_elite_exe)
          if (current_task == 1L) {
            # We may reach this point in the first iteration so we need to calculate best.
            if (nb_alive == 1L) {
              best <- which_alive
            } else  {
              tmpResults <- Results[1L, which_alive, drop = FALSE]
              irace_assert(!anyNA(tmpResults))
              # which.min returns only the first minimum.
              best <- which_alive[which.min(get_ranks(tmpResults, test = stat_test))]
            }
          }
          if (is.na(best)) {
            utils::dump.frames(dumpto = "best_crash", to.file = TRUE,
                        include.GlobalEnv = TRUE)
            irace_assert(!is.na(best))
          }
          id_best <- configurations[[".ID."]][best]
          print_task(".", Results[seq_len(current_task), , drop = FALSE],
                     race_instances[current_task],
                     current_task, which_alive = which_alive,
                     id_best = id_best,
                     best = best, experiments_used, start_time = start_time,
                     # FIXME: Why do we pass NA as bound? Why not pass the actual bound if any?
                     bound = if (is.null(scenario$boundMax)) NA_real_ else scenario$boundMax, capping)
          next
        }
      }
      all_bounds <- final_execution_bound(experimentsTime,
                                          elites = which(is_elite > 0L),
                                          current_task, which_alive,
                                          which_exe, scenario)
      final_bounds <- all_bounds$final_bounds
      elite_bound <- all_bounds$elite_bound
    } else {
      final_bounds <- rep(scenario$boundMax, length(which_alive))
      elite_bound <- NULL
    }

    # Execute experiments.
    output <- race_wrapper(race_state, configurations = configurations[which_alive, , drop = FALSE],
                           instance_idx = race_instances[current_task],
                           bounds = final_bounds,
                           is_exe = which_alive %in% which_exe,
                           scenario = scenario)
    # Extract results
    # Set max execution bound to timed out executions which have execution
    # times smaller than boundMax and implement parX if required.
    vcost <- output[["cost"]]
    # Output is not indexed in the same way as configurations.
    which_has_time <- which(which_alive %in% which_exe)
    # With !is.null(scenario$targetEvaluator) we will have duplicated (instance, configuration) in output.
    irace_assert(all.equal(output[["bound"]], if (is.null(scenario$targetEvaluator)) final_bounds[which_has_time]
                                              else final_bounds))
    if (capping) {
      vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
      if (scenario$boundAsTimeout) {
        timeout_bounds <- if (is.null(scenario$targetEvaluator)) final_bounds[which_has_time]
                          else final_bounds
        irace_assert(all.equal(output[["bound"]], timeout_bounds))
        # We do not want to change Inf or -Inf because those represent rejection.
        vcost[is.finite(vcost) & (vcost >= timeout_bounds) & (vcost < scenario$boundMax)] <- scenario$boundMax
      }

      # If targetEvaluator was used, we do not update the times because no
      # evaluation actually happened, only the cost values possibly changed.
      vtimes <- if (is.null(scenario$targetEvaluator)) output[["time"]] else output[["time"]][which_has_time]
      # Correct higher execution times.
      irace_assert(all.equal(if (is.null(scenario$targetEvaluator)) output[["bound"]] else output[["bound"]][which_has_time], final_bounds[which_has_time]))
      experimentsTime[current_task, which_has_time] <- pmin(vtimes, final_bounds[which_has_time])
    }
    ## Currently, targetEvaluator always re-evaluates, which implies that the
    ## value may change without counting as an evaluation. We do this to allow online normalization.
    which_has_cost <- if (is.null(scenario$targetEvaluator)) which_exe else which_alive
    irace_assert(length(output[["cost"]]) == length(which_has_cost))
    Results[current_task, which_has_cost] <- vcost
    experiments_used <- experiments_used + length(which_exe)

    # We update the elites that have been executed.
    is_elite <- update_is_elite(is_elite, which_elite_exe)

    ## Drop bad configurations.
    ## Infinite values denote immediate rejection of a configuration.
    # FIXME: Should this be which_has_cost?
    rejected <- is.infinite(Results[current_task, which_exe])
    if (any(rejected)) {
      irace_note ("Immediately rejected configurations: ",
                  paste0(configurations[which_exe[rejected], ".ID."],
                         collapse = ", ") , "\n")
      is_rejected[which_exe] <- rejected
      is_elite[is_rejected] <- 0L
      alive[is_rejected] <- FALSE
      if (!any(alive)) {
        # FIXME: Only report this error if (all(is_rejected)); otherwise
        # restore non-rejected non-alive ones. Restoring a non-alive
        # configuration is difficult. We need to evaluate it in all the
        # instances that it has missed.
        irace_error("All configurations have been immediately rejected (all of them returned Inf) !")
      }
      which_alive <- which(alive)
      nb_alive    <- length(which_alive)
      # FIXME: Should we stop  if (nbAlive <= minSurvival) ???
      elite_safe <- update_elite_safe(Results, is_elite)
    }
    irace_assert(!anyNA(Results[seq_len(current_task), alive, drop=FALSE]))
    irace_assert(!any(is.infinite(Results[, alive, drop=FALSE])))

    # Variables required to produce output of elimination test.
    cap_done     <- FALSE # if dominance elimination was performed
    test.done    <- FALSE # if statistical test elimination was performed
    cap_dropped  <- FALSE # if capping has drop any configuration
    test.dropped <- FALSE # if any candidates has been eliminated by testing
    cap_alive    <- test.alive <- alive

    ## Dominance elimination (Capping only).
    # The second condition can be false if we eliminated via immediate
    # rejection.  The third condition ensures that we see the block before capping.
    if (capping && nb_alive > minSurvival && (current_task %% blockSize) == 0L
      && (!scenario$cappingAfterFirstTest || current_task >= firstTest)) {
      irace_assert(!any(is_elite > 0L) == (current_task >= elite_safe))
      cap_alive <- dom_elim(Results[seq_len(current_task), , drop = FALSE],
                            # Get current elite configurations.
                            elites = which(is_elite > 0L),
                            alive, scenario, minSurvival)
      cap_dropped <- nb_alive > sum(cap_alive)
      cap_done    <- TRUE
    }

    # We assume that firstTest is a multiple of eachTest.  In any
    # case, this will only do the first test after the first multiple
    # of eachTest that is larger than firstTest.
    if (current_task >= firstTest && (current_task %% eachTest) == 0L
        && nb_alive > 1L) {
      irace_assert(sum(alive) == nb_alive)
      test_res <- do_test(Results[seq_len(current_task), ], alive, which_alive)
      # FIXME: This race_ranks is unused. We should check if it matches the one computed below.
      race_ranks <- test_res$ranks
      test.alive <- test_res$alive
      test.dropped <- nb_alive > sum(test.alive)
      test.done   <- TRUE
    }

    # Merge the result of both eliminations.
    prev_nb_alive <- nb_alive
    prev_which_alive  <- which_alive
    alive <- cap_alive & test.alive

    # Handle elites when elimination is performed.  The elite configurations
    # can be removed only when they have no more previously-executed instances.
    irace_assert(!any(is_elite > 0L) == (current_task >= elite_safe))
    if (!is.null(elite_data) && any(is_elite > 0L)) {
      irace_assert (length(alive) == length(is_elite))
      alive <- alive | (is_elite > 0L)
    }

    # It may happen that the capping and the test eliminate together all
    # configurations. In that case, we only trust the capping elimination.
    if (capping && !any(alive)) {
      if (scenario$debugLevel >= 2L) {
        irace_warning("Elimination tests have eliminated all configurations, keeping the capping results.\n")
        irace_note("Alive according to capping:", which(cap_alive), "\n")
        irace_note("Alive according to test:", which(test.alive), "\n")
      }
      alive <- cap_alive
    }
    which_alive <- which(alive)
    nb_alive <- length(which_alive)
    # Output the result of the elimination test.
    res.symb <- if (cap_dropped && !test.dropped && prev_nb_alive != nb_alive) {
                  "c" # Removed just by capping.
                } else if (cap_dropped || test.dropped) {
                  if (prev_nb_alive != nb_alive) "-" else "!"
                } else if (cap_done || test.done) "=" else "x"

    # Rank alive configurations: order all configurations (eliminated or not)
    # LESLIE: we have to make the ranking outside: we can have configurations eliminated by capping
    # that are not eliminated by the test.
    # MANUEL: I don't understand the above comment.
    if (length(prev_which_alive) == 1L) {
      race_ranks <- 1L
      best <- prev_which_alive
    } else  {
      tmpResults <- Results[seq_len(current_task), prev_which_alive, drop = FALSE]
      irace_assert(!anyNA(tmpResults))
      race_ranks <- get_ranks(tmpResults, test = stat_test)
      # which.min() returns only the first minimum.
      best <- prev_which_alive[which.min(race_ranks)]
    }

    irace_assert(best == prev_which_alive[order(race_ranks)][1L])
    irace_assert(length(race_ranks) == length(prev_which_alive))

    # Remove the ranks of those that are not alive anymore
    race_ranks <- race_ranks[which_alive]
    irace_assert(length(race_ranks) == nb_alive)
    id_best <- configurations[[".ID."]][best]
    print_task(res.symb, Results[seq_len(current_task), , drop = FALSE],
               race_instances[current_task],
               current_task, which_alive = which_alive,
               id_best = id_best, best = best, experiments_used, start_time = start_time,
               bound = elite_bound, capping)

    if (elitist) {
      # Compute number of statistical tests without eliminations.
      irace_assert(!any(is_elite > 0L) == (current_task >= elite_safe))
      if (!any(is_elite > 0L)
          && current_task > firstTest && (current_task %% eachTest) == 0L) {
        no_elimination <- if (nb_alive == prev_nb_alive) no_elimination + 1L else 0L
      }
    }
  }

  if (is.null(break_msg))
    break_msg <- paste0("all instances (", nb_tasks, ") evaluated")

  # Adding this given that when ncandidates = minsurvival+1
  # and there one elite configuration that gets discarded in the new instances
  # execution the race is finished with no executions.
  # FIXME: we should handle this better, maybe allowing irace to handle no elite
  # in irace()
  # MANUEL: Leslie, how can we reach this error in normal circumstances?
  # Can we handle this better?
  if (current_task == 1L && all(is_elite == 0L))
    irace_error ("Maximum number configurations immediately rejected reached!")

  # All instances that are not new in this race must have been evaluated by at
  # least one configuration.
  irace_assert(all_elite_instances_evaluated(),
               eval_after = { print(Results[,alive, drop=FALSE])})
  # If we stop the loop before we see all new instances, there may be new
  # instances that have not been executed by any configuration.
  Results <- Results[rowAnyNotNAs(Results), , drop = FALSE]
  # If we reject configurations so that sum(alive) <= minSurvival, we may stop
  # before we evaluate some configurations in any instance.
  if (any(is_rejected)) {
    alive <- alive & colAnyNotNAs(Results)
    which_alive <- which(alive)
    if (!any(alive)) {
      # FIXME: Only report this error if (all(is_rejected)); otherwise
      # restore non-rejected non-alive ones. Restoring a non-alive
      # configuration is difficult. We need to evaluate it in all the
      # instances that it has missed.
      irace_error("All configurations have been immediately rejected (all of them returned Inf) !")
    }
  }

  race_ranks <- overall_ranks(Results[, alive, drop = FALSE], test = stat_test)
  if (!scenario$quiet) {
    old_best <- best # old_best could be NA.
    best <- which_alive[which.min(race_ranks)]
    mean_best <- mean(Results[, best])
    print_footer(bestconf = configurations[best, , drop = FALSE],
                 # FIXME: This is the mean of the best, but perhaps it
                 # should be the sum of ranks in the case of test == friedman?
                 mean_best = mean_best,
                 break_msg = break_msg, debug_level = scenario$debugLevel,
                 capping = capping,
                 old_best_id  = if (old_best == best || is.na(old_best)) NULL else id_best)
  }
  rejected_ids <- configurations[is_rejected, ".ID."]
  scenario$parameters$forbid_configurations(
    race_state$update_rejected(rejected_ids, configurations)
  )
  # Only return alive ones.
  configurations <- configurations[alive, , drop=FALSE]
  irace_assert(all(configurations[[".ID."]] %not_in% rejected_ids)) # No rejected is alive.
  # Assign the proper ranks in the configurations data.frame.
  configurations[[".RANK."]] <- race_ranks
  # Now we can sort the data.frame by the rank.
  configurations <- configurations[order(configurations[[".RANK."]]), , drop=FALSE]
  if (scenario$debugLevel >= 3L) {
    irace_note ("Memory used in race():\n")
    race_state$print_mem_used()
  }
  local_experiment_log <- race_state$reset_race_experiment_log()

  # nrow(Results) may be smaller, equal or larger than current_task.
  if (is.null(scenario$targetEvaluator)) {
    # With targetEvaluator, we may have the recorded a new cost value but not
    # counted it as an experiment used if targetRunner was not called.
    irace_assert(anyDuplicated(local_experiment_log[, c("instance", "configuration")]) == 0L,
      eval_after = {
        print(local_experiment_log)
        print(mget(ls()))
      })
    irace_assert(nrow(local_experiment_log) == experiments_used)
  }
  list(experiments = Results,
       experiment_log = local_experiment_log,
       experimentsUsed = experiments_used,
       configurations = configurations)
}
