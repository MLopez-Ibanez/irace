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
                                 instances, instances_ID, seeds,
                                 bounds = NULL)
{
  instances <- instances[instances_ID]
  n_configurations <- nrow(configurations)
  n_instances <- length(instances)
  pnames <- parameters$names
  configurations_id <- configurations[[".ID."]]
  configurations <- as.list(configurations[, pnames, drop=FALSE])
  # FIXME: How to do this faster?
  # - maybe purrr::transpose() ?
  # - mlr3misc::transpose_list() uses .mapply(list, configurations, list(), which is slower. 
  configurations <- lapply(seq_len(n_configurations), function(i) lapply(configurations, `[`, i))
  dots <- list(id_configuration = rep(configurations_id, each=n_instances), id_instance = instances_ID,
               seed = seeds, configuration = rep(configurations, each = n_instances),
               instance = instances)
  if (!is.null(bounds)) {
    # There must be a bound for each configuration.
    # FIXME: There must be a bound for each configuration AND each instance.
    irace.assert(length(bounds) == n_configurations)
    dots$bound <- rep.int(bounds, n_instances)
  }
  dots$switches <- list(parameters$switches)
  .mapply(list, dots, MoreArgs = NULL)
}

## Executes a list of configurations in a particular instance
## configurations: description having the id of the configuration
## instance.idx: index of the instance,seed pair in race_state$instances_log
## bounds: execution bounds (if needed).
## which_alive: index of the configurations that are still alive
## which_exe: index of the alive configurations that should be executed
race_wrapper <- function(race_state, configurations, instance_idx, bounds = NULL,
                         # FIXME: we actually only need which_exps, not
                         # which_alive nor which_exe
                         which_alive, which_exe, scenario)
{
  # Experiment list to execute
  experiments <- createExperimentList(configurations, parameters = scenario$parameters,
                                      instances = scenario$instances,
                                      instances_ID = race_state$instances_log[["instanceID"]][instance_idx],
                                      seeds = race_state$instances_log[["seed"]][instance_idx],
                                      bounds = bounds)

  target_output <- vector("list", length(experiments))
  # Execute commands
  if (length(which_exe)) {
    # which_exe values are within 1:nbConfigurations, whereas experiments
    # indices are within 1:length(which_alive). The following line converts
    # from one to the other.
    which_exps <- which(which_alive %in% which_exe)
    irace.assert(length(which_exps) == length(which_exe))
    target_output[which_exps] <- execute_experiments(race_state, experiments[which_exps], scenario)
  }

  # targetEvaluator may be NULL. If so, target_output must contain the right
  # output already.  Otherwise, targetEvaluator always re-evaluates.
  if (!is.null(scenario$targetEvaluator))
    target_output <- execute_evaluator(race_state$target_evaluator, experiments, scenario, target_output,
      configurations_id = configurations[[".ID."]])
  
  target_output
}

aux2.friedman <- function(y, I, alive, conf.level = 0.95)
{
  dropped.any <- FALSE
  n <- nrow(y)
  k <- length(I)
  r <- rowRanks(y, cols = I, ties.method = "average")
  R <- colSums2(r)
  o <- order(R)
  best <- I[o[1]]
  TIES <- tapply(c(r), row(r), table)
  STATISTIC <- ((12 * sum((R - n * (k + 1) / 2)^2)) /
                (n * k * (k + 1)
                  - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                     (k - 1))))
  PARAMETER <- k - 1
  PVAL      <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  #names(STATISTIC) <- "Friedman chi-squared"
  #names(PARAMETER) <- "df"

  alpha <- 1 - conf.level
  if (!is.nan(PVAL) && PVAL < alpha) {
    # This formula for multiple comparisons comes from Conover, "Practical
    # Nonparametric Statistics", 1999, pages 369-371.
    A <- sum(as.vector(r)^2)
    t <- qt(1 - alpha / 2, df = (n - 1) * (k - 1)) *
      (2 * (n * A - sum(R^2)) / ((n - 1) * (k - 1)))^(1 / 2)
    J <- best
    for (j in 2:k) {
      if (abs(R[o[j]] - R[o[1]]) > t) {
        break
      } else {
        J <- c(J, I[o[j]])
      }
    }
    alive[-J] <- FALSE
    dropped.any <- TRUE
  }
  irace.assert(I[which.min(R)] == best)
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
  V1 <- results[, which_alive[1]]
  V2 <- results[, which_alive[2]]
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
    irace.assert(!is.nan(PVAL) & !is.na(PVAL))
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
  irace.assert(sum(alive) == length(which_alive))
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
    irace.assert(!is.nan(PVAL) & !is.na(PVAL))
    pvals[j] <- PVAL
  }
  pvals <- p.adjust(pvals, method = adjust)
  dropj <- which_alive[pvals < 1.0 - conf.level]
  dropped_any <- length(dropj) > 0
  irace.assert(all(alive[dropj]))
  alive[dropj] <- FALSE
  list(ranks = means, alive = alive, dropped.any = dropped_any, p.value = min(pvals))
}

aux.ttest <- function(results, alive, which_alive, conf.level,
                      adjust = c("none","bonferroni","holm"))
{
  adjust <- match.arg(adjust)
  irace.assert(sum(alive) == length(which_alive))
  
  results <- results[, which_alive]
  means <- colMeans2(results)
  # FIXME: break ties using median or ranks?
  best <- which.min(means)
  mean_best <- means[best]
  pvals <- sapply(means, function(x) as.numeric(isTRUE(
                                       all.equal.numeric(mean_best[[1]], x[[1]], check.attributes = FALSE))))
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
    irace.assert(!is.nan(PVAL) & !is.na(PVAL))
    pvals[j] <- PVAL
  }
  pvals <- p.adjust(pvals, method = adjust)
  dropj <- which_alive[pvals < 1.0 - conf.level]
  dropped_any <- length(dropj) > 0
  irace.assert(all(alive[dropj]))
  alive[dropj] <- FALSE
  list(ranks = means, alive = alive, dropped.any = dropped_any, p.value = min(pvals))
}

table_hline <- function(widths) {
  s <- "+"
  for (w in widths) {
    s <- paste0(s, strrep("-", w), "+")
  }
  paste0(s, "\n")
}

table_sprint <- function(text, widths) {
  s <- "|"
  for (i in seq_along(text)) {
      s <- paste0(s, sprintf("%*s", widths[i], text[i]), "|")
  }
  paste0(s, "\n")
}
.nocap_table_fields_width <- as.integer(c(1, 11, 11, 11, 16, 11, 8, 5, 4, 6))
.nocap_colum_names <- c(" ", "Instance", "Alive", "Best", "Mean best", "Exp so far",
                        "W time", "rho", "KenW", "Qvar")
.capping_table_fields_width <- as.integer(c(1, 11, 8, 11, 11, 16, 11, 8, 5, 4, 6))
.capping_colum_names <- c(" ", "Instance", "Bound", "Alive", "Best", "Mean best", "Exp so far",
                        "W time", "rho", "KenW", "Qvar")
capping_hline <- table_hline(.capping_table_fields_width)
capping_header <- table_sprint(.capping_colum_names, .capping_table_fields_width) 
nocap_hline <- table_hline(.nocap_table_fields_width)
nocap_header <- table_sprint(.nocap_colum_names, .nocap_table_fields_width) 
race_common_header <-  "# Markers:
     x No test is performed.
     c Configurations are discarded only due to capping.
     - The test is performed and some configurations are discarded.
     = The test is performed but no configuration is discarded.
     ! The test is performed and configurations could be discarded but elite configurations are preserved.
     . All alive configurations are elite and nothing is discarded.\n\n"

race_print_header_nocap <- function()
  cat(sep = "", race_common_header, nocap_hline, nocap_header, nocap_hline)

race_print_header_cap <- function()
  cat(sep = "", race_common_header, capping_hline, capping_header, capping_hline)

race_print_task <- function(res.symb, Results,
                            instance,
                            current_task,
                            alive,
                            id_best,
                            best,
                            experiments_used,
                            start_time,
                            bound, capping)
{
  elapsed_wctime_str <- function(now, start) {
    if (now <= start) return("00:00:00")
    elapsed <- difftime(now, start, units = "secs")
    # FIXME: Maybe better and faster if we only print seconds?
    format(.POSIXct(elapsed, tz="GMT"), "%H:%M:%S")
  }
  # FIXME: This is the mean of the best, but perhaps it should
  # be the sum of ranks in the case of test == friedman?
  mean_best <- mean(Results[, best])
  time_str <- elapsed_wctime_str(Sys.time(), start_time)
  cat(sprintf("|%s|%11d|", res.symb, instance))
  if (capping) {
    if (is.null(bound)) cat("      NA|") else cat(sprintf("%8.2f|", bound))
  }
  cat(sprintf(paste0("%11d|%11d|", .irace.format.perf, "|%11d|%s"),
              sum(alive), id_best, mean_best, experiments_used, time_str))
  
  if (current_task > 1L && sum(alive) > 1L) {
    res <- Results[seq_len(current_task), alive, drop = FALSE]
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
      if (capping) capping_hline else nocap_hline,
      if (debug_level >= 1) paste0("# Stopped because ", break_msg, "\n"),
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
  irace.assert (ncol(data) >= 1L)
  
  if (ncol(data) == 1L) {
    return (mean(data[,1L], na.rm = TRUE))
  }
  # This should never happen because the data used to obtain the execution
  # bound should be complete, that is, the bounding configurations should have
  # been executed on all previous instances.
  
  irace.assert (all(!is.na(data)))
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
  irace.assert (all(!is.na(data)))
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
dom.elim <- function(results, elites, alive, scenario, minSurvival, eps = 1e-5)
{
  irace.assert(sum(alive) >= minSurvival)
  which_alive <- which(alive)
  cmeans <- colMeans2(results[, alive, drop = FALSE])
  irace.assert(!all(is.na(cmeans))) # Only NA values when calculating mean for dominance.
  
  # When there are no protected elites left, select the best configurations to
  # calculate the bounds. This is quite aggressive and another alternative
  # would be to disable dom.elim when elites == 0.
  if (length(elites) == 0) {
    # In the case we have only two alive configurations only one can be elite.
    if (sum(alive) <= 2L)
      elites <- which_alive[which.min(cmeans)]
    else
      elites <- which_alive[order(cmeans, na.last = TRUE, decreasing = FALSE)[seq_len(minSurvival)]]
  }
  
  bound <- executionBound(results[, elites, drop = FALSE], type = scenario$cappingType)
  alive[which_alive] <- ((bound + eps) >= cmeans)
  return (alive)
}

## This function applies PARX (X=boundPar) to all experiments
## that exceed the maximum execution time (boundMax)
applyPAR <- function(results, boundMax, boundPar)
{
  # We do not want to change Inf or -Inf because those represent rejection.
  if (boundPar != 1)
    results[is.finite(results) & results >= boundMax] <- boundMax * boundPar
  return(results)
}

## This function calculates the execution time allowed for the executions
## of configurations based on previous execution times.
## It returns a list with two elements: 
## * elite.bound : value (mean execution time or per instance execution time) 
## used to calculate the maximum execution time (final.bounds) for each configuration.
## * final.bounds[i] : maximum execution time for candidate i on the current instance. 
final.execution.bound <- function(experimentsTime, elites, no.configurations,
                                  current_task, which_exe, scenario)
{
  minMeasurableTime <- scenario$minMeasurableTime
  boundMax <- scenario$boundMax
  # FIXME: should we use an adjusted boundMax 
  final.bounds <- rep(boundMax, no.configurations)
  total.time  <- current_task * boundMax
  elite.bound <- boundMax
  # Elite candidates can have NA values due to the rejection 
  if (length(elites))
    elites <- elites[!is.na(experimentsTime[current_task,elites])]
    
  # Only apply bounds when there is previous data
  if (length(elites) > 0L && length(which_exe) > 0L) {
    # The elite membership is updated before calling this function to know 
    # the configurations used for calculating the bounds we need to do this.
    # Note that some of these configurations could be not elite anymore for the 
    # elimination phase, given that all their evaluations have been used.
    if (scenario$boundType == "instance") {
       elite.bound <- instanceBound(experimentsTime[current_task, elites],
                                    type = scenario$cappingType)
       final.bounds[which_exe] <- min(elite.bound + minMeasurableTime, boundMax)
       final.bounds[which_exe] <- ceiling_digits(final.bounds[which_exe], scenario$boundDigits)
     } else {
       elite.bound <- executionBound(experimentsTime[seq_len(current_task), elites, drop = FALSE],
                                     type = scenario$cappingType)
       elite.bound <- min(elite.bound, boundMax)
       total.time <- (current_task * elite.bound) + minMeasurableTime
       time.left <- total.time - colSums2(experimentsTime, rows = seq_len(current_task), cols = which_exe, na.rm = TRUE)
       final.bounds[which_exe] <- sapply(time.left, min, boundMax)
       # We round up the bounds up to the specified number of digits. This may
       # be necessary if the target-algorithm does not support higher precision.
       final.bounds[which_exe] <- ceiling_digits(final.bounds[which_exe], scenario$boundDigits)
       # There are cases in which a small negative budget is used. For example:
       # assuming candidates 1 and 2 are elite maxbound=80
       # executionTime <- matrix(c(0.010,0.0170,0.010, 24,28,27, 0.010,0.017,NA), 
       #        byrow=TRUE, ncol=3, nrow=3, dimnames=list(c(1,2,3),c(1,2,3)))
       # current_task <- 3; boundDigits=0
       # elite.bound  <- irace:::executionBound(executionTime[1:current_task,1:2], "median") 
       # total.time   <- elite.bound * current_task + 0.01
       # time.left    <- total.time - colSums2(executionTime[1:current_task,3,drop=FALSE], na.rm=TRUE)
       # We set the execution time to the elite.bound this should be enough
       # to eliminate a bad candidate for the next task.
       final.bounds[final.bounds <= 0] <- elite.bound 
       irace.assert(all(final.bounds > 0))
     }
  }
  list(final.bounds = final.bounds, elite.bound = elite.bound)
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
  uniq.ninstances <- sort(unique(ninstances), decreasing = TRUE)
  last.r <- 0L
  ranks <- rep(Inf, ncol(x))
  # Iterate from the largest to the lowest number of instances.
  for (k in uniq.ninstances) {
    confs <- which(ninstances == k)
    irace.assert(all(is.infinite(ranks[confs])))
    r <- 1L
    if (length(confs) > 1L) {
      # Select only non-NA rows
      y <- x[, confs, drop = FALSE]
      y <- y[complete.cases(y), , drop = FALSE]
      irace.assert(!any(is.na(y)))
      r <- get_ranks(y, test = test)
    }
    r <- r + last.r
    last.r <- max(r)
    ranks[confs] <- r
  }
  ranks
}

# Remove one elite count from every configuration not executed.
update_is_elite <- function(is.elite, which_exe)
{
  which.notexecuted <- setdiff(which(is.elite > 0L), which_exe) 
  is.elite[which.notexecuted] <- is.elite[which.notexecuted] - 1L
  irace.assert (all(is.elite >= 0L))
  is.elite
}

update_elite_safe <- function(Results, is.elite)
{
  elites <- is.elite > 0L
  if (!any(elites)) return(0L) # All elites rejected.
  max(which(rowAnys(!is.na(Results[, elites, drop=FALSE]))))
}

generateTimeMatrix <- function(elite_ids, experiment_log)
{
  is_elite <- experiment_log[["configuration"]] %in% elite_ids
  # Remove everything that we don't need.
  experiment_log <- experiment_log[is_elite, c("configuration", "instance", "time", "bound")]
  experiment_log[["time"]] <- pmin(experiment_log[["time"]], experiment_log[["bound"]])
  # FIXME: It would be better to use spread() from tidyr
  resultsTime <- reshape(as.data.frame(experiment_log), direction = "wide",
                         idvar = "instance", timevar = "configuration",
                         drop = "bound")
  rownames(resultsTime) <- resultsTime$instance
  resultsTime <- resultsTime[order(resultsTime$instance), , drop = FALSE]
  colnames(resultsTime) <- substring(colnames(resultsTime), nchar("time.") + 1L)
  as.matrix(resultsTime[, as.character(elite_ids), drop = FALSE])
}

race <- function(race_state, maxExp = 0L,
                 minSurvival = 1L,
                 configurations,
                 scenario)
  elitist_race(race_state, maxExp = maxExp,
               minSurvival = minSurvival,
               elite.data = NULL,
               configurations = configurations,
               scenario = scenario,
               elitist_new_instances = 0L)

elitist_race <- function(race_state, maxExp = 0L,
                 minSurvival = 1L,
                 elite.data = NULL,
                 configurations,
                 scenario,
                 elitist_new_instances)
{
  blockSize <- scenario$blockSize
  # FIXME: We should take this from scenario. However, this value should be
  # zero for the first iteration.
  ## FIXME2: Probably, instead of this, we should keep elite_safe in the race_state.
  stat.test <- scenario$testType
  conf.level <- scenario$confidence
  first.test <- blockSize * scenario$firstTest
  each.test <- blockSize * scenario$eachTest
  elitist <- scenario$elitist
  capping <- scenario$capping
  no.configurations <- nrow(configurations)
  experiment_log <- data.table(instance=integer(0), configuration=integer(0), time=numeric(0), bound=numeric(0))
  alive <- rep(TRUE, no.configurations)
  is.rejected <- rep(FALSE, no.configurations)
  
  ## FIXME: Remove argument checking. This must have been done by the caller.
  # Check argument: maxExp
  if (!missing(maxExp)
    && (!is.numeric(maxExp) || length(maxExp)!=1L || !is.finite(maxExp)))
    stop("maxExp must be an single number")
  maxExp <- if (maxExp > 0) floor(maxExp) else 0L
  if (maxExp && no.configurations > maxExp)
    irace.error("Max number of experiments is smaller than number of configurations")

  if (no.configurations <= minSurvival) {
    irace.error("Not enough configurations (", no.configurations,
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
  
  # Create the instance list according to the algorithm selected.
  if (elitist) {
    race_instances <- elitist_init_instances(race_state, deterministic = scenario$deterministic,
      sampleInstances = scenario$sampleInstances, elitist_new_instances = elitist_new_instances, block_size = blockSize)
    # It may be reduced further by elitist_init_instances()
    elitist_new_instances <- min(elitist_new_instances, race_state$elitist_new_instances)
  } else
    race_instances <- no_elitist_init_instances(race_state, deterministic = scenario$deterministic)
  
  irace.assert(!anyDuplicated(race_instances))
  irace.assert(identical(sort(race_instances), seq_along(race_instances)))
  no.tasks <- length(race_instances)

  # Initialize some variables...
  experiments_used <- 0L
  # is.elite[i] : number of instances to be seen in this race on which i has
  # been previously evaluated.
  is.elite <- rep(0L, no.configurations)

  if (is.null(elite.data)) {
    elite_safe <- 0L
    elite_instances_ID <- NULL
  } else {
    irace.assert(race_state$next_instance - 1L == nrow(elite.data))
    # There must be a non-NA entry for each instance.
    irace.assert(all(rowAnys(!is.na(elite.data))),
                 eval_after = { print(elite.data)})
    # There must be a non-NA entry for each configuration.
    irace.assert(all(colAnys(!is.na(elite.data))),
                 eval_after = {
                   cat("elite.data:\n")
                   print(elite.data)
                   cat("experiment_log:\n")
                   print(race_state$experiment_log)
                 })
    
    # elite_safe: maximum instance number for which any configuration may be
    # considered elite. After evaluating this instance, no configuration is
    # elite.
    elite_safe <- elitist_new_instances + nrow(elite.data)
    elite_instances_ID <- as.character(race_instances[seq_len(elite_safe)])
  }

  configurations_ID <- as.character(configurations[[".ID."]])
  Results <- matrix(NA,
                    nrow = elite_safe,
                    ncol = no.configurations,
                    dimnames = list(elite_instances_ID, configurations_ID))
  if (capping)
    experimentsTime <- matrix(NA,
                              nrow = elite_safe,
                              ncol = no.configurations, 
                              dimnames = list(elite_instances_ID, configurations_ID))

  if (! is.null(elite.data)) {
    Results[rownames(elite.data), colnames(elite.data)] <- elite.data

    if (capping) {
      tmp <- generateTimeMatrix(elite_ids = colnames(elite.data), 
                                experiment_log = race_state$experiment_log)
      experimentsTime[rownames(tmp), colnames(tmp)] <- tmp
      # Preliminary execution of elite configurations to calculate
      # the execution bound of initial configurations (capping only).
      if (elitist_new_instances > 0L) {
        irace.assert(elitist_new_instances %% blockSize == 0L)
        # FIXME: This should go into its own function.
        n_elite <- ncol(elite.data)
        which_elites <- which(rep(TRUE, n_elite))
        irace.note("Preliminary execution of ", n_elite,
          " elite configuration(s) over ", elitist_new_instances, " instance(s).\n")
        for (k in seq_len(elitist_new_instances)) {
          output <- race_wrapper (race_state,
            configurations = configurations[which_elites, , drop = FALSE], 
            instance_idx = race_instances[k],
            bounds = rep(scenario$boundMax, n_elite),
            which_alive = which_elites, 
            which_exe = which_elites,
            scenario = scenario)
          # Extract results
          # FIXME: check what would happen in case of having the target evaluator
          # MANUEL: Note how similar is this to what we do in do.experiments(),
          # perhaps we can create a function that takes output and experiment_log
          # and returns experiment_log. 
          # LESLIE: Yes you are right, Ill do it once we figure out the rest!
          vcost <- unlist(lapply(output, "[[", "cost"))
          irace.assert(length(vcost) == n_elite)
          vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
          Results[k, seq_len(n_elite)] <- vcost
          vtimes <- unlist(lapply(output, "[[", "time"))
          irace.assert(length(vtimes) == n_elite)
          experimentsTime[k, which_elites] <- vtimes # capping is enabled
          experiment_log <- update_experiment_log(experiment_log,
            instance=race_instances[k],
            configuration = configurations[[".ID."]][which_elites],
            time = vtimes, bound = scenario$boundMax)
        experiments_used <- experiments_used + n_elite
          
          # We remove elite configurations that are rejected given that
          # is not possible to calculate the bounds.
          rejected <- is.infinite(Results[k, which_elites])
          is.rejected[which_elites] <- rejected
          which_elites <- which_elites[!rejected]
          n_elite <- length(which_elites)
          
          # If all elite are eliminated we stop execution of initial instances.
          if (n_elite == 0L) {
            irace.note ("All elite configurations are rejected. Execution of non-elites will be not bounded.\n")
            break
          }
        }
        if (any(is.rejected)) {
          irace.note ("Immediately rejected configurations: ",
            paste0(configurations[is.rejected, ".ID."],
              collapse = ", ") , "\n")
          alive[is.rejected] <- FALSE
          # Calculate the maximum instance that has any non-NA value.
          # FIXME: Use update_elite_safe()
          if (n_elite > 0L)
            elite_safe <- max(which(rowAnys(!is.na(Results[, which_elites, drop=FALSE]))))
          else
            elite_safe <- 0L
        }
      }
    }
    # Compute the elite membership.
    is.elite <- colSums2(!is.na(Results))
    # Remove rejected configurations.
    is.elite[is.rejected] <- 0L
  }
  no_elimination <- 0L # number of tasks without elimination.
  print_header()

  if (elitist)
    all_elite_instances_evaluated <- function() {
      if (race_state$next_instance == 1L) return(TRUE)
      evaluated <- !is.na(Results[, alive, drop=FALSE])
      # All instances that have been previously seen have been evaluated by at
      # least one configuration
      all(rowAnys(evaluated)) && 
      # and the number of instances evaluated per configuration is a multiple
      # of blockSize.
        all(colSums2(evaluated) %% blockSize == 0L)
    }
  else
    all_elite_instances_evaluated <- function() TRUE

  # Start main loop
  break_msg <- NULL
  best <- NA
  for (current_task in seq_len(no.tasks)) {
    which_alive <- which(alive)
    nbAlive     <- length(which_alive)
    which_exe   <- which_alive

    if (elitist && any(is.elite > 0L)) {
      # Filter configurations that do not need to be executed (elites).
      # This is valid only for previous iteration instances.
      irace.assert(current_task <= elite_safe)
      # Execute everything that is alive and not yet executed.
      which_exe <- which(alive & is.na(Results[current_task, ]))
      if (length(which_exe) == 0L) {
        is.elite <- update_is_elite(is.elite, which_exe)
        # LESLIE: This is the case in which there are only elite configurations alive
        # and we are still in the previous instances execution, but we can still 
        # continue with the race. (This is only possible because the early termination
        # criterion is disabled)
        ## MANUEL: So what is the reason to not immediately terminate here? Is
        ## there a reason to continue?
        if (current_task == 1L) {
          # We may reach this point in the first iteration so we need to calculate best.
          if (sum(alive) == 1L) {
            best <- which_alive
          } else  {
            tmpResults <- Results[1, which_alive, drop = FALSE]
            irace.assert(!any(is.na(tmpResults)))
            # which.min returns only the first minimum.
            best <- which_alive[which.min(get_ranks(tmpResults, test = stat.test))]
          }
        }
        if (is.na(best)) {
          dump.frames(dumpto = "best_crash", to.file = TRUE,
                      include.GlobalEnv = TRUE)
          irace.assert(!is.na(best))
        }
        id_best <- configurations[[".ID."]][best]
        print_task(".", Results[seq_len(current_task), , drop = FALSE],
                   race_instances[current_task],
                   current_task, alive = alive,
                   id_best = id_best,
                   best = best, experiments_used, start_time = Sys.time(),
                   # FIXME: Why do we pass NA as bound? Why not pass the actual bound if any?
                   bound = NA, capping = capping)
        next
      }
    }

    # We stop when we have less configurations than required.
    if (nbAlive <= minSurvival && all_elite_instances_evaluated()) {
      # Stop race if we have less or equal than the minimum number of
      # configurations.
      break_msg <- paste0("number of alive configurations (", nbAlive,
                          ") <= minimum number of configurations (",
                          minSurvival, ")")
      break
    }
    # LESLIE: FIXME: Stopping deactivated by Thomas suggestion. Remove second
    # condition to restore.
    # LESLIE: Should we keep the early termination disabled? The difference between keeping it or 
    # not is that elite configurations could be eliminated later
    # LESLIE: I think we should remove this
    ## We continue running if (1) we have not reached the first.test or (2)
    ## there are instances previously seen that have not been evaluated on any
    ## alive configuration.
    if (current_task > first.test) {
      #if ((current_task > first.test && !capping) 
        # MANUEL: This is new and I'm not sure what it does.
        # LESLIE: When using capping, we dont finish any race until all 
        # previous instances have been executed (this makes sure that all non-elite 
        # configurations execute all the previous instances)
    #    || (capping && (current_task > elite_safe))) {
          # MANUEL: How is this even possible?
          # LESLIE: It can be that the capping eliminate all but one configuration
          # (which should be an elite one) after we finish the new instances to be evaluated, 
          # we allow the race to be finished. Maybe it wold be better: sum(is.elite) == sum(alive) 
          # instead of nbAlive == 1,
          # LESLIE:Removing this because now is ponitless because of the elite candidates previos
          # execution
          # || (current_task > elitist_new_instances && nbAlive == 1)))) {
      # If we just did a test, check that we have enough budget to reach the
      # next test.
      if (maxExp && ( (current_task - 1L) %% each.test) == 0L
          && experiments_used + length(which_exe) * each.test > maxExp
          && all_elite_instances_evaluated()) {
        break_msg <- paste0("experiments for next test (",
                            experiments_used + length(which_exe) * each.test,
                            ") > max experiments (", maxExp, ")")
        break
      }
    }
    
    if (elitist) {
      if (scenario$elitistLimit != 0L && no_elimination >= scenario$elitistLimit
          && all_elite_instances_evaluated()) {
        break_msg <- paste0("tests without elimination (", no_elimination,
                            ") >= elitistLimit (", scenario$elitistLimit, ")")
        break
      }
##     This is not needed anymore... 
#      else if (current_task > initial.tests && nbAlive <= minSurvival) {
#        # We can stop the race ONLY when we pass the elite_safe
#        # this is because how we are recovering the data from
#        # previous runs (based on iteration).
#        break_msg <- paste0("number of alive configurations (", nbAlive,
#                            ") less or equal than minimum number (",
#                            minSurvival, ")")
#        break
#      }
    }
    
                                
    if (nrow(Results) < current_task) {
      Results <- rbind(Results, rep(NA, ncol(Results)))
      rownames(Results) <- race_instances[seq_nrow(Results)]
      if (capping) {
        experimentsTime <- rbind(experimentsTime, rep(NA, ncol(experimentsTime)))
        rownames(experimentsTime) <- race_instances[seq_nrow(experimentsTime)]
      }
    }

    start_time <- Sys.time()
  
    # Execution bounds calculation (capping only)
    final.bounds <- elite.bound <- NULL
    # Calculate bounds for executing if needed.
    which.elite.exe <- intersect(which_exe, which(is.elite > 0))
    irace.assert(setequal(which.elite.exe, which(is.elite & is.na(Results[current_task,]))))
    if (capping) {
      # Pre-execute elite configurations that are not yet executed in the current instance.
      if (length(which.elite.exe)) {
        # FIXME: This should go into its own function
        output <- race_wrapper(race_state,
          configurations = configurations[which.elite.exe, , drop = FALSE], 
                                instance_idx = race_instances[current_task],
                                # FIXME: What if we already have a bound for this instance?
                                bounds = rep(scenario$boundMax, length(which.elite.exe)),
                                # MANUEL: How does this work for target-evaluator?
                                # We are telling race_wrapper that only some elites are alive!
                                which_alive = which.elite.exe, 
                                which_exe = which.elite.exe,
                                scenario = scenario)
        # Extract results
        vcost <- unlist(lapply(output, "[[", "cost"))
        irace.assert(length(vcost) == length(which.elite.exe))
        vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
        Results[current_task, which.elite.exe] <- vcost
        vtimes <- unlist(lapply(output, "[[", "time"))
        irace.assert(length(vtimes) == length(which.elite.exe))
        experimentsTime[current_task, which.elite.exe] <- vtimes
        experiment_log <- update_experiment_log(experiment_log,
          instance = race_instances[current_task],
          configuration = configurations[which.elite.exe, ".ID."],
          time = vtimes, bound = scenario$boundMax)
        experiments_used <- experiments_used + length(which.elite.exe)
          
        # We remove elite configurations that are rejected given that
        # is not possible to calculate the bounds
        rejected <- is.infinite(Results[current_task, which.elite.exe])
        if (any(rejected)) {
          irace.note ("Immediately rejected configurations: ",
                      paste0(configurations[which.elite.exe[rejected], ".ID."],
                             collapse = ", ") , "\n")
          is.rejected[which.elite.exe] <- rejected
          is.elite[is.rejected] <- 0L
          alive[which.elite.exe] <- !rejected
          if (!any(alive)) {
            # FIXME: Only report this error if (all(is.rejected)); otherwise restore non-rejected non-alive ones  
            irace.error("All configurations have been immediately rejected (all of them returned Inf) !")
          }
          which_alive <- which(alive)
          nbAlive     <- length(which_alive)
          elite_safe <- update_elite_safe(Results, is.elite)
        }
        which_exe <- setdiff(which_exe, which.elite.exe)
        # FIXME: There is similar code above. Can we merge these code paths?
        if (length(which_exe) == 0L) {
          is.elite <- update_is_elite(is.elite, which.elite.exe)
          if (current_task == 1L) {
            # We may reach this point in the first iteration so we need to calculate best.
            if (sum(alive) == 1L) {
              best <- which_alive
            } else  {
              tmpResults <- Results[1, which_alive, drop = FALSE]
              irace.assert(!any(is.na(tmpResults)))
              # which.min returns only the first minimum.
              best <- which_alive[which.min(get_ranks(tmpResults, test = stat.test))]
            }
          }
          if (is.na(best)) {
            dump.frames(dumpto = "best_crash", to.file = TRUE,
                        include.GlobalEnv = TRUE)
            irace.assert(!is.na(best))
          }
          id_best <- configurations[[".ID."]][best]
          print_task(".", Results[seq_len(current_task), , drop = FALSE],
                     race_instances[current_task],
                     current_task, alive = alive,
                     id_best = id_best,
                     best = best, experiments_used, start_time = start_time,
                     # FIXME: Why do we pass NA as bound? Why not pass the actual bound if any?
                     bound = if (is.null(scenario$boundMax)) NA else scenario$boundMax, capping)
          next
        }
      }
      
      all.bounds <- final.execution.bound(experimentsTime,
                                          elites = which(is.elite > 0),
                                          no.configurations, current_task,
                                          which_exe, scenario)
      final.bounds <- all.bounds$final.bounds
      elite.bound <- all.bounds$elite.bound
    } else {
      final.bounds <- rep(scenario$boundMax, no.configurations)
    }
    
    # Execute experiments
    output <- race_wrapper(race_state, configurations = configurations[which_alive, , drop = FALSE],
                           instance_idx = race_instances[current_task],
                           # FIXME: Why are we keeping final.bounds values for configurations that are dead?
                           # Also, do we use the final.bounds of which_alive or only the ones of which_exe?
                           bounds = final.bounds[which_alive],
                           which_alive = which_alive, which_exe = which_exe,
                           scenario = scenario)

    # Extract results
    vcost <- unlist(lapply(output, "[[", "cost"))
    # If the experiment was executed or target.evaluator exists
    # then the result is in the output.
    ## Currently, targetEvaluator always re-evaluates, which implies that the
    ## value may change without counting as an evaluation. We do this to allow online normalization.
    which_exps <- if (is.null(scenario$targetEvaluator)) which_exe else which_alive
    irace.assert(length(vcost) == length(which_exps))
    # Set max execution bound to timed out executions which have execution
    # times smaller than boundMax and implement parX if required
    if (capping) {
      vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
      if (scenario$boundAsTimeout)
        vcost[(vcost >= final.bounds[which_exps]) & (vcost < scenario$boundMax)] <- scenario$boundMax
    }
    Results[current_task, which_exps] <- vcost

    # Output is not indexed in the same way as configurations.
    which_exps <- which(which_alive %in% which_exe)
    irace.assert(length(which_exps) == length(which_exe))
    vtimes <- unlist(lapply(output[which_exps], "[[", "time"))
    irace.assert(length(vtimes) == length(which_exe))
    if (capping) {
      # Correct higher execution times.
      # final.bounds indexes are 1:nbConfigurations, vtimes are 1:length(which_alive)
      experimentsTime[current_task, which_exps] <- pmin(vtimes, final.bounds[which_exe])
    }
    experiment_log <- update_experiment_log(experiment_log,
      instance = race_instances[current_task],
      configuration = configurations[which_exe, ".ID."],
      time = vtimes, bound = if (is.null(final.bounds)) NA else final.bounds[which_exe])

    irace.assert(anyDuplicated(experiment_log[, c("instance", "configuration")]) == 0L,
      eval_after = {
        print(experiment_log)
        print(mget(ls()))
      })
    experiments_used <- experiments_used + length(which_exe)
    # We update the elites that have been executed.
    is.elite <- update_is_elite(is.elite, which.elite.exe)

    ## Drop bad configurations.
    ## Infinite values denote immediate rejection of a configuration.
    rejected <- is.infinite(Results[current_task, which_exe])
    if (any(rejected)) {
      irace.note ("Immediately rejected configurations: ",
                  paste0(configurations[which_exe[rejected], ".ID."],
                         collapse = ", ") , "\n")
      is.rejected[which_exe] <- rejected
      is.elite[is.rejected] <- 0L
      alive[is.rejected] <- FALSE
      if (!any(alive))
        irace.error("All configurations have been immediately rejected (all of them returned Inf) !")
      which_alive <- which(alive)
      nbAlive     <- length(which_alive)
      # FIXME: Should we stop  if (nbAlive <= minSurvival) ???
      elite_safe <- update_elite_safe(Results, is.elite)  
    }
    irace.assert(!anyNA(Results[seq_len(current_task), alive, drop=FALSE]))
    irace.assert(!any(is.infinite(Results[, alive, drop=FALSE])))
    
    # Variables required to produce output of elimination test.
    cap.done     <- FALSE #if dominance elimination was performed
    test.done    <- FALSE #if statistical test elimination was performed
    cap.dropped  <- FALSE #if capping has drop any configuration
    test.dropped <- FALSE #if any candidates has been eliminated by testing
    cap.alive    <- test.alive <- alive
    
    ## Dominance elimination (Capping only).
    # The second condition can be false if we eliminated via immediate
    # rejection.  The third condition ensures that we see the block before capping.
    if (capping && sum(alive) > minSurvival && (current_task %% blockSize) == 0L) {
      irace.assert(!any(is.elite > 0) == (current_task >= elite_safe))
      cap.alive <- dom.elim(Results[seq_len(current_task), , drop = FALSE],
                            # Get current elite configurations
                            elites = which(is.elite > 0L),
                            alive, scenario, minSurvival)
      cap.dropped <- sum(alive) > sum(cap.alive)
      cap.done    <- TRUE
    }
    
    # We assume that first.test is a multiple of each.test.  In any
    # case, this will only do the first test after the first multiple
    # of each.test that is larger than first.test.
    if (current_task >= first.test && (current_task %% each.test) == 0L
        && nbAlive > 1L) {
      irace.assert(sum(alive) == nbAlive)
      test.res <-
        switch(stat.test,
               friedman = aux_friedman(Results[seq_len(current_task), ], alive, which_alive, conf.level),
               t.none = aux.ttest(Results[seq_len(current_task), ], alive, which_alive, conf.level, adjust = "none"),
               t.holm = aux.ttest(Results[seq_len(current_task), ], alive, which_alive, conf.level, adjust = "holm"),
               t.bonferroni = aux.ttest(Results[seq_len(current_task), ], alive, which_alive, conf.level, adjust = "bonferroni"))
      
      race.ranks <- test.res$ranks
      test.alive <- test.res$alive
      test.dropped <- sum(alive) > sum(test.alive)
      test.done   <- TRUE
    }
    
    # Merge the result of both eliminations.
    prev.sum.alive <- sum(alive)
    alive <- cap.alive & test.alive
    
    # Handle elites when elimination is performed.  The elite configurations
    # can be removed only when they have no more previously-executed instances.
    irace.assert(!any(is.elite > 0L) == (current_task >= elite_safe))
    if (!is.null(elite.data) && any(is.elite > 0L)) {
      irace.assert (length(alive) == length(is.elite))
      alive <- alive | (is.elite > 0L)
    }

    # It may happen that the capping and the test eliminate together all
    # configurations. In that case, we only trust the capping elimination.
    if (capping && !any(alive)) {
      if (scenario$debugLevel >= 2L) {
        irace.warning("Elimination tests have eliminated all configurations, keeping the capping results.\n")
        irace.note("Alive according to capping:", which(cap.alive), "\n")
        irace.note("Alive according to test:", which(test.alive), "\n")
      }
      alive <- cap.alive
    }
    
    # Output the result of the elimination test
    res.symb <- if (cap.dropped && !test.dropped && prev.sum.alive != sum(alive)) {
                  "c" # Removed just by capping
                } else if (cap.dropped || test.dropped) {
                  if (prev.sum.alive != sum(alive)) "-" else "!"
                } else if (cap.done || test.done) "=" else "x"
    
    # Rank alive configurations: order all configurations (eliminated or not)
    # LESLIE: we have to make the ranking outside: we can have configurations eliminated by capping
    # that are not eliminated by the test.
    # MANUEL: I don't understand the above comment.
    if (length(which_alive) == 1L) {
      race.ranks <- 1L
      best <- which_alive
    } else  {
      tmpResults <- Results[seq_len(current_task), which_alive, drop = FALSE]
      irace.assert(!any(is.na(tmpResults)))
      race.ranks <- get_ranks(tmpResults, test = stat.test) 
      # which.min returns only the first minimum.
      best <- which_alive[which.min(race.ranks)]
    }
    
    irace.assert(best == which_alive[order(race.ranks)][1L])
    irace.assert(length(race.ranks) == length(which_alive))

    prev_alive  <- which_alive
    which_alive <- which(alive)
    # Remove the ranks of those that are not alive anymore
    race.ranks <- race.ranks[which_alive]
    irace.assert(length(race.ranks) == sum(alive))
    id_best <- configurations[[".ID."]][best]
    print_task(res.symb, Results[seq_len(current_task), , drop = FALSE],
               race_instances[current_task],
               current_task, alive = alive,
               id_best = id_best, best = best, experiments_used, start_time = start_time, 
               bound = elite.bound, capping)
    
    if (elitist) {
      # Compute number of statistical tests without eliminations.
      irace.assert(!any(is.elite > 0) == (current_task >= elite_safe))
      if (!any(is.elite > 0)
          && current_task > first.test && (current_task %% each.test) == 0L) {
        if (length(which_alive) == length(prev_alive)) {
          no_elimination <- no_elimination + 1L
        } else {
          no_elimination <- 0L
        }
      }
    } 
  }
  
  if (is.null(break_msg))
    break_msg <- paste0("all instances (", no.tasks, ") evaluated")

  # Adding this given that when ncandidates = minsurvival+1
  # and there one elite configuration that gets discarded in the new instances
  # execution the race is finished with no executions.
  # FIXME: we should handle this better, maybe allowing irace to handle no elite 
  # in irace()
  # MANUEL: Leslie, how can we reach this error in normal circumstances?
  # Can we handle this better?
  if (current_task == 1L && all(is.elite == 0L))
    irace.error ("Maximum number configurations immediately rejected reached!")
  
  # All instances that are not new in this race must have been evaluated by at
  # least one configuration.
  irace.assert(all_elite_instances_evaluated(),
               eval_after = { print(Results[,alive, drop=FALSE])})
  # If we stop the loop before we see all new instances, there may be new
  # instances that have not been executed by any configuration.
  Results <- Results[rowAnys(!is.na(Results)), , drop = FALSE]
  race.ranks <- overall_ranks(Results[, alive, drop = FALSE], test = stat.test)
  if (!scenario$quiet) {
    old_best <- best # old_best could be NA.
    best <- which_alive[which.min(race.ranks)]
    mean_best <- mean(Results[, best])
    print_footer(bestconf = configurations[best, , drop = FALSE],
                 # FIXME: This is the mean of the best, but perhaps it
                 # should be the sum of ranks in the case of test ==
                 # friedman?
                 mean_best = mean_best,
                 break_msg = break_msg, debug_level = scenario$debugLevel, 
                 capping = capping,
                 old_best_id  = if (old_best == best || is.na(old_best)) NULL else id_best)
  }
  rejected_ids <- configurations[is.rejected, ".ID."]
  scenario$parameters$forbid_configurations(
    race_state$update_rejected(rejected_ids, configurations)
  )
  # Only return alive ones.
  nbAlive <- sum(alive)
  configurations <- configurations[as.logical(alive), , drop=FALSE]
  irace.assert(nbAlive == nrow(configurations))
  irace.assert(all(configurations[[".ID."]] %not_in% rejected_ids)) # No rejected is alive.
  # Assign the proper ranks in the configurations data.frame.
  configurations[[".RANK."]] <- race.ranks
  # Now we can sort the data.frame by the rank.
  configurations <- configurations[order(configurations[[".RANK."]]), , drop=FALSE]
  if (scenario$debugLevel >= 3L) {
    irace.note ("Memory used in race():\n")
    race_state$print_mem_used()
  }
  # nrow(Results) may be smaller, equal or larger than current_task.
  irace.assert(nrow(experiment_log) == experiments_used)

  list(experiments = Results,
       experiment_log = experiment_log,
       experimentsUsed = experiments_used,
       configurations = configurations)
}

update_experiment_log <- function(experiment_log, instance, configuration, time, bound)
  rbind(experiment_log,
    data.table(instance = instance, configuration = configuration, time = time, bound = bound))
  
