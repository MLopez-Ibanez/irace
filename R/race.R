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
                                 instances, instances.ID, seeds,
                                 scenario, bounds = NULL)
{
  instances <- instances[instances.ID]
  experiments <- vector("list", nrow(configurations) * length(instances))
  configurations.ID <- configurations[, ".ID."]
  pnames <- parameters$names
  values <- configurations[, pnames, drop = FALSE]
  switches <- parameters$switches[pnames]

  # There must be a bound for each configuration.
  # FIXME: There must be a bound for each configuration AND each instance.
  irace.assert(is.null(bounds) || length(bounds) == nrow(configurations))
  bounds <- rep(bounds, length(instances))

  count <- 1
  for (i in seq_len(nrow(configurations))) {
    values_i <- values[i, , drop = FALSE]
    for (j in seq_along(instances)) {
      experiments[[count]] <- list (id.configuration = configurations.ID[i],
                                    id.instance  = instances.ID[j],
                                    seed = seeds[j],
                                    configuration = values_i,
                                    # mlr uses lists of objects as instances.
                                    instance = instances[[j]],
                                    bound = bounds[count],
                                    switches = switches) 
      count <- count + 1 
    }
  }
  return(experiments)
}



## Executes a list of configurations in a particular instance
## configurations: description having the id of the configuration
## instance.idx: index of the instance,seed pair in .irace$instancesList
## bounds: execution bounds (if needed).
## which.alive: index of the configurations that are still alive
## which.exe: index of the alive configurations that should be executed
race.wrapper <- function(configurations, instance.idx, bounds = NULL,
                         # FIXME: we actually only need which.exps, not
                         # which.alive nor which.exe
                         which.alive, which.exe, parameters, scenario)
{
  irace.assert (parameters$nbVariable > 0)
  irace.assert (length(parameters$names) == parameters$nbParameters)

  # FIXME: Accessing 'seed' and 'instance' should be moved to createExperimentList.
  seed <- .irace$instancesList[instance.idx, "seed"]
  id.instance  <- .irace$instancesList[instance.idx, "instance"]
  # Experiment list to execute
  experiments <- createExperimentList(configurations, parameters,
                                      instances = scenario$instances,
                                      instances.ID = id.instance,
                                      seeds = seed, scenario, bounds = bounds)

  target.output <- vector("list", length(experiments))
  # Execute commands
  if (length(which.exe) > 0) {
    # which.exe values are within 1:nbConfigurations, whereas experiments
    # indices are within 1:length(which.alive). The following line converts
    # from one to the other.
    which.exps <- which(which.alive %in% which.exe)
    irace.assert(length(which.exps) == length(which.exe))
    target.output[which.exps] <- execute.experiments (experiments[which.exps], scenario)
  }

  # targetEvaluator may be NULL. If so, target.output must
  # contain the right output already.
  # Otherwise, targetEvaluator always re-evaluates.
  if (!is.null(scenario$targetEvaluator))
    target.output <- execute.evaluator (experiments, scenario, target.output,
                                        configurations[, ".ID."])
  target.output
}

aux2.friedman <- function(y, I, alive, conf.level = 0.95)
{
  dropped.any <- FALSE
  n <- nrow(y)
  k <- length(I)
  r <- t(apply(y[,I], 1L, rank))
  R <- colSums(r)
  o <- order(R)
  best <- I[o[1]]
  TIES <- tapply(r, row(r), table)
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
  return(list(best = best, ranks = R, alive = alive, dropped.any = dropped.any, p.value = PVAL))
}

aux_friedman <- function(results, alive, which.alive, conf.level)
{
  no.alive <- length(which.alive)
  
  if (no.alive == 2) {
    best <- NULL
    ranks <- NULL
    dropped.any <- TRUE
    PVAL <- 0
    # If only 2 configurations are left, switch to Wilcoxon
    V1 <- results[, which.alive[1]]
    V2 <- results[, which.alive[2]]
    
    # Avoid the test if the answer is obvious
    if (all(V1 <= V2)) {
      ranks <- c(1,2)
    } else if (all(V2 <= V1)) {
      ranks <- c(2,1)
    } else {
      res <- wilcox.test(V1, V2, paired = TRUE, conf.int = TRUE)
      PVAL <- res$p.value
      irace.assert(!is.nan(PVAL) & !is.na(PVAL))
      if (PVAL >= 1 - conf.level) dropped.any <- FALSE
      # We use the pseudo median estimated by the test.
      ranks <- if (res$estimate <= 0) c(1,2) else c(2,1)
    }
    best <- which.alive[ranks[1]]
    if (dropped.any)
      alive[which.alive[ranks[2]]] <- FALSE

    irace.assert(which.alive[which.min(ranks)] == best)
    return(list(best = best, ranks = ranks, alive = alive, dropped.any = dropped.any, p.value = PVAL))
  } else {
    # If more then 2 configurations are left, use Friedman
    return (aux2.friedman(results, which.alive, alive, conf.level = conf.level))
  }
}

aux.one_ttest <- function(results, alive, which.alive, conf.level,
                          adjust = c("none","bonferroni","holm"))
{
  adjust <- match.arg(adjust)
  irace.assert(sum(alive) == length(which.alive))
  results <- results[, which.alive]
  means <- colMeans(results)
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
    # equal or zero if they are different
    if (min(var(results_best), var(results_j)) < 10 * .Machine$double.eps) next
    # The t.test may fail if the data are not normal despite one configuration
    # clearly dominating the other.
    if (all(results_best <= results_j)) next
    try(PVAL <- t.test(results_best, results_j, alternative = "less",
                       paired = TRUE)$p.value)
    irace.assert(!is.nan(PVAL) & !is.na(PVAL))
    pvals[j] <- PVAL
  }
  pvals <- p.adjust(pvals, method = adjust)
  dropj <- which.alive[pvals < 1.0 - conf.level]
  dropped_any <- length(dropj) > 0
  irace.assert(all(alive[dropj]))
  alive[dropj] <- FALSE
  return(list(best = which.alive[best], ranks = means, alive = alive,
              dropped.any = dropped_any, p.value = min(pvals)))
}

aux.ttest <- function(results, alive, which.alive, conf.level,
                      adjust = c("none","bonferroni","holm"))
{
  adjust <- match.arg(adjust)
  irace.assert(sum(alive) == length(which.alive))
  
  results <- results[, which.alive]
  means <- colMeans(results)
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
    # equal or zero if they are different
    if (min(var(results_best), var(results_j)) < 10 * .Machine$double.eps) next
    # The t.test may fail if the data are not normal despite one configuration
    # clearly dominating the other.
    if (all(results_best <= results_j)) next
    try(PVAL <- t.test(results_best, results_j, paired = TRUE)$p.value)
    irace.assert(!is.nan(PVAL) & !is.na(PVAL))
    pvals[j] <- PVAL
  }
  pvals <- p.adjust(pvals, method = adjust)
  dropj <- which.alive[pvals < 1.0 - conf.level]
  dropped_any <- length(dropj) > 0
  irace.assert(all(alive[dropj]))
  alive[dropj] <- FALSE
  return(list(best = which.alive[best], ranks = means, alive = alive,
              dropped.any = dropped_any, p.value = min(pvals)))
}

no_elitrace.init.instances <- function(deterministic, max_instances)
{
  # if next.instance == 1 then this is the first iteration.
  # If deterministic consider all (do not resample).
  if (.irace$next.instance == 1 || deterministic) return(1:max_instances)
  irace.assert(.irace$next.instance < max_instances)
  return(.irace$next.instance : max_instances)
}

elitrace.init.instances <- function(race.env, deterministic, max_instances, sampleInstances)
{
  # if next.instance == 1 then this is the first iteration.
  if (.irace$next.instance == 1) return(1:max_instances) # Consider all

  new.instances <- NULL
  last.new <- .irace$next.instance + race.env$elitistNewInstances - 1
  # Do we need to add new instances?
  if (race.env$elitistNewInstances > 0) {
    if (last.new > max_instances) {
      # This may happen if the scenario is deterministic and we would need
      # more instances than what we have.
      irace.assert(deterministic)
      if (.irace$next.instance <= max_instances) {
        # Add all instances that we have not seen yet as new ones.
        last.new <- max_instances
        new.instances <- .irace$next.instance : last.new
      } # else new.instances remains NULL and last.new remains > number of instances.
      # We need to update this because the value is used below and now there
      # may be fewer than expected, even zero.
      race.env$elitistNewInstances <- length(new.instances)
    } else {
      new.instances <- .irace$next.instance : last.new
    }
  }
  future.instances <- NULL
  if ((last.new + 1) <= max_instances) {
    future.instances <- (last.new + 1) : max_instances
  }
  # new.instances + past.instances + future.instances
  # FIXME: we should sample taking into account the block-size, so we sample blocks, not instances.
  past_instances <- if (sampleInstances)
                      sample.int(.irace$next.instance - 1) else
                                                             1:.irace$next.instance
  c(new.instances, past_instances, future.instances)
}

table_hline <- function(widths) {
  s <- "+"
  for(w in widths) {
    s <- paste0(s, strrep("-", w), "+")
  }
  return(paste0(s, "\n"))
}
table_sprint <- function(text, widths) {
  s <- "|"
  for (i in seq_along(text)) {
      s <- paste0(s, sprintf("%*s", widths[i], text[i]), "|")
  }
  return(paste0(s, "\n"))
}
.nocap_table_fields_width <- c(1, 11, 11, 11, 16, 11, 8, 5, 4, 6)
.nocap_colum_names <- c(" ", "Instance", "Alive", "Best", "Mean best", "Exp so far",
                        "W time", "rho", "KenW", "Qvar")
.capping_table_fields_width <- c(1, 11, 8, 11, 11, 16, 11, 8, 5, 4, 6)  
.capping_colum_names <- c(" ", "Instance", "Bound", "Alive", "Best", "Mean best", "Exp so far",
                        "W time", "rho", "KenW", "Qvar")
capping_hline <- table_hline(.capping_table_fields_width)
capping_header <- table_sprint(.capping_colum_names, .capping_table_fields_width) 
nocap_hline <- table_hline(.nocap_table_fields_width)
nocap_header <- table_sprint(.nocap_colum_names, .nocap_table_fields_width) 

# FIXME: Depending on capping here is ugly. We should simply set-up the correct printing functions at the start of race().
race.print.header <- function(capping)
{
  cat(sep = "", "# Markers:
     x No test is performed.
     c Configurations are discarded only due to capping.
     - The test is performed and some configurations are discarded.
     = The test is performed but no configuration is discarded.
     ! The test is performed and configurations could be discarded but elite configurations are preserved.
     . All alive configurations are elite and nothing is discarded\n\n")
  if (capping)
    cat(sep = "", capping_hline, capping_header, capping_hline)
  else
    cat(sep = "", nocap_hline, nocap_header, nocap_hline)
}

race.print.task <- function(res.symb, Results,
                            instance,
                            current.task,
                            alive,
                            id.best,
                            best,
                            experimentsUsed,
                            start.time,
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
  time_str <- elapsed_wctime_str(Sys.time(), start.time)
  cat(sprintf("|%s|%11d|", res.symb, instance))
  if (capping) {
    if (is.null(bound)) cat("      NA|") else cat(sprintf("%8.2f|", bound))
  }
  cat(sprintf(paste0("%11d|%11d|", .irace.format.perf, "|%11d|%s"),
              sum(alive), id.best, mean_best, experimentsUsed, time_str))
  
  if (current.task > 1 && sum(alive) > 1) {
    conc <- concordance(Results[1:current.task, alive, drop = FALSE])
    qvar <- dataVariance(Results[1:current.task, alive, drop = FALSE])
    # FIXME: We would like to use %+#4.2f but this causes problems with
    # https://github.com/oracle/fastr/issues/191
    cat(sprintf("|%+4.2f|%.2f|%.4f|\n", conc$spearman.rho, conc$kendall.w, qvar))
  } else {
    cat("|   NA|  NA|    NA|\n")
  }
}

race.print.footer <- function(bestconf, mean.best, break.msg, debug.level, capping = FALSE)
{
  cat(sep = "",
      if (capping) capping_hline else nocap_hline,
      if (debug.level >= 1) paste0("# Stopped because ", break.msg, "\n"),
      sprintf("Best-so-far configuration: %11d", bestconf[1, ".ID."]),
      "    mean value: ",
      sprintf(.irace.format.perf, mean.best), "\n",
      "Description of the best-so-far configuration:\n")
  configurations.print(bestconf, metadata = TRUE)
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
  irace.assert (ncol(data) >= 1)
  
  if (ncol(data) == 1) {
    return (mean(data[,1], na.rm = TRUE))
  }
  # This should never happen because the data used to obtain the execution
  # bound should be complete, that is, the bounding configurations should have
  # been executed on all previous instances.
  
  irace.assert (all(!is.na(data)))
  bound <- switch (type,
                   median = median(colMeans(data)),
                   mean   = mean(colMeans(data)),
                   worst  = max(colMeans(data)),
                   # default:
                   min(colMeans(data)))
  return (bound)
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
  bound <- switch (type,
                   median = median(data, na.rm=TRUE),
                   mean   = mean(data, na.rm=TRUE),
                   worst  = max(data, na.rm=TRUE),
                   # default:
                   min(data, na.rm=TRUE))
  return (bound)

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
  which.alive <- which(alive)
  cmeans <- colMeans(results[, alive, drop = FALSE])
  irace.assert(!all(is.na(cmeans))) # Only NA values when calculating mean for dominance.
  
  # When there are no protected elites left, select the best configurations to
  # calculate the bounds. This is quite aggressive and another alternative
  # would be to disable dom.elim when elites == 0.
  if (length(elites) == 0) {
    # In the case we have only two alive configurations only one can be elite.
    if (sum(alive) <= 2)
      elites <- which.alive[which.min(cmeans)]
    else
      elites <- which.alive[order(cmeans, na.last = TRUE, decreasing = FALSE)[1:minSurvival]]
  }
  
  bound <- executionBound(results[, elites, drop = FALSE], type = scenario$cappingType)
  alive[which.alive] <- ((bound + eps) >= cmeans)
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
## * elite.bound : value (ei. mean execution time or per instance execution time) 
## used to calculate the maximimum exeuction time (final.bounds) for each configuration.
## * final.bounds[i] : maximum execution time for candidate i on the current instance. 
final.execution.bound <- function(experimentsTime, elites, no.configurations,
                                  current.task, which.exe, scenario)
{
  minMeasurableTime <- scenario$minMeasurableTime
  boundMax <- scenario$boundMax
  # FIXME: should we use an adjusted boundMax 
  final.bounds <- rep(boundMax, no.configurations)
  total.time  <- current.task * boundMax
  elite.bound <- boundMax
  # Elite candidates can have NA values due to the rejection 
  if (length(elites) > 0)
    elites <- elites[!is.na(experimentsTime[current.task,elites])]
    
  # Only apply bounds when there is previous data
  if (length(elites) > 0 && length(which.exe) > 0) {
    # The elite membership is updated before calling this function to know 
    # the configurations used for calculating the bounds we need to do this.
    # Note that some of these configurations could be not elite anymore for the 
    # elimination phase, given that all their evaluations have been used.
    if (scenario$boundType == "instance") {
       elite.bound <- instanceBound(experimentsTime[current.task, elites],
                                    type = scenario$cappingType)
       final.bounds[which.exe] <- min(elite.bound + minMeasurableTime, boundMax)
       final.bounds[which.exe] <- ceiling.digits(final.bounds[which.exe], scenario$boundDigits)
     } else {
       elite.bound <- executionBound(experimentsTime[1:current.task, elites, drop = FALSE],
                                     type = scenario$cappingType)
       elite.bound <- min(elite.bound, boundMax)
       # FIXME: This minMeasurableTime should be a scenario setting and it
       # should be the same value that we use in check.output.target.runner
       total.time <- (current.task * elite.bound) + minMeasurableTime
       time.left <- total.time - colSums(experimentsTime[1:current.task, which.exe, drop = FALSE], na.rm = TRUE)
       final.bounds[which.exe] <- sapply(time.left, min, boundMax)
       # We round up the bounds up to the specified number of digits. This may
       # be necessary if the target-algorithm does not support higher precision.
       final.bounds[which.exe] <- ceiling.digits(final.bounds[which.exe], scenario$boundDigits)
       # There are cases in which a small negative budget is used. For example:
       # assuming candidates 1 and 2 are elite maxbound=80
       # executionTime <- matrix(c(0.010,0.0170,0.010, 24,28,27, 0.010,0.017,NA), 
       #        byrow=TRUE, ncol=3, nrow=3, dimnames=list(c(1,2,3),c(1,2,3)))
       # current.task <- 3; boundDigits=0
       # elite.bound  <- irace:::executionBound(executionTime[1:current.task,1:2], "median") 
       # total.time   <- elite.bound * current.task + 0.01
       # time.left    <- total.time - colSums(executionTime[1:current.task,3,drop=FALSE], na.rm=TRUE)
       # We set the execution time to the elite.bound this should be enough
       # to eliminate a bad candidate for the next task.
       final.bounds[final.bounds <= 0] <- elite.bound 
       irace.assert(all(final.bounds > 0))
     }
  }
  return(list(final.bounds = final.bounds, elite.bound = elite.bound))
}

# Recompute the best as follows. Given two configurations, the one evaluated
# on more instances is ranked better. Otherwise, break ties according to the
# criteria of the stat test.
overall.ranks <- function(x, stat.test)
{
  if (ncol(x) == 1) return(1)
    
  ninstances <- colSums(!is.na(x))
  uniq.ninstances <- sort(unique(ninstances), decreasing = TRUE)
  last.r <- 0
  ranks <- rep(Inf, ncol(x))
  # Iterate from the largest to the lowest number of instances.
  for (k in uniq.ninstances) {
    confs <- which(ninstances == k)
    irace.assert(all(is.infinite(ranks[confs])))
    r <- 1
    if (length(confs) > 1) {
      # Select only non-NA rows
      y <- x[, confs, drop = FALSE]
      y <- y[complete.cases(y), , drop = FALSE]
      irace.assert(!any(is.na(y)))
      if (stat.test == "friedman") {
        r <- colSums(t(apply(y, 1L, rank)))
      } else {
        r <- rank(colMeans(y))
      }
    }
    r <- r + last.r
    last.r <- max(r)
    ranks[confs] <- r
  }
  return(ranks)
}

# Remove one elite count from every configuration not executed.
update.is.elite <- function(is.elite, which.exe)
{
  which.notexecuted <- setdiff(which(is.elite > 0), which.exe) 
  is.elite[which.notexecuted] <- is.elite[which.notexecuted] - 1
  irace.assert (all(is.elite >= 0))
  return(is.elite)
}

update.elite.safe <- function(Results, is.elite)
{
  if (any(is.elite > 0L))
    return(max(which(apply(!is.na(Results[, is.elite > 0, drop=FALSE]), 1, any))))
  # All elites rejected.
  return(0L)
}

race <- function(maxExp = 0,
                 minSurvival = 1,
                 elite.data = NULL,
                 configurations,
                 parameters,
                 scenario,
                 elitistNewInstances)
{
  race.env <- new.env(parent = emptyenv())
  # FIXME: We should take this from scenario. However, this value should be
  # zero for the first iteration.
  ## FIXME2: Probably, instead of this, we should keep elite.safe in the race.env.
  race.env$elitistNewInstances <- elitistNewInstances
  stat.test <- scenario$testType
  conf.level <- scenario$confidence
  first.test <- scenario$firstTest
  each.test <- scenario$eachTest
  elitist <- scenario$elitist
  capping <- scenario$capping
  quiet <- scenario$quiet
  no.configurations <- nrow(configurations)
  experimentLog <- matrix(nrow = 0, ncol = 4,
                          dimnames = list(NULL, c("instance", "configuration", "time", "bound")))
  alive <- rep(TRUE, no.configurations)
  is.rejected <- rep(FALSE, no.configurations)
  
  ## FIXME: Remove argument checking. This must have been done by the caller.
  # Check argument: maxExp
  if (!missing(maxExp) &&
      (!is.numeric(maxExp) ||
       length(maxExp)!=1 ||
       !is.finite(maxExp)))
    stop("maxExp must be an single number")
  maxExp <- ifelse(maxExp>0,maxExp,0)
  maxExp <- floor(maxExp)
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

  # Create the instance list according to the algorithm selected
  if (elitist)
    race.instances <- elitrace.init.instances(race.env,
                                              scenario$deterministic,
                                              max_instances = nrow(.irace$instancesList),
                                              sampleInstances = scenario$sampleInstances)
  else
    race.instances <- no_elitrace.init.instances(scenario$deterministic,
                                                 max_instances = nrow(.irace$instancesList))
  no.tasks <- length(race.instances)

  # Initialize some variables...
  experimentsUsed <- 0L
  # is.elite[i] : number of instances to be seen in this race on which i has
  # been previously evaluated.
  is.elite <- rep(0L, no.configurations)

  if (is.null(elite.data)) {
    elite.safe <- 0L
    elite.instances.ID <- NULL
  } else {
    irace.assert(.irace$next.instance - 1 == nrow(elite.data$experiments))
    # There must be a non-NA entry for each instance.
    irace.assert(all(apply(!is.na(elite.data$experiments), 1, any)),
                 eval.after = { print(elite.data$experiments)})
    # There must be a non-NA entry for each configuration.
    irace.assert(all(apply(!is.na(elite.data$experiments), 2, any)))
    
    # elite.safe: maximum instance number for which any configuration may be
    # considered elite. After evaluating this instance, no configuration is
    # elite.
    elite.safe <- race.env$elitistNewInstances + nrow(elite.data[["experiments"]])
    elite.instances.ID <- as.character(race.instances[1:elite.safe])
  }

  configurations.ID <- as.character(configurations[, ".ID."])
  Results <- matrix(NA,
                    nrow = elite.safe,
                    ncol = no.configurations,
                    dimnames = list(elite.instances.ID, configurations.ID))
  if (capping)
    experimentsTime <- matrix(NA,
                              nrow = elite.safe,
                              ncol = no.configurations, 
                              dimnames = list(elite.instances.ID, configurations.ID))

  if (! is.null(elite.data)) {
    Results[rownames(elite.data[["experiments"]]),
            colnames(elite.data[["experiments"]])] <- elite.data[["experiments"]]

    if (capping) {
      experimentsTime[rownames(elite.data[["time"]]),
                      colnames(elite.data[["time"]])] <- elite.data[["time"]]
    }

    # Preliminary execution of elite configurations to calculate
    # the execution bound of initial configurations (capping only).
    if (capping && elitistNewInstances != 0) {
      # FIXME: This should go into its own function.
      n.elite <- ncol(elite.data[["experiments"]])
      which.elites <- which(rep(TRUE, n.elite))
      irace.note("Preliminary execution of ", n.elite,
                 " elite configuration(s) over ", elitistNewInstances, " instance(s).\n")
      for (k in 1:elitistNewInstances) {
        output <- race.wrapper (configurations = configurations[which.elites, , drop = FALSE], 
                                instance.idx = race.instances[k],
                                bounds = rep(scenario$boundMax, n.elite),
                                which.alive = which.elites, 
                                which.exe = which.elites,
                                parameters = parameters, 
                                scenario = scenario)
        # Extract results
        # FIXME: check what would happen in case of having the target evaluator
        # MANUEL: Note how similar is this to what we do in do.experiments(),
        # perhaps we can create a function that takes output and experimentLog
        # and returns experimentLog. 
        # LESLIE: Yes you are right, Ill do it once we figure out the rest!
        vcost <- unlist(lapply(output, "[[", "cost"))
        irace.assert(length(vcost) == n.elite)
        vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
        Results[k, 1:n.elite] <- vcost
        vtimes <- unlist(lapply(output, "[[", "time"))
        irace.assert(length(vtimes) == n.elite)
        experimentsTime[k, which.elites] <- vtimes
        experimentLog <- rbind(experimentLog,
                               cbind(race.instances[k],
                                     configurations[which.elites, ".ID."],
                                     vtimes,
                                     scenario$boundMax))     
        experimentsUsed <- experimentsUsed + n.elite
        
        # We remove elite configurations that are rejected given that
        # is not possible to calculate the bounds.
        rejected <- is.infinite(Results[k, which.elites])
        is.rejected[which.elites] <- rejected
        which.elites <- which.elites[!rejected]
        n.elite <- length(which.elites)

      	# If all elite are eliminated we stop execution of initial instances.
      	if (n.elite == 0L) {
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
        # FIXME: Use update.elite.safe()
        if (n.elite > 0L)
          elite.safe <- max(which(apply(!is.na(Results[, which.elites, drop=FALSE]), 1, any)))
        else 
          elite.safe <- 0L
      }
    }
    # Compute the elite membership.
    is.elite <- colSums(!is.na(Results))
    # Remove rejected configurations.
    is.elite[is.rejected] <- 0L
  }

  best <- 0
  race.ranks <- c()
  no.elimination <- 0 # number of tasks without elimination.
  if (!quiet)
    race.print.header(capping)

  # Test that all instances that have been previously seen have been evaluated
  # by at least one configuration.
  all_elite_instances_evaluated <- function() {
    if (!elitist)
      return(TRUE)
    return(all(apply(!is.na(Results[
                        as.character(seq_len(.irace$next.instance - 1)),
                        alive, drop=FALSE]), 1, any)))
  }

  # Start main loop
  break.msg <- NULL
  for (current.task in seq_len(no.tasks)) {
    which.alive <- which(alive)
    nbAlive     <- length(which.alive)
    which.exe   <- which.alive

    if (elitist && any(is.elite > 0)) {
      # Filter configurations that do not need to be executed (elites).
      # This is valid only for previous iteration instances.
      irace.assert(current.task <= elite.safe)
      # Execute everything that is alive and not yet executed.
      which.exe <- which(alive & is.na(Results[current.task, ]))
      if (length(which.exe) == 0) {
        is.elite <- update.is.elite(is.elite, which.exe)
        # LESLIE: This is the case in which there are only elite configurations alive
        # and we are still in the previous instances execution, but we can still 
        # continue with the race. (This is only possible because the early termination
        # criterion is disabled)
        ## MANUEL: So what is the reason to not immediately terminate here? Is
        ## there a reason to continue?
        if (!quiet)
          race.print.task(".", Results[1:current.task, , drop = FALSE],
                          race.instances[current.task],
                          current.task, alive = alive,
                          configurations[best, ".ID."],
                          best = best, experimentsUsed, Sys.time(), 
                          bound = NA, capping)
        next
      }
    }

    # LESLIE: FIXME: Stopping deactivated by Thomas suggestion. Remove second
    # condition to restore.
    # LESLIE: Should we keep the early termination disabled? The difference between keeping it or 
    # not is that elite configurations could be eliminated later
    # LESLIE: I think we should remove this
    ## We continue running if (1) we have not reached the first.test or (2)
    ## there are instances previously seen that have not been evaluated on any
    ## alive configuration.
    if (current.task > first.test) {
      #if ((current.task > first.test && !capping) 
        # MANUEL: This is new and I'm not sure what it does.
        # LESLIE: When using capping, we dont finish any race until all 
        # previous instances have been executed (this makes sure that all non-elite 
        # configurations execute all the previous instances)
    #    || (capping && (current.task > elite.safe))) {
          # MANUEL: How is this even possible?
          # LESLIE: It can be that the capping eliminate all but one configuration
          # (which should be an elite one) after we finish the new instances to be evaluated, 
          # we allow the race to be finished. Maybe it wold be better: sum(is.elite) == sum(alive) 
          # instead of nbAlive == 1,
          # LESLIE:Removing this because now is ponitless because of the elite candidates previos
          # execution
          # || (current.task > elitistNewInstances && nbAlive == 1)))) {
      # We always stop when we have less configurations than required.
      if (nbAlive <= minSurvival && all_elite_instances_evaluated()) {
        # Stop race if we have less or equal than the minimum number of
        # configurations.
        break.msg <- paste0("number of alive configurations (", nbAlive,
                            ") <= minimum number of configurations (",
                            minSurvival, ")")
        break
      }
      # If we just did a test, check that we have enough budget to reach the
      # next test.
      if (maxExp && ( (current.task - 1) %% each.test) == 0
          && experimentsUsed + length(which.exe) * each.test > maxExp
          && all_elite_instances_evaluated()) {
        break.msg <- paste0("experiments for next test (",
                            experimentsUsed + length(which.exe) * each.test,
                            ") > max experiments (", maxExp, ")")
        break
      }
    }
    
    if (elitist) {
      if (scenario$elitistLimit != 0 && no.elimination >= scenario$elitistLimit
          && all_elite_instances_evaluated()) {
        break.msg <- paste0("tests without elimination (", no.elimination,
                            ") >= elitistLimit (", scenario$elitistLimit, ")")
        break
      }
##     This is not needed anymore... 
#      else if (current.task > initial.tests && nbAlive <= minSurvival) {
#        # We can stop the race ONLY when we pass the elite.safe
#        # this is because how we are recovering the data from
#        # previous runs (based on iteration).
#        break.msg <- paste0("number of alive configurations (", nbAlive,
#                            ") less or equal than minimum number (",
#                            minSurvival, ")")
#        break
#      }
    }
    
                                
    if (nrow(Results) < current.task) {
      Results <- rbind(Results, rep(NA, ncol(Results)))
      rownames(Results) <- race.instances[1:nrow(Results)]
      if (capping) {
        experimentsTime <- rbind(experimentsTime, rep(NA, ncol(experimentsTime)))
        rownames(experimentsTime) <- race.instances[1:nrow(experimentsTime)]
      }
    }

    start.time <- Sys.time()
  
    # Execution bounds calculation (capping only)
    final.bounds <- elite.bound <- NULL
    # Calculate bounds for executing if needed.
    which.elite.exe <- intersect(which.exe, which(is.elite > 0))
    irace.assert(setequal(which.elite.exe, which(is.elite & is.na(Results[current.task,]))))
    if (capping) {
      # Pre-execute elite configurations that are not yet executed in the current instance.
      if (length(which.elite.exe)) {
        # FIXME: This should go into its own function
        output <- race.wrapper (configurations = configurations[which.elite.exe, , drop = FALSE], 
                                instance.idx = race.instances[current.task],
                                bounds = rep(scenario$boundMax, length(which.elite.exe)),
                                # MANUEL: How does this work for target-evaluator?
                                # We are telling race.wrapper that only some elites are alive!
                                which.alive = which.elite.exe, 
                                which.exe = which.elite.exe,
                                parameters = parameters,
                                scenario = scenario)
        # Extract results
        vcost <- unlist(lapply(output, "[[", "cost"))
        irace.assert(length(vcost) == length(which.elite.exe))
        vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
        Results[current.task, which.elite.exe] <- vcost
        vtimes <- unlist(lapply(output, "[[", "time"))
        irace.assert(length(vtimes) == length(which.elite.exe))
        experimentsTime[current.task, which.elite.exe] <- vtimes
        experimentLog <- rbind(experimentLog,
                               cbind(race.instances[current.task],
                                     configurations[which.elite.exe, ".ID."],
                                     vtimes,
                                     scenario$boundMax))
        experimentsUsed <- experimentsUsed + length(which.elite.exe)
          
        # We remove elite configurations that are rejected given that
        # is not possible to calculate the bounds
        rejected <- is.infinite(Results[current.task, which.elite.exe])
        if (any(rejected)) {
          irace.note ("Immediately rejected configurations: ",
                      paste0(configurations[which.elite.exe[rejected], ".ID."],
                             collapse = ", ") , "\n")
          is.rejected[which.elite.exe] <- rejected
          is.elite [is.rejected] <- 0
          alive[which.elite.exe] <- !rejected
          if (!any(alive))
            irace.error("All configurations have been immediately rejected (all of them returned Inf) !")
          which.alive <- which(alive)
          nbAlive     <- length(which.alive)
          elite.safe <- update.elite.safe(Results, is.elite)
        }
        which.exe <- setdiff(which.exe, which.elite.exe)
        # FIXME: There is similar code above.
        if (length(which.exe) == 0L) {
          is.elite <- update.is.elite(is.elite, which.elite.exe)
          if (!quiet)
            race.print.task(".", Results[1:current.task, , drop = FALSE],
                            race.instances[current.task],
                            current.task, alive = alive,
                            configurations[best, ".ID."],
                            best = best, experimentsUsed, start.time,
                            bound = NA, capping)
          next
        }
      }
      
      all.bounds <- final.execution.bound(experimentsTime,
                                          elites = which(is.elite > 0),
                                          no.configurations, current.task,
                                          which.exe, scenario)
      final.bounds <- all.bounds$final.bounds
      elite.bound <- all.bounds$elite.bound
    } else {
      final.bounds <- rep(scenario$boundMax, no.configurations)
    }
    
    # Execute experiments
    output <- race.wrapper (configurations = configurations[which.alive, , drop = FALSE],
                            instance.idx = race.instances[current.task],
                            # FIXME: Why are we computing bounds for configurations that are dead?
                            # Also, do we use the final.bounds of which.alive or only the ones of which.exe?
                            bounds = final.bounds[which.alive],
                            which.alive = which.alive, which.exe = which.exe,
                            parameters = parameters, scenario = scenario)

    # Extract results
    vcost <- unlist(lapply(output, "[[", "cost"))
    # If the experiment was executed or target.evaluator exists
    # then the result is in the output.
    ## Currently, targetEvaluator always re-evaluates, which implies that the
    ## value may change. We do this to allow online normalization.
    which.exps <- if (is.null(scenario$targetEvaluator)) which.exe else which.alive
    irace.assert(length(vcost) == length(which.exps))
    # Set max execution bound to timed out executions which have execution
    # times smaller than boundMax and implement parX if required
    if (capping) {
      vcost <- applyPAR(vcost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
      if (scenario$boundAsTimeout)
        vcost[(vcost >= final.bounds[which.exps]) & (vcost < scenario$boundMax)] <- scenario$boundMax
    }
    Results[current.task, which.exps] <- vcost

    # Output is not indexed in the same way as configurations.
    which.exps <- which(which.alive %in% which.exe)
    irace.assert(length(which.exps) == length(which.exe))
    vtimes <- unlist(lapply(output[which.exps], "[[", "time"))
    irace.assert(length(vtimes) == length(which.exe))
    if (capping) {
      # Correct higher execution times.
      experimentsTime[current.task, which.exps] <- pmin(vtimes, final.bounds[which.exps])
    }
    experimentLog <- rbind(experimentLog,
                           cbind(race.instances[current.task],
                                 configurations[which.exe, ".ID."],
                                 vtimes, 
                                 if (is.null(final.bounds)) NA else final.bounds[which.exe]))

    experimentsUsed <- experimentsUsed + length(which.exe)
    # We update the elites that have been executed.
    is.elite <- update.is.elite(is.elite, which.elite.exe)

    ## Drop bad configurations.
    ## Infinite values denote immediate rejection of a configuration.
    rejected <- is.infinite(Results[current.task, which.exe])
    if (any(rejected)) {
      irace.note ("Immediately rejected configurations: ",
                  paste0(configurations[which.exe[rejected], ".ID."],
                         collapse = ", ") , "\n")
      is.rejected[which.exe] <- rejected
      is.elite[is.rejected] <- 0
      alive[is.rejected] <- FALSE
      if (!any(alive))
        irace.error("All configurations have been immediately rejected (all of them returned Inf) !")
      which.alive <- which(alive)
      nbAlive     <- length(which.alive)
      # FIXME: Should we stop  if (nbAlive <= minSurvival) ???
      elite.safe <- update.elite.safe(Results, is.elite)  
    }
    irace.assert(!anyNA(Results[1:current.task, alive, drop=FALSE]))
    irace.assert(!any(is.infinite(Results[, alive, drop=FALSE])))
    
    # Variables required to produce output of elimination test.
    cap.done     <- FALSE #if dominance elimination was performed
    test.done    <- FALSE #if statistical test elimination was performed
    cap.dropped  <- FALSE #if capping has drop any configuration
    test.dropped <- FALSE #if any candidates has been eliminated by testing
    cap.alive    <- test.alive <- alive
    
    ## Dominance elimination (Capping only).
    # The second condition can be false if we eliminated via immediate
    # rejection
    if (capping && sum(alive) > minSurvival) {
      irace.assert(!any(is.elite > 0) == (current.task >= elite.safe))
      cap.alive <- dom.elim(Results[1:current.task, , drop = FALSE],
                            # Get current elite configurations
                            elites = which(is.elite > 0),
                            alive, scenario, minSurvival)
      cap.dropped <- sum(alive) > sum(cap.alive)
      cap.done    <- TRUE
    }
    
    # We assume that first.test is a multiple of each.test.  In any
    # case, this will only do the first test after the first multiple
    # of each.test that is larger than first.test.
    if (current.task >= first.test && (current.task %% each.test) == 0
        && nbAlive > 1L) {
      irace.assert(sum(alive) == nbAlive)
      test.res <-
        switch(stat.test,
               friedman = aux_friedman(Results[1:current.task, ], alive, which.alive, conf.level),
               t.none = aux.ttest(Results[1:current.task, ], alive, which.alive, conf.level, adjust = "none"),
               t.holm = aux.ttest(Results[1:current.task, ], alive, which.alive, conf.level, adjust = "holm"),
               t.bonferroni = aux.ttest(Results[1:current.task, ], alive, which.alive, conf.level, adjust = "bonferroni"))
      
      best <- test.res$best
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
    irace.assert(!any(is.elite > 0) == (current.task >= elite.safe))
    if (!is.null(elite.data) && any(is.elite > 0)) {
      irace.assert (length(alive) == length(is.elite))
      alive <- alive | (is.elite > 0)
    }

    # It may happen that the capping and the test eliminate together all
    # configurations. In that case, we only trust the capping elimination.
    if (capping && !any(alive)) {
      if (scenario$debugLevel >= 2) {
        irace.warning("Elimination tests have eliminated all configurations, keeping the capping results.\n")
        irace.note("Alive according to capping:", which(cap.alive), "\n")
        irace.note("Alive according to test:", which(test.alive), "\n")
      }
      alive <- cap.alive
    }
    
    # Output the result of the elimination test
    res.symb <- if (cap.dropped && !test.dropped
                    && prev.sum.alive != sum(alive)) {
                  "c" # Removed just by capping
                  } else if (cap.dropped || test.dropped) {
                    if (prev.sum.alive != sum(alive)) "-" else "!"
                  } else if (cap.done || test.done) "=" else "x"
    
    # Rank alive configurations: order all configurations (eliminated or not)
    # LESLIE: we have to make the ranking outside: we can have configurations eliminated by capping
    # that are not eliminated by the test.
    # MANUEL: I don't understand the above comment.
    if (length(which.alive) == 1) {
      race.ranks <- 1
      best <- which.alive
    } else  {
      tmpResults <- Results[1:current.task, which.alive, drop = FALSE]
      irace.assert(!any(is.na(tmpResults)))
      if (stat.test == "friedman") {
        race.ranks <- colSums(t(apply(tmpResults, 1, rank)))
      } else {
        race.ranks <- colMeans(tmpResults)
      }
      # which.min returns only the first minimum.
      best <- which.alive[which.min(race.ranks)]
    }
    
    irace.assert(best == which.alive[order(race.ranks)][1])
    irace.assert(length(race.ranks) == length(which.alive))

    prev.alive  <- which.alive
    which.alive <- which(alive)
    # Remove the ranks of those that are not alive anymore
    race.ranks <- race.ranks[which.alive]
    irace.assert(length(race.ranks) == sum(alive))
    if (!quiet)
      race.print.task(res.symb, Results[1:current.task, , drop = FALSE],
                      race.instances[current.task],
                      current.task, alive = alive,
                      configurations[best, ".ID."],
                      best = best, experimentsUsed, start.time, 
                      bound = elite.bound, capping)
    
    if (elitist) {
      # Compute number of statistical tests without eliminations.
      irace.assert(!any(is.elite > 0) == (current.task >= elite.safe))
      if (!any(is.elite > 0)
          && current.task > first.test && (current.task %% each.test) == 0) {
        if (length(which.alive) == length(prev.alive)) {
          no.elimination <- no.elimination + 1
        } else {
          no.elimination <- 0
        }
      }
    } 
  }
  
  if (is.null(break.msg))
    break.msg <- paste0("all instances (", no.tasks, ") evaluated")

  # Adding this given that when ncandidates = minsurvival+1
  # and there one elite configuration that gets discarded in the new instances
  # execution the race is finished with no executions.
  # FIXME: we should handle this better, maybe allowing irace to handle no elite 
  # in irace()
  # MANUEL: Leslie, how can we reach this error in normal circumstances?
  # Can we handle this better?
  if (current.task == 1 && !any(is.elite > 0))
    irace.error ("Maximum number configurations immediately rejected reached!")
  
  # All instances that are not new in this race must have been evaluated by at
  # least one configuration.
  irace.assert(all_elite_instances_evaluated(),
               eval.after = { print(Results[,alive, drop=FALSE])})
  # If we stop the loop before we see all new instances, there may be new
  # instances that have not been executed by any configuration.
  Results <- Results[apply(!is.na(Results), 1, any), , drop = FALSE]
  
  race.ranks <- overall.ranks(Results[, alive, drop = FALSE], stat.test = stat.test)
  best <- which.alive[which.min(race.ranks)]

  if (!quiet)
    race.print.footer(bestconf = configurations[best, , drop = FALSE],
                      # FIXME: This is the mean of the best, but perhaps it
                      # should be the sum of ranks in the case of test ==
                      # friedman?
                      mean.best = mean(Results[, best]),
                      break.msg = break.msg, debug.level = scenario$debugLevel, 
                      capping = capping)

  nbAlive <- sum(alive)
  configurations$.ALIVE. <- as.logical(alive)
  # Assign the proper ranks in the configurations data.frame.
  configurations$.RANK. <- Inf
  configurations[which.alive, ".RANK."] <- race.ranks
  # Now we can sort the data.frame by the rank.
  configurations <- configurations[order(as.numeric(configurations[, ".RANK."])), ]
  # Consistency check.
  irace.assert (all(configurations[1:nbAlive, ".ALIVE."]))
  if (nbAlive < nrow(configurations))
    irace.assert(!any(configurations[(nbAlive + 1):nrow(configurations),
                                     ".ALIVE."]))

  if (scenario$debugLevel >= 3) {
    irace.note ("Memory used in race():\n")
    irace.print.memUsed()
  }

  # nrow(Results) may be smaller, equal or larger than current.task.
  irace.assert(nrow(experimentLog) == experimentsUsed)

  return(list(experiments = Results,
              experimentLog = experimentLog,
              experimentsUsed = experimentsUsed,
              nbAlive = nbAlive,
              configurations = configurations,
              rejectedIDs = configurations[is.rejected, ".ID."]))
}
