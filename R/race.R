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


## Executes a list of configurations in a particular instance
## configurations: description having the id of the configuration
## instance.idx: index of the instance that can be found in scenario$instances
## which.alive: index of the configurations that are still alive
# LESLIE: should we replace data for the direct things? using enviroments would be better
race.wrapper <- function(configurations, instance.idx, which.alive, which.exe, parameters, scenario)
{
  debugLevel <- scenario$debugLevel
  
  irace.assert (isTRUE(parameters$nbParameters > 0))
  irace.assert (length(parameters$names) == parameters$nbParameters)
  
  seed <- .irace$instancesList[instance.idx, "seed"]
  id.instance  <- .irace$instancesList[instance.idx, "instance"]
  instance <- scenario$instances[[id.instance]]
  extra.params <- NULL
  if (!is.null (scenario$instances.extra.params)
      && !is.na (scenario$instances.extra.params[[id.instance]]))
    extra.params <- scenario$instances.extra.params[[id.instance]]

  values <- removeConfigurationsMetaData(configurations)
  values <- values[, parameters$names, drop = FALSE]
  switches <- parameters$switches[parameters$names]
  
  # Experiment list to execute 
  experiments <- list()
  ntest <- 1
  for (i in which.alive) {
    experiments[[ntest]] <- list (id.configuration = configurations[i, ".ID."],
                                  id.instance  = id.instance,
                                  seed = seed,
                                  configuration = values[i, , drop = FALSE],
                                  instance = instance, 
                                  extra.params = extra.params,
                                  switches = switches)
    ntest <- ntest + 1
  }

  target.output <- rep(list(NA), length(experiments))
  # Execute commands
  if (length(which.exe) > 0) {
    # which.exe values are within 1:nbConfigurations, whereas experiments
    # indices are within 1:length(which.alive). The following line converts
    # from one to the other.
    which.exps <- which(which.alive %in% which.exe)
    irace.assert(length(which.exps) == length(which.exe))
    target.output[which.exps] <- execute.experiments (experiments[which.exps], scenario)
  }
  irace.assert(!any(sapply(target.output, is.null)))

  # target.evaluator may be NULL. If so, target.output must
  # contain the right output already.
  if (!is.null(.irace$target.evaluator))
    target.output <- execute.evaluator (experiments, scenario, target.output,
                                        configurations[which.alive, ".ID."])
  return(target.output) 
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

aux.friedman <- function(results, alive, which.alive, no.alive, conf.level)
{
  irace.assert(no.alive == length(which.alive))
  
  if (no.alive == 2) {
    best <- NULL
    ranks <- NULL
    # If only 2 configurations are left, switch to Wilcoxon
    V1   <- results[, which.alive[1]]
    V2   <- results[, which.alive[2]]
    PVAL <- wilcox.test(V1, V2, paired = TRUE, exact = FALSE)$p.value
    if (!is.nan(PVAL) && !is.na(PVAL) && PVAL < 1 - conf.level) {
      dropped.any <- TRUE
      if (median(V1 - V2) < 0) {
        best <- which.alive[1]
        ranks <- c(1,2)
        alive[which.alive[2]] <- FALSE
      } else {
        best <- which.alive[2]
        ranks <- c(2,1)
        alive[which.alive[1]] <- FALSE
      }
    } else {
      dropped.any <- FALSE
      if (median(V1 - V2) < 0) {
        best <- which.alive[1]
        ranks <- c(1,2)
      } else {
        best <- which.alive[2]
        ranks <- c(2,1)
      }
    }
    irace.assert(which.alive[which.min(ranks)] == best)
    return(list(best = best, ranks = ranks, alive = alive, dropped.any = dropped.any, p.value = PVAL))
  } else {
    # If more then 2 configurations are left, use Friedman
    # LESLIE: is there a better way to extract info??
    # MANUEL: Which info?
    return (aux2.friedman(results, which.alive, alive, conf.level = conf.level))
  }
}

aux.ttest <- function(results, no.tasks.sofar, alive, which.alive, no.alive, conf.level,
                      adjust = c("none","bonferroni","holm"))
{
  irace.assert(no.alive == length(which.alive))
  
  adjust <- match.arg(adjust)
  mean.all <- c()
  for (j in 1:ncol(results)) {
    # FIXME: why not just mean() ?
    mean.all[j] <- sum(results[,j]) / no.tasks.sofar
  }
  # FIXME: which.min?
  best <- match(min(mean.all[alive]), mean.all)
  ranks <- mean.all[alive]

  # FIXME: Use matrix()
  PJ <- array(0, dim = c(2,0))
  Vb <- results[, best]
  for (j in which.alive) {
    Vj <- results[, j]
    #cat("Vb:", Vb, "\n")
    #cat("Vj:", Vj, "\n")
    # t.test may fail if the data in each group is almost
    # constant. Hence, we surround the call in a try() and we
    # initialize p with 1 if the means are equal or zero if they are
    # different.
    # FIXME: mean(Vb) doesn't seem to change either.
    PVAL <- as.integer(isTRUE(all.equal(mean(Vb), mean(Vj))))
    try(PVAL <- t.test(Vb, Vj, paired = TRUE)$p.value)
    if (is.nan(PVAL) | is.na(PVAL)) {
      # This should not happen, but it happens sometimes if all values are
      # equal.  We assume that we cannot reject anything.
      PVAL <- 1
    }
    # FIXME: Is this equivalent to cbind or rbind?
    PJ <- array(c(PJ, j, PVAL), dim = dim(PJ) + c(0,1))
  }
  PJ[2,] <- p.adjust(PJ[2,], method = adjust)
  dropped.any <- FALSE
  for (j in 1:ncol(PJ)) {
    if (PJ[2,j] < 1 - conf.level) {
      alive[PJ[1, j]] <- FALSE
      dropped.any <- TRUE
    }
  }
  irace.assert(which.alive[which.min(ranks)] == best)
  return(list(best = best, ranks = ranks, alive = alive,
              dropped.any = dropped.any, p.value = min(PJ[2,])))
}

# FIXME: This can be simplified a lot more. Some arguments already appear in
# scenario.
race <- function(maxExp = 0,
                 minSurvival = 1,
                 elite.data = NULL,
                 configurations,
                 parameters,
                 scenario,
                 elitistNewInstances)
{
  # FIXME: Remove argument checking. This must have been done by the caller.
  stat.test <- scenario$testType
  conf.level <- scenario$confidence
  first.test <- scenario$firstTest
  each.test <- scenario$eachTest
  elitist <- scenario$elitist
  
  # Check argument: maxExp
  if (!missing(maxExp) &&
      (!is.numeric(maxExp) ||
       length(maxExp)!=1 ||
       !is.finite(maxExp)))
    stop("maxExp must be an single number")
  maxExp <- ifelse(maxExp>0,maxExp,0)
  maxExp <- floor(maxExp)

  # Check argument: conf.level
  if (!missing(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level)!=1 ||
       !is.finite(conf.level) || conf.level<0 || conf.level>1)) 
    stop("conf.level must be a single number between 0 and 1")

  # Create the instance list according to the algorithm selected
  # if next.instance == 1 then this is the first iteration.
  max.instances <- nrow(.irace$instancesList)
  if (elitist && .irace$next.instance != 1) {
    new.instances <- NULL
    last.new <- .irace$next.instance - 1 + elitistNewInstances
    # Do we need to add new instances?
    if (elitistNewInstances > 0) {
      if (last.new > max.instances) {
        # This may happen if the scenario is deterministic and we would need
        # more instances than what we have.
        irace.assert(scenario$deterministic)
        if (.irace$next.instance <= max.instances) {
          # Add all instances that we have not seen yet as new ones.
          last.new <- max.instances
          new.instances <- .irace$next.instance : last.new
        } # else new.instances remains NULL and last.new remains > number of instances.
        # We need to update this because the value is used below and now there
        # may be fewer than expected, even zero.
        elitistNewInstances <- length(new.instances)
      } else {
        new.instances <- .irace$next.instance : last.new
      }
    }
    future.instances <- NULL
    if ((last.new + 1) <= max.instances) {
      future.instances <- (last.new + 1) : max.instances
    }
    # new.instances + past.instances + future.instances
    race.instances <- c(new.instances, sample.int(.irace$next.instance - 1),
                        future.instances)
  } else if (.irace$next.instance <= max.instances) {
    race.instances <- .irace$next.instance : max.instances
  } else {
    # This may happen if the scenario is deterministic and we would need
    # more instances than what we have.
    irace.assert(scenario$deterministic)
    race.instances <- 1 : max.instances
  }
  no.tasks      <- length(race.instances)
  no.configurations <- nrow(configurations)

  interactive <- TRUE

  if (maxExp && no.configurations > maxExp)
    irace.error("Max number of experiments is smaller than number of configurations")

  if (no.configurations <= minSurvival) {
    irace.error("Not enough configurations (", no.configurations,
                ") for a race (minSurvival=", minSurvival, ")")
  }
  
  # Initialize some variables...
  if (is.null(elite.data)) {
    Results <- matrix(nrow = 0, ncol = no.configurations,
                      dimnames = list(NULL, as.character(configurations[, ".ID."])))
  } else {
    Results <- matrix(NA, 
                      nrow = elitistNewInstances + nrow(elite.data), 
                      ncol = no.configurations, 
                      dimnames = list(as.character(race.instances[
                        1:(elitistNewInstances + nrow(elite.data))]),
                        as.character(configurations[, ".ID."])) )
    Results[rownames(elite.data), colnames(elite.data)] <- elite.data
    is.elite <- colSums(!is.na(Results))
  }

  # Elitist irace needed info
  if (elitist) {
    if (is.null(elite.data)) {
      elite.safe <- first.test
    } else {
      elite.safe <- elitistNewInstances + nrow(elite.data)
    }
  }


  experimentLog <- matrix(nrow = 0, ncol = 3,
                          dimnames = list(NULL, c("instance", "configuration", "time")))
  alive <- rep(TRUE, no.configurations)
  best <- 0
  race.ranks <- c()
  no.experiments.sofar <- 0

  if (interactive)
    cat(sep = "", "  Markers:
     x No test is performed.
     - The test is performed and some configurations are discarded.
     = The test is performed but no configuration is discarded.
     ! The test is performed and configurations could be discarded but elite configurations are preserved.
                                                                   
+-+-----------+-----------+-----------+---------------+-----------+--------+-----+----+------+
| |   Instance|      Alive|       Best|      Mean best| Exp so far|  W time|  rho|KenW|  Qvar|
+-+-----------+-----------+-----------+---------------+-----------+--------+-----+----+------+
")

  no.elimination <- 0 # number of tasks without elimination
  # Start main loop
  break.msg <- paste0("all instances (", no.tasks, ") evaluated")
  for (current.task in seq_len (no.tasks)) {
    which.alive <- which(alive)
    nbAlive     <- length(which.alive)
    which.exe   <- which.alive

    if (elitist) {  
      # Filter configurations that do not need to be executed (elites).
      # This is valid only for previous iteration instances.
      if (!is.null(elite.data) && elitistNewInstances < current.task
          && current.task <= elite.safe ) {
        # Execute everything that is alive and not yet executed.
        not.executed <- is.na(Results[current.task, ])
        irace.assert(length(not.executed) == length(alive))
        which.exe <- which(alive & not.executed)
        # Remove one elite count from every configuration already executed.
        is.elite <- is.elite - (!not.executed)
        irace.assert (all(is.elite >= 0))
      }
    }

    if (current.task > first.test) {
    # We always stop when we have less configurations than required.
      if (nbAlive <= minSurvival) {
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
          && no.experiments.sofar + length(which.exe) * each.test > maxExp) {
        break.msg <- paste0("experiments for next test (",
                            no.experiments.sofar + length(which.exe) * each.test,
                            ") > max experiments (", maxExp, ")")
        break
      }
    }

    if (elitist) {
      if (scenario$elitistLimit != 0 && current.task > elite.safe
          && no.elimination >= scenario$elitistLimit) {
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

    if (nbAlive == 1) {
      break.msg <- "only one alive configuration"
      break
    }

    start.time <- Sys.time()
    # Execute experiments
    output <- race.wrapper (configurations = configurations, instance.idx = race.instances[current.task],
                            which.alive = which.alive, which.exe = which.exe,
                            parameters = parameters, scenario = scenario)
                            
    if (nrow(Results) < current.task) {
      Results <- rbind(Results, rep(NA, ncol(Results)))
      rownames(Results) <- race.instances[1:nrow(Results)]
    }
    
    # Extract results
    for (i in seq_along(which.alive)) {
      current.configuration <- which.alive[i]
      # If the experiment was executed or target.evaluator exists
      # then the result is in the output.
      if (current.configuration %in% which.exe
          || !is.null(scenario$targetEvaluator)) {
        Results[current.task, current.configuration] <- output[[i]]$cost
        # LESLIE: For the elitist version in the case target.evaluator is used,
        # the value of the elites is changing from iteration to iteration maybe
        # we could add a result info, even though the information would be
        # repeated.
        
        # Log time if needed
        output.time <- output[[i]]$time
        experimentLog <- rbind(experimentLog,
                               c(race.instances[current.task],
                                 configurations[current.configuration, ".ID."],
                                 output.time))
      }
    }

    no.experiments.sofar <- no.experiments.sofar + length(which.exe)

    ## Drop bad configurations.
    # We assume that first.test is a multiple of each.test.  In any
    # case, this will only do the first test after the first multiple
    # of each.test that is larger than first.test.
    if (current.task >= first.test && (current.task %% each.test) == 0
        && length(which.alive) > 1) {
      test.res <-
        switch(stat.test,
               friedman = aux.friedman(Results[1:current.task, ], alive, which.alive, nbAlive, conf.level),
               t.none = aux.ttest(Results[1:current.task, ], current.task, alive, which.alive, nbAlive, conf.level, adjust = "none"),
               t.holm = aux.ttest(Results[1:current.task, ], current.task, alive, which.alive, nbAlive, conf.level, adjust = "holm"),
               t.bonferroni = aux.ttest(Results[1:current.task, ], current.task, alive, which.alive, nbAlive, conf.level, adjust = "bonferroni"))

      best <- test.res$best
      race.ranks <- test.res$ranks
      alive <- test.res$alive
      aux.alive <- sum(alive)

      # The elite configurations can be removed only when they have no more
      # previously-executed instances.
      if (!is.null(elite.data) && current.task <= elite.safe) {
        irace.assert (length(alive) == length(is.elite))
        irace.assert (all(is.elite >= 0))
        alive <- alive | (is.elite > 0)
      }

      if (interactive) {
        if (test.res$dropped.any) {
          if (aux.alive != sum(alive)) cat("|!|") else cat("|-|") 
        } else { 
          cat("|=|") 
        }
      }
      
    } else {
      if (interactive) cat("|x|")
      # LESLIE : Not sure this is needed, but just in case.
      if (length(which.alive) == 1) {
        race.ranks <- 1
        best <- which.alive
      } else if (current.task == 1)  {
        # FIXME: Shouldn't these be ranks when stat.test == "friedman" ?
        race.ranks <- Results[1,]
        best <- which.min(race.ranks)
      } else  {
        tmpResults <- Results[1:current.task, which.alive]
        irace.assert(!any(is.na(tmpResults)))
        if (stat.test == "friedman") {
          race.ranks <- colSums(t(apply(tmpResults, 1, rank)))
        } else {
          race.ranks <- colMeans(tmpResults)
        }
        best <- which.alive[which.min(race.ranks)]
      }
    }
    irace.assert(best == which.alive[order(race.ranks)][1])
    irace.assert(length(race.ranks) == length(which.alive))
    # Remove the ranks of those that are not alive anymore
    race.ranks <- race.ranks[which.alive %in% which(alive)]
    irace.assert(length(race.ranks) == sum(alive))
    # FIXME: This is the mean of the best, but perhaps it should be
    # the sum of ranks in the case of test == friedman?
    mean.best <- mean(Results[1:current.task, best])

    if (interactive) {
      time.diff <- difftime(Sys.time(), start.time, units = "secs")
      cat(sprintf("%11d|%11d|%11d|%#15.10g|%11d|%s",
                  race.instances[current.task],
                  sum(alive),
                  configurations[best, ".ID."],
                  mean.best,
                  no.experiments.sofar,
                  # FIXME: Maybe better and faster if we only print seconds?
                  format(.POSIXct(time.diff, tz="GMT"), "%H:%M:%S")))
      if (current.task > 1 && sum(alive) > 1) {
        conc <- concordance(Results[1:current.task, alive])
        qvar <- dataVariance(Results[1:current.task, alive])
        cat(sprintf("|%+#4.2f|%.2f|%.4f|\n", conc$spearman.rho, conc$kendall.w,
                    qvar))
      } else {
        cat("|   NA|  NA|    NA|\n")
      }
    }
    prev.alive  <- which.alive
    which.alive <- which(alive)

    if (elitist) {
      # Compute number of statistical tests without eliminations.
      # FIXME: Remove elite.safe check?
      if (current.task >= elite.safe
          && current.task > first.test && (current.task %% each.test) == 0) {
        if (length(which.alive) == length(prev.alive)) {
          no.elimination <- no.elimination + 1
        } else {
          no.elimination <- 0
        }
      }
    } 
  }

  description.best <- configurations[best, , drop = FALSE]
  
  if (interactive) {
    cat(sep = "",
        "+-+-----------+-----------+-----------+---------------+-----------+--------+-----+----+------+\n",
        if (scenario$debugLevel >= 1) paste0("# Stopped because ", break.msg, "\n"),
        sprintf("Best configuration: %11d", description.best[1, ".ID."]),
        sprintf("    mean value: %#15.10g", mean.best), "\n",
        "Description of the best configuration:\n")
    print(description.best)
    cat("\n")
  }

  nbAlive <- sum(alive)
  configurations$.ALIVE. <- as.logical(alive)
  # Assign the proper ranks in the configurations data.frame.
  configurations$.RANK. <- Inf
  configurations[which(alive), ".RANK."] <- race.ranks
  # Now we can sort the data.frame by the rank.
  configurations <- configurations[order(as.numeric(configurations[, ".RANK."])), ]
  # Consistency check.
  irace.assert (all(configurations[1:nbAlive, ".ALIVE."]))
  if (nbAlive < nrow(configurations))
    irace.assert(!any(configurations[(nbAlive + 1):nrow(configurations), ".ALIVE."]))

  if (scenario$debugLevel >= 3) {
    irace.note ("Memory used in race():\n")
    irace.print.memUsed()
  }

  # nrow(Results) may be smaller, equal or larger than current.task.
  return(list(experiments = Results,
              experimentLog = experimentLog,
              # FIXME: Rename this to experimentsUsed for consistency
              experimentsUsed = no.experiments.sofar,
              nbAlive = nbAlive,
              configurations = configurations))
}
