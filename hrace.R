###############################################################################
# ??? What is this file for?
###############################################################################

## ??? What is this loaded for?
source("util.R")

# FIXME: This is a parameter that could be configured, it does not need to be linear.
compute.mu <- function (iteration)
{
  MU.BASE <- 6
  return(MU.BASE + iteration)
}

hrace.wrapper <-
  function(maxAllotedExperiments,
           parameter.type.list,
           parameter.boundary.list,
           parameter.param.list,
           experiment.name,
           extra.description,
           executable,
           instance.dir,
           test.instance.dir,
           parameter.subsidiary.list, 
           parameter.name.list)
{
  parameter.names.vector <- names(parameter.boundary.list)
  num.parameters <- length(parameter.names.vector)

  ## The number of iterations of F-Race is set dynamically according
  ## to the parameter space dimension.
  num.iterations <- 2 + round( log(num.parameters) / log(2) )

  stats <- NULL

  iteration <- 0

  # mu * number of candidates = budget for one F-Race execution.
  # (mu is the candidate-evaluation trade-off factor, with value 6,  7,  ...
  # FIXME: This is a parameter that could be configured.
  mu <- compute.mu (iteration)

  used.experiments <- 0
  # max number of experiments in each iteration
  maxExperiments <- floor( (maxAllotedExperiments - used.experiments) / (num.iterations - iteration) )
  # the number of candidates
  num.candidates <- floor(maxExperiments / mu)

  cat ("number of parameters:", num.parameters,
       "\nnumber of iterations:", num.iterations,
       "\nmax quota of experiments:", maxAllotedExperiments,
       "\nnumber of candidates of", (iteration + 1),
       "iteration:", num.candidates, "\n")

  candidate.configurations.dataframe <- generate.configurations.uniform (num.candidates, parameter.names.vector, parameter.type.list,
                                                                         parameter.boundary.list,  parameter.subsidiary.list)
  
  # This is a hash table to store the prob vectors of each catogarical variable.
  prob.vector.type.c.list <- init.prob.list(parameter.type.list,
                                            parameter.boundary.list,
                                            candidate.configurations.dataframe)

  # FIXME: These two could be parameters.
  # We stop the race when the number of candidates is less than this number.
  survival.quota <- 2 + round(log(num.parameters) / log(2))
  # The number of candidates we use for generating new candidates.
  # Right now is the same, but it could be different.
  elite.quota <- survival.quota
  
  while (iteration < num.iterations) {
    
    cat("maxExperiments:",  maxExperiments,  "\n")
    if (debug.level >= 3) {
      print (candidate.configurations.dataframe)
    }
    result <- race(.tune.race.wrapper, maxExp = maxExperiments,
                   first.test = 5, # Minimum number of test before performing Friedman-test.
                   stop.min.cand = survival.quota,
                   candidates = candidate.configurations.dataframe,
                   maxIns = maxInstance, 
                   experiment.name = experiment.name,
                   extra.description = extra.description,
                   # Parameters for the race-wrapper:
                   executable = executable,
                   instance.dir = instance.dir,
                   parameter.name.list = parameter.name.list)
    # end of one iteration F-race
    
    cat("Experiments in this iteration:",  result$no.experiments,  "\n")
    
    used.experiments <- used.experiments + result$no.experiments 
    cat("used.experiments:",  used.experiments,  "\n")
    
    iteration <- iteration + 1
    if (iteration == num.iterations) {
      break
    } else if (iteration == num.iterations - 1) {
      # We do not want to stop earlier in the last iteration.
      survival.quota <- 1
    }

    mu <- compute.mu (iteration)
    maxExperiments <- floor( (maxAllotedExperiments - used.experiments) / (num.iterations - iteration) )
    num.candidates <- floor(maxExperiments / mu)
    cat("iteration =",  iteration,  "mu =",  mu,  "maxExperiments =",  maxExperiments,  "num.candidates =",  num.candidates,  "\n")
  
    # writing stat results
    stats <- rbind(stats,
                   c(num.race = iteration, no.canidates = nrow(candidate.configurations.dataframe),
                     max.exp = maxExperiments, max.ins = maxInstance,
                     used.exp = result$no.experiments,
                     used.ins = result$no.tasks,
                     no.alive = result$no.alive))
    
    # compute the ranks of survivors for the update later
    alive.configurations.index <- which(result$alive == TRUE)
    alive.configurations.dataframe <- candidate.configurations.dataframe[result$alive, ]
    maximum.elite.configurations <- min(result$no.alive, elite.quota)
    
    if (maximum.elite.configurations > 1) {
      # Rank the ones alive.
      r <- t(apply(result$results[1:result$no.tasks, alive.configurations.index],  1,  rank))
      # ??? Calculate the mean of the ranks.
      alive.configurations.ranks <- rank(apply(r,  2,  mean))
      alive.configurations.ranked.index <- alive.configurations.index[I(order(alive.configurations.ranks))]
    } else {
      alive.configurations.ranked.index <- alive.configurations.index
    }
    elite.configurations.ranked.index <- alive.configurations.ranked.index[1:maximum.elite.configurations]
    
    # compute probability for each elite configurations
    K <- length(elite.configurations.ranked.index)
    sumK <- K * (K + 1) / 2
    ## FIXME: this should be equivalent to
    ## prob.elite.configuration <- (K - (1:K) + 1) / sumK
    prob.elite.configuration <- NULL
    for (i in 1:K){
      prob.elite.configuration <- c(prob.elite.configuration, (K - i + 1) / sumK)
    }
    
    elite.configurations.dataframe.aux2 <- NULL
    
    alive.configurations.dataframe <- data.frame(alive.configurations.dataframe)
    
    print(elite.configurations.ranked.index)

    for (i in 1 : length(elite.configurations.ranked.index)){
      # aux2 contains all the elite configurations for update
      elite.configurations.dataframe.aux2 <- rbind(elite.configurations.dataframe.aux2, 
      data.frame(cbind(config.ranks=i, config.prob=prob.elite.configuration[i], alive.configurations.dataframe[row.names(alive.configurations.dataframe)== elite.configurations.ranked.index[i], ]), row.names=i))
    }
    
    print(elite.configurations.dataframe.aux2)
    
    # to update and generate new candidates for the next iteration
    configurations.dataframe.aux3 <- generate.configurations.normal(num.candidates, iteration,
                                                                    num.iterations,
                                                                    elite.configurations.dataframe.aux2,
                                                                    parameter.names.vector,
                                                                    parameter.type.list,
                                                                    parameter.boundary.list,
                                                                    parameter.subsidiary.list,
                                                                    prob.vector.type.c.list)
    
    candidate.configurations.dataframe <- configurations.dataframe.aux3
    
    if (nrow(unique(candidate.configurations.dataframe)) == 1) break;
  }
  
  stats <- data.frame(stats)
  save(stats, file=paste("stats", ".Rdata", sep=""))
  #eval(result=result,  executable=executable,  test.instance.dir=test.instance.dir)
}

