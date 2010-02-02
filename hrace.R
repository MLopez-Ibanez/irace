###############################################################################
# ??? What is this file for?
###############################################################################

## ??? What is this loaded for?
source("../util.R")

hrace.wrapper <-
  function(maxAllotedExperiments, parameter.type.list, parameter.boundary.list,
           parameter.param.list,
           experiment.name, extra.description, executable,
           instance.dir, test.instance.dir, parameter.subsidiary.list, 
           parameter.name.list)
{
  parameter.names.vector <- names(parameter.boundary.list)
  #print(parameter.names.vector)
  # number of parameters
  d <- length(parameter.names.vector)

  # the number of iterations is set dynamically according to the parameter space dimension
  num.iterations = 2 + round( log(d) / log(2) )

  usedExperiments <- 0
  stats <- NULL
  step <- 0

  # ??? Is this a parameter?
  # mu is the candidate-evaluation trade-off factor,  with value 6,  7,  ...
  mu <- 6 + step
  
  # max number of experiments in each iteration
  maxExperiments <- floor( (maxAllotedExperiments - usedExperiments) / (num.iterations - step) )
  # the number of candidates
  N <- floor(maxExperiments / mu)
  
  cat ("number of parameters:", d,
       "\nnumber of iterations:", num.iterations,
       "\nmax quota of experiments:", maxAllotedExperiments,
       "\nnumber of candidates of", (step + 1),
       "iteration:", N, "\n")

  candidate.configurations.dataframe <- generate.configurations.uniform (N, parameter.names.vector, parameter.type.list,
                                                                         parameter.boundary.list,  parameter.subsidiary.list)
  
  store.prob.vector(parameter.type.list,  parameter.boundary.list, candidate.configurations.dataframe)
  
  # the minimal candidate number before we stop race. 
  survival.quota = 2 + round( log(d) / log(2) )
  elite.quota = survival.quota
  
  #the maximum number of experiments for the next one complete F-race
  #maxExperiments <- floor(min(maxAllotedExperiments/num.iterations,  maxAllotedExperiments-usedExperiments))
  
  while( step < num.iterations ) {
    
    cat("maxExperiments:",  maxExperiments,  "\n")
    if (debug.level >= 3) {
      print (candidate.configurations.dataframe)
    }
    ## ??? Which parameters are for F-Race and which ones are for the wrappers?
    result <- race("../race-wrapper.R", maxExp=maxExperiments,  first.test=5,  stop.min.cand=survival.quota, candidates=candidate.configurations.dataframe, maxIns=maxInstance, 
                   experiment.name=experiment.name, extra.description=extra.description, executable=executable, instance.dir=instance.dir, parameter.name.list=parameter.name.list)
    # end of one iteration F-race
    
    cat("Experiments in this iteration:",  result$no.experiments,  "\n")
    
    usedExperiments <- usedExperiments+result$no.experiments 
    cat("usedExperiments:",  usedExperiments,  "\n")
    
    step <- step+1
    if (step == num.iterations) {
    	break
    } else if (step == num.iterations-1) {
    	survival.quota = 1
    }
    mu = 6 + step
    maxExperiments <- floor( (maxAllotedExperiments - usedExperiments) / (num.iterations - step) )
	  N=floor(maxExperiments / mu)
	  
	  cat("step =",  step,  "mu =",  mu,  "maxExperiments =",  maxExperiments,  "N =",  N,  "\n")
  
    alive.configurations.index <- which(result$alive==TRUE)
    alive.configurations.dataframe.aux1 <- candidate.configurations.dataframe[result$alive, ]
    
    maximum.elite.configurations <- min(result$no.alive, elite.quota)
    
    # writing stat results
    stats <- rbind(stats, c(num.race=step, no.canidates=nrow(candidate.configurations.dataframe), max.exp=maxExperiments, 
    max.ins=maxInstance, used.exp=result$no.experiments, used.ins=result$no.tasks, no.alive=result$no.alive))
    
    # if the next iteration will be less than 6 rounds of evaluations,  stop.
    #if( (usedExperiments + 6*N) > maxAllotedExperiments )  break;
    
    # compute the ranks of survivors for the update later
    if(maximum.elite.configurations>1){
      r <- t(apply(result$results[1:result$no.tasks, alive.configurations.index],  1,  rank))
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
    
    alive.configurations.dataframe.aux1 <- data.frame(alive.configurations.dataframe.aux1)
    
    print(elite.configurations.ranked.index)

    for (i in 1 : length(elite.configurations.ranked.index)){
      # aux2 contains all the elite configurations for update
      elite.configurations.dataframe.aux2 <- rbind(elite.configurations.dataframe.aux2, 
      data.frame(cbind(config.ranks=i, config.prob=prob.elite.configuration[i], alive.configurations.dataframe.aux1[row.names(alive.configurations.dataframe.aux1)== elite.configurations.ranked.index[i], ]), row.names=i))
    }
    
    print(elite.configurations.dataframe.aux2)
    
    # to update and generate new candidates for the next iteration
    configurations.dataframe.aux3 <- generate.configurations.normal(N, step,
                                                                    num.iterations,
                                                                    elite.configurations.dataframe.aux2,
                                                                    parameter.names.vector,
                                                                    parameter.type.list,
                                                                    parameter.boundary.list,
                                                                    parameter.subsidiary.list)
    
    candidate.configurations.dataframe <- configurations.dataframe.aux3
    
    if (nrow(unique(candidate.configurations.dataframe)) == 1) break;
  }
  
  stats <- data.frame(stats)
  save(stats, file=paste("stats", ".Rdata", sep=""))
  #eval(result=result,  executable=executable,  test.instance.dir=test.instance.dir)
}

