testCandidates <- function(candidates, tunerConfig, parameters)
{
  testInstances <- tunerConfig$testInstances
  extra.params  <- tunerConfig$testInstances.extra.params
    
  parameters.names <- names(parameters$names)
  values <- removeCandidatesMetaData(candidates)
  values <- values[parameters.names]
  switches <- parameters$switches[parameters.names]
  
  # Create experiment list
  experiments <- list()
  ntest <- 1
  for (i in 1:nrow(candidates)){
    for (j in 1:length(testInstances)){
      experiments[[ntest]] <- list(id = paste(candidates[i,".ID."], "-", j, sep=""),
                                   candidate = values[i,],
                                   instance = testInstances[j],
                                   extra.params = extra.params[j], switches = switches, 
                                   cand.id = candidates[i,".ID."])
      ntest <- ntest + 1
    }
  }

  startParallel(tunerConfig)
  on.exit(stopParallel())

  hook.output <- execute.experiments (experiments, nrow(candidates), tunerConfig)

  testResults <- matrix(NA, ncol=nrow(candidates), nrow=length(testInstances))
  colnames(testResults) <- candidates$.ID.
  rownames(testResults) <- testInstances
  for (i in seq_along(experiments)){
    testResults[rownames(testResults) == experiments[[i]]$instance,
                colnames(testResults) == experiments[[i]]$cand.id] <- hook.output[[i]][1]
  }
  
  return(testResults)
}
