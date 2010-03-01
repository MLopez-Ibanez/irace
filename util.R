
checkBoundary<-function(min,max,vecValues){


if(min(vecValues) < min){
minVal<-min
} else {
minVal<-min(vecValues)
}

if(max(vecValues) > max){
maxVal<-max
} else {
maxVal<-max(vecValues)
}

tmp<-c(minVal,maxVal)

return (tmp)

}


marginProb<-function(n,MinMaxVal){
 MinVal<-MinMaxVal[1]
 MaxVal<-MinMaxVal[2]
 
 temp<-runif(n,min=MinVal,max=MaxVal)

return (temp)

}



runiform<-function(n,MidVal,MinMaxVal) {

return(runif(n,min(MinMaxVal),max(MinMaxVal)))
  
}

# return n samples with a normal distribution within some boundary
rnormalfull<-function(n,MidVal,MinMaxVal,MinMaxBound){
 MinVal<-min(MinMaxVal)
 MaxVal<-max(MinMaxVal)
 
 range<-MaxVal-MinVal
 
 StdDev=range/2
 
 numSamp<-0
 vecVal<-NULL
 
 minBound = min(MinMaxBound)
 maxBound = max(MinMaxBound)
 
 while(numSamp<n){
  #print(MidVal)
  #print(StdDev)
  temp<-rnorm(1,MidVal,StdDev)
  #print(temp)
  count = 0
  
  while (temp < minBound | temp > maxBound) {
  # if the sample out of boundary, resample. 
  # if sample more than maximum times, take the border value
  	if (count < max.sample.itr) {
  		count = count + 1
	  	next;
  	} else {
  		if (temp < minBound) {
  			temp = minBound
  		} else {
  			temp = maxBound
  		}
  		
  	}
  }
  
  vecVal<-c(vecVal,temp)
  
  numSamp<-numSamp+1
 }
 
 return (vecVal)

}


printInfo<-function(paramName,rank,numSamples,pram,minmax,samples){

cat("rank",rank,"\n",sep=" ")
cat("samples",numSamples,"\n",sep=" ")
cat(paramName,pram,minmax,"\n",sep=" ")
cat("samples",samples,"\n",sep=" ")

}


# Compute the standard deviation for the gaussian distribution for
# sampling the next iteration candidates. And returns the range of
# values c(mean - sd, mean + sd).
compute.gaussian.range <- function(num.candidates, iteration, num.parameters, best.parameter.value, parameter.boundary)
{
  if (debug.level >= 3) {
    cat(num.candidates, iteration, num.parameters, best.parameter.value, parameter.boundary, sep=" ")
  }

  # Described in the book chapter:
  #
  # F-Race and iterated F-Race: An overview. Mauro Birattari, Zhi
  # Yuan, Prasanna Balaprakash, and Thomas Stuetzle.  Experimental
  # Methods for the Analysis of Optimization Algorithms, Thomas
  # Bartz-Beielstein, Marco Chiarandini, Luis Paquete, Mike Preuss,
  # (editors), Springer, 2010 (to appear).
  #cat(1/num.candidates, 1/d, (1/num.candidates)^(1/d), ((1/num.candidates)^(1/d))^iteration, sep=" ")
  rate <- num.candidates^(-iteration / num.parameters)

  half <- ((max(parameter.boundary) - min(parameter.boundary)) * rate) / 2
  
  best.parameter.value <- as.numeric(best.parameter.value)

  if (debug.level >= 2) {
    cat ("gaussian range: (", best.parameter.value - half, ", ",
         best.parameter.value + half, ")\n")
  }
  return (c(best.parameter.value - half, best.parameter.value + half))
}

# ###########################################################
# change some unnecessary subsidiary parameter values to NA
# ###########################################################
validate.subsidiary.parameters <- function(configurations, parameter.subsidiary.list)
{
  sub.names <- names(parameter.subsidiary.list)
  for (i in seq_len(nrow(configurations))) {
    for (sname in sub.names) {
      master <- parameter.subsidiary.list[[sname]]
      for (mname in names(master)) {
        # If the configuration value equals none of the value in the
        # master vector.
        if (is.na (configurations[i,][[mname]])
            || ! any(master[[mname]] == configurations[i,][[mname]])) {
          # We take the corresponding unnecessary subsidiary parameter
          # value away by setting them to be "NA". Note that this is
          # not the keyword NA, but under double quotes. We will test
          # this in the wrapper file. Further note that master$mname
          # is a vector, it can take more than one value, but we
          # currently consider single value only.
          configurations[i,][[sname]] <- NA
        }
      }
    }
  }
  return(configurations)
}


init.prob.list <- function (parameter.type.list, parameter.boundary.list,
                            candidate.configurations.dataframe)
{
  prob.list <- list()
  type.c.names <- names (which (parameter.type.list[] == "c"))

  # hashtable with
  type.c.prob.vector <- list()
  if (any (parameter.type.list[] == "c")) {
    ## ??? use seq_along(type.c.names)
    for (i in 1:length(type.c.names)) {
      # name of parameter
      type.c.name <- type.c.names[i]
      # num.c.name - number of levels
      num.c.name <- length(parameter.boundary.list[[type.c.name]])
      prob <- (1 / num.c.name)
      type.c.prob.vector[[type.c.name]] <- rep(prob, times = num.c.name)
    }

    for (i in 1:length(type.c.names)){
      type.c.name <- type.c.names[i]
      # compute key for the hashtable. e.g. "0.18#0.39#0.2#4.6#50#35#ants" (configurations+parameter_name)
      tmp <- compute.key.value(candidate.configurations.dataframe,type.c.name,type.c.prob.vector[[type.c.name]])
      prob.list <- append(prob.list, tmp)
    }
  }
  return(prob.list)
}

generate.configurations.uniform <- function (num.samples,
                                             parameter.names.vector,
                                             parameter.type.list,
                                             parameter.boundary.list,
                                             parameter.subsidiary.list)
{
  if (debug.level >= 1) {
    cat ("Start sampling", num.samples, "candidates by uniform distribution.\n")
  }
  part <- NULL
  num.samples.left <- num.samples
  is.first <- TRUE
  counter <- 0
	
  # ###############################################################################################
  # To prevent repetitions in configurations
  # ###############################################################################################
  while (TRUE) {
    configurations <- NULL
    for (i in seq_len (length(parameter.names.vector))) {
      parameter.name <- parameter.names.vector[i]
      tmp <- c()
      if (debug.level >= 3) {
        print (parameter.name)
      }
      
      if (parameter.type.list[[parameter.name]] == "c") {
        # Sample with repetitions.
        tmp <- sample(parameter.boundary.list[[parameter.name]], num.samples.left, replace = TRUE)
      } else {
        tmp <- runif(num.samples.left,
                     min = min(parameter.boundary.list[[parameter.name]]),
                     max = max(parameter.boundary.list[[parameter.name]]))
        tmp <- signif(tmp, digits = signif.digit)
        
        if (parameter.type.list[[parameter.name]] == "i") {
          tmp <- round(tmp)
        }
      }
      configurations <- cbind(configurations, tmp)
    }
    colnames (configurations) <- parameter.names.vector
    
    stopifnot (! is.null(configurations) && nrow(configurations) > 0)
    part <- unique(rbind(part, configurations)) 
    stopifnot (! is.null(part))

    num.samples.left <- num.samples - nrow(part)

    if (debug.level >= 3) {
      cat("Sampled", nrow(part), "configurations out of", num.samples, ", ", num.samples.left, "more needed (iteration", counter, ")\n")
      cat("new configurations:\n")
      print(configurations)
      cat("all current configurations:\n")
      print(part)
    }

    if (num.samples.left <= 0) {
      break
    }

    if (counter >= max.sample.itr) {
      cat("maximum sampling iterations (", counter, ") has been reached, return", nrow(part), "candidates instead of requested", num.samples,"\n")
      break
    } else {
      counter <- counter + 1
    }
  }
  
  configurations <- part
  configurations <- as.data.frame(configurations)
  names(configurations) <- parameter.names.vector
  configurations <- validate.subsidiary.parameters(configurations, parameter.subsidiary.list)

  return(configurations)
}


update.prob.vector<-function(prob.vector,update.index,num.race,min.num.race){

# update by Hebbian inspired rule, used in neural network
num.race<-min(num.race,min.num.race)
ptmp<-(1-(num.race/min.num.race))*prob.vector
ptmp[update.index]<-ptmp[update.index]+(num.race/min.num.race)
return(ptmp)

}


generate.configurations.normal <- function(num.candidates,
                                           step,
                                           num.iterations,
                                           elite.configurations.dataframe.aux2,
                                           parameter.names.vector,parameter.type.list,
                                           parameter.boundary.list,
                                           parameter.subsidiary.list)
#                                           prob.vector.type.c.list)
{
  cat("Start sampling", num.candidates, "candidates by normal distribution.\n")
  
  d <- length(parameter.names.vector)
  prob.vector.type.c.list.aux <- list()
  
  # Number of new generated configurations.
  number.samples <- num.candidates - nrow(elite.configurations.dataframe.aux2)

  if (number.samples <= 0) {
    cat ("number candidates", num.candidates, "but we have already",
         nrow(elite.configurations.dataframe.aux2), "elite configurations: no sampling needed!\n")
    return (elite.configurations.dataframe.aux2)
  }
  
  prob.vector <- elite.configurations.dataframe.aux2$config.prob
  elite.ranks <- elite.configurations.dataframe.aux2$config.ranks
  
  if (debug.level >= 2) {
    cat ("prob.vector\n")
    print(prob.vector)
    cat ("elite.ranks:\n")
    print(elite.ranks)
    cat ("number.samples:\n")
    print(number.samples)
    cat ("prob.vector.type.c.list:\n")
    print(prob.vector.type.c.list)
  }
  
  elite.configurations.dataframe.aux3 <- elite.configurations.dataframe.aux2
  elite.configurations.dataframe.aux3$config.ranks <- NULL
  elite.configurations.dataframe.aux3$config.prob <- NULL
  
  # sample of ranks from some probability vector
  sampled.ranks <- sample(elite.ranks, size = number.samples, replace = TRUE, prob = prob.vector)
  if (debug.level >= 5) {
    cat ("sampled.ranks:\n")
    print (sampled.ranks)
  }
  
  for (i in 1:nrow(elite.configurations.dataframe.aux2)) {
    configurations = NULL
    part = NULL
    type.c.prob.vector<-list()
    i.index<-which(elite.configurations.dataframe.aux2$config.ranks==i)
    i.index.data.row <- elite.configurations.dataframe.aux2[i.index,]
    i.number.samples<-length(which(sampled.ranks==i.index.data.row$config.ranks))
    # if rank i elite conf really have some samples by rank.
    if(i.number.samples>=0){
      num.samples=i.number.samples
      is.first=TRUE
      counter=0
      
      # ###############################################################################################
      # To prevent repetitions in configurations
      # ###############################################################################################
      while (TRUE) {
        if (! is.null(configurations)) {
          part=unique(rbind(part, configurations))
        }
        if (debug.level >= 2) {
          cat("is.first=",is.first,
              " is.null(part)=", is.null(part),
              " nrow(part)=", nrow(part),
              " num.samples=", num.samples,
              " i.number.samples=", i.number.samples, "\n")
        }
        
        #print("new configurations")
        #print(configurations)
        #print("all current configurations")
        #print(part)
        if (! is.first) {
          if (is.null(part)) {
            break
          } else {
            if (nrow(part) >= num.samples) {
              oldConfig = as.matrix(unique(elite.configurations.dataframe.aux3))
              numOld = nrow(oldConfig)
              allConfig = unique(rbind(part, oldConfig))
              numAll = nrow(allConfig)
              extraSample = num.samples + numOld - numAll
              if (extraSample <= 0) {
                break
              } else {
                i.number.samples <- extraSample
              }
              
            } else {
              i.number.samples <- num.samples - nrow(part)
            }
          }
        } else {
          is.first <- FALSE
        }
        
        configurations <- NULL
        
        if (counter >= max.sample.itr) {
          cat("maximum sampling iterations (", counter, ") has been reached, sample uniformly\n")
          
          configurations <- generate.configurations.uniform(i.number.samples, parameter.names.vector, parameter.type.list, parameter.boundary.list, parameter.subsidiary.list)
          #print(configurations)
          configurations <- as.matrix(configurations)
          next
        } else {
          counter=counter + 1
        }
      	
        for ( j in 1:length(parameter.names.vector) ){
          
          parameter.name <- parameter.names.vector[j]
          parameter.value <- as.vector(i.index.data.row[[parameter.name]])
          if (debug.level >= 3) {
            cat("name=", parameter.name, "value=", parameter.value, "\n")
          }
          
          if (parameter.type.list[[parameter.name]] != "c"){
            
            parameter.value<-as.numeric(parameter.value)
            
            if(i.number.samples>0){
              
              boundary<-c(min(parameter.boundary.list[[parameter.name]]),max(parameter.boundary.list[[parameter.name]]))
              #print(parameter.name)
              #print(as.vector(parameter.value))
              #print(boundary)
              if (! is.na(parameter.value)) { # parameter has value in the last iteration
                # left-right by standard deviation
                # rumor has it that the standard deviation is getting smaller each race
                #cat(num.candidates,step,d,parameter.value,boundary,sep=" ")
                left.right.values <- compute.gaussian.range(num.candidates = num.candidates,
                                                            iteration = step, num.parameters = d,
                                                            best.parameter.value = parameter.value,
                                                            parameter.boundary = boundary)
                #print(left.right.values)

                if (debug.level >= 3) {
                  cat("parameter.name=",parameter.name,
                      "parameter.value=",parameter.value,
                      "left.right.values=",left.right.values,
                      "\n")
                }                
                
                tmp<-rnormalfull (i.number.samples,parameter.value,
                                 left.right.values,boundary)
                
              } else {
                # parameter has no value in the last iteration, sample the next value uniformly. this is just a temporary solution, better ways should come.
                tmp<-runif(i.number.samples, min=min(boundary), max=max(boundary))
                
              }
              
              # rounding to 2 significant digit after decimal
              tmp<-signif(tmp,digits=signif.digit)
              
              if(parameter.type.list[[parameter.name]] == "i"){
                tmp<-round(tmp)
              }
              #print(tmp)
            }
          } else {
            # categorical parameters
            
            # take the key. modified on 2008.09.02, we temporarily used only parameter name as the key, so that only the uniform probability is stored in the "prob.vector.type.c.list"
            #key<-sprintf("%s#%s",as.vector(apply(tmp,MARGIN=1,FUN=paste,collapse="#")),parameter.name)
            # extract method read.by.key
            #prob.vector<-prob.vector.type.c.list[[key]]
            
            # the code is really badly written
            
            tmp <- i.index.data.row
            tmp$config.ranks <- NULL
            tmp$config.prob <- NULL
           
            key <- generate.key(tmp, parameter.name)
            prob.vector <- read.by.key (tmp,parameter.name, prob.vector.type.c.list)

            if (debug.level >= 3) {
              cat ("prob.vector before update: ")
              print(prob.vector)
            }
            
            #if(length(prob.vector.type.c.list[[key]])==0){
            if (is.null(prob.vector) | length(prob.vector)<=0) {
              cat ("tmp:\n")
              print (tmp);
              print(key)
              print(prob.vector)
              # FIXME!!! It often happens that we do not find the key!
              # stop ("key not found")
              cat ("key not found!\n")
            }
            #cat("prob.vector",sep="\n")
            #cat(prob.vector,sep="\n")
            
            if (! is.na(parameter.value)) {
              # the parameter level index to be updated
              update.index<-which(parameter.boundary.list[[parameter.name]]==parameter.value)
              
              #print(prob.vector)
              #print(update.index)
              
              # update the prob vector. 5=number of iterations
              prob.vector<-update.prob.vector(prob.vector,update.index,step,num.iterations)
            }

            if (debug.level >= 3) {
              cat ("prob.vector after update: ")
              print(prob.vector)
            }
            
            # temp vector for storing the prob.vector by parameter name
            type.c.prob.vector[[parameter.name]]<-prob.vector
            
            # a list for storing type c parameter keys and the new updated probability vector. here we first store the elite configuration.
            prob.vector.type.c.list.aux[[key]] <- prob.vector
            
            if(i.number.samples>0) {
              # to sample according to the new probability
              tmp<-sample(parameter.boundary.list[[parameter.name]],i.number.samples,replace=TRUE,prob=prob.vector)
              
            }
          } # end update for a categorical parameter
          
          if(i.number.samples>0) {
            #cat("tmp\n")
            #print(tmp)
            configurations<-cbind(configurations,tmp)
          }
          
        } # end for each parameter
        
      } # end of the whole sampling process
      #print(configurations)
      configurations=part

      if(i.number.samples>0){
        configurations<-as.data.frame(configurations)
        #print(names(configurations))
        #print(parameter.names.vector)
        names(configurations)<-parameter.names.vector
        
        configurations=validate.subsidiary.parameters(configurations, parameter.subsidiary.list)

        
        type.c.names <- names(which(parameter.type.list[] == "c"))
        
        if(any(parameter.type.list[]=="c")){
          #print(prob.vector.type.c.list.aux)
          for( k in 1:length(type.c.names)){
            typ.c.name<-type.c.names[k]
            if (debug.level >= 3) {
              cat ("type.c.name:")
              print(typ.c.name)
              cat ("configurations:")
              print(configurations)
              cat ("type.c.prob.vector:")
              print(type.c.prob.vector[[typ.c.name]])
            }
            prob.vector.type.c.list.aux <- c(prob.vector.type.c.list.aux,
                                             compute.key.value(configurations,typ.c.name,
                                                               type.c.prob.vector[[typ.c.name]]))
          }
        }

        elite.configurations.dataframe.aux3<-unique(rbind(elite.configurations.dataframe.aux3,configurations))

      }

    } # end if sample size >= 0
    
  } # end for each elite configuration
  
  
  # To let the next iteration not be affected by the last configuration of the last iteration, modified on 2008.08.29. 
  prob.vector.type.c.list <<- prob.vector.type.c.list.aux
  
  row.names(elite.configurations.dataframe.aux3)<-c(1:nrow(elite.configurations.dataframe.aux3))

  if (debug.level >= 3) {
    cat("size before unique:", nrow(elite.configurations.dataframe.aux3), "\n")
  }
  
  elite.configurations.dataframe.aux3=unique(elite.configurations.dataframe.aux3)
  elite.configurations.dataframe.aux3=as.data.frame(as.list(elite.configurations.dataframe.aux3))
  if (debug.level >= 3) {
    cat("size after unique:", nrow(elite.configurations.dataframe.aux3), "\n")
    print("the final list")
    print(elite.configurations.dataframe.aux3)
  }
  
  return (elite.configurations.dataframe.aux3)
}



compute.key.value<-function(candidate.configurations.dataframe,name,prob.vector){

tmp<-list()

# modified on 2008.09.02, to make sure every key is just the parameter name
#key<-sprintf("%s#%s",as.vector(apply(candidate.configurations.dataframe,MARGIN=1,FUN=paste,collapse="#")),name)
#key<-c(name)
key<-generate.key(candidate.configurations.dataframe,name)


for (i in 1:length(key)){
tmp[[key[i]]]<-prob.vector
}
return (tmp)

}

read.by.key<-function(candidate.configurations.dataframe,name,prob.vector){
  #key<-sprintf("%s#%s",as.vector(apply(candidate.configurations.dataframe,MARGIN=1,FUN=paste,collapse="#")),name)
  
  #key<-c(name)
  
  key<-generate.key(candidate.configurations.dataframe,name)
  
  tmp<-prob.vector[[key]]
  
  return(tmp)
}

generate.key<-function(candidate.configurations.dataframe,name) {
  key <- sprintf("%s#%s",as.vector(apply(candidate.configurations.dataframe,MARGIN=1,FUN=paste,collapse="#")),name)
  #key<-c(name)
  return(key)
}
