# configurations: configuration structure in irace results (iraceResults$allConfigurations)
# parameters: parameter structure (iraceResults$parameters)

# TODO:
# * change slightly background of conditional parameters
#
# * accept a configuration (e.g, best one) and mark its values of the best
#   configuration somehow (bold?)
#
# * print number of NAs in numerical parameters within the plot "# NA = Total
#   (Percentage%)"
#
# * export this function and add an R manual page, perhaps with example.
#
# * how to resize the Window when filename == NULL to have a rows/cols aspect ratio.
#
# * add tests!
#parameterFrequency(iraceResults$allConfigurations, iraceResults$parameters)
#best <- colnames(experiments)[which(apply(experiments, 2, function(x) { sum(!is.na(x))}) > 5)]
#parameterFrequency(allConfigurations[best,], parameters)
parameterFrequency <- function(configurations, parameters,
                               rows = 4, cols = 3,
                               filename = NULL,
                               pdf.width = 12,
                               col = "gray")
{
  xlab <- "values"
  ylab.cat <- "Frequency"
  ylab.num <- "Probability density"
  
  def.par <- par(no.readonly = TRUE) # save default, for resetting...

  # This is the same as removeConfigurationsMetaData
  configurations <- configurations[, grep("^\\.", colnames(configurations), invert=TRUE), drop = FALSE]

  param.names <- as.character(parameters$names)
  nparams <- parameters$nbVariable
  nlines <- ceiling(nparams / cols)  
  if (nlines < rows) rows <- nlines

  nplot <- 1
  cplot <- 1
  if (!is.null(filename)) {
    pdf(file = paste0(filename, "-", cplot, ".pdf"), onefile = TRUE, width = pdf.width)
  }
  par(mfrow=c(rows,cols))
  
  for (param.name in param.names) {

    if (parameters$isFixed[[param.name]]){
      cat("Skipping fixed parameter:", param.name, "\n")
      next
    } else {
      cat("Plotting:", param.name, "\n")
    }
    
    if (nplot > rows * cols) {
      cat("Make new plot\n")
      cplot <- cplot + 1
      if (!is.null(filename)) {
        dev.off()
        pdf(file = paste0(filename, "-", cplot, ".pdf"), onefile = TRUE, width = pdf.width)
      } else {
        dev.new()
      }
      par(mfrow=c(rows, cols))
      nplot <- 1
    }

    data <- configurations[, param.name]
    type <- parameters$types[[param.name]]
    domain <- parameters$domain[[param.name]]
    if (type %in% c("c", "o")) {
      data <- factor(data, domain)
      data <- addNA(data, ifany = TRUE)
      levels(data)[is.na(levels(data))] <- "<NA>"
      data <- table(data)
      barplot(data, main = param.name, xlab = xlab, ylab = ylab.cat, col = col)
    } else if (type %in% c("i", "r")) {
      data <- data[!is.na(data)]
      if (length(data) == 0) {
        cat("All values are NA for: ", param.name, ", skipping plot\n")
        next
      } else {
        hist(data, xlim = domain, prob = TRUE,
             main = param.name, xlab = xlab, ylab = ylab.num, col = col)
        if (length(data) > 1) {
          lines(density(data), col = "blue", lwd = 2)
        } else {
          abline(v = data[1], col = "blue", lwd = 2)
        }
      }
    }
    nplot <- nplot + 1
  }

  if (!is.null(filename)) dev.off()
  par(def.par)  #- reset to default
}

#Function parcoordlabel plots parallel coordinates for categorical and numerical configurations
#idea from http://stackoverflow.com/questions/23553210/add-ticks-to-parcoord-parallel-coordinates-plot
parcoordlabel <- function (configurations, parameters, col = "green", lty = 1,
                           lblcol="blue", title="Parameters parallel coordinates", ...)
{
  replace.cat <- function(y, vals){
      for(i in 1:length(vals))
        y[y %in% vals[i]] <- i
      return(y)
    }
    
    replace.na <- function(x,r) {
      x <- unlist(x)
      x <- (x-r[1])/(r[2]-r[1]) 
      x[is.na(x)] <- 1
      return(x)
    }
    
    add.level <- function(x, bound, type){
       if(type== "i" || type == "r"){
         
         x <- c(x, x[length(x)] + (x[2]-x[1]))
       }else{
         x <- c(x, x[length(x)] +  x[length(x)]/length(bound))
       }
       return(x)
    }
    
    param.names = colnames(configurations)
    
    bound.num <- parameters$domain   
    #Categorical -> Numerical
    for(i in 1:ncol(configurations)){
      pname <- param.names[i]
      if(parameters$types[[pname]]=="c"){
        configurations[,i]<- as.numeric(replace.cat(configurations[,i], bound.num[[pname]]))
        bound.num[[pname]] <- seq(1,length(bound.num[[pname]]))
      }
    }
    
     
    #Intervals
    pr <- lapply(bound.num[param.names], pretty)
    #add extra level for NA values
    for(param in param.names)
    pr[[param]] <- add.level( pr[[param]], bound.num[[param]], parameters$types[[param]])
    #Total range
    rx <- lapply(pr, range, na.rm = TRUE)
    

    #Values x plot
    configurations <- mapply(replace.na, as.data.frame(configurations), rx) 

    matplot(1:ncol(configurations), t(configurations), type = "l", col = col,
            lty = lty,  xlab = "", ylab = "", ylim=c(0,1), axes = FALSE, main=title, ...)
        
    axis(1, at = 1:ncol(configurations), labels = colnames(configurations), las=2)
    
    for (i in 1:ncol(configurations)) {
        pnames <- param.names[i]
        lines(c(i, i), c(0, 1), col = "grey70")
        if(parameters$types[[param.names[i]]]=="c"){
          labels <- c(parameters$domain[[param.names[i]]], "NA")
        }else{
          labels <- pr[[pnames]]
          labels[length(labels)] <- "<NA>"
        }
        text(c(i, i), seq(0,1,length.out=length(labels)), labels = labels,  xpd = NA, col=lblcol)
    }
    invisible()
}

# TODO:
# * add color scheme
#
## Filters and prepare data to plots parallel coordinate of configurations 
## configurations: structure as in iraceResults
## parameters: structure as in iraceResults
## param_names: selection of parameters names that must be plotted
## hierarchy: separate plots according to dependencies of parameters (one level)
## mar: margin for the plots
## filename: prefix to save pdf files
parallelCoordinatesPlot <- function(configurations, parameters, param_names=parameters$names, hierarchy=TRUE, filename=NULL, pdf.width=14 , mar=c(8,1,4,1)){
  getDependency <- function(){
    sdep <- list()
    independent <- c()
    for(param in param_names){
      constraint <- all.vars(parameters$conditions[[param]])
      if(length(constraint) <1){
        independent <- unique(c(independent, param))
        next
      }
        
      for(cc in constraint){
        if(is.null(sdep[[cc]]) || is.na(sdep[[cc]]))
          sdep[[cc]] <- param
        else 
          sdep[[cc]] <- c(sdep[[cc]], param)
      }
    }    
    return(list(independent=independent, dependencies=sdep))
  } 

  
  configurations <- configurations[, grep("^\\.", colnames(configurations), invert=TRUE), drop = FALSE]
  configurations <- configurations[, param_names]

  if(hierarchy){
    aux <- getDependency()
    if(length(aux$independent) >= 2){
      indconfigurations <- configurations[, aux$independent] 
      if(!is.null(filename))
        pdf(file = paste0(filename,"_independent.pdf"), width=pdf.width)
      else dev.new()
      par(mar=mar)
      parcoordlabel(indconfigurations, parameters, title="Independent parameter parallel coordinates")
      if(!is.null(filename)) dev.off()
    }else{
      cat("Skipping independent parameters",aux$independent,"\n")
    }
    
    dep <- aux$dependencies
    dnames <- names(dep) 


    for(i in 1:length(dep)){
      if(length(dep[[i]]) < 2){
        cat("Skipping parameters",dnames[i],"\n")
        next
      }
      depconfigurations <- configurations[, dep[[i]]]
      depconfigurations  <- depconfigurations[!apply(apply(depconfigurations, 1, is.na), 2, all),]
      
      if(nrow(depconfigurations) < 2 ){
        cat("Skipping parameter",dnames[i],"\n")
        next
      }
      if(!is.null(filename))
        pdf(file = paste0(filename,"_",dnames[i],".pdf"), width=pdf.width)
      else dev.new()
      par(mar=mar)
      parcoordlabel(depconfigurations, parameters,
                    title = paste0("Parameters of dependent of ", dnames[i], " parallel coordinates"))
      if (!is.null(filename)) dev.off()   
    }
  
  }else{
    if(!is.null(filename))
      pdf(file = paste0(filename,".pdf"), width=pdf.width)
    #else dev.new()
    par(mar=mar)
    parcoordlabel(configurations, parameters)
    if(!is.null(filename)) dev.off()
  }

}

## Get the "n" best final elite configurations.
## iraceResults: object created by irace and saved in scenario$logFile.
## iraceLog: log file created by irace, this file must contain the iraceResults object.
## n: number of elite configurations to return, if n is larger than the number of 
##    configurations, then only the existing ones are returned.
## drop.internals: Remove the internal identifier and parent identifier from the returned 
##    configurations data frame.
## * iraceResults or iraceLog must be provided, in case both are give iraceResults will be used.
## This function returns a data frame containing the selected candidate configurations 
getFinalElites <- function(iraceResults=NULL, irace.logFile=NULL, n=0, drop.internals=FALSE) {
  if(is.null(iraceResults)){
    if(is.null(irace.logFile))
      stop("You must supply either iraceResults or iraceLog argument.\n")
    else
      load(irace.logFile)
  }
  
  #Following is done to avoid the note "make check"
  .ID.<-NULL
  
  last.elites  <- iraceResults$allElites[[length(iraceResults$allElites)]]
  
  if (n==0) 
    n <- length(last.elites) 
    
  if (length(last.elites) < n) {
    cat("Only", length(last.elites), "configurations available, reducing n,\n")
    n <- length(last.elites)
  }
  last.elites <- last.elites[1:n]
  
  configurations <-as.data.frame(t(sapply(last.elites,function (x) subset(iraceResults$allConfigurations, .ID.==x))))
  
  if(drop.internals)
    configurations <-  removeConfigurationsMetaData(configurations)
  return(configurations)

}


## Get configuration(s) by the id(s).
## iraceResults: object created by irace and saved in scenario$logFile.
## iraceLog: log file created by irace, this file must contain the iraceResults object.
## ids: the id or a vector of ids of the candidates configurations to obtain.
## drop.internals: Remove the internal identifier and parent identifier from the returned 
##    configurations data frame.
## * iraceResults or iraceLog must be provided, in case both are give iraceResults will be used.
## This function returns a data frame containing the selected candidate configurations 
getConfigurationById <- function(iraceResults=NULL, irace.logFile=NULL, ids, drop.internals=FALSE) {

  if(is.null(iraceResults)){
    if(is.null(irace.logFile))
      stop("You must supply either iraceResults or iraceLog argument.\n")
    else
      load(irace.logFile)
  }
  
  if(length(ids) <1 ) stop("You must provide at least one configuration id.\n")
  
  selection <- iraceResults$allConfigurations[,".ID."] %in% ids
  
  if(length(selection) < 1) stop("No configuration found with id", ids,".\n")
  
  configurations <-iraceResults$allConfigurations[selection, , drop=FALSE]
  
  if(drop.internals)
    configurations <-  removeConfigurationsMetaData(configurations)
  return(configurations)
}


## Get configuration(s) by the iteration in which they were executed.
## iraceResults: object created by irace and saved in scenario$logFile.
## iraceLog: log file created by irace, this file must contain the iraceResults object.
## iterations: the iteration or a vector of iterations from where the configurations should be obtained.
## drop.internals: Remove the internal identifier and parent identifier from the returned 
##    configurations data frame.
## * iraceResults or iraceLog must be provided, in case both are give iraceResults will be used.
## This function returns a data frame containing the selected candidate configurations 
getConfigurationByIteration <- function(iraceResults=NULL, irace.logFile=NULL, iterations, drop.internals=FALSE) {

  if(is.null(iraceResults)){
    if(is.null(irace.logFile))
      stop("You must supply either iraceResults or iraceLog argument.\n")
    else
      load(irace.logFile)
  }
  
  if(length(iterations) <1 ) stop("You must provide at least one configuration id.\n")
  
  #Following is done to avoid the note "make check"
  iteration <-NULL
  
  ids <- unique(subset(as.data.frame(iraceResults$experimentLog), iteration==iterations,  select=c("configuration"), drop=TRUE))
  
  selection <- iraceResults$allConfigurations[,".ID."] %in% ids
  
  if(length(selection) < 1) stop("No configuration found with id", ids,".\n")
  
  configurations <-iraceResults$allConfigurations[selection, , drop=FALSE]
  
  if(drop.internals)
    configurations <-  removeConfigurationsMetaData(configurations)
  return(configurations)
}


