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

# Configuration variables
.race.warn.quiet<--1
.race.warn.level<-1
.race.slave.sh<-path.expand(file.path(system.file(package = "race"),
	"slave","srace.sh"))
.slave.init.function<-"race.init"
.slave.wrapper.function<-"race.wrapper"
.slave.info.function<-"race.info"
.slave.describe.function<-"race.describe"
.title.width<-30
.value.width<-33

# Load library rpvm if available
.race.warn.save<-getOption("warn")
.race.usePVM<- suppressWarnings(require(rpvm, quietly = TRUE))

# Conventional master-slave messages
.race.MSG<-list(INIT       = 11,
                NAME       = 22,
                WORK       = 33,
                RESULT     = 44,
                SERROR     = 55,  
                EXIT       = 66,
                int2str    = function(x)
                            {return(names(.race.MSG)[match(x,.race.MSG)])})


# The master:
race<-function(wrapper.file=stop("Argument \"wrapper.file\" is mandatory"),
               maxExp=0,
               stat.test=c("friedman","t.bonferroni","t.holm","t.none"),
               conf.level=0.95,
               first.test=5,
               each.test=1,
               interactive=TRUE,
               log.file="",
               no.slaves=0,
               stop.min.cand=1,
               ...){

  timestamp.start<-date()

  # Change warning behavior
  .race.warn.save<-getOption("warn")
  on.exit(options(warn=.race.warn.save))
  options(warn=.race.warn.level)
  
  # Check argument: wrapper.file
  wrapper.file # Just to chech if it is defined
  if (!is.character(wrapper.file))
    stop("Option \"wrapper.file\" must be a string")
  wrapper.file<-path.expand(wrapper.file)       
  if (dirname(wrapper.file) == ".")
    wrapper.file<-file.path(getwd(),wrapper.file)
  try(source(wrapper.file,local=TRUE),silent=TRUE)
  if (!exists(.slave.wrapper.function,inherits=FALSE,mode="function")||
      !exists(.slave.info.function,inherits=FALSE,mode="function"))
    stop(paste("Either file \"",wrapper.file,"\" does not exist,\n",
               "or it does not define properly the wrapper \"",
               .slave.wrapper.function,"\"\n",
               "and/or the function \"",
               .slave.info.function,"\"",
               sep=""))
  
  # Check argument: maxExp
  if (!missing(maxExp) &&
      (!is.numeric(maxExp) ||
       length(maxExp)!=1 ||
       !is.finite(maxExp)))
    stop("maxExp must be an single number")
  maxExp<-ifelse(maxExp>0,maxExp,0)
  maxExp<-floor(maxExp)

  # Check argument: stat.test
  stat.test<-match.arg(stat.test)

  # Check argument: conf.level
  if (!missing(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level)!=1 ||
       !is.finite(conf.level) || conf.level<0 || conf.level>1)) 
    stop("conf.level must be a single number between 0 and 1")

  # Check argument: first.test
  if (!missing(first.test) &&
      (!is.numeric(first.test) ||
       length(first.test)!=1 ||
       !is.finite(first.test)))
    stop("first.test must be an single number")
  first.test<-ifelse(first.test>0,first.test,0)
  first.test<-floor(first.test)

  # Check argument: interactive
  if (!missing(interactive) &&
      (!is.logical(interactive) || length(interactive)!=1))
    stop("interactive must be a logical")

  # Check argument: log.file
  if (.Platform$OS.type=="unix")
    if (!missing(log.file) && (log.file!="") && 
        (system(paste("touch",log.file),ignore.stderr=TRUE)!=0))
      stop(paste("I cannot create file ",log.file,sep=""))
  
  # Check argument: no.slaves
  if (!missing(no.slaves) &&
      (!is.numeric(no.slaves) ||
       length(no.slaves)!=1 ||
       !is.finite(no.slaves)||
       no.slaves<0))
    stop("no.slaves must be an single number >= 0")
  no.slaves<-floor(no.slaves)
  if (!.race.usePVM && no.slaves!=0)
    warning(paste("This is not a PVM!\n",
                  "Option \"no.slaves=",no.slaves,"\" overridden."))
  if (no.slaves==0)
    .race.usePVM<-FALSE

  # Run init function (if any defined in wrapper.file)
  if (exists(.slave.init.function,inherits=FALSE,mode="function")){
    race.data<-do.call(.slave.init.function,list(...))
    if(!is.list(race.data))
      stop(paste("Error while running",.slave.init.function))
    precis.init<-TRUE
  }else{
    race.data<-list()
    precis.init<-FALSE
  }

  # Collect info on race from wrapper
  race.info<-do.call(.slave.info.function,list(race.data))

  # Check race.info
  if (# race.info$race.name must be a string
      is.na(match("race.name",names(race.info)))||
      !is.character(race.info$race.name)||
      length(race.info$race.name)!=1 ||
      # race.info$no.candidates must be an integer
      is.na(match("no.candidates",names(race.info)))||
      !is.numeric(race.info$no.candidates) ||
      length(race.info$no.candidates)!=1 ||
      !is.finite(race.info$no.candidates) ||
      race.info$no.candidates!=as.integer(race.info$no.candidates) ||
      # race.info$no.tasks must be an integer
      is.na(match("no.tasks",names(race.info)))||
      !is.numeric(race.info$no.tasks) ||
      length(race.info$no.tasks)!=1 ||
      !is.finite(race.info$no.tasks) ||
      race.info$no.tasks!=as.integer(race.info$no.tasks)||
      # race.info$no.subtasks is a non-compulsory integer.
      (!is.na(match("no.subtasks",names(race.info)))&&
      (!is.numeric(race.info$no.subtasks) ||
       (length(race.info$no.subtasks)!=1 &&
        length(race.info$no.subtasks)!=race.info$no.tasks) ||
       any(!is.finite(race.info$no.subtasks)) ||
       any(race.info$no.subtasks!=as.integer(race.info$no.subtasks))))||
      # race.info$extra is a non-compulsory string or paragraph.
      (!is.na(match("extra",names(race.info)))&&
       !is.character(race.info$extra)))
    stop(paste("Function \"",.slave.info.function,
               "\" returned an invalid object",sep=""))

  # Default for no.subtasks
  if (is.na(match("no.subtasks",names(race.info))))
    race.info$no.subtasks<-1

  # copy race.info contents to workspace for convenience
  race.name<-race.info$race.name
  no.candidates<-race.info$no.candidates
  no.tasks<-race.info$no.tasks
  no.subtasks<-race.info$no.subtasks   

  
  # Prepare a precis for documentation
  format.precis<-function(title,value){
    dots<-function(n)
      return(paste(rep(".",n),collapse=""))
    spaces<-function(n)
      return(paste(rep(" ",n),collapse=""))
    string<-paste(title,dots(.title.width-nchar(title)),sep="")
    if (nchar(value)<=.value.width){
      string<-paste(string,dots(.value.width-nchar(value)),value,sep="")
    }else{
      f.vec<-strwrap(value,width=.value.width)
      first<-f.vec[1]
      first<-paste(dots(.title.width-nchar(first)),first,sep="")
      rest<-format(f.vec[-1])
      rest<-paste(spaces(.title.width+.value.width-max(nchar(rest))),
                  rest,sep="",collapse="\n")
      string<-paste(string,paste(first,rest,sep="\n"),sep="")
    }
    return(paste(string,"\n"))
  }

  precis<-paste("\n",
                "Racing methods for the selection of the best\n",
                "Copyright (C) 2003 Mauro Birattari\n",
                "This software comes with ABSOLUTELY NO WARRANTY\n\n",
                format.precis("Race name",race.name),
                format.precis("Number of candidates",no.candidates),
                format.precis("Number of available tasks",no.tasks),
                ifelse(length(no.subtasks)>1,
                       format.precis("Subtasks per task","task-dependent"),
                       ifelse(no.subtasks>1,
                              format.precis("Subtasks per task",no.subtasks),
                              "")),
                format.precis("Max number of experiments",
                              ifelse(maxExp,maxExp,"unlimited")),
                format.precis("Statistical test",
                              switch(stat.test,
                                     friedman="Friedman test",
                                     t.bonferroni=paste("t-test with",
                                       "Bonferroni's correction",
                                       "for multiple comparisons"),
                                     t.holm=paste("t-test with",
                                       "Holm's correction",
                                       "for multiple comparisons"),
                                     t.none=paste("t-test with",
                                       "no correction",
                                       "for multiple comparisons"))),
                format.precis("Tasks seen before discarding",first.test),
                format.precis("Initialization function",
                              ifelse(precis.init,"ok","none found")),
                format.precis("Parallel Virtual Machine",
                              ifelse(.race.usePVM,"yes","no")),
                sep="")

  if (!is.null(race.info$extra)){
    extra<-paste(strwrap(race.info$extra,width=60,prefix="\t"),collapse="\n")
    precis<-paste(precis,"\n",extra,"\n")
  }


  # Print out precis if interactive
  if (interactive) 
    cat(paste(precis,"\n\n"))

  if (maxExp && no.candidates>maxExp)
    stop("Max number of experiments is smaller than number of candidates")

  check.result<-function(result){
    expected.length<-ifelse(length(no.subtasks)==1,
                            no.subtasks,
                            no.subtasks[current.task])
    if (length(result)!=expected.length)
      stop(paste("Bad output returned by \"",
                 .slave.wrapper.function,"\"",sep=""))
  }

  if (.race.usePVM){
    # The following shouldn't be necessary... but apparently it is:
    library(rpvm)

    # Function for cleaning the PVM. To be called on exit
    killSlaves.exit<-function(){
      if (length(slaves)>0 || slaves>0){
        if (.PVM.initsend()==-1)
          warning("Can't kill slaves")
        else
          for(s in slaves)
            if (.PVM.send(s,.race.MSG$EXIT)==-1)
              warning("Error while killing slave")
      }
      .PVM.exit()
    }


    
    # Enroll the master process into PVM
    if ((mytid<-.PVM.mytid())==-1)
      stop("Can't enroll process into PVM")
    slaves<-0
    on.exit(killSlaves.exit(),add=TRUE)
    
    # Spawn slaves
    cat ("\nTry to spawn slaves...\n")
    slaves<-.PVM.spawn(task=.race.slave.sh,ntask=no.slaves)
    if (length(slaves)==1 && slaves==-1)
      stop("Can't spawn slaves")
    no.slaves<-length(slaves)

    
    # Initialize slaves
    if (.PVM.initsend()==-1 ||
        .PVM.pkstrvec(wrapper.file)==-1)
      stop("Error while initializing slave: can't prepare message")
    for (current.slave in slaves){
      if (.PVM.send(current.slave,.race.MSG$INIT)==-1)
        stop("Error while initializing slave: can't send message")
      if ((buf<-.PVM.recv(current.slave,.race.MSG$NAME))==-1)
        stop("Error while initializing slave: can't get its name")
      msg<-.PVM.bufinfo(buf)$msgtag
      slave.name<-.PVM.upkstrvec()
      cat(paste("\tSlave running on",slave.name,"\n"))
    }
    cat("Done.\n\n")
    # PVM function for starting a job
    job.start<-function(){
      current.candidate<-which.alive[no.jobs.submitted+1]
      current.slave<-available.slaves[1]
      available.slaves<<-available.slaves[-1]
      if (.PVM.initsend()==-1 ||
          .PVM.pkint(current.candidate)==-1 ||
          .PVM.pkint(current.task)==-1 ||
          .PVM.send(current.slave,.race.MSG$WORK)==-1)
        stop("Error while sending msg to slave")
      no.jobs.submitted<<-no.jobs.submitted+1
    }      

    # PVM function for waiting for a job
    job.wait<-function(){
      if ((bufid<-.PVM.recv(-1,-1))==-1)
        stop("Error while waiting for slave")
      # .PVM.bufinfo is rather noisy...
      options(warn=.race.warn.quiet)
      bufinfo<-.PVM.bufinfo(bufid)
      options(warn=.race.warn.level)
      # ...normal warn.level
      current.slave<-bufinfo$tid
      # Check if any slave reported an error
      if (.race.MSG$int2str(bufinfo$msgtag)!="RESULT")
        stop("Error reported by slave")
      available.slaves<<-c(current.slave,available.slaves)
      current.candidate<-.PVM.upkint()
      result<-.PVM.upkdblvec()
      check.result(result)
      no.jobs.completed<<-no.jobs.completed+1
      return(list(candidate=current.candidate,result=result))
    }
  }
  
  # Initialize some variables...
  Tasks<-1:no.tasks
  Results<-matrix(data=NA,
                  nrow=ifelse(length(no.subtasks)==1,
                    no.tasks*no.subtasks,sum(no.subtasks)),
                  ncol=no.candidates)
  alive<-array(TRUE,no.candidates)
  no.experiments.sofar<-0
  no.subexperiments.sofar<-0
  best<-0
  race.ranks <- c()
  no.tasks.sofar<-0
  no.subtasks.sofar<-0
  
  # Define some functions...
  log.list<-function(end=FALSE){
    timestamp.current<-date()
    log<-list(precis=precis,
              results=Results[1:no.subtasks.sofar,],
              no.candidates=no.candidates,
              no.tasks=no.tasks.sofar,
              no.subtasks=no.subtasks,
              no.experiments=no.experiments.sofar,
              no.alive=sum(alive),
              alive=alive,
              best=best,
              mean.best=mean.best,
              timestamp.start=timestamp.start,
	      race.data=race.data,
              ranks = race.ranks)

    if (end)
      log<-c(log,list(timestamp.end=timestamp.current,
                      description.best=description.best,
                      alive.inTime=ifelse(no.subtasks.sofar>1,
		      	apply(Results[1:no.subtasks.sofar,],
                        	1,function(u){sum(!(is.na(u)))}),
			sum(!is.na(Results[1,])))))
    else
      log<-c(log,list(timestamp.current=timestamp.current))
    return(log)
  }
  
  logger<-function(){
    if (log.file!=""){
      log<-log.list()
      save(log,file=log.file)
    }
  }
   
  aux2.friedman<-function(y,I=1:ncol(y),n=nrow(y),conf.level=0.95){
    k<-length(I)
    r<-t(apply(y[1:n,I], 1, rank))
    A<-sum(as.vector(r)^2)
    R<-apply(r, 2, sum)
    J<-I[order(R)]
    alpha<-1-conf.level
    TIES<-tapply(r, row(r), table)
    STATISTIC<-((12 * sum((R - n * (k + 1) / 2)^2)) /
                (n * k * (k + 1)
                 - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                    (k - 1))))
    PARAMETER<-k-1
    PVAL<-pchisq(STATISTIC, PARAMETER, lower = FALSE)
      
    if (!is.nan(PVAL) && (PVAL<alpha)){
      if (interactive)
        cat("|-|")
      t<-qt(1-alpha/2,(n-1)*(k-1))*(2*(n*A-sum(R^2))/((n-1)*(k-1)))^(1/2)
      o<-order(R)
      J<-I[o[1]]
      for (j in 2:k) 
        if (abs(R[o[j]]-R[o[1]])>t) 
          break
        else
          J<-c(J,I[o[j]])
    }else{
      if (interactive)
        cat("|=|")
    }
    race.ranks <<- R
    return(J)
  }

  
  aux.friedman<-function(){
    if (no.alive==2) {
      # If only 2 candidates are left, switch to Wilcoxon
      V1<-Results[1:(no.subtasks.sofar),which.alive[1]]
      V2<-Results[1:(no.subtasks.sofar),which.alive[2]]
      PVAL<-wilcox.test(V1,V2,paired=TRUE,exact=FALSE)$p.value
      if (!is.nan(PVAL)&&!is.na(PVAL)&&(PVAL<1-conf.level)){
        if (interactive)
          cat("|-|")
        if (median(V1-V2)<0){
          best<<-which.alive[1]
          alive[which.alive[2]]<<-FALSE
          race.ranks<<-c(1)
        }else{
          best<<-which.alive[2]
          alive[which.alive[1]]<<-FALSE
          race.ranks<<-c(1)
        }
      }else{
        if (interactive)
          cat("|=|")
        if (median(V1-V2)<0){
          best<<-which.alive[1]
          race.ranks<<-c(1,2)
        }else{
          best<<-which.alive[2]
          race.ranks<<-c(2,1)
        }
      }
    }else{
      # If more then 2 candidates are left, use Friedman
      J<-aux2.friedman(Results[1:(no.subtasks.sofar),],which.alive,
                       conf.level=conf.level)
      alive[-J]<<-FALSE
      best<<-J[1]
    }
  }


  
  aux.ttest<-function(adjust=c("none","bonferroni","holm")){
    adjust<-match.arg(adjust)
    mean.all<-array(0,c(ncol(Results)))
    for (j in 1:ncol(Results))
      mean.all[j]<-sum(Results[1:no.subtasks.sofar,j]/no.subtasks.sofar)
    best<<-match(min(mean.all[alive]),mean.all)
    race.ranks <<- mean.all[alive]

    PJ<-array(0,dim=c(2,0))
    for (j in which.alive) {
      Vb<-Results[1:no.subtasks.sofar,best]
      Vj<-Results[1:no.subtasks.sofar,j]
      #cat("Vb:", Vb, "\n")
      #cat("Vj:", Vj, "\n")
      # t.test may fail if the data in each group is almost
      # constant. Hence, we sourround the call in a try() and we
      # initialize p with 1 if the means are equal or zero if they are
      # different.
      p <- as.integer(isTRUE(all.equal(mean(Vb),mean(Vj))))
      try(p <- t.test(Vb,Vj,paired=TRUE)$p.value)
      if (!is.nan(p) & !is.na(p))
        PJ<-array(c(PJ,j,p),dim=dim(PJ)+c(0,1))
    }
    PJ[2,]<-p.adjust(PJ[2,],method=adjust)
    dropped.any<-FALSE
    for (j in 1:ncol(PJ))
      if (PJ[2,j]<(1-conf.level)){
        alive[PJ[1,j]]<<-FALSE
        dropped.any<-TRUE
      }
    if (interactive){
      if (dropped.any) 
        cat("|-|")
      else
        cat("|=|")
    }
  }

  if (interactive)
    cat("                            Markers:                           \n",
        "                               x No test is performed.         \n",
        "                               - The test is performed and     \n",
        "                                 some candidates are discarded.\n", 
        "                               = The test is performed but     \n",
        "                                 no candidate is discarded.    \n",
        "                                                               \n",
        "                                                               \n",
        "+-+-----------+-----------+-----------+-----------+-----------+\n",
        "| |       Task|      Alive|       Best|  Mean best| Exp so far|\n",
        "+-+-----------+-----------+-----------+-----------+-----------+\n",
        sep="")
  
  # Start main loop
  for (current.task in 1:no.tasks) {
    which.alive<-which(alive)
    no.alive<-length(which.alive)
    if (maxExp && no.experiments.sofar+no.alive>maxExp)
      break
    if (no.alive==1)
      break

    current.no.subtasks<-ifelse(length(no.subtasks)==1,
                                no.subtasks,
                                no.subtasks[current.task])
    
    if(length(no.subtasks)==1)
      subtasks.range<-
        (current.task-1)*no.subtasks+1:no.subtasks
    else
      subtasks.range<-
        cumsum(c(0,no.subtasks))[current.task]+1:no.subtasks[current.task]
    
    if (.race.usePVM){
      # Running under PVM 
      no.jobs.submitted<-0
      no.jobs.completed<-0
      available.slaves<-slaves

      # Start some initial jobs...
      for (i in 1:min(no.slaves,no.alive))
        job.start()
      
      # ...wait for their termination and start some more
      while(TRUE){
        out<-job.wait()
        Results[subtasks.range,out$candidate]<-out$result
        if (no.jobs.completed==no.alive)
          break
        if (no.jobs.submitted<no.alive)
          job.start()
      }
    }else{
      # PVM not available: running on a single processor
      for (current.candidate in which.alive){
        result <- do.call (.slave.wrapper.function,
                           list(current.candidate,current.task,race.data))
        check.result(result)
        Results[subtasks.range,current.candidate]<-result
      }
    }

    no.experiments.sofar<-no.experiments.sofar+no.alive
    no.subexperiments.sofar<-no.subexperiments.sofar+
      current.no.subtasks*no.alive
    
    no.tasks.sofar<-no.tasks.sofar+1
    no.subtasks.sofar<-no.subtasks.sofar+current.no.subtasks
    
    # Drop bad candidates
    if ( (no.tasks.sofar>=first.test) && ((no.tasks.sofar %% each.test) == 0)) {
      switch(stat.test,
             friedman=aux.friedman(),
             t.none=aux.ttest("none"),
             t.holm=aux.ttest("holm"),
             t.bonferroni=aux.ttest("bonferroni"))
    } else {
      if (interactive)
        cat("|x|")
      if (no.subtasks.sofar==1)  {
        race.ranks <- Results[1,]
        best <- order(race.ranks)[1]
      } else  {
        tmpResults <- Results[1:no.subtasks.sofar, which.alive]
        stopifnot(!any(is.na(tmpResults)))
        if (stat.test == "friedman") {
          race.ranks <- apply(t(apply(tmpResults, 1, rank)), 2, mean)
        } else {
          race.ranks <- apply(tmpResults, 2, mean)
        }
        best <- which.alive[order(race.ranks)[1]]
      }
    }
    stopifnot (best == which.alive[order(race.ranks)][1])
    race.ranks <- race.ranks[which(which.alive %in% which(alive))]

    mean.best<-mean(Results[1:(no.subtasks.sofar),best])

    if (interactive) 
      cat(paste(formatC(no.tasks.sofar,width=11),"|",
                formatC(sum(alive),width=11),"|",
                formatC(best,width=11),"|",
                formatC(mean.best,width=11),"|",
                formatC(no.experiments.sofar,width=11),"|\n",
                sep=""))

    # stop race if we have less than the minimum number of candidates
    if (no.tasks.sofar>=first.test) {
        which.alive<-which(alive)
    	if(length(which.alive) <= stop.min.cand)
    	break	
   }
    logger()
  }

  if (exists(.slave.describe.function,inherits=FALSE,mode="function"))
    description.best<-do.call(.slave.describe.function,list(best,race.data))
  else
    description.best<-NULL
  
  if (interactive) {
   cat(paste("+-+-----------+-----------+-----------+-----------+-----------+",
              "\n\n",
              "Selected candidate:",formatC(best,width=12),
              "\tmean value:",formatC(mean.best,width=11),
              "\n\n",sep=""))
    if (!is.null(description.best)){
      cat("Description of the selected candidate:\n")
      print(description.best)
      cat("\n\n")
    }
  }
  
  # Build the return variable with everything inside: 
  invisible(log.list(end=TRUE))
}


if (.race.usePVM){
  .race.slave<-function(){

    # Be sure that rpvm is installed on slave
    library(rpvm)

    # Get Master id
    myMaster<-.PVM.parent()
    if (!myMaster)
      stop("Can't get Master id")
  
    # Prepare function for reporting problems to Master
    all.done.properly<-FALSE
    report.slave.error<-function(){
      if (!all.done.properly){
        warning("Some error occurred: reporting to Master")
        if (.PVM.initsend()==-1 ||
            .PVM.send(myMaster,.race.MSG$SERROR)==-1)
          stop("Can't report to Master")
      }
      .PVM.exit()
    }
    on.exit(report.slave.error())
  
    # hostname for documentation
    if (.Platform$OS.type=="unix")
      cat(paste("\nSlave running on host:",
                system("hostname",intern=TRUE),"\n\n"))
    if (.Platform$OS.type=="windows")
      cat(paste("\nSlave running a windows machine :(\n\n"))
    
    # Change warning behavior
    .race.warn.save<-getOption("warn")
    on.exit(options(warn=.race.warn.save),add=TRUE)
    options(warn=.race.warn.level)

    while(TRUE){
      # Receive instruction from Master
      if ((buf<-.PVM.recv(myMaster,-1))==-1)
        stop("Error while listening to Master")
      msg<-.PVM.bufinfo(buf)$msgtag
      tag<-.race.MSG$int2str(msg)
    
      switch(tag,
             INIT={# Extract wrapper.file
               wrapper.file<-.PVM.upkstrvec()
               # Answer to Master giving your name:
               this.slave.name<-"Boh?"
               if (.Platform$OS.type=="unix")
                 this.slave.name<-system("hostname",intern=TRUE)
               if (.Platform$OS.type=="windows")
                 this.slave.name<-"a windows machine :("
               if (.PVM.initsend()==-1 ||
                   .PVM.pkstrvec(this.slave.name)==-1 ||
                   .PVM.send(myMaster,.race.MSG$NAME)==-1)
                 stop("Error while telling my humble name to Master")
               # Source wrapper.file
               cat(paste("Sourcing wrapper file:",wrapper.file,"\n"))
               source(wrapper.file,local=TRUE)
               # 
               if (exists(.slave.init.function,inherits=FALSE,
                          mode="function")){
                 cat(paste("Found function \"",.slave.init.function,
                           "\"\n  ... Proceeding to initialization:\n",sep=""))
                 race.data<-do.call(.slave.init.function,list())
                 if (!is.list(race.data))
                   stop("Error during initialization")
                 cat("Done.\n\n")
               }else{
                 race.data<-list()
                 cat(paste("No function \"",.slave.init.function,
                           "\"\n defined in file \"",wrapper.file,
                           "\" ... Proceeding anyway!\n\n",sep=""))
               }  
             },
             WORK={# Extract candidate and task from msg
               candidate<-.PVM.upkint()
               task<-.PVM.upkint()
               # Run job
               result<-do.call(.slave.wrapper.function,
                               list(candidate,task,race.data))
              # Return result to Master
               if (.PVM.initsend()==-1 ||
                   .PVM.pkint(candidate)==-1 ||
                   .PVM.pkdblvec(result)==-1 ||
                   .PVM.send(myMaster,.race.MSG$RESULT)==-1)
                 stop("Error while reporting results to Master")
             },
             EXIT={all.done.properly<-TRUE
                   break})
    }
  }
}
