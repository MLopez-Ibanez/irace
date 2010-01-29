
signif.digit=4

eval=function(result, executable, test.instance.dir) {
	
	cnd<-result$description.best
	#print(cnd)
	l<-length(names(cnd))
	names<-names(cnd)[1:l]
	
	#executable<- "../../../Bin/acotsp --mmas --tries 1 --localsearch 0"
	instancesTest<-list.files(path=test.instance.dir,pattern=".tsp",full.names=TRUE)
	instancesTestNames<-list.files(path=test.instance.dir,pattern=".tsp",full.names=FALSE)
	
	# What is this deleting?
	system("rm -f *.txt",intern=TRUE,TRUE)
	
	trailNum<-1
	
	res.dir<-"Results"
	system(paste("rm -rf", res.dir),intern=TRUE)
	system(paste("mkdir", res.dir),intern=TRUE)
	
	
	for (ins in 1:length(instancesTest)){
		
		command<-executable
		for (i in names){
			if (! is.na(cnd[[i]])) {
				if (i == "mode") {
					command<-paste(command,"  --",cnd[[i]],sep="")
				} else {
					command<-paste(command,"  --",i," ",signif(as.numeric(as.vector(cnd[[i]])), signif.digit),sep="")
				}
			}
		}
		
		command<-paste(command," -i ",instancesTest[ins],sep="")
		command<-paste(command," | tail -1 > ",res.dir, "/", instancesTestNames[ins], ".txt", sep="")
		print(command)
		
		system(command,intern=TRUE)
	
	}
}
