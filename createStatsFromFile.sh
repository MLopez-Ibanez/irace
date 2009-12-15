
INS1=$(cat JointRace.o* | grep "|x|" | wc -l)
INS2=$(cat JointRace.o* | grep "|-|" | wc -l)
INS3=$(cat JointRace.o* | grep "|=|" | wc -l)

INS=$(( $INS1 + $INS2 + $INS3 ))

NumRaces=$(cat JointRace.o* | grep "Selected candidate:" | wc -l)

UsedExp=$(grep -B 1 "Jobs" JointRace.o* | head -n 1 | awk -F" " {'print $2'})

R --no-save --no-restore --slave<<EOF

stats<-NULL

stats<-rbind(stats,c(num.race=$NumRaces,no.canidates=30,max.exp=7290,used.exp=$UsedExp,used.ins=$INS,no.alive=10))
stats<-data.frame(stats)
save(stats,file=paste("stats",".Rdata",sep=""))

EOF