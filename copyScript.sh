#!/bin/bash

I=10

# EXP=Joint
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%02d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done



# EXP=Joint2ND
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%02d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done



# EXP=JointTD
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%02d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done

req=300


# I=1
# 
# EXP=RobustLS
# 
# if [ -d "$EXP-Results" ]; then
# 	echo "$EXP-Results already exists"
# 	echo "Removing $EXP-Results ....."
# 	rm -r "$EXP-Results"
# 	echo "Creating $EXP-Results ....."
# 	mkdir "$EXP-Results"
# else
# 	echo "Creating $EXP-Results ....."
# 	mkdir "$EXP-Results"
# fi
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
#  
# num_files=$(cat $TRY/Results/*.txt | wc -l)
# 
#     if [ "$num_files" -eq "$req" ] ; then
#     	cat $TRY/Results/*.txt > $EXP-Results/$i.txt
#     	echo Success $TRY
#     fi
# done



# I=10
# 
# EXP=GridG
# 
# if [ -d "$EXP-Results" ]; then
# 	echo "$EXP-Results already exists"
# 	echo "Removing $EXP-Results ....."
# 	rm -r "$EXP-Results"
# 	echo "Creating $EXP-Results ....."
# 	mkdir "$EXP-Results"
# else
# 	echo "Creating $EXP-Results ....."
# 	mkdir "$EXP-Results"
# fi
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
#  
# num_files=$(cat $TRY/Results/*.txt | wc -l)
# 
#     if [ "$num_files" -eq "$req" ] ; then
#     	cat $TRY/Results/*.txt > $EXP-Results/$i.txt
#     	echo Success $TRY
#     fi
# done
# 
# 
# I=10
# 
# EXP=GridK
# 
# if [ -d "$EXP-Results" ]; then
# 	echo "$EXP-Results already exists"
# 	echo "Removing $EXP-Results ....."
# 	rm -r "$EXP-Results"
# 	echo "Creating $EXP-Results ....."
# 	mkdir "$EXP-Results"
# else
# 	echo "Creating $EXP-Results ....."
# 	mkdir "$EXP-Results"
# fi
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
#  
# num_files=$(cat $TRY/Results/*.txt | wc -l)
# 
#     if [ "$num_files" -eq "$req" ] ; then
#     	cat $TRY/Results/*.txt > $EXP-Results/$i.txt
#     	echo Success $TRY
#     fi
# done


I=10

EXP=Joint1NDF

if [ -d "$EXP-Stats" ]; then
	echo "$EXP-Stats already exists"
	echo "Removing $EXP-Stats ....."
	rm -r "$EXP-Stats"
	echo "Creating $EXP-Stats ....."
	mkdir "$EXP-Stats"
else
	echo "Creating $EXP-Stats ....."
	mkdir "$EXP-Stats"
fi

for i in $(seq 1 $I); do

TRY=$(printf '%s-%002d' $EXP $i)
 
stat_file=$TRY/stats.Rdata
echo $stat_file
cat createStatsFromFile.sh > $TRY/createStatsFromFile.sh
cd $TRY
sh createStatsFromFile.sh
cd ..

done



# EXP=Joint1NDF
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done
# 
# 
# 
# EXP=Joint2NDF
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done
# 
# 
# 
# EXP=Joint3NDF
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done


# EXP=JointNDH
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done


# 
# 
# 
# 
# EXP=JointAND
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..
# 
# 
# 
# done



# EXP=JointHD
# 
# for i in $(seq 1 $I); do
# 
# TRY=$(printf '%s-%002d' $EXP $i)
# cp -r $EXP $TRY
# 
# perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/els-tune.sh
# 
# cd $TRY
# 
# qsub els-tune.sh
# 
# sleep 1
# 
# cd ..







