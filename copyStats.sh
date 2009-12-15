#!/bin/bash

I=10

req=300



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
cp "$stat_file" $EXP-Stats/$EXP-$i.Rdata

done



