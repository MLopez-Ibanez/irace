#!/bin/bash

I=10

req=300

EXP=TUNE

if [ -d "$EXP-Results" ]; then
	echo "$EXP-Results already exists"
	echo "Removing $EXP-Results ....."
	rm -r "$EXP-Results"
	echo "Creating $EXP-Results ....."
	mkdir "$EXP-Results"
else
	echo "Creating $EXP-Results ....."
	mkdir "$EXP-Results"
fi

for i in $(seq 1 $I); do

TRY=$(printf '%s-%002d' $EXP $i)
 
num_files=$(cat $TRY/Results/*.txt | wc -l)

    if [ "$num_files" -eq "$req" ] ; then
    	cat $TRY/Results/*.txt > $EXP-Results/$i.txt
    	echo Success $TRY
    else 
      echo Fail $TRY. not finished: $num_files not equals $req
    fi
done


