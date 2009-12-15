#!/bin/bash
########################################
# This file is to print all results once Tuning is finished. Not part
# of I-F-Race.
########################################
filename=`ls ./TUNE-01/*.o*`
filename2=`basename $filename`
prefix=${filename2%*.o*}
cat "TUNE-01"/$prefix*\.o* | tail --lines=-7 | head -1
for n in $(seq 5)
do
    cat "TUNE-0$n"/$prefix*\.o* | tail --lines=-6 | head -1 | sed 's/\s\+/,/g' | cut -d, -f2-5
done



