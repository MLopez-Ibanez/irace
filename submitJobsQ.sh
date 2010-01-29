#!/bin/bash
############################################################################### 
# ??? This file needs a description
###############################################################################
LIMIT=3000
 
for i in *.job
do
    num_job=$(qstat -u jdubois | wc -l)

    while [ "$num_job" -gt "$LIMIT" ]
    do
        sleep 5
        num_job=$(qstat -u jdubois | wc -l)
    done
    
    # ??? This file needs a description
    qsub $i

done






