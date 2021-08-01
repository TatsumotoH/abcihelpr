#!/bin/sh

# source /etc/profile.d/modules.sh

# module load gcc/9.3.0
# module load R/4.0.4


#Rscript do_tune.R $1 $2 $3
Rscript do_tune.R ${grid} ${start} ${end}
