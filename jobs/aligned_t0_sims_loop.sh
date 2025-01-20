#!/usr/bin/bash

#SBATCH -c 64 ## number of cores
#SBATCH -t 1-00:00 ## amount of time in D-HH:MM
#SBATCH -p fasse_bigmem  ## Partition to submit to
#SBATCH --mem=450000 ## memory pool for all cores
#SBATCH --account=haneuse_lab

module load R/4.2.2-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.2.2:$R_LIBS_USER

cd $HOME/matched_cohort

Rscript scripts/simulations/aligned_t0/run_simulation.R $EST_ID $SIM_ID ### Order in manner consistent with job-arrays