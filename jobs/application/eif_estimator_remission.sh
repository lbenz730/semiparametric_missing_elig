#!/usr/bin/bash

#SBATCH -c 4 ## number of cores
#SBATCH -t 0-04:00 ## amount of time in D-HH:MM
#SBATCH -p fasse  ## Partition to submit to
#SBATCH --mem=50000 ## memory pool for all cores
#SBATCH -o logs/application/aligned_t0/EIF/log_eif_remission.stdout_%a ## STDOUT
#SBATCH -e logs/application/aligned_t0/EIF/log_eif_remission.stderr_%a ## STDERR
#SBATCH --account=haneuse_lab
#SBATCH --array=1-40

module load R/4.2.2-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.2.2:$R_LIBS_USER

cd $HOME/matched_cohort

Rscript scripts/analysis/aligned_t0/fit_EIF_estimator_remission.R $SLURM_ARRAY_TASK_ID