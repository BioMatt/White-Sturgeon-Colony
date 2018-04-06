#!/bin/bash -l
#SBATCH --array=1-100
#SBATCH --mem=20G
#SBATCH -t 20-12:00:00

module load colony2

list=blcj2011list.txt

string="sed -n "$SLURM_ARRAY_TASK_ID"p ${list}"
str=$($string)

colony2s.ifort.out IFN:${str}
