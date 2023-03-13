#!/bin/bash
#SBATCH --account=def-ubcxzh
#SBATCH --output=./rout/%x-%j.out
#SBATCH --cpus-per-task=1
#SBATCH --mail-user=elhmajd@gmail.com
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=REQUEUE
#SBATCH --mail-type=ALL
#SBATCH --ntasks=4  
#SBATCH --mem-per-cpu=4G # 2GiB of memery 
#SBATCH -t 0-04:00 
Rscript ./code/CV.R; 
Rscript ./code/OS-method1.R;
Rscript ./WilcoxonTests-Fig3.R
