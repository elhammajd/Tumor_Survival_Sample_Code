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
Rscript ./code/Merge-Results.R;
Rscript ./code/6WithoutCV.R; 
Rscript ./code/7WithoutCV.R; 
Rscript ./code/8WithoutCV.R; 
Rscript ./code/9WithoutCV.R; 
Rscript ./code/11WithoutCV.R; 
Rscript ./code/12WithoutCV.R; 
Rscript ./code/16WithoutCV.R; 
Rscript ./code/17WithoutCV.R; 
Rscript ./code/18WithoutCV.R; 
Rscript ./code/22WithoutCV.R; 
Rscript ./code/23WithoutCV.R; 
Rscript ./code/31WithoutCV.R; 
Rscript ./code/32WithoutCV.R; 
Rscript ./code/34WithoutCV.R; 
Rscript ./code/35WithoutCV.R; 
Rscript ./code/37WithoutCV.R; 
Rscript ./code/41WithoutCV.R;
Rscript ./code/42WithoutCV.R; 
Rscript ./code/53WithoutCV.R; 
Rscript ./code/54WithoutCV.R; 
Rscript ./code/55WithoutCV.R; 
Rscript ./code/58WithoutCV.R; 
Rscript ./code/59WithoutCV.R;  
Rscript  ./code/Mean-coeffs.R;
Rscript  ./code/WilcoxonTests-Fig3.R;
Rscript  ./code/WilcoxonTests-Fig4.R;
Rscript  ./code/OS-method1.R;
Rscript  ./code/Elasticnet-weights.R;
Rscript  ./code/Segmentation-method2.R;
Rscript  ./code/OS-method2.R;
Rscript  ./code/RegressionTree-Rates.R;
Rscript  ./code/Segmentation-method3.R;
Rscript  ./code/OS-method3.R;
Rscript  ./code/Cindex-Fig9.R;
Rscript  ./code/Cindex-Appendix1.R;
Rscript  ./code/Cindex-Appendix2.R;







