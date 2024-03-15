### Project description:
This project assessed the effects of vitamin D supplementation in pregnancy on maternal, neonatal, and infant outcomes. We aimed to update a prior meta-analysis (Roth et al. BMJ. 2017). We included new studies published through November 2023. 

### Datasets: 
The study used two datasets.
-	One dataset contains 66 observations (trials):  vitd_2024.03.08.csv
-	One dataset contains 82 observations (intervention control pairs) because some trials had more than two arms: vitdpairs_2024.03.08.csv
-	Datasets are available upon request. Please email Dr. Emily Smith at emilysmith@gwu.edu

### R codes:
## This repository contains a R script called vitd_2024.03.11.R 
## The R codes contains 4 major parts of analysis
o	I. line 35 to 325: Descriptive statistics of included trials (Both vitd_2024.03.08.csv and vitdpairs_2024.03.08.csv were used.)  
o	II. line 326 to 1306: Primary analysis, sensitivity analysis, and subgroup analysis (vitd_2024.03.08.csv was used.) 
o	III. line 1307 to 1416: Subgroup analysis by intervention dose (vitdpairs_2024.03.08.csv was used for this subgroup analysis.)
o	IV. line 1417 to 1698: Figures, including funnel plots, ROB traffic light, and heatmap of trial contributions (vitd_2024.03.08.csv was used.)
