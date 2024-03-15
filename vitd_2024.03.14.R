# Project name: The effects of vitamin D supplementation on maternal, birth, and infant outcomes
# Created by Wen-Chien Yang, reviewed by Fouzia Farooq, Erin Oakley, Ramaa Chitale, and Emily Smith  
# Update: 2024.03.14 

library(meta)
library(dplyr)
library(tidyr)
library(robvis)
library(writexl)
library(ggplot2)
library(RColorBrewer)  

#                                  <<  Contents  >>
# 
#   I.  Descriptive statistics of included trials                           (line 35)
#   II. Primary analysis, sensitivity analysis, and subgroup analysis                            
#        1. Data management                                                 (line 326)
#        2. Primary analysis, subgroup analysis for binary outcomes         (line 376)
#        3. Subgroup analysis by health conditions for RR of GDM            (line 849)
#        4. Sensitivity analysis for binary outcomes                        (line 948)       
#        5. Primary analysis, subgroup analysis for continuous outcomes     (line 1022)
#   III. Subgroup analysis by intervention dose
#        1. subgroup analysis by intervention dose for binary outcomes      (line 1307)
#        2. subgroup analysis by intervention dose for continuous outcomes  (line 1365)
#   IV.  Figures 
#        1. Risk of bias traffic light figure                               (line 1417)
#        2. Funnel plots                                                    (line 1498)   
#        3. Heatmap of trial contributions                                  (line 1626)

#  Note I uses data: vitd_2024.03.08.csv and vitdpairs_2024.03.08.csv
#       II uses data vitd_2024.03.08.csv
#       III uses data vitdpairs_2024.03.08.csv
#       IV and I use data vitd_2024.03.08.csv

################################################################################
#   I. Descriptive statistics of included trials   
################################################################################
# Read dataset 
d<-read.csv("vitd_2024.03.08.csv")

# 1. N of trials and % by control type 
     table(d$control_type)                      # n of trials by control type 
     prop.table(table(d$control_type))*100      # % of trials by control type

# 2. N of trials and % by number of of intervention arms
     table(d$n_arms)                            # n of trials by number of arms
     prop.table(table(d$n_arms))*100            # % of trials by number of arms
   # 2.1 N of trials and % by N of intervention arms in placebo control studies 
     d1<-subset(d, control_type==1)             # Create subset d1 for trials using placebo control 
     table(d1$n_arms)                           # n of trials by number of arms
     prop.table(table(d1$n_arms))*100           # % of trials by number of arms
   # 2.2 N of trials and % by N of intervention arms in active control studies 
     d2<-subset(d, control_type==2)             # Create subset d3 for trials using active control
     table(d2$n_arms)                           # n of trials by number of arms
     prop.table(table(d2$n_arms))*100           # % of trials by number of arms

# 3. N of participants randomized: overall and by control type 
     sum(d$n_randomized)      
     summary(d$n_randomized)
   # 3.1 N of participants in placebo control studies 
     sum(d1$n_randomized)       
     summary(d1$n_randomized)
   # 3.2 N of participants in active control studies 
     sum(d2$n_randomized)      
     summary(d2$n_randomized)

# 4. N of outcomes that each trial contribute 
#    (note: mcri=1 when meeting outcome definitions, mcri=0 not meeting outcome defnitions, mcri=0 or 1 meaning the trials reported this outcome)
# 4.1 Maternal outcomes 
    # 4.1.1 select maternal outcomes (10 outcomes) with mcri for placebo control studies
      d1.mat.outcomev1 <-  select(d1, study_id2, study, year, control_type, 
                           delivery_mcri, pe_mcri, gdm_mcri, ght_mcri, csection_mcri, 
                           PreLabor_mcri, MHypercalcem_mcri, MHypercalciu_mcri,
                           MHypocalcem_mcri, MatHosp_mcri)
    # 4.1.2 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1) for placebo control studies
      d1.mat.outcomev2<- as.data.frame(lapply(d1.mat.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))
    # 4.1.3 create a var n.mat.outcome reporting the n of maternal outcomes in each trial for placebo control studies
      d1.mat.outcomev2$n.mat.outcome<-rowSums(d1.mat.outcomev2[,5:14], na.rm = T)
      summary (d1.mat.outcomev2$n.mat.outcome)
    # 4.1.4 select maternal outcomes (10 outcomes) with mcri for active control studies
      d2.mat.outcomev1 <- select(d2, study_id2, study, year, control_type, 
                                 delivery_mcri, pe_mcri, gdm_mcri, ght_mcri, csection_mcri, 
                                 PreLabor_mcri, MHypercalcem_mcri, MHypercalciu_mcri,
                                 MHypocalcem_mcri, MatHosp_mcri)
    # 4.1.5 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1) for active control studies
      d2.mat.outcomev2<- as.data.frame(lapply(d2.mat.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))
    # 4.1.6 create a var n.mat.outcome reporting the n of maternal outcomes in each trial for active control studies
      d2.mat.outcomev2$n.mat.outcome<-rowSums(d2.mat.outcomev2[,5:14], na.rm = T)
      summary (d2.mat.outcomev2$n.mat.outcome)

# 4.2 Child outcome (n=28) including birth outcomes (n=11) and infant outcomes (n=17) 
    # 4.2.1 select birth outcomes (n=11) and infant outcomes (n=17) with mcri
      d.chi.outcomev1 <- select(d, study_id2, study, year, control_type, 
                         iud_mcri,   lbw_mcri,  PretermBirth_mcri, sga_mcri, CongMal_mcri, 
                         nicu_mcri,  ga_mcri,   bw_mcri,           bl_mcri,  hc_mcri, 
                         cord_mcri,                                             # 11 birth outcomes
                 
                         NeoDeath_mcri, NHypercalcem_mcri, NHypocalcem_mcri, RespInf_mcri, urti_mcri, 
                         lrti_mcri,     asthma_mcri,       wei_mcri,         len_mcri,     hc1_mcri, 
                         waz_mcri,      laz_mcri,          hcaz_mcri,        nbmc_mcri,    nbmd_mcri, 
                         ibmc_mcri,     ibmd_mcri)                              # 17 infant outcomes 
    # 4.2.2 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1)
      d.chi.outcomev2<- as.data.frame(lapply(d.chi.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))
    # 4.2.3 create a var n.mat.outcome reporting the n of maternal outcomes in each trial
      d.chi.outcomev2$n.chi.outcome<-rowSums(d.chi.outcomev2[,5:32], na.rm = T)
      summary (d.chi.outcomev2$n.chi.outcome)
    # 4.2.4 select birth outcomes (n=11) and infant outcomes (n=17) with mcri for placebo control studies 
      d1.chi.outcomev1 <- select(d1, study_id2, study, year, control_type, 
                                 iud_mcri, lbw_mcri, PretermBirth_mcri, sga_mcri, CongMal_mcri, nicu_mcri,
                                 ga_mcri, bw_mcri, bl_mcri, hc_mcri, cord_mcri, 
                                
                                 NeoDeath_mcri, NHypercalcem_mcri, NHypocalcem_mcri, 
                                 RespInf_mcri, urti_mcri, lrti_mcri, asthma_mcri,
                                 wei_mcri, len_mcri, hc1_mcri, 
                                 waz_mcri, laz_mcri, hcaz_mcri, 
                                 nbmc_mcri, nbmd_mcri, ibmc_mcri, ibmd_mcri) 
    # 4.2.5 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1) for placebo control studies 
      d1.chi.outcomev2<- as.data.frame(lapply(d1.chi.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))
    # 4.2.6 create a var n.mat.outcome reporting the n of maternal outcomes in each trial for placebo control studies 
      d1.chi.outcomev2$n.chi.outcome<-rowSums(d1.chi.outcomev2[,5:32], na.rm = T)
      summary (d1.chi.outcomev2$n.chi.outcome)
    # 4.2.7 Select birth outcomes (n=11) and infant outcomes (n=17) with mcri for active control studies 
      d2.chi.outcomev1 <- select(d2, study_id2, study, year, control_type, 
                                 iud_mcri, lbw_mcri, PretermBirth_mcri, sga_mcri, CongMal_mcri, nicu_mcri,
                                 ga_mcri, bw_mcri, bl_mcri, hc_mcri, cord_mcri, 
                                 
                                 NeoDeath_mcri, NHypercalcem_mcri, NHypocalcem_mcri, 
                                 RespInf_mcri, urti_mcri, lrti_mcri, asthma_mcri,
                                 wei_mcri, len_mcri, hc1_mcri, 
                                 waz_mcri, laz_mcri, hcaz_mcri, 
                                 nbmc_mcri, nbmd_mcri, ibmc_mcri, ibmd_mcri) 
    # 4.2.8 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1) for active control studies 
      d2.chi.outcomev2<- as.data.frame(lapply(d2.chi.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))
    # 4.2.9 create a var n.mat.outcome reporting the n of maternal outcomes in each trial for active control studies 
      d2.chi.outcomev2$n.chi.outcome<-rowSums(d2.chi.outcomev2[,5:32], na.rm = T)
      summary (d2.chi.outcomev2$n.chi.outcome)

# 4.3 All outcomes (n=38)
    # 4.3.1 select all outcomes with mcri
      d.all.outcomev1 <- select(d, study_id2, study, year, control_type, 
                         delivery_mcri, pe_mcri, gdm_mcri, ght_mcri, csection_mcri, PreLabor_mcri, MHypercalcem_mcri, MHypercalciu_mcri,
                         MHypocalcem_mcri, MatHosp_mcri,
                 
                         iud_mcri, lbw_mcri, PretermBirth_mcri, sga_mcri, CongMal_mcri, nicu_mcri,
                         ga_mcri, bw_mcri, bl_mcri, hc_mcri, cord_mcri, 
                 
                         NeoDeath_mcri, NHypercalcem_mcri, NHypocalcem_mcri, 
                         RespInf_mcri, urti_mcri, lrti_mcri, asthma_mcri,
                         wei_mcri, len_mcri, hc1_mcri, 
                         waz_mcri, laz_mcri, hcaz_mcri, 
                         nbmc_mcri, nbmd_mcri, ibmc_mcri, ibmd_mcri)
    # 4.3.2 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1)
      d.all.outcomev2<- as.data.frame(lapply(d.all.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))

    # 4.3.3 create a var n.mat.outcome reporting the n of maternal outcomes in each trial
      d.all.outcomev2$n.all.outcome<-rowSums(d.all.outcomev2[,5:42], na.rm = T)
      summary(d.all.outcomev2$n.all.outcome)

    # 4.3.4 select all outcomes with mcri for placebo control studies
      d1.all.outcomev1 <- select(d1, study_id2, study, year, control_type, 
                                 delivery_mcri,  pe_mcri,           gdm_mcri,          ght_mcri,          csection_mcri, 
                                 PreLabor_mcri,  MHypercalcem_mcri, MHypercalciu_mcri, MHypocalcem_mcri,  MatHosp_mcri,
                                
                                 iud_mcri,       lbw_mcri,          PretermBirth_mcri, sga_mcri,          CongMal_mcri, 
                                 nicu_mcri,      ga_mcri,           bw_mcri,           bl_mcri,           hc_mcri, 
                                 cord_mcri, 
                                
                                 NeoDeath_mcri,  NHypercalcem_mcri, NHypocalcem_mcri,  RespInf_mcri,      urti_mcri, 
                                 lrti_mcri,      asthma_mcri,       wei_mcri,          len_mcri,          hc1_mcri, 
                                 waz_mcri,       laz_mcri,          hcaz_mcri,         nbmc_mcri,         nbmd_mcri, 
                                 ibmc_mcri,      ibmd_mcri)
    # 4.3.5 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1) for placebo control studies
      d1.all.outcomev2<- as.data.frame(lapply(d1.all.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))
    # 4.3.6 create a var n.mat.outcome reporting the n of maternal outcomes in each trial for placebo control studies
      d1.all.outcomev2$n.all.outcome<-rowSums(d1.all.outcomev2[,5:42], na.rm = T)
      summary(d1.all.outcomev2$n.all.outcome)
    # 4.3.7 select all outcomes with mcri for active control studies
      d2.all.outcomev1 <- select(d2, study_id2, study, year, control_type, 
                                 delivery_mcri,  pe_mcri,           gdm_mcri,          ght_mcri,          csection_mcri, 
                                 PreLabor_mcri,  MHypercalcem_mcri, MHypercalciu_mcri, MHypocalcem_mcri,  MatHosp_mcri,
                                 
                                 iud_mcri,       lbw_mcri,          PretermBirth_mcri, sga_mcri,          CongMal_mcri, 
                                 nicu_mcri,      ga_mcri,           bw_mcri,           bl_mcri,           hc_mcri, 
                                 cord_mcri, 
                                 
                                 NeoDeath_mcri,  NHypercalcem_mcri, NHypocalcem_mcri,  RespInf_mcri,      urti_mcri, 
                                 lrti_mcri,      asthma_mcri,       wei_mcri,          len_mcri,          hc1_mcri, 
                                 waz_mcri,       laz_mcri,          hcaz_mcri,         nbmc_mcri,         nbmd_mcri, 
                                 ibmc_mcri,      ibmd_mcri)
    # 4.3.8 create new dataset v2 where mcri=1 regardless of meeting criteria or not (mcri=0 or 1 in v1) for active control studies
      d2.all.outcomev2<- as.data.frame(lapply(d2.all.outcomev1, function(x) ifelse(is.na(x), NA, ifelse(x == 0, 1, x))))
    # 4.3.9 create a var n.mat.outcome reporting the n of maternal outcomes in each trial for active control studies
      d2.all.outcomev2$n.all.outcome<-rowSums(d2.all.outcomev2[,5:42], na.rm = T)
      summary(d2.all.outcomev2$n.all.outcome)

# 5. N of trials by geographic location 
     table(d$Region)
     prop.table(table(d$Region))*100
   # 5.1 N of trials by geographic location in placebo control studies
     table(d1$Region)
     prop.table(table(d1$Region))*100
   # 5.2 N of trials by geographic location in active control studies
     table(d2$Region)
     prop.table(table(d2$Region))*100

# 6. N of trials by health conditions
     table(d$pop_health)
     prop.table(table(d$pop_health))*100 
   # 6.1 N of trials by health conditions in placebo control studies 
     table(d1$pop_health)
     prop.table(table(d1$pop_health))*100
   # 6.2 N of trials by health conditions in active control studies 
     table(d2$pop_health)
     prop.table(table(d2$pop_health))*100

# 7. N of participants by health conditions 
   # 7.1 N of participants among trials in general population
     gen_pop<-subset(d, pop_type==1)
     sum(gen_pop$n_randomized)
   # 7.2 N of participants among trials in population with morbidities
     mor_pop<-subset(d, pop_type==2)
     sum(mor_pop$n_randomized)

# 8. N of trials by baseline vitamin D level (30);use control group level to determine 
     table(d$baseline_vitd30)
     prop.table(table(d$baseline_vitd30))*100     
   # 8.1 among studies with placebo control 
     table(d1$baseline_vitd30)
     prop.table(table(d1$baseline_vitd30))*100   
   # 8.2 among studies with active control 
     table(d2$baseline_vitd30)
     prop.table(table(d2$baseline_vitd30))*100 

# 9. N of trials by vitamin D type
     table(d$supp_form)
     prop.table(table(d$supp_form))*100
   # 9.1 among studies with placebo control 
     table(d1$supp_form)
     prop.table(table(d1$supp_form))*100
   # 9.2 among studies with active control 
     table(d2$supp_form)
     prop.table(table(d2$supp_form))*100

# 10. N of trials by regularity (regular, bolus)
      table(d$dose_freq)
      prop.table(table(d$dose_freq))*100
    # 10.1 among studies with placebo control 
      table(d1$dose_freq)
      prop.table(table(d1$dose_freq))*100
    # 10.2 among studies with active control 
      table(d2$dose_freq)
      prop.table(table(d2$dose_freq))*100

# 11. N of trials by frequency (daily, weekly, etc)
      table(d$frequency)
      prop.table(table(d$frequency))*100
    # 11.1 among studies with placebo control 
      table(d1$frequency)
      prop.table(table(d1$frequency))*100
    # 11.2 among studies with active control 
      table(d2$frequency)
      prop.table(table(d2$frequency))*100

# 12. N of trials by trimester in which supplementation was initiated
      table(d$ini_trimester)
      prop.table(table(d$ini_trimester))*100
    # 12.1 among studies with placebo control 
      table(d1$ini_trimester)
      prop.table(table(d1$ini_trimester))*100
    # 12.2 among studies with placebo control 
      table(d2$ini_trimester)
      prop.table(table(d2$ini_trimester))*100

# 13. N of participants in 2017 Roth BMJ and 2023 GW identified 
    # 13.1 N of participants in the 38 trials of BMJ 
      d3<-subset(d, study_id2<39)
      sum(d3$n_randomized)
    # 13.2 of participants in the 28 trials we identified
      d4<-subset(d, study_id2>=39)
      sum(d4$n_randomized)

# Information about intervention doses need to use another dataset       
# Read dataset 
d<-read.csv("vitdpairs_2024.03.08.csv")
      
# 1. N of pairs and % by intervention dose 
# vitd_range= 1: intervention dose ≤600 IU/day
#             2: intervention dose >600 and ≤2000 IU/day
#             3: intervention dose >2000 IU/day 
#             4: bolus 
#             5: other 
d.f<-subset(d, dose_freq==1)          # create subset including pairs that gave vitD regularly 
d.f.1<-subset (d.f, control_type==1)  # create subset including pairs that gave vitD regularly and placebo control
d.f.2<-subset (d.f, control_type==2)  # create subset including pairs that gave vitD regularly and active control

table(d.f$vitd_range)                    # n of trials by intervention dose  
prop.table(table(d.f$vitd_range))*100    # % of trials by intervention dose  

table(d.f.1$vitd_range)                  # n of trials by intervention dose in placebo controls trials
prop.table(table(d.f.1$vitd_range))*100  # % of trials by intervention dose in placebo controls trials

table(d.f.2$vitd_range)                  # n of trials by intervention dose in active controls trials
prop.table(table(d.f.2$vitd_range))*100  # % of trials by intervention dose in active controls trials

# export xlsx to generate intervention dose for each pair 
d.dose <- select(d.f, study_id2, pair_id, study, year, 
                 control_dose, control_type, 
                 dose_freq, int1_dose, int2_dose, int3_dose, int4_dose, 
                 vitd_range)
write_xlsx(d.dose, "tab1_inter.dose_2023.12.05.xlsx")

# read the cleaned csv with intervention dose for each pair
d.inter.dose.pair<-read.csv("tab1_inter.dose_2023.12.05_cleaned.csv")
summary(d.inter.dose.pair$inter_dose) 

# summary of intervention dose for placebo control pairs 
d.inter.dose.pair.1<-subset(d.inter.dose.pair, control_type==1)
summary(d.inter.dose.pair.1$inter_dose)

# summary of intervention dose for active control pairs 
d.inter.dose.pair.2<-subset(d.inter.dose.pair, control_type==2)
summary(d.inter.dose.pair.2$inter_dose)

################################################################################  
#   II.1. Data management                                
################################################################################
# Read dataset 
d<-read.csv("vitd_2024.03.08.csv")

# Create new variables for subgroups using and add labels 
# Create inter_type2 and labeling           
d$inter_type2<-ifelse(d$inter_type==1, "Vitamin D alone versus placebo or no intervention",
                      ifelse(d$inter_type==2, "Vitamin D + Ca + vitamins + minerals versus Ca + vitamins + minerals",
                             ifelse(d$inter_type==3, "Vitamin D versus vitamin D of a lower dose", 
                             "Vitamin D + Ca + vitamins + minerals versus vitamin D of a lower dose + Ca + vitamins + minerals")))
d$inter_type2<-factor(d$inter_type2, 
                      levels=c("Vitamin D alone versus placebo or no intervention",
                               "Vitamin D + Ca + vitamins + minerals versus Ca + vitamins + minerals",
                               "Vitamin D versus vitamin D of a lower dose",
                               "Vitamin D + Ca + vitamins + minerals versus vitamin D of a lower dose + Ca + vitamins + minerals"))

# Create pop_type2 and labeling  
d$pop_type2<-ifelse(d$pop_type==1, "General population", "Population with morbidities")
d$pop_type2<-factor(d$pop_type2, levels=c("General population", "Population with morbidities"))

# Create dose_freq2 and labeling  
d$dose_freq2<-ifelse(d$dose_freq==1, "regular", 
                     ifelse(d$dose_freq==2, "bolus", 
                            ifelse(d$dose_freq==3, "unreported", "other")))
d$dose_freq2<-factor(d$dose_freq2, levels=c("regular", "bolus", "unreported", "other"))

# Create supp_form2 and labeling  
d$supp_form2<-ifelse(d$supp_form==3, "vitamin D3", 
                     ifelse(d$supp_form==2, "vitamin D2", "other"))
d$supp_form2<-factor(d$supp_form2, levels=c("vitamin D3", "vitamin D2", "other"))

# Create ini_trimester2 and labeling  
d$ini_trimester2<-ifelse(d$ini_trimester==1, "1st trimester",
                         ifelse(d$ini_trimester==2, "2nd trimester",
                                ifelse(d$ini_trimester==3, "3rd trimester", "other")))
d$ini_trimester2<-factor(d$ini_trimester2, levels=c("1st trimester", "2nd trimester", "3rd trimester", "other"))

# Create bl_vit30 (baseline vitamin D level >=30 or <30) and labeling  
d$bl_vitd30<-ifelse(d$baseline_vitd30==0, "Baseline vitamin D level <30 nmol/L",
                    ifelse(d$baseline_vitd30==1, "Baseline vitamin D level >=30 nmol/L", "unreported"))
d$bl_vitd30<-factor(d$bl_vitd30, levels=c("Baseline vitamin D level <30 nmol/L",
                                          "Baseline vitamin D level >=30 nmol/L", 
                                          "unreported"))

# Create bl_vit50 (baseline vitamin D level >=50 or <50) and labeling  
d$bl_vitd50<-ifelse(d$baseline_vitd50==0, "Baseline vitamin D level <50 nmol/L",
                    ifelse(d$baseline_vitd50==1, "Baseline vitamin D level >=50 nmol/L", "unreported"))
d$bl_vitd50<-factor(d$bl_vitd50, levels=c("Baseline vitamin D level <50 nmol/L",
                                          "Baseline vitamin D level >=50 nmol/L", 
                                          "unreported"))

################################################################################
#   II.2. Primary analysis, subgroup analysis for binary outcomes          
################################################################################
## List of binary outcomes                              abbreviation   output number  
#  I. Maternal morbidity 
#    1. Pre-eclampsia                                       pe                1
#    2. Gestational hypertension                            ght               2      
#    3. Gestational diabetes                                gdm               3
#    4. Preterm labor                                       PreLabor          4
#    5. Cesarean delivery                                   csection          5
#    6. Maternal hospitalization                            MatHosp           6
#    7. Maternal hypercalcemia                              MHypercalcem      7
#    8. Maternal hypocalcemia                               MHypocalcem       8
#    9. Maternal hypercalciuria                             MHypercalciu      9 
#  II. Birth Outcomes
#    1. Stillbirth (or fetal death)                         iud              10 
#    2. Low birthweight (<2500 g)                           lbw              11        
#    3. Preterm (<37 weeks)                                 PretermBirth     12
#    4. Small-for-gestational age (<10th percentile)        sga              13  
#    5. Congenital malformation                             CongMal          14 
#    6. Admission to NICU                                   nicu             15  
#  III. Infant outcomes 
#    1. Neonatal death                                      NeoDeath         16 
#    2. Neonatal hypercalcemia                              NHypercalcem     17  
#    3. Neonatal hypocalcemia                               NHypocalcem      18 
#    4. Respiratory infections                              RespInf          19 
#    5. Upper respiratory tract infections                  urti             20
#    6. Lower respiratory tract infections                  lrti             21  
#    7. Asthma or recurrent/persistent wheeze by 3 y/o      asthma           22 

#   For each outcome, the analysis includes at most 10 forest plots
#    1. Primary analysis 
#    2. Sensitivity analysis 
#    3. Subgroup analysis by intervention type                         inter_type2
#    4. Subgroup analysis by population type                           pop_type2 
#    5. Subgroup analysis by intervention dose                 
#    6. Subgroup analysis by frequency                                 dose_freq2
#    7. Subgroup analysis by vitamin D form                            supp_form2 
#    8. Subgroup analysis by trimester of supplementation initiation   ini_trimester2
#    9. Subgroup analysis by maternal vitamin D baseline cutoff 30     bl_vitd30
#    10.Subgroup analysis by maternal vitamin D baseline cutoff 50     bl_vitd50 

# 1. create list 
names<-list ("pe", "ght", "gdm", "PreLabor","csection","MatHosp","MHypercalcem", "MHypocalcem", "MHypercalciu",
             "iud", "lbw", "PretermBirth", "sga", "CongMal", "nicu",
             "NeoDeath", "NHypercalcem", "NHypocalcem", "RespInf", "urti", "lrti", "asthma")
ed<-list()    # the list indicating the var for n. of events in vitamin D group  
td<-list()    # the list indicating the var for n. of participants in vitamin D group        
ec<-list()    # the list indicating the var for n. of events in control  
tc<-list()    # the list indicating the var for n. of participants in control  
mcri<-list()  
output<-list()                   # the list indicating primary MA output
output.inter_type<-list()        # the list indicating primary MA by inter_type output   
output.pop_type<-list()          # the list indicating primary MA by pop_type output     
output.dose_freq<-list()         # the list indicating primary MA by dose_freq output
output.supp_form<-list()         # the list indicating primary MA by supp_form output
output.ini_trimester<-list()     # the list indicating primary MA by ini_trimester output
output.baseline_vitd30<-list()   # the list indicating primary MA by baseline_vitd30 output
output.baseline_vitd50<-list()   # the list indicating primary MA by baseline_vitd50 output

# 2. for loop for primary analysis of binary outcomes 
for(i in seq_along(names)){
  ed[[i]] <- paste(names[[i]], "_vitd_event", sep = "")
  td[[i]] <- paste(names[[i]], "_vitd_total", sep = "") 
  ec[[i]] <- paste(names[[i]], "_control_event", sep = "") 
  tc[[i]] <- paste(names[[i]], "_control_total", sep = "") 
  mcri[[i]] <- paste(names[[i]], "_mcri", sep = "") 
  
  # create sub-dataset for each outcome that definition meets criteria 
  data.pri<-subset(d, eval(parse(text = mcri[[i]])) == 1)
  
  # run MA for each outcome and assign them to list  
  output[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                         eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                         data = data.pri,
                         studlab = paste(study, year), 
                         sm = "RR", method = "Inverse", method.tau = "DL",
                         method.incr = "only0", incr = 0.5,
                         random = TRUE, fixed = FALSE)
  
  # run MA by inter_type for each outcome and assign them to list  
  output.inter_type[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                    eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                    data = data.pri,
                                    subgroup = data.pri$inter_type2,
                                    studlab = paste(study, year), 
                                    sm = "RR", method = "Inverse", method.tau = "DL",
                                    method.incr = "only0", incr = 0.5,
                                    random = TRUE, fixed = FALSE)
  
  # run MA by pop_type for each outcome and assign them to list  
  output.pop_type[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                  eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                  data = data.pri,
                                  subgroup = data.pri$pop_type2,
                                  studlab = paste(study, year), 
                                  sm = "RR", method = "Inverse", method.tau = "DL",
                                  method.incr = "only0", incr = 0.5,
                                  random = TRUE, fixed = FALSE)
  
  # run MA by dose_freq for each outcome and assign them to list 
  output.dose_freq[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                   eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                   data = data.pri,
                                   subgroup = data.pri$dose_freq2,
                                   studlab = paste(study, year), 
                                   sm = "RR", method = "Inverse", method.tau = "DL",
                                   method.incr = "only0", incr = 0.5,
                                   random = TRUE, fixed = FALSE)
  
  # run MA by supp_form for each outcome and assign them to list 
  output.supp_form[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                   eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                   data = data.pri,
                                   subgroup = data.pri$supp_form2,
                                   studlab = paste(study, year), 
                                   sm = "RR", method = "Inverse", method.tau = "DL",
                                   method.incr = "only0", incr = 0.5,
                                   random = TRUE, fixed = FALSE)
  
  # run MA by ini_trimester for each outcome and assign them to list
  output.ini_trimester[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                       eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                       data = data.pri,
                                       subgroup = data.pri$ini_trimester2,
                                       studlab = paste(study, year), 
                                       sm = "RR", method = "Inverse", method.tau = "DL",
                                       method.incr = "only0", incr = 0.5,
                                       random = TRUE, fixed = FALSE)
  
  # run MA by baseline vitamin D level cutoff 30 for each outcome and assign them to list
  output.baseline_vitd30[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                         eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                         data = data.pri,
                                         subgroup = data.pri$bl_vitd30,
                                         studlab = paste(study, year), 
                                         sm = "RR", method = "Inverse", method.tau = "DL",
                                         method.incr = "only0", incr = 0.5,
                                         random = TRUE, fixed = FALSE)
  
  # run MA by baseline vitamin D level cutoff 50 for each outcome and assign them to list
  output.baseline_vitd50[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                         eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                         data = data.pri,
                                         subgroup = data.pri$bl_vitd50,
                                         studlab = paste(study, year), 
                                         sm = "RR", method = "Inverse", method.tau = "DL",
                                         method.incr = "only0", incr = 0.5,
                                         random = TRUE, fixed = FALSE)}

# 3. Assign primary analysis results of binary outcomes
# Maternal outcome 
  pe.pri           <-output[[1]]
  ght.pri          <-output[[2]]
  gdm.pri          <-output[[3]]
  PreLabor.pri     <-output[[4]]
  csection.pri     <-output[[5]]
  MatHosp.pri      <-output[[6]]
  MHypercalcem.pri <-output[[7]]
  MHypocalcem.pri  <-output[[8]]
  MHypercalciu.pri <-output[[9]]
# Birth outcome 
  iud.pri          <-output[[10]]
  lbw.pri          <-output[[11]]
  PretermBirth.pri <-output[[12]]
  sga.pri          <-output[[13]]
  CongMal.pri      <-output[[14]]
  nicu.pri         <-output[[15]]
  NeoDeath.pri     <-output[[16]]
  NHypercalcem.pri <-output[[17]]
  NHypocalcem.pri  <-output[[18]]
# infant outcome
  RespInf.pri      <-output[[19]]
  urti.pri         <-output[[20]]
  lrti.pri         <-output[[21]]
  asthma.pri       <-output[[22]]

# 4. create forest plot function for binary outcome: forest.fun
forest.fun<- function(X){
             forest (X, leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
                    label.e = "Vitamin D", label.c = "Control", 
                    label.left = "favors vitamin D",label.right = "favors control",
                    col.diamond  = "dark blue",
                    sortvar = X$TE,
                    pooled.totals = TRUE, pooled.events = TRUE)}

# 5. generate forest plot using forest.fun
# Maternal outcome
  # PE 
    forest.fun(pe.pri)
    forest.fun(output.inter_type[[1]])
    forest.fun(output.pop_type[[1]])
    forest.fun(output.dose_freq[[1]])
    forest.fun(output.supp_form[[1]])
    forest.fun(output.ini_trimester[[1]])
    forest.fun(output.baseline_vitd30[[1]])
    forest.fun(output.baseline_vitd50[[1]])
  # GHTN 
    forest.fun(ght.pri)
    forest.fun(output.inter_type[[2]])
    forest.fun(output.pop_type[[2]])
    forest.fun(output.dose_freq[[2]])
    forest.fun(output.supp_form[[2]])
    forest.fun(output.ini_trimester[[2]])
    forest.fun(output.baseline_vitd30[[2]])
    forest.fun(output.baseline_vitd50[[2]])
  # GDM 
    forest.fun(gdm.pri) 
    forest.fun(output.inter_type[[3]])
    forest.fun(output.pop_type[[3]])
    forest.fun(output.dose_freq[[3]])
    forest.fun(output.supp_form[[3]])
    forest.fun(output.ini_trimester[[3]])
    forest.fun(output.baseline_vitd30[[3]])
    forest.fun(output.baseline_vitd50[[3]])
  # Preterm labor 
    forest.fun(PreLabor.pri)
    forest.fun(output.inter_type[[4]])
    forest.fun(output.pop_type[[4]])
    forest.fun(output.dose_freq[[4]])
    forest.fun(output.supp_form[[4]])
    forest.fun(output.ini_trimester[[4]])
    forest.fun(output.baseline_vitd30[[4]])
    forest.fun(output.baseline_vitd50[[4]])
  # Csection 
    forest.fun(csection.pri)
    forest.fun(output.inter_type[[5]])
    forest.fun(output.pop_type[[5]])
    forest.fun(output.dose_freq[[5]])
    forest.fun(output.supp_form[[5]])
    forest.fun(output.ini_trimester[[5]])
    forest.fun(output.baseline_vitd30[[5]])
    forest.fun(output.baseline_vitd50[[5]])
  # Maternal hospitalization 
    forest.fun(MatHosp.pri)
    forest.fun(output.inter_type[[6]])
    forest.fun(output.pop_type[[6]])
    forest.fun(output.dose_freq[[6]])
    forest.fun(output.supp_form[[6]])
    forest.fun(output.ini_trimester[[6]])
    forest.fun(output.baseline_vitd30[[6]])
    forest.fun(output.baseline_vitd50[[6]])
  # Maternal calcium status  
    forest.fun(MHypercalcem.pri)
    forest.fun(MHypocalcem.pri)
    forest.fun(MHypercalciu.pri)

# Birth outcome 
  # Intrauterine death or stillbirth (IUD)
    forest.fun(iud.pri)
    forest.fun(output.inter_type[[10]])
    forest.fun(output.pop_type[[10]])
    forest.fun(output.dose_freq[[10]])
    forest.fun(output.supp_form[[10]])
    forest.fun(output.ini_trimester[[10]])
    forest.fun(output.baseline_vitd30[[10]])
    forest.fun(output.baseline_vitd50[[10]])
  # LBW 
    forest.fun(lbw.pri) 
    forest.fun(output.inter_type[[11]])
    forest.fun(output.pop_type[[11]])
    forest.fun(output.dose_freq[[11]])
    forest.fun(output.supp_form[[11]])
    forest.fun(output.ini_trimester[[11]])
    forest.fun(output.baseline_vitd30[[11]])
    forest.fun(output.baseline_vitd50[[11]])
  # Preterm birth 
    forest.fun(PretermBirth.pri)
    forest.fun(output.inter_type[[12]])
    forest.fun(output.pop_type[[12]])
    forest.fun(output.dose_freq[[12]])
    forest.fun(output.supp_form[[12]])
    forest.fun(output.ini_trimester[[12]])
    forest.fun(output.baseline_vitd30[[12]])
    forest.fun(output.baseline_vitd50[[12]])
  # SGA 
    forest.fun(sga.pri)
    forest.fun(output.inter_type[[13]])
    forest.fun(output.pop_type[[13]])
    forest.fun(output.dose_freq[[13]])
    forest.fun(output.supp_form[[13]])
    forest.fun(output.ini_trimester[[13]])
    forest.fun(output.baseline_vitd30[[13]])
    forest.fun(output.baseline_vitd50[[13]])
  # Congenital malformation 
    forest.fun(CongMal.pri)
    forest.fun(output.inter_type[[14]])
    forest.fun(output.pop_type[[14]])
    forest.fun(output.dose_freq[[14]])
    forest.fun(output.supp_form[[14]])
    forest.fun(output.ini_trimester[[14]])
    forest.fun(output.baseline_vitd30[[14]])
    forest.fun(output.baseline_vitd50[[14]])
  # NICU admission 
    forest.fun(nicu.pri)
    forest.fun(output.inter_type[[15]])
    forest.fun(output.pop_type[[15]])
    forest.fun(output.dose_freq[[15]])
    forest.fun(output.supp_form[[15]])
    forest.fun(output.ini_trimester[[15]])
    forest.fun(output.baseline_vitd30[[15]])
    forest.fun(output.baseline_vitd50[[15]])

# Neonatal and infant outcome 
  # Neonatal death
    forest.fun(NeoDeath.pri)
    forest.fun(output.inter_type[[16]])
    forest.fun(output.pop_type[[16]])
    forest.fun(output.dose_freq[[16]])
    forest.fun(output.supp_form[[16]])
    forest.fun(output.ini_trimester[[16]])
    forest.fun(output.baseline_vitd30[[16]])
    forest.fun(output.baseline_vitd50[[16]])
  # Neonatal hypercalcemia
    forest.fun(NHypercalcem.pri)
    forest.fun(NHypocalcem.pri)
  # Respiratory tract infection
    forest.fun(RespInf.pri)
  # Upper respiratory tract infection 
    forest.fun(urti.pri)
  # Lower respiratory tract infection
    forest.fun(lrti.pri)
    forest.fun(output.inter_type[[21]])
    forest.fun(output.pop_type[[21]])
    forest.fun(output.dose_freq[[21]])
    forest.fun(output.supp_form[[21]])
    forest.fun(output.ini_trimester[[21]])
    forest.fun(output.baseline_vitd30[[21]])
    forest.fun(output.baseline_vitd50[[21]])
  # Asthma 
    forest.fun(asthma.pri)
  # NOTE:
  #      Get a subgroup analysis by a chacteristic Y for outcome variable X: output.Y[[the index of X]]
  #      Make a forestplot using the function forest.fun (output.Y[[the index of X]])
 
# 6. forest plot of subgroup analysis (labeling subgroups in forestplots, slightly different from 5)
  # 6.1 subgroup analysis by intervention type
    forest(output.inter_type[[1]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           print.subgroup.name = FALSE,
           sort.subgroup = FALSE,
           sortvar = output.inter_type[[1]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")
    # Note: replace [[N]] withe outcome number to obatin foresplot
  # loop function for 6.1 
    for (i in 1:22){
    forest(output.inter_type[[i]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           print.subgroup.name = FALSE,
           sortvar = output.inter_type[[i]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue", fontsize = 9)}
    
  # 6.2 subgroup analysis by population type
    forest(output.pop_type[[10]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors vitamin D",label.right = "favors control",
             print.subgroup.name = FALSE,
             sortvar = output.pop_type[[10]]$TE,
             pooled.totals = TRUE, pooled.events = TRUE,
             col.diamond  = "dark blue")
  # loop function for 6.2 
    for (i in 1:22){
    forest(output.pop_type[[i]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           print.subgroup.name = FALSE,
           sortvar = output.pop_type[[i]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")}

  # 6.3 subgroup analysis by dose frequency
    forest(output.dose_freq[[10]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           subgroup.name="Dose frequency",
           sep.subgroup = ": ",
           sortvar = output.pop_type[[10]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")
  # loop function for 6.3 
    for (i in 1:22){
    forest(output.dose_freq[[i]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           subgroup.name="Dose frequency",
           sep.subgroup = ": ",
           sortvar = output.pop_type[[i]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")}

  # 6.4 subgroup analysis by vitamin D form
    forest(output.supp_form[[10]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           subgroup.name="Supplement form",
           sep.subgroup = ": ",
           sortvar = output.supp_form[[10]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")
   # loop function for 6.4 
     for (i in 1:22){
     forest(output.supp_form[[i]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
            label.e = "Vitamin D", label.c = "Control", 
            label.left = "favors vitamin D",label.right = "favors control",
            subgroup.name="Supplement form",
            sep.subgroup = ": ",
            sortvar = output.supp_form[[i]]$TE,
            pooled.totals = TRUE, pooled.events = TRUE,
            col.diamond  = "dark blue")}

  # 6.5 subgroup analysis by trimester
    forest(output.ini_trimester[[10]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           subgroup.name="Supplementation initiation",
           sep.subgroup = ": ",
           sortvar = output.ini_trimester[[10]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
       col.diamond  = "dark blue")
  # loop function for 6.5 
    for (i in 1:22){
    forest(output.ini_trimester[[2]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           subgroup.name="Supplementation initiation",
           sep.subgroup = ": ",
           sortvar = output.ini_trimester[[2]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")}

  # 6.6 subgroup analysis by maternal vitamin D level (30)
    forest(output.baseline_vitd30[[10]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           print.subgroup.name = FALSE,
           sortvar = output.baseline_vitd30[[10]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")
  # loop function for 6.6 
    for (i in 1:22){
    forest(output.baseline_vitd30[[i]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           print.subgroup.name = FALSE,
           sortvar = output.baseline_vitd30[[i]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")}
  # 6.7 subgroup analysis by maternal vitamin D level (50)
    forest(output.baseline_vitd50[[10]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           print.subgroup.name = FALSE,
           sortvar = output.baseline_vitd50[[10]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")
  # loop function for 6.7
    for (i in 1:22){
    forest(output.baseline_vitd50[[i]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
           label.e = "Vitamin D", label.c = "Control", 
           label.left = "favors vitamin D",label.right = "favors control",
           print.subgroup.name = FALSE,
           sortvar = output.baseline_vitd50[[i]]$TE,
           pooled.totals = TRUE, pooled.events = TRUE,
           col.diamond  = "dark blue")} 

################################################################################
#   II.3  Subgroup analysis by health conditions for RR of GDM             
################################################################################
# I. Original health condition groupings
# pop_health coding                             
#   1 generally healthy;                      
#   2 GDM/GDM risk factors;                     
#   3 vitamin D def; 
#   4 vitamin D def + GDM risk factors; 
#   5 vitamin D def + PE risk factors; 
#   6 vitamin D def + hypocalcaemia; 
#   7 HIV
    
# create new var pop_health2 and labeling    
d$pop_health2<-ifelse(d$pop_health==1, "Generally healthy", 
                      ifelse(d$pop_health==2, "GDM/GDM risk factors",
                             ifelse(d$pop_health==3, "vitamin D deficiency",
                                    ifelse(d$pop_health==4, "vitamin D deficiency + GDM/GDM risk factors",
                                           ifelse(d$pop_health==5, "vitamin D deficiency + PE risk factors",
                                                  ifelse(d$pop_health==6, "vitamin D deficiency + hypocalcaemia","HIV"))))))
d$pop_health2<-factor(d$pop_health2, 
                      levels=c("Generally healthy", 
                               "GDM/GDM risk factors",
                               "vitamin D deficiency",
                               "vitamin D deficiency + GDM/GDM risk factors",
                               "vitamin D deficiency + PE risk factors",
                               "vitamin D deficiency + hypocalcaemia",
                               "HIV"))

# create sub dataset for GDM, subgroup analysis by health condition
d.gdm.sub<-subset(d, gdm_mcri==1)

# run meta-analysis 
gdm.sub<- metabin(gdm_vitd_event, gdm_vitd_total, 
          gdm_control_event, gdm_control_total, 
          data = d.gdm.sub,
          subgroup = d.gdm.sub$pop_health2,
          studlab = paste(study, year), 
          sm = "RR", method = "Inverse", method.tau = "DL",
          method.incr = "only0", incr = 0.5,
          random = TRUE, fixed = FALSE)
  
# create forest plot 
forest (gdm.sub, leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
        label.e = "Vitamin D", label.c = "Control", 
        label.left = "favors vitamin D",label.right = "favors control",
        print.subgroup.name = FALSE,
        sortvar = gdm.sub$TE,
        pooled.totals = TRUE, pooled.events = TRUE,
        col.diamond  = "dark blue" )

# II. alternative grouping that collapsed "GDM/GDM risk factor" and "vitamin D deficiency + GDM risk factors"
# alternative pop_health grouping                           
#   1 Generally healthy;                      
#   2 GDM/GDM risk factors;                     
#   3 vitamin D def; 
#   4 vitamin D def + PE risk factors; 
#   5 vitamin D def + hypocalcaemia; 
#   6 HIV

# create new var pop_health3 and labeling  
d$pop_health3<-ifelse(d$pop_health==1, "Generally healthy", 
                      ifelse(d$pop_health==2, "GDM/GDM risk factors",
                             ifelse(d$pop_health==3, "vitamin D deficiency",
                                    ifelse(d$pop_health==4, "GDM/GDM risk factors",
                                           ifelse(d$pop_health==5, "vitamin D deficiency + PE risk factors",
                                                  ifelse(d$pop_health==6, "vitamin D deficiency + hypocalcaemia", 
                                                         "HIV"))))))

d$pop_health3<-factor(d$pop_health3, 
                      levels=c("Generally healthy", 
                               "GDM/GDM risk factors",
                               "vitamin D deficiency",
                               "vitamin D deficiency + PE risk factors",
                               "vitamin D deficiency + hypocalcaemia",
                               "HIV"))

# create a subset for GDM, subgroup analysis by health condition
d.gdm.sub<-subset(d,gdm_mcri==1)

# run meta-analysis 
gdm.sub.2<- metabin(gdm_vitd_event, gdm_vitd_total, 
                  gdm_control_event, gdm_control_total, 
                  data = d.gdm.sub,
                  subgroup = d.gdm.sub$pop_health3,
                  studlab = paste(study, year), 
                  sm = "RR", method = "Inverse", method.tau = "DL",
                  method.incr = "only0", incr = 0.5,
                  random = TRUE, fixed = FALSE)

# create forest plot 
forest ( gdm.sub.2, leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
         label.e = "Vitamin D", label.c = "Control", 
         label.left = "favors vitamin D",label.right = "favors control",
         print.subgroup.name = FALSE,
         sortvar = gdm.sub.2$TE,
         pooled.totals = TRUE, pooled.events = TRUE,
         col.diamond  = "dark blue" )

################################################################################
#   II.4  Sensitivity analysis for binary outcomes                    
################################################################################
# 1. create empty list to store sensitivity MA output
output.sen<-list() 

# 2. create for loop for meta-analysis of binary outcomes, sensitivity analysis
for(i in seq_along(names)) {
    ed[[i]] <- paste(names[[i]], "_vitd_event", sep = "")
    td[[i]] <- paste(names[[i]], "_vitd_total", sep = "") 
    ec[[i]] <- paste(names[[i]], "_control_event", sep = "") 
    tc[[i]] <- paste(names[[i]], "_control_total", sep = "") 
    mcri[[i]] <- paste(names[[i]], "_mcri", sep = "") 
    
    # create sub-dataset for sensitivity analysis of binary outcomes 
    data.sen<-subset(d, eval(parse(text = mcri[[i]])) == 1|eval(parse(text = mcri[[i]])) == 0)
    
    # run sensitivity analysis for each outcome and assign them in list 
    output.sen[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                               eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                               data = data.sen,
                               studlab = paste(study, year), 
                               method = "Inverse", sm = "RR", method.tau = "DL",
                               method.incr = "only0", incr = 0.5,
                               random = TRUE, fixed = FALSE)} 

# 3. assign sensitivity analysis results of binary outcomes
pe.sen           <-output.sen[[1]]
ght.sen          <-output.sen[[2]]
gdm.sen          <-output.sen[[3]]
PreLabor.sen     <-output.sen[[4]]
csection.sen     <-output.sen[[5]]
MatHosp.sen      <-output.sen[[6]]
MHypercalcem.sen <-output.sen[[7]]
MHypocalcem.sen  <-output.sen[[8]]
MHypercalciu.sen <-output.sen[[9]]
iud.sen          <-output.sen[[10]]
lbw.sen          <-output.sen[[11]] 
PretermBirth.sen <-output.sen[[12]]
sga.sen          <-output.sen[[13]]
CongMal.sen      <-output.sen[[14]]
nicu.sen         <-output.sen[[15]]
NeoDeath.sen     <-output.sen[[16]]
NHypercalcem.sen <-output.sen[[17]]
NHypocalcem.sen  <-output.sen[[18]]
RespInf.sen      <-output.sen[[19]]
urti.sen         <-output.sen[[20]]
lrti.sen         <-output.sen[[21]]
asthma.sen       <-output.sen[[22]]

# 4. generate forest plot using forest.fun 
forest.fun(pe.sen)
forest.fun(ght.sen)
forest.fun(gdm.sen) 
forest.fun(PreLabor.sen)     # no sensitivity analysis, same as primary analysis 
forest.fun(csection.sen)     # no sensitivity analysis, same as primary analysis 
forest.fun(MatHosp.sen)      # no sensitivity analysis, same as primary analysis
forest.fun(MHypercalcem.sen) 
forest.fun(MHypocalcem.sen)
forest.fun(MHypercalciu.sen)
forest.fun(iud.sen)          # no sensitivity analysis, same as primary analysis 
forest.fun(lbw.sen)          # no sensitivity analysis, same as primary analysis 
forest.fun(PretermBirth.sen) # no sensitivity analysis, same as primary analysis 
forest.fun(sga.sen)          # no sensitivity analysis, same as primary analysis 
forest.fun(CongMal.sen)      # no sensitivity analysis, same as primary analysis 
forest.fun(nicu.sen)         # no sensitivity analysis, same as primary analysis   
forest.fun(NeoDeath.sen)     # no sensitivity analysis, same as primary analysis 
forest.fun(NHypercalcem.sen)
forest.fun(NHypocalcem.sen)
forest.fun(RespInf.sen)      # no sensitivity analysis, same as primary analysis 
forest.fun(urti.sen)         # no sensitivity analysis, same as primary analysis 
forest.fun(lrti.sen)         # no sensitivity analysis, same as primary analysis 
forest.fun(asthma.sen)

################################################################################
#   II.5  Primary analysis, subgroup analysis for continuous variables       
################################################################################
## List of continuous outcomes                        abbreviation  output number  
#  I. Maternal morbidity 
#    1. maternal 25(OH)D level (at or near delivery)      delivery             1
#  II. Birth Outcomes
#    1. birth body weight                                 bw                   2  
#    2. birth body length                                 bl                   3 
#    3. birth head circumference                          hc                   4 
#    4. gestational age                                   ga                   5
#    5. cord 25(OH)D D level                              cord                 6 
#  III. Infant Outcomes
#    1. body weight at 1 year old                         wei                  7 
#    2. body length at 1 year old                         len                  8 
#    3. head circumference at 1 year old                  hc1                  9 
#    4. weight for age z score at 1 year old              waz                 10 
#    5. length for age z score at 1 year old              laz                 11
#    6. head circumference for age z score at 1 year old  hcaz                12 
#    7. neonatal bone mineral content                     nbmc                13 
#    8. neonatal bone mineral density                     nbmd                14 
#    9. infant bone mineral content                       ibmc                15 
#    10. infant bone mineral density                      ibmd                16 

# 1. create empty lists
names.c<-list("delivery",  "bw",    "bl",   "hc",   "ga",
              "cord",      "wei",   "len",  "hc1",  "waz", 
              "laz",        "hcaz", "nbmc", "nbmd", "ibmc", "ibmd") # the list indicating continuous outcomes
n.d<-list()      # the list indicating the var for n. of participants in vitamin D group  
m.d<-list()      # the list indicating the var for mean in vitamin D group        
sd.d<-list()     # the list indicating the var for sd in vitamin D group
n.c<-list()      # the list indicating the var for n. of participants in control group  
m.c<-list()      # the list indicating the var for mean in control group 
sd.c<-list()     # the list indicating the var for sd in control group
mcri.c<-list()                     # the list indicating for each outcome, meeting outcome definitions or not
output.c<-list()                   # the list indicating MA output for continuous outcomes       
output.c.inter_type<-list()        # the list indicating MA for continuous outcomes by inter_type2 output   
output.c.pop_type<-list()          # the list indicating MA for continuous outcomes by pop_type2 output     
output.c.dose_freq<-list()         # the list indicating MA for continuous outcomes by dose_freq2 output
output.c.supp_form<-list()         # the list indicating MA for continuous outcomes by supp_form2 output
output.c.ini_trimester<-list()     # the list indicating MA for continuous outcomes by ini_trimester2 output
output.c.baseline_vitd30<-list()   # the list indicating MA for continuous outcomes by bl_vitd30 output
output.c.baseline_vitd50<-list()   # the list indicating MA for continuous outcomes by bl_vitd50 output

# 2. create for loop for meta-analysis of binary outcomes primary analysis
for(i in seq_along(names.c)) {
    n.d[[i]]  <- paste(names.c[[i]], "_vitd_n", sep = "")
    m.d[[i]]  <- paste(names.c[[i]], "_vitd_mean", sep = "") 
    sd.d[[i]] <- paste(names.c[[i]], "_vitd_sd", sep = "") 
    n.c[[i]]  <- paste(names.c[[i]], "_control_n", sep = "")
    m.c[[i]]  <- paste(names.c[[i]], "_control_mean", sep = "")
    sd.c[[i]] <- paste(names.c[[i]], "_control_sd", sep = "") 
    mcri.c[[i]]  <- paste(names.c[[i]], "_mcri", sep = "")
  
  # create sub-dataset for each outcome that meets criteria 
    data.c<-subset(d, eval(parse(text = mcri.c[[i]])) == 1)
  
  # run MA for each outcome and assign them to list  
    output.c[[i]] <- metacont(n.e    = eval(parse(text = n.d[[i]])), 
                              mean.e = eval(parse(text = m.d[[i]])), 
                              sd.e   = eval(parse(text = sd.d[[i]])),
                              n.c    = eval(parse(text = n.c[[i]])), 
                              mean.c = eval(parse(text = m.c[[i]])), 
                              sd.c   = eval(parse(text = sd.c[[i]])),
                              studlab = paste(study, year), 
                              data = data.c,
                              sm = "MD", method.tau= "DL", method.ci= "z", 
                              random = TRUE, fixed = FALSE)

  # run MA by inter_type for each outcome and assign them to list 
    output.c.inter_type[[i]]<- metacont(n.e    = eval(parse(text = n.d[[i]])), 
                                        mean.e = eval(parse(text = m.d[[i]])), 
                                        sd.e   = eval(parse(text = sd.d[[i]])),
                                        n.c    = eval(parse(text = n.c[[i]])), 
                                        mean.c = eval(parse(text = m.c[[i]])), 
                                        sd.c   = eval(parse(text = sd.c[[i]])),
                                        studlab = paste(study, year), 
                                        data = data.c,
                                        subgroup = data.c$inter_type2,
                                        sm = "MD", method.tau= "DL",method.ci= "z", 
                                        random = TRUE, fixed = FALSE)
  
  # run MA by pop_type for each outcome and assign them to list 
    output.c.pop_type[[i]]<- metacont (n.e    = eval(parse(text = n.d[[i]])), 
                                       mean.e = eval(parse(text = m.d[[i]])), 
                                       sd.e   = eval(parse(text = sd.d[[i]])),
                                       n.c    = eval(parse(text = n.c[[i]])), 
                                       mean.c = eval(parse(text = m.c[[i]])), 
                                       sd.c   = eval(parse(text = sd.c[[i]])),
                                       studlab = paste(study, year), 
                                       data = data.c,
                                       subgroup = data.c$pop_type2,
                                       sm = "MD", method.tau= "DL", method.ci= "z",
                                       random = TRUE, fixed = FALSE)
  
  # run MA by dose_freq for each outcome and assign them to list 
  output.c.dose_freq[[i]]<- metacont (n.e    = eval(parse(text = n.d[[i]])), 
                                      mean.e = eval(parse(text = m.d[[i]])), 
                                      sd.e   = eval(parse(text = sd.d[[i]])),
                                      n.c    = eval(parse(text = n.c[[i]])), 
                                      mean.c = eval(parse(text = m.c[[i]])), 
                                      sd.c   = eval(parse(text = sd.c[[i]])),
                                      studlab = paste(study, year), 
                                      data = data.c,
                                      subgroup = data.c$dose_freq2,
                                      sm = "MD", method.tau= "DL", method.ci= "z", 
                                      random = TRUE, fixed = FALSE)
  
  # run MA by inter_type for each outcome and assign them to list 
  output.c.supp_form[[i]]<- metacont (n.e    = eval(parse(text = n.d[[i]])), 
                                      mean.e = eval(parse(text = m.d[[i]])), 
                                      sd.e   = eval(parse(text = sd.d[[i]])),
                                      n.c    = eval(parse(text = n.c[[i]])), 
                                      mean.c = eval(parse(text = m.c[[i]])), 
                                      sd.c   = eval(parse(text = sd.c[[i]])),
                                      studlab = paste(study, year), 
                                      data = data.c,
                                      subgroup = data.c$supp_form2,
                                      sm = "MD", method.tau= "DL", method.ci= "z",
                                      random = TRUE, fixed = FALSE)
  
  # run MA by ini_trimester for each outcome and assign them to list 
  output.c.ini_trimester[[i]]<- metacont(n.e    =eval(parse(text = n.d[[i]])), 
                                         mean.e = eval(parse(text = m.d[[i]])), 
                                         sd.e   = eval(parse(text = sd.d[[i]])),
                                         n.c    = eval(parse(text = n.c[[i]])), 
                                         mean.c = eval(parse(text = m.c[[i]])), 
                                         sd.c   = eval(parse(text = sd.c[[i]])),
                                         studlab = paste(study, year), 
                                         data = data.c,
                                         subgroup = data.c$ini_trimester2,
                                         sm = "MD", method.tau= "DL", method.ci= "z",
                                         random = TRUE, fixed = FALSE)
  
  # run MA by baseline vitamin D level cutoff 30 each outcome and assign them to list 
  output.c.baseline_vitd30[[i]]<- metacont(n.e    = eval(parse(text = n.d[[i]])), 
                                           mean.e = eval(parse(text = m.d[[i]])), 
                                           sd.e   = eval(parse(text = sd.d[[i]])),
                                           n.c    = eval(parse(text = n.c[[i]])), 
                                           mean.c = eval(parse(text = m.c[[i]])), 
                                           sd.c   = eval(parse(text = sd.c[[i]])),
                                           studlab = paste(study, year), 
                                           data = data.c,
                                           subgroup = data.c$bl_vitd30,
                                           sm = "MD", method.tau= "DL", method.ci= "z",
                                           random = TRUE, fixed = FALSE)
  
  # run MA by baseline vitamin D level cutoff 50 each outcome and assign them to list 
  output.c.baseline_vitd50[[i]]<- metacont(n.e    = eval(parse(text = n.d[[i]])), 
                                           mean.e = eval(parse(text = m.d[[i]])), 
                                           sd.e   = eval(parse(text = sd.d[[i]])),
                                           n.c    = eval(parse(text = n.c[[i]])), 
                                           mean.c = eval(parse(text = m.c[[i]])), 
                                           sd.c   = eval(parse(text = sd.c[[i]])),
                                           studlab = paste(study, year), 
                                           data = data.c,
                                           subgroup = data.c$bl_vitd50,
                                           sm = "MD", method.tau= "DL", method.ci= "z",
                                           random = TRUE, fixed = FALSE)}

# 3. assign MA primary analysis results of binary outcomes
delivery.pri<-output.c[[1]]
bw.pri<-output.c[[2]]
bl.pri<-output.c[[3]]
hc.pri<-output.c[[4]]
ga.pri<-output.c[[5]]
cord.pri<-output.c[[6]]
wei.pri<-output.c[[7]]
len.pri<-output.c[[8]]
hc1.pri<-output.c[[9]]
waz.pri<-output.c[[10]]
laz.pri<-output.c[[11]]
hcaz.pri<-output.c[[12]]
nbmc.pri<-output.c[[13]]
nbmd.pri<-output.c[[14]]
ibmc.pri<-output.c[[15]]
ibmd.pri<-output.c[[16]]

# 4. create forest plot function for continuous outcome, forest.fun.c 
forest.fun.c<-function(X) {
              forest(X, leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
              label.e = "Vitamin D", label.c = "Control", 
              label.left = "favors control",label.right = "favors vitamin D",
              col.diamond  = "dark blue", 
              digits.mean=1, digits.sd=1, 
              sortvar = X$TE,
              pooled.totals = TRUE, pooled.events = TRUE)} 

# 5. generate forest plot using forest.fun.c
forest.fun.c(delivery.pri) 
forest.fun.c(bw.pri)
forest.fun.c(bl.pri)
forest.fun.c(hc.pri)
forest.fun.c(ga.pri)
forest.fun.c(cord.pri)
forest.fun.c(wei.pri)
forest.fun.c(len.pri)
forest.fun.c(hc1.pri)
forest.fun.c(waz.pri)
forest.fun.c(laz.pri)
forest.fun.c(hcaz.pri)
forest.fun.c(nbmc.pri)
forest.fun.c(nbmd.pri)
forest.fun.c(ibmc.pri)
forest.fun.c(ibmd.pri)

# 6. subgroup analysis
# 6.1 subgroup analysis by intervention type
      forest(output.c.inter_type[[1]], leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors control",label.right = "favors vitamin D",
             print.subgroup.name = FALSE,
             sort.subgroup = FALSE,
             sortvar = output.c.inter_type[[1]]$TE,
             digits.mean=0, digits.sd=0, 
             pooled.totals = TRUE, pooled.events = TRUE,
             col.diamond  = "dark blue", fontsize=11.5, spacing=0.9)
# note: replace [[N]] N with continuous outcome number to obtain forestplot 
      
# 6.2 subgroup analysis by population type
      forest(output.c.pop_type[[1]], leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors control",label.right = "favors vitamin D",
             print.subgroup.name = FALSE,
             sort.subgroup = FALSE,
             sortvar = output.c.pop_type[[1]]$TE,
             digits.mean=1, digits.sd=1, 
             pooled.totals = TRUE, pooled.events = TRUE,
             col.diamond  = "dark blue", fontsize=11.5, spacing=0.9)

# 6.3 subgroup analysis by dose frequency
      forest(output.c.dose_freq[[1]], leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors control",label.right = "favors vitamin D",
             subgroup.name="Dose frequency",
             sep.subgroup = ": ",
             sort.subgroup = FALSE,
             sortvar = output.c.dose_freq[[1]]$TE,
             digits.mean=1, digits.sd=1, 
             pooled.totals = TRUE, pooled.events = TRUE,
             col.diamond  = "dark blue", fontsize=11.5, spacing=0.9)

# 6.4 subgroup analysis by vitamin D form
      forest(output.c.supp_form[[1]], leftcols =  c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors control",label.right = "favors vitamin D",
             subgroup.name="Supplement form",
             sep.subgroup = ": ",
             sort.subgroup = FALSE,
             sortvar = output.c.supp_form[[1]]$TE,
             digits.mean=1, digits.sd=1, 
             pooled.totals = TRUE, pooled.events = TRUE,
             col.diamond  = "dark blue", fontsize=11.5, spacing=0.9)

# 6.5 subgroup analysis by trimester
      forest(output.c.ini_trimester[[1]], leftcols =  c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors control",label.right = "favors vitamin D",
             subgroup.name="Supplementation initiation",
             sep.subgroup = ": ",
             sortvar = output.c.ini_trimester[[1]]$TE,
             pooled.totals = TRUE, pooled.events = TRUE,
             digits.mean=1, digits.sd=1, 
             col.diamond  = "dark blue", fontsize=11.5, spacing=0.9)

# 6.6 subgroup analysis by maternal vitamin D level (30)
      forest(output.c.baseline_vitd30[[1]], leftcols =  c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors control",label.right = "favors vitamin D",
             print.subgroup.name = FALSE,
             sortvar = output.c.baseline_vitd30[[1]]$TE,
             pooled.totals = TRUE, pooled.events = TRUE,
             digits.mean=1, digits.sd=1, 
             col.diamond  = "dark blue", fontsize=11.5, spacing=0.9)

# 6.7 subgroup analysis by maternal vitamin D level (50)
      forest(output.c.baseline_vitd50[[1]], leftcols =  c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             label.e = "Vitamin D", label.c = "Control", 
             label.left = "favors control",label.right = "favors vitamin D",
             print.subgroup.name = FALSE,
             sortvar = output.c.baseline_vitd50[[1]]$TE,
             pooled.totals = TRUE, pooled.events = TRUE,
             digits.mean=1, digits.sd=1, 
             col.diamond  = "dark blue", fontsize=11.5, spacing=0.9)

################################################################################
#   III.1 Subgroup analysis by intervention dose for binary variables         
################################################################################
# Read dataset
d<-read.csv("vitdpairs_2024.03.08.csv")

#re-label subgroups 
d$vitd_range2 <- ifelse(d$vitd_range==1, "Vitamin D dose <= 600 IU/day",
                        ifelse(d$vitd_range==2, "Vitamin D dose > 600, <= 2000 IU/day",
                               ifelse(d$vitd_range==3, "Vitamin D dose > 2000 IU/day", "other")))
d$vitd_range2 <- factor(d$vitd_range2, levels=c("Vitamin D dose <= 600 IU/day",
                                                "Vitamin D dose > 600, <= 2000 IU/day",
                                                "Vitamin D dose > 2000 IU/day",
                                                "other"))

# 1. create list 
names<-list("pe", "ght", "gdm", "PreLabor","csection","MatHosp","MHypercalcem", "MHypocalcem", "MHypercalciu",
            "iud", "lbw", "PretermBirth", "sga", "CongMal", "nicu",
            "NeoDeath", "NHypercalcem", "NHypocalcem", "RespInf", "urti", "lrti", "asthma")
ed<-list()    # the list indicating the var for n. of events in vitamin D group  
td<-list()    # the list indicating the var for n. of participants in vitamin D group        
ec<-list()    # the list indicating the var for n. of events in control  
tc<-list()    # the list indicating the var for n. of participants in control  
mcri<-list()  # the list indicating for each outcome, meeting outcome definitions or not 
output.vitd_range<-list()  # the list indicating subgroup analysis by vitamin d dose range   

# 2. for loop for subgroup analysis of binary outcomes by intervention dose  
for(i in seq_along(names)){
    ed[[i]] <- paste(names[[i]], "_vitd_event", sep = "")
    td[[i]] <- paste(names[[i]], "_vitd_total", sep = "") 
    ec[[i]] <- paste(names[[i]], "_control_event", sep = "") 
    tc[[i]] <- paste(names[[i]], "_control_total", sep = "") 
    mcri[[i]] <- paste(names[[i]], "_mcri", sep = "") 
    
  # create sub-dataset for each outcome that definition meets criteria 
  data.pri<-subset(d, eval(parse(text = mcri[[i]])) == 1)
  
  # run MA by intervention dose for each outcome and assign them to list  
  output.vitd_range[[i]] <- metabin(eval(parse(text = ed[[i]])), eval(parse(text = td[[i]])),
                                    eval(parse(text = ec[[i]])), eval(parse(text = tc[[i]])),
                                    data = data.pri,
                                    subgroup = data.pri$vitd_range2,
                                    studlab = paste(study, year), 
                                    sm = "RR", method = "Inverse", method.tau = "DL",
                                    method.incr = "only0", incr = 0.5,
                                    random = TRUE, fixed = FALSE)}

# 3. forest plot of subgroup analysis by intervention dose
forest(output.vitd_range[[1]], leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
       label.e = "Vitamin D", label.c = "Control", 
       label.left = "favors vitamin D",label.right = "favors control",
       print.subgroup.name = FALSE,
       sortvar = output.vitd_range[[1]]$TE,
       pooled.totals = TRUE, pooled.events = TRUE,
       col.diamond  = "dark blue")
# [[N]] replace N with binary outcome number

################################################################################
#   III.2 Subgroup analysis by intervention dose for continuous variables         
################################################################################
# 1. create list 
names.c<-list("delivery", "bw",   "bl",  "hc",  "ga", 
              "cord",     "wei",  "len", "hc1", "waz", "laz")
n.vitd<-list()      # the list indicating the var for n. of participants in vitamin D group  
m.vitd<-list()      # the list indicating the var for mean in vitamin D group        
sd.vitd<-list()     # the list indicating the var for sd in vitamin D group
n.c<-list()         # the list indicating the var for n. of participants in control group  
m.c<-list()         # the list indicating the var for mean in control group 
sd.c<-list()        # the list indicating the var for sd in control group
mcri.c<-list()      # the list indicating for each outcome, meeting outcome definitions or not
output.c.vitd_range<-list()        # the list indicating MA for continuous outcomes by intervention dose

# 2. create for loop for meta-analysis of binary outcomes primary analysis
for(i in seq_along(names.c)) {
    n.vitd[[i]]  <- paste(names.c[[i]], "_vitd_n", sep = "")
    m.vitd[[i]]  <- paste(names.c[[i]], "_vitd_mean", sep = "") 
    sd.vitd[[i]] <- paste(names.c[[i]], "_vitd_sd", sep = "") 
    n.c[[i]]  <- paste(names.c[[i]], "_control_n", sep = "")
    m.c[[i]]  <- paste(names.c[[i]], "_control_mean", sep = "")
    sd.c[[i]] <- paste(names.c[[i]], "_control_sd", sep = "") 
    mcri.c[[i]]  <- paste(names.c[[i]], "_mcri", sep = "")
    
  # create sub-dataset for each outcome that meets criteria 
  data.c<-subset(d, eval(parse(text = mcri.c[[i]])) == 1)
  
  # run MA by vitd_range for each outcome and assign them to list 
  output.c.vitd_range[[i]]<- metacont(n.e    = eval(parse(text = n.vitd[[i]])), 
                                      mean.e = eval(parse(text = m.vitd[[i]])), 
                                      sd.e   = eval(parse(text = sd.vitd[[i]])),
                                      n.c    = eval(parse(text = n.c[[i]])), 
                                      mean.c = eval(parse(text = m.c[[i]])), 
                                      sd.c   = eval(parse(text = sd.c[[i]])),
                                      studlab = paste(study, year), 
                                      data = data.c,
                                      subgroup = data.c$vitd_range2,
                                      sm = "MD", method.tau= "DL",method.ci= "z", 
                                      random = TRUE, fixed = FALSE)}
# 3. forest plot 
forest(output.c.vitd_range[[1]], leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
       label.e = "Vitamin D", label.c = "Control", 
       label.left = "favors control",label.right = "favors vitamin D",
       print.subgroup.name = FALSE,
       sort.subgroup = FALSE,
       sortvar = output.c.vitd_range[[1]]$TE,
       digits.mean= 1, digits.sd= 1, 
       pooled.totals = TRUE, pooled.events = TRUE,
       col.diamond  = "dark blue", fontsize=11, spacing=0.9)
# [[N]] replace N with continuous outcome number

################################################################################
#   IV.1. ROB traffic light figure 
################################################################################
# Read dataset 
setwd("C:/Users/wench/Desktop/21 GW/Smith Lab/vitamin D review/5. data/4. data analylsis/2023.12.03_66 trials/2023.12.03 dataset_66 trials/2024.03.08 version_for revision")
d<-read.csv("vitd_2024.03.08.csv") 

# Create a new var studyyear to paste study and year
d$studyyear<-paste(d$study, d$year)

# Create data subset for traffic light figure, rename variables 
d_rob<-select(d, studyyear, rob_seq_gen, rob_alloc_conc, rob_blind_part, rob_blind_outcome, 
              rob_incomp_outcome, rob_select_report, rob_other_bias) 
colnames(d_rob)<-c( "Study",
                    "Random sequence generation",
                    "Allocation concealment",
                    "Blinding of participants and personnel",
                    "Blinding of outcome assessment",
                    "Incomplete outcome data",
                    "Selective reporting",
                    "Other bias")

# Define the desired order of domains in reverse
domain_order <- rev( c( "Random sequence generation",
                        "Allocation concealment",
                        "Blinding of participants and personnel",
                        "Blinding of outcome assessment",
                        "Incomplete outcome data",
                        "Selective reporting",
                        "Other bias"))

# Defined the desired order of study
study_order<-c("Brooke 1980",             "Delvin 1986",      "Mallet 1986",      "Marya 1988",       "Kaur 1991",       
               "Sahu 2009",               "Yu 2009",          "Hollis 2011",      "Sabet 2012",       "Dawodu 2013",     
               "Hashemipour 2013",        "Roth 2013",        "Soheilykhah 2013", "Grant 2014",       "Hossain 2014",    
               "Mutlu 2014",              "Yap 2014",         "Charandabi 2015",  "March 2015",       "Mojibian 2015",    
               "Rodda 2015",              "Sablok 2015",      "Chawes 2016",      "Cooper 2016",      "Khan 2016",        
               "Litonjua 2016",           "Naghshineh 2016",  "Sahoo 2016",       "Shahgheibi 2016",  "Thiele 2016",      
               "Valizadeh 2016",          "Vaziri 2016",      "Yazdchi 2016",     "Zerofsky 2016",    "Abotorabi 2017", 
               "Samimi 2017",             "Sasan 2017",       "Tehrani 2017",     "Azadehrah 2017",   "Keshavarzi 2017",
               "Best 2018",               "O’Callaghan 2018", "Roth 2018",        "Ali 2019",         "Enkhmaa 2019",     
               "Jefferson 2019",          "Zhao 2019",        "Corcoy 2020",      "Rahbar 2020",      "Sircar 2020",      
               "Alhomaid 2021",           "Manasova 2021",    "Mohammadi 2021",   "Nausheen 2021",    "Srilekha 2021",    
               "Xiaomang 2021",           "Aggarwal 2022",    "Memon 2022",       "Pulido 2022",      "Sudfeld 2022",
               "Mirzaei-Azandaryani 2022","Vanda 2022",       "Cagiran 2023",     "Ma 2023",          "Nadeem 2023",      
               "Sunarno 2023")

# Transform dataset from wide to long format 
d_long <- pivot_longer(d_rob, cols = c( "Random sequence generation",
                                        "Allocation concealment",
                                        "Blinding of participants and personnel",
                                        "Blinding of outcome assessment",
                                        "Incomplete outcome data",
                                        "Selective reporting",
                                        "Other bias"),
                       names_to = "domain",
                       values_to = "ROB")

# Convert the domain column to an ordered factor with the reversed order
d_long$domain <- factor(d_long$domain, levels = domain_order, ordered = TRUE)
d_long$Study <- factor(d_long$Study, levels = study_order)

# Create traffic light figure using ggplot 
ggplot(d_long, aes(x = Study, y = domain, color = as.factor(ROB))) +
  geom_point() +
  scale_color_manual(values = c("red", "green2", "yellow2"),
                     breaks = c(1, 2, 3),
                     labels = c("High risk", "Low risk", "Unclear risk")) +
  theme(axis.text.x = element_text(family = "Arial", size = 7, color="gray20", angle = 90, hjust = 1, vjust=0.5),
        axis.text.y = element_text(family = "Arial", size = 8, color="gray20"), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        
        panel.background = element_rect(fill = "white", color = "gray90"),
        panel.grid.major = element_line(color = NULL),
        
        legend.text = element_text(family = "Arial", size = 8, color="gray20"),  
        legend.title = element_text(family = "Arial", size = 8, color="gray20"))+
  coord_equal()+
  labs(x = "", y = "", color = "ROB assessment")

################################################################################
#   IV.2. Funnel plots
################################################################################
# Create funnel plots for outcomes with more than 10 trials 

# 1. Binary outcomes  
# 1.1 GDM 
funnel.meta(gdm.pri,
            xlim=c (0.09,5),
            xlab = "Risk ratio of gestational diabetes",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(gdm.pri, method.bias = "Egger")

# 1.2 C-section  
funnel.meta(csection.pri,
            xlim=c (0.3,3.7),
            xlab = "Risk ratio of Cesarean delivery",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(csection.pri, method.bias = "Egger")

# 1.3 stillbirth or intrauterine death 
funnel.meta(iud.pri,
            xlim=c (0.04, 18),
            xlab = "Risk ratio of stillbirth or intrauterine death",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(iud.pri, method.bias = "Egger")

# 1.4 LBW infants 
funnel.meta(lbw.pri,
            xlim=c (0.04, 20),
            xlab = "Risk ratio of low birthweight infants",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(lbw.pri, method.bias = "Egger")

# 1.5 Preterm birth
funnel.meta(PretermBirth.pri,
            xlim=c (0.05,20),
            xlab = "Risk ratio of preterm birth",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(PretermBirth.pri, method.bias = "Egger")

# 2. continuous outcomes 
# 2.1 maternal 25(OH)D at delivery 
funnel.meta(delivery.pri,
            xlim = c(-15, 80),
            xlab = "Mean difference of maternal 25(OH)D concentration near or at delivery (nmol/L)",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(delivery.pri, method.bias = "Egger")

# 2.2 gestational age 
funnel.meta(ga.pri,
            xlim = c(-2.0, 2.0),
            xlab = "Mean difference of gestational age (weeks)",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(ga.pri, method.bias = "Egger")

# 2.3 birth weight 
funnel.meta(bw.pri,
            xlim = c(-500, 580),
            xlab = "Mean difference of birthweight (gm)",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(bw.pri, method.bias = "Egger")

# 2.4 birth length 
funnel.meta(bl.pri,
            xlim = c(-2.5, 3.0), 
            xlab = "Mean difference of birth body length (cm)",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(bl.pri, method.bias = "Egger")

# 2.5 birth head circumference
funnel.meta(hc.pri,
            xlim = c(-1.7, 2.0), 
            xlab = "Mean difference of birth head circumfence (cm)",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(hc.pri, method.bias = "Egger")

# 2.6 cord 25(OH)D level
funnel.meta(cord.pri,
            xlim = c(-1.7, 2.0), 
            xlab = "Mean difference of birth head circumfence (cm)",
            ylab = "standard error",
            studlab = T,
            cex=1.5,
            cex.studlab = 0.75,
            pos.studlab = 1)
metabias(cord.pri, method.bias = "Egger")

################################################################################
#   IV.3  Heatmap of trial contributions
################################################################################
# Create data subset dmcri including variables that are needed 
# (mcri=1: report the outcome in accordance with definitions, mcri=0, report the outcome but not meeting outcome definitions)

dmcri <- d [,c("pe_mcri",           "ght_mcri",          "gdm_mcri",          "PreLabor_mcri",       # Maternal outcomes 
               "csection_mcri",     "MatHosp_mcri",      "MHypercalcem_mcri", "MHypocalcem_mcri", 
               "MHypercalciu_mcri", "delivery_mcri",
               
               "iud_mcri",          "lbw_mcri",          "PretermBirth_mcri", "sga_mcri",            # Neonatal outcomes 
               "CongMal_mcri",      "nicu_mcri",         "ga_mcri",           "bw_mcri",           
               "bl_mcri",           "hc_mcri",           "cord_mcri",
               
               "NeoDeath_mcri",     "NHypercalcem_mcri", "NHypocalcem_mcri",  "RespInf_mcri",        # Infant outcomes
               "urti_mcri",         "lrti_mcri",         "asthma_mcri",       "wei_mcri",          
               "len_mcri",          "hc1_mcri",          "waz_mcri",          "laz_mcri",          
               "hcaz_mcri",         "nbmc_mcri",         "nbmd_mcri",
               "ibmc_mcri",         "ibmd_mcri")]

# Rename variable names and row names
colnames(dmcri)<-
  c("Preeclampsia",             "Gestational HTN",           "GDM",                     "Preterm labor",    
    "Cesarean delivery",        "Maternal hospitalization",  "Maternal hypercalcemia",  "Maternal Hypocalcemia", 
    "Maternal hypercalciuria",  "Maternal 25(OH)D level",
    
    "Stillbirth/fetal death",   "Low birthweight infants",   "Preterm birth",           "SGA",
    "Congenital malformation",  "NICU admission",            "Gestational age",         "Birth body weight",          
    "Birth body height",        "Birth head circumference",  "Cord 25(OH)D level",  
    
    "Neonatal death",           "Neonatal hypercalcemia",    "Neonatal hypocalcemia",   "Respiratory infections", 
    "URTI",                     "LRTI",                      "Asthma",                  "Weight at 1 y/o",               
    "Length at 1 y/o",          "HC at 1 y/o",               "WAZ at 1 y/o",            "LAZ at 1 y/o",    
    "HCZ at 1 y/o",             "Neonatal BMC",              "Neonatal BMD",
    "Infant BMC",               "Infant BMD")

rownames(dmcri)<- c("Brooke 1980",             "Delvin 1986",      "Mallet 1986",      "Marya 1988",       "Kaur 1991",       
                    "Sahu 2009",               "Yu 2009",          "Hollis 2011",      "Sabet 2012",       "Dawodu 2013",     
                    "Hashemipour 2013",        "Roth 2013",        "Soheilykhah 2013", "Grant 2014",       "Hossain 2014",    
                    "Mutlu 2014",              "Yap 2014",         "Charandabi 2015",  "March 2015",       "Mojibian 2015",    
                    "Rodda 2015",              "Sablok 2015",      "Chawes 2016",      "Cooper 2016",      "Khan 2016",        
                    "Litonjua 2016",           "Naghshineh 2016",  "Sahoo 2016",       "Shahgheibi 2016",  "Thiele 2016",      
                    "Valizadeh 2016",          "Vaziri 2016",      "Yazdchi 2016",     "Zerofsky 2016",    "Abotorabi 2017", 
                    "Samimi 2017",             "Sasan 2017",       "Tehrani 2017",     "Azadehrah 2017",   "Keshavarzi 2017",
                    "Best 2018",               "O’Callaghan 2018", "Roth 2018",        "Ali 2019",         "Enkhmaa 2019",     
                    "Jefferson 2019",          "Zhao 2019",        "Corcoy 2020",      "Rahbar 2020",      "Sircar 2020",      
                    "Alhomaid 2021",           "Manasova 2021",    "Mohammadi 2021",   "Nausheen 2021",    "Srilekha 2021",    
                    "Xiaomang 2021",           "Aggarwal 2022",    "Memon 2022",       "Pulido 2022",      "Sudfeld 2022",
                    "Mirzaei-Azandaryani 2022","Vanda 2022",       "Cagiran 2023",     "Ma 2023",          "Nadeem 2023",      
                    "Sunarno 2023")

# Create numeric matrix for heatmap function
dmcri.map <- data.matrix(dmcri)
cols <- c("springgreen", "springgreen4")
par(pin = c(5, 5), plt = c(0.1, 0.9, 0.1, 0.9))

# Create heatmap using heatmap function 
heatmap(dmcri.map, Rowv = NA, Colv = NA, 
        cexCol = 0.45, cexRow = 0.50, scale = "none", col = cols,
        add.expr = {
          abline(h = seq(-0.5, nrow(dmcri.map) + 0.5, length.out = nrow(dmcri.map) + 2), 
                 col = "lightgrey", lwd = 1)
          abline(v = seq(-0.5, ncol(dmcri.map) + 0.5, length.out = ncol(dmcri.map) + 2), 
                 col = "lightgrey", lwd = 1) })

# Add legend
legend(x = "right", 
       legend=c("Sensitivity analysis)", 
                "Primary analysis (meeting outcome definitions)"), 
       cex=0.5, fill=cols,
       xpd=FALSE, inset = c(-0.34,-0.2),
       border="lightgrey",
       box.col = "white")