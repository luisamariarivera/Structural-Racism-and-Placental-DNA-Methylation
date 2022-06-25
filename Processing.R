#clean up
rm(list=ls())

## Make ICE Variables

#load packages
library(tidycensus)
library(tidyverse)
library(tigris)
library(acs)
library(tmap)



p <- read_csv("data_raw/candle_psyc.csv")
a <- read_csv("data_raw/aces_psle.csv")
r<- read_csv("data_raw/PLAC_ages_RPC_clean.csv")
c<-read_csv("data_raw/PLAC_ages_CPC_clean.csv")
i<-read_csv("data_raw/newinc.csv")
ct <- read_csv("data_raw/cell_type.csv")



##recode the ID to match naming convention for lab samples


r$STUDYID<-as.numeric(r$STUDYID)
c$STUDYID<-as.numeric(c$STUDYID)
i$STUDYID<-as.numeric(i$studyid)
p$STUDYID <- as.numeric(p$STUDYID)
a$STUDYID <- as.numeric(a$studyid)
ct$STUDYID <- as.numeric(ct$sample_id)

 
##merge in data

d1<-merge(p,i, by= "STUDYID")
d2<-merge(d1, c, by="STUDYID")
d3<-merge(d2, r, by="STUDYID")
d4 <- merge (d3, a, by="STUDYID", all.x=TRUE)
d5 <- merge(d4, ct, by="STUDYID", all.x=TRUE)

d<-d5
rm(d1,d2,d3,d4,d5,ct,c,p,r,i,a)

##set variable names, labels, levels, etc

d$aces1 <- d$MATERNAL_STRESS_Q1
d$aces2 <- d$MATERNAL_STRESS_Q2
d$aces3 <- d$MATERNAL_STRESS_Q3
d$aces4 <- d$MATERNAL_STRESS_Q4
d$aces5 <- d$MATERNAL_STRESS_Q5
d$aces6 <- d$MATERNAL_STRESS_Q6
d$aces7 <- d$MATERNAL_STRESS_Q7
d$aces8 <- d$MATERNAL_STRESS_Q8
d$aces9 <- d$MATERNAL_STRESS_Q9

d$psle1 <- d$MATERNAL_STRESS_A
d$psle2 <- d$MATERNAL_STRESS_B
d$psle3 <- d$MATERNAL_STRESS_C
d$psle4 <- d$MATERNAL_STRESS_D
d$psle5 <- d$MATERNAL_STRESS_E
d$psle6 <- d$MATERNAL_STRESS_F
d$psle7 <- d$MATERNAL_STRESS_G
d$psle8 <- d$MATERNAL_STRESS_H
d$psle9 <- d$MATERNAL_STRESS_I
d$psle10 <- d$MATERNAL_STRESS_J
d$psle11 <- d$MATERNAL_STRESS_K
d$psle12 <- d$MATERNAL_STRESS_L
d$psle13 <- d$MATERNAL_STRESS_M
d$psle14 <- d$MATERNAL_STRESS_N

d$psle1_stress <- d$MATERNAL_STRESS_A_STRESS
d$psle2_stress <- d$MATERNAL_STRESS_B_STRESS
d$psle3_stress <- d$MATERNAL_STRESS_C_STRESS
d$psle4_stress <- d$MATERNAL_STRESS_D_STRESS
d$psle5_stress <- d$MATERNAL_STRESS_E_STRESS
d$psle6_stress <- d$MATERNAL_STRESS_F_STRESS
d$psle7_stress <- d$MATERNAL_STRESS_G_STRESS
d$psle8_stress <- d$MATERNAL_STRESS_H_STRESS
d$psle9_stress <- d$MATERNAL_STRESS_I_STRESS
d$psle10_stress <- d$MATERNAL_STRESS_J_STRESS
d$psle11_stress <- d$MATERNAL_STRESS_K_STRESS
d$psle12_stress <- d$MATERNAL_STRESS_L_STRESS
d$psle13_stress <- d$MATERNAL_STRESS_M_STRESS
d$psle14_stress <- d$MATERNAL_STRESS_N_STRESS



d$ICEincome<-as.numeric(d$ICEincome)
d$ICErace<-as.numeric(d$ICErace)
d$ICEraceinc<-as.numeric(d$ICEraceinc)
d$age_difference_CPC<-d$DNAme_GA_CPC-d$gestage
d$age_difference_RPC<-d$DNAme_GA_RPC-d$gestage
d$delivery<-substr(d$delclass,1,1)
d$delivery<-as.factor(d$delivery)
levels(d$delivery) <- c("Term", "Spontaneous preterm", "PROM preterm", "PROM c-section", "Preterm Fetal Ind", "Preterm  Maternal Ind") 
d$black<-ifelse(d$aa==1, "African American", "Not AA")
d$bmi<-d$mweight/(d$mheight/100)^2
d$hypertension<-ifelse(d$M3LD_GESTHTN4==1, "Hypertension", "No Hypertension")
d$diabetes<-ifelse(d$mdiabetes==1, "Gestational Diabetes", "No Diabetes")
d$term<-ifelse(d$delivery=="Term",1,0)
d$term<-as.factor(d$term)
levels(d$term)<-c("Preterm", "Full Term")
d$csex<-substr(d$csex, 1, 1)
d$csex<-ifelse(d$csex=="1", 1,0)
d$csex<-factor(d$csex,levels = c(0,1),
               labels = c("Female", "Male"))
d$adjusted_income<-as.numeric(d$adjusted_income)
d$minc<-factor(d$minc, levels =c(1,2,3,4,5,6,7,8,9,10,11),
               labels = c("<4.9k", "5-9.9k", "10-14.9k", "15-19.9k", "20-24.9k", "25-34.9k", "35- 44.9k", "45- 54.9k", "55 - 64.9k", "65-74.9k", ">75k"))

d$meduc<-factor(d$meduc, levels=c(1,2,3,4,5), labels = c("<HS", "HS/GED", "Technical School","College", "Graduate/Professional"))
d$hs <- ifelse(d$meduc== 1|d$meduc==2,1,0)
d$mrace<-factor(d$mrace, levels= c(1,2,3), labels = c("Identifies as Black", "Identifies as White", "Does not identify as White or Black"))
d$other_race<-d$`Other race`
d$aces_pa <- d$tleq12
d$aces_fv <- d$tleq13
d$aces_sa <- d$tleq15
d$worsttleq <- d$tleq21s

## select variables for analysis and save dataset

df<-d %>%
  select( mage, mtotpreg, csex, hs, term, delivery, hypertension, diabetes, bmi, white, aa, asian, other_race, mhisp, mrace,gestage, aces1, aces2, aces3, aces4, aces5, aces6, aces7, aces8, aces9,aces_sa, aces_fv, aces_pa, psle1,psle3,psle4,psle5,psle6,psle7, psle8,psle9,psle10,psle11,psle12,psle13,psle14, tleq1, tleq2, tleq3, tleq4, tleq5, tleq6, tleq7, tleq8, tleq9, tleq10,tleq11, tleq14, tleq16, tleq17, tleq18, tleq19, tleq20, worsttleq, ICErace, ICEraceinc, ICEincome, adjusted_income, age_difference_RPC, age_difference_CPC, Trophoblasts, Hofbauer,Endothelial,nRBC, Syncytiotrophoblast, CellType_PC1, CellType_PC2, CellType_PC3, CellType_PC4, CellType_PC5, meduc )

write_csv(df, "data_cooked/df.csv")

rm(d)

