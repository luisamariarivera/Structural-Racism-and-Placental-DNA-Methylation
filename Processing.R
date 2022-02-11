#clean up
rm(list=ls())

## Make ICE Variables

#load packages
library(tidycensus)
library(tidyverse)
library(tigris)
library(acs)
library(tmap)



#you'll need to get an API token from the census.gov, each are individual

census_api_key("a2b0bffcbfc22406b200e2626ade32194a0b60c9", install= TRUE)
 
#specify variables list

vars<- c("B01003_001", "B03002_003", "B03002_004", "B19001_002", "B19001_003", "B19001_004", "B19001_005" , 
         "B19001_014", "B19001_015", "B19001_016","B19001_017", "B19001A_014","B19001A_015", "B19001A_016", 
         "B19001A_017", "B19001B_002", "B19001B_003", "B19001B_004" , "B19001B_005")

#pull data from 2104 5 year ACS, specifying census tract level data for Shelby County TN

shelby<- get_acs(geography = "tract", 
              variables = vars,
              state = "TN", 
              county ="Shelby",
              year = 2014,
              output = "wide")


#calculate ICE income

shelby$ICEr<- (shelby$B19001_014E+ shelby$B19001_015E+shelby$B19001_016E+ shelby$B19001_017E)
shelby$ICEp<-(shelby$B19001_002E+shelby$B19001_003E+shelby$B19001_004E+shelby$B19001_005E)
shelby$ICEinc<-((shelby$ICEr - shelby$ICEp)/shelby$B01003_001E)



#calculate ICE race * income

shelby$ICEwr<-(shelby$B19001A_014E + shelby$B19001A_015E + shelby$B19001A_016E + shelby$B19001A_017E)
shelby$ICEbp<-(shelby$B19001B_002E + shelby$B19001B_003E + shelby$B19001B_004E + shelby$B19001B_005E)
shelby$ICEraceinc<- ((shelby$ICEwr-shelby$ICEbp)/shelby$B01003_001E)

#coerce to a dataframe

d<-as.data.frame(shelby)


 #plot ICE Race * income 

ggplot(d, aes(x = ICEraceinc, color=ICEraceinc)) +
  geom_histogram( fill = "lightblue") + 
  geom_vline(aes(xintercept = median(ICEraceinc)), linetype = "dashed") +
  theme(panel.background = element_blank())+
  xlab("Shelby County ICE Race x Income") +
  ylab("Frequency")


#Load a shape file for Shelby county census tracts


shape<-tracts(47, county = 157, cb = TRUE, year = 2014)


# convert the GEOID to numeric and join up datasets

shape$GEOID <- as.numeric(shape$GEOID)
d$GEOID<-as.numeric(d$GEOID)

d<-merge(d, coi, by= GEOID, all.x=TRUE)


spatial<-geo_join(shape, d, by = "GEOID", how = "left")
ICEmap <- ggplot() + 
  geom_sf(data = spatial, aes(fill = ICEraceinc))

 
ICEmap + labs(x= "Racial Wealth Inequality in Shelby County, TN", fill = "Concentration of White Wealth")+ theme(panel.background = element_blank())




## Link ICE Variables to CANDLE Dataset (this was already done for you by UTHSC)
## Load dataset

setwd("~/Structural-Racism-and-Placental-DNA-Methylation/data_raw")

library(readr)
p <- read_csv("candle_psyc.csv")
m<- read_csv("PLAC_ages_RPC_clean.csv")
c<-read_csv("PLAC_ages_CPC_clean.csv")

##recode the ID to match naming convention for lab samples


m$STUDYID<-as.numeric(m$STUDYID)
c$STUDYID<-as.numeric(c$STUDYID)


print(m$STUDYID)
print(p$STUDYID)

##merge in data

d1<-merge(m,p, by= "STUDYID")
d2<-merge(d1, c, by="STUDYID")

d<-d2
d<-merge(d,m,by="STUDYID")
rm(d1,d2, c,p,m)

##set variable labels, levels, etc
d$residuals_RPC<-d$residuals_RPC.x
d$DNAme_GA_RPC<-d$DNAme_GA_RPC.x
d$ICEincome<-as.numeric(d$ICEincome)
d$ICErace<-as.numeric(d$ICErace)
d$ICEraceinc<-as.numeric(d$ICEraceinc)
d$age_difference_CPC<-d$DNAme_GA_CPC-d$GestAge
d$age_difference_RPC<-d$DNAme_GA_RPC-d$GestAge
d$abs_res_cpc<-abs(d$residuals_CPC)
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
d$minc<-factor(d$minc, levels =c(1,2,3,4,5,6,7,8,9,10,11),
               labels = c("<4.9k", "5-9.9k", "10-14.9k", "15-19.9k", "20-24.9k", "25-34.9k", "35- 44.9k", "45- 54.9k", "55 - 64.9k", "65-74.9k", ">75k"))
d$meduc<-factor(d$meduc, levels=c(1,2,3,4,5), labels = c("<HS", "HS/GED", "Technical School","College", "Graduate/Professional"))
d$mrace<-factor(d$mrace, levels= c(1,2,3), labels = c("Identifies as Black", "Identifies as White", "Does not identify as White or Black"))
## select variables for analysis and save dataset

setwd("~/Structural-Racism-and-Placental-DNA-Methylation/data cooked")

library(dplyr)

df<-d %>%
  select(STUDYID, mage, mtotpreg, csex, meduc, minc, term, delivery, hypertension, diabetes, bmi, white, aa, asian, `Other race`, mhisp, mrace,gestage, 
         residuals_CPC, residuals_RPC, acestot, psletot, tleqtot, ICErace, ICEraceinc, ICEincome )


write.csv(df, "df.csv")
    