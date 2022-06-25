
source("Processing.R")


library(apaTables)
library(tidyverse)
library(ggplot2)
library(mice)
library(dplyr)
library(qgraph)
library(Matrix)
library(VIM)
library(psych)
library(performance)
library(corrplot)
library(htmlTable)
library(circlize)


##make a logit function
logit <- function(x) {
  log(x) - log(1 - x)
}

## calculate trauma summary scores

d_tleq <- df %>% select(starts_with("tleq")) 
df$tleqtot <- rowSums(d_tleq)

d_aces <- df %>% select(starts_with("aces")) 
df$acestot <- rowSums(d_aces)

d_psle <- df %>% select(starts_with("psle")) 
df$psletot <- rowSums(d_psle)

rm(d_aces, d_psle, d_tleq)


## how much missing data is there?
colSums(is.na(df))/677*100


## are aces and psle missing in non random ways?
## code attrition
df$attrit<-ifelse(is.na(df$acestot), 1, 0)
df$attrit <- as.factor(df$attrit)

t.test(df$tleqtot~df$attrit)
t.test(df$ICErace~df$attrit)
t.test(df$ICEraceinc~df$attrit)
t.test(df$adjusted_income~df$attrit)
t.test(df$mage~df$attrit)
chisq.test(df$attrit,df$aa)


## make a missing data plot


dmissing<-df %>%
  select( mage, mtotpreg, csex, hs, term, bmi, mrace, gestage,  tleqtot, acestot, psletot, ICErace, ICEraceinc, age_difference_CPC)

mice_plot <- aggr(dmissing, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(dmissing), cex.axis=.7,
                  gap=2, ylab=c("Missing data","Pattern"))



##test for mcar

dmcar<-df %>%
  select( mage, mtotpreg, white, aa, gestage,  tleqtot, acestot, psletot, ICErace, ICEraceinc, df$age_difference_CPC,age_difference_RPC )

mcar(dmcar)


##data not MCAR, don't use imputation

##Describe the whole sample and explain your decision to subset on Black and white moms

table(df$mrace)/677*100
table(df$mhisp)/677*100
table(df$mhisp,df$mrace)


## Visually inspect cell heterogeneity PCs and predicted proportions

plot(df$age_difference_CPC, df$CellType_PC1, xlab= "Placental Age Acceleration", ylab= "Cell Type PC1", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC2,xlab= "Placental Age Acceleration", ylab= "Cell Type PC2", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC3, xlab= "Placental Age Acceleration", ylab= "Cell Type PC3", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC4, xlab= "Placental Age Acceleration", ylab= "Cell Type PC4", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC5, xlab= "Placental Age Acceleration", ylab= "Cell Type PC5", col=df$mrace, legend=TRUE)



## standardize continuous variables, transform ICE and stress measures


df$ICErace_s <- (df$ICErace + 1)/2
df$ICErace_s <- ifelse(df$ICErace_s == 0, min(df$ICErace_s[df$ICErace_s > 0]), df$ICErace_s)
df$ICEraceinc_s <- (df$ICEraceinc + 1)/2
df$ICEraceinc_s <- ifelse(df$ICEraceinc_s == 0, min(df$ICEraceinc_s[df$ICErace_s > 0]), df$ICEraceinc_s)
df$mage_s <- scale(df$mage)
df$mtotpreg_s <- scale(df$mtotpreg)
df$adjusted_income_s <- scale(df$adjusted_income)
df$bmi_s <- scale(df$bmi)
df$acestot_s <- scale(df$acestot)
df$psletot_s<- scale(df$psletot)
df$tleqtot_s <- scale(df$tleqtot)
df$pc1_s <- scale(df$CellType_PC1)
df$pc2_s <- scale(df$CellType_PC2)
df$pc3_s <- scale(df$CellType_PC3)
df$gestage <- scale(df$gestage)
df$sync_s <- scale(df$Syncytiotrophoblast)



##make two complete case datasets with standardized variables for analysis

d_black <- df %>% filter(mrace=="Identifies as Black") %>% select(c(age_difference_CPC, acestot_s, tleqtot_s, psletot_s, ICErace_s,ICEraceinc_s, mage_s, mtotpreg_s, adjusted_income_s, bmi_s, hs, csex, sync_s))
d_black <- d_black[complete.cases(d_black),]

d_white <- df %>% filter(mrace=="Identifies as White") %>% select(c(age_difference_CPC, acestot_s, tleqtot_s, psletot_s, ICErace_s,ICEraceinc_s, mage_s, mtotpreg_s, adjusted_income_s,hs, csex, bmi_s, sync_s))
d_white <- d_white[complete.cases(d_white),]


##make an unscaled datasets for reported descriptives

d_black2 <- df %>% filter(mrace=="Identifies as Black") %>% select(c(age_difference_CPC, acestot, tleqtot, psletot, ICErace,ICEraceinc, mage, mtotpreg, adjusted_income, bmi, csex, mrace, meduc))
d_black2 <- d_black2[complete.cases(d_black2),]

d_white2 <- df %>% filter(mrace=="Identifies as White") %>% select(c(age_difference_CPC, acestot, tleqtot, psletot, ICErace,ICEraceinc, mage, mtotpreg, adjusted_income, bmi, csex, mrace, meduc))
d_white2 <- d_white2[complete.cases(d_white2),]



d_comb <- bind_rows(d_black, d_white)
d_comb2 <- bind_rows(d_black2, d_white2)

##Report descriptive statistics

library(furniture)
furniture::table1
t1 <- table1(d_comb2, mage, mtotpreg, meduc, adjusted_income, bmi, csex, acestot, tleqtot, psletot, ICErace, ICEraceinc,
             age_difference_CPC, splitby = ~mrace, output = "pandoc", test= TRUE, format_number = TRUE, export = "Table_1")


##make a density plot for ICE variables and age acelleration

p1 <- ggplot(data=d_comb2, aes(x=ICErace, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  scale_x_continuous(limits=c(-1,1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size=16) +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top")  + 
  ylab("")

p1

p2 <- ggplot(data=d_comb2, aes(x=ICEraceinc, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  scale_x_continuous(limits=c(-1,1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size=16) +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top")  + 
  ylab("")

p2

library(patchwork)

p1 + p2 + plot_layout(guides="collect") & theme(legend.position = 'top')


p3 <- ggplot(data=d_comb2, aes(x=age_difference_CPC, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  theme_minimal(base_size=16) +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top")  + 
  ylab("") + 
  xlab("Placental Age Acceleration")

p3


##correlation table 
d_comb2$aa <- ifelse(d_comb2$mrace == "Identifies as Black", 1, 0)

d_corr<-d_comb2 %>%select(mage, bmi, mtotpreg, adjusted_income, acestot, tleqtot, psletot, ICErace, ICEraceinc, age_difference_CPC, aa)

d_corr_w<-d_white2 %>%select(mage, bmi, mtotpreg, adjusted_income, acestot, tleqtot, psletot, ICErace, ICEraceinc, age_difference_CPC)

d_corr_b<-d_black2 %>%select(mage, bmi, mtotpreg, adjusted_income, acestot, tleqtot, psletot, ICErace, ICEraceinc, age_difference_CPC)

apa.cor.table(d_corr,filename="correlation_tot.doc")
apa.cor.table(d_corr_w, filename="correlation_w.doc")
apa.cor.table(d_corr_b,filename="correlation_b.doc")

## lets see these as cord plots because I would like a better look

library(qgraph)

cor_mat <- cor(d_corr_b)

labs <- c("mage", "BMI", "mtotpreg", "income", "aces", "tleq", "psle", "ICErace", "ICEraceinc", "age_diff") # short names here
names <- c("Maternal Age", "BMI", "mtotpreg", "Adjusted Income", "ACES Total", "TLEQ Total", "PSLE Total", "ICE Race", "ICE Race Income", "Age Difference CPC") # long names here

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr_b), edge.labels=T, labels=labs, nodeNames=names)


cor_mat <- cor(d_corr_w, use="complete.obs")

labs <- c("mage", "BMI", "mtotpreg", "income", "aces", "tleq", "psle", "ICErace", "ICEraceinc", "age_diff") # short names here
names <- c("Maternal Age", "BMI", "mtotpreg", "Adjusted Income", "ACES Total", "TLEQ Total", "PSLE Total", "ICE Race", "ICE Race Income", "Age Difference CPC") # long names here

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr_w), edge.labels=T, labels=labs, nodeNames=names)



##Linear models for ICErace

##Black mothers
m1 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s, data= d_black )
summary(m1)

m2 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s, data= d_black)
summary(m2)

m3 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s + ICErace_s, data= d_black)
summary(m3)

apa.reg.table(m1, filename= "m1_black.doc")
apa.reg.table(m2, filename= "m2_black.doc")
apa.reg.table(m3, filename= "m3_black.doc")

aic_black <- AIC(m1,m2,m3)
aic_black

## white mothers

m4 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s, data= d_white )
summary(m4)

m5 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s, data= d_white)
summary(m5)

m6 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s + ICErace_s, data= d_white)
summary(m6)

AIC(m4,m5,m6)

apa.reg.table(m4, filename= "m4_white.doc")
apa.reg.table(m5, filename= "m5_white.doc")
apa.reg.table(m6, filename= "m6_white.doc")



##ICEraceinc

##Black mothers
m7 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s, data= d_black )
summary(m7)

m8 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s, data= d_black)
summary(m8)

m9 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s + ICEraceinc_s, data= d_black)
summary(m9)

apa.reg.table(m7, filename= "m7_black.doc")
apa.reg.table(m8, filename= "m8_black.doc")
apa.reg.table(m9, filename= "m9_black.doc")

aic_black <- AIC(m7,m8,m9)
aic_black

## white mothers

m10 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s, data= d_white )
summary(m10)

m11 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s, data= d_white)
summary(m11)

m12 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + sync_s + psletot_s + acestot_s + tleqtot_s + ICEraceinc_s, data= d_white)
summary(m12)

AIC(m10,m11,m12) 

apa.reg.table(m10, filename= "m10_white.doc")
apa.reg.table(m11, filename= "m11_white.doc")
apa.reg.table(m12, filename= "m12_white.doc")


## Check models here 

check_model(m3)
car::vif(m3)


check_model(m6)
car::vif(m6)

check_model(m9)
car::vif(m9)


check_model(m12)
car::vif(m12)


##SEM models

library(GGally)
library(lavaan)
library(lavaanPlot)
library(tidyverse)


### Make Black ICErace SEM ###############

model_ICE_trauma <- 'age_difference_CPC ~ ICErace_s + acestot_s + tleqtot_s + psletot_s + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     acestot_s ~ ICErace_s
                     tleqtot_s ~ ICErace_s
                     psletot_s ~ ICErace_s
                     acestot_s ~ adjusted_income_s
                     tleqtot_s ~ adjusted_income_s
                     psletot ~ adjusted_income_s
                     # residual correlations
                     acestot_s ~~ tleqtot
                     acestot_s ~~ psletot
                     tleqtot_s ~~ psletot
                     mage_s ~~ 0*ICErace_s
                    




black_SEM_plot <- lavaanPlot(model = model_ICE_trauma, edge_options = list(color="grey"), coefs=T, sig=0.05)
black_SEM_plot




### Make white participant dataset

d_white <- df %>% filter(mrace=="Identifies as White") %>% select(c(age_difference_CPC, acestot, tleqtot, psletot, ICErace, ICEraceinc, mage, csex, adjusted_income, bmi, CellType_PC1, CellType_PC2, CellType_PC3, mtotpreg))

d_white$ICErace_s <- (d_white$ICErace + 1)/2
d_white$ICErace_s <- ifelse(d_white$ICErace_s == 0, min(d_white$ICErace_s[d_white$ICErace_s > 0]), d_white$ICErace_s)
d_white$ICEraceinc_s <- (d_white$ICEraceinc + 1)/2
d_white$ICEraceinc_s <- ifelse(d_white$ICEraceinc_s == 0, min(d_white$ICEraceinc_s[d_white$ICEraceinc_s > 0]), d_white$ICEraceinc_s)

##do all scaling in full df prior to stratfication

d_white <- d_white %>% 
  mutate(
    acestot = sqrt(acestot),
    tleqtot = sqrt(tleqtot),
    psletot = sqrt(psletot),
    age_s = scale(mage),
    ICErace_s = logit(ICErace_s),
    ICEraceinc_s = logit(ICEraceinc_s),
    bmi_s = scale(bmi),
    mage_s = scale(mage),
    adjusted_income_s = scale(adjusted_income),
    PC1 = scale(CellType_PC1),
    PC2 = scale(CellType_PC2),
    PC3 = scale(CellType_PC3),
    female = ifelse(csex== "Female", 1, 0),
    mtotpreg_s = scale(mtotpreg)
  )



d_white_complete <- select(d_white,c(age_difference_CPC, acestot, tleqtot, psletot, ICErace_s, ICEraceinc_s, mage_s, bmi_s, female, PC1, PC2, PC3, mtotpreg_s, adjusted_income_s ) )
d_white_complete <- d_white_complete[complete.cases(d_white_complete),]



### SEM model for ICErace in white women

model_ICE <- 'age_difference_CPC ~ ICErace_s + mage'

fit_model_ICE <- sem(model_ICE, data=d_white_complete)
summary(fit_model_ICE, standardized=T)

lavaanPlot(model = fit_model_ICE, edge_options = list(color="grey"), coefs=T, stand=T)

## Now add trauma measures
model_ICE_trauma <- 'age_difference_CPC ~ ICErace_s + acestot + tleqtot + psletot + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     acestot ~ ICErace_s
                     tleqtot ~ ICErace_s
                     psletot ~ ICErace_s
                     acestot ~ adjusted_income_s
                     tleqtot ~ adjusted_income_s
                     psletot ~ adjusted_income_s
                     # residual correlations
                     acestot ~~ tleqtot
                     acestot ~~ psletot
                     tleqtot ~~ psletot
                     mage_s ~~ 0*ICErace_s
                    '

fit_model_ICE_trauma <- sem(model_ICE_trauma, data=d_white_complete, fixed.x=F)
summary(fit_model_ICE_trauma)

white_SEM_plot <- lavaanPlot(model = fit_model_ICE_trauma, edge_options = list(color="grey"), coefs=T, sig=0.05)
white_SEM_plot

#### SEM ICEraceinc for Black women

### Make Black ICErace SEM ###############
model_ICE <- 'age_difference_CPC ~ ICEraceinc_s + mage'

fit_model_ICE <- sem(model_ICE, data=d_black_complete)
summary(fit_model_ICE, standardized=T)

lavaanPlot(model = fit_model_ICE, edge_options = list(color="grey"), coefs=T, stand=T)

## Now add trauma measures
model_ICE_trauma <- 'age_difference_CPC ~ ICEraceinc_s + acestot + tleqtot + psletot + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     acestot ~ ICEraceinc_s
                     tleqtot ~ ICEraceinc_s
                     psletot ~ ICEraceinc_s
                     acestot ~ adjusted_income_s
                     tleqtot ~ adjusted_income_s
                     psletot ~ adjusted_income_s
                     # residual correlations
                     acestot ~~ tleqtot
                     acestot ~~ psletot
                     tleqtot ~~ psletot
                     mage_s ~~ 0*ICEraceinc_s
                    '

fit_model_ICE_trauma <- sem(model_ICE_trauma, data=d_black_complete, fixed.x=F)
summary(fit_model_ICE_trauma, standardized=T)

black_SEM_plot2 <- lavaanPlot(model = fit_model_ICE_trauma, edge_options = list(color="grey"), coefs=T, sig=0.05)
black_SEM_plot2



###ICEraceinc SEM for white women


model_ICE <- 'age_difference_CPC ~ ICEraceinc_s + mage'

fit_model_ICE <- sem(model_ICE, data=d_white_complete)
summary(fit_model_ICE, standardized=T)

lavaanPlot(model = fit_model_ICE, edge_options = list(color="grey"), coefs=T, stand=T)

## Now add trauma measures
model_ICE_trauma <- 'age_difference_CPC ~ ICEraceinc_s + acestot + tleqtot + psletot + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     acestot ~ ICEraceinc_s
                     tleqtot ~ ICEraceinc_s
                     psletot ~ ICEraceinc_s
                     acestot ~ adjusted_income_s
                     tleqtot ~ adjusted_income_s
                     psletot ~ adjusted_income_s
                     # residual correlations
                     acestot ~~ tleqtot
                     acestot ~~ psletot
                     tleqtot ~~ psletot
                     mage_s ~~ 0*ICEraceinc_s
                    '

fit_model_ICE_trauma <- sem(model_ICE_trauma, data=d_white_complete, fixed.x=F)
summary(fit_model_ICE_trauma, standardized=T)

white_SEM_plot2 <- lavaanPlot(model = fit_model_ICE_trauma, edge_options = list(color="grey"), coefs=T, sig=0.05)
white_SEM_plot2










##impute full dataset using mice##


##M <- 5
##imp<-mice(df, m=M)
##saveRDS(imp,"data_cooked/imp.rds")

##imp<-readRDS("data_cooked/imp.rds")

##implong <- complete(imp, action="long") %>% select(-tleq21)
df <- select(df, -tleq21)

##implong$psle2 <- ifelse(implong$psle2 > 1, 1, 0)

################


