source("Processing.R")
library(GGally)
library(lavaan)
library(lavaanPlot)
library(tidyverse)


logit <- function(x) {
  log(x) - log(1 - x)
}

#### MODELS FOR BLACK WOMEN #######################

d_black <- df %>% filter(mrace=="Identifies as Black") %>% select(c(age_difference_CPC, acestot, tleqtot, psletot, ICErace, ICEraceinc, mage, csex, adjusted_income, bmi, CellType_PC1, CellType_PC2, CellType_PC3, mtotpreg))

d_black$ICErace_s <- (d_black$ICErace + 1)/2
d_black$ICErace_s <- ifelse(d_black$ICErace_s == 0, min(d_black$ICErace_s[d_black$ICErace_s > 0]), d_black$ICErace_s)
d_black$ICEraceinc_s <- (d_black$ICEraceinc + 1)/2
d_black$ICEraceinc_s <- ifelse(d_black$ICEraceinc_s == 0, min(d_black$ICEraceinc_s[d_black$ICEraceinc_s > 0]), d_black$ICEraceinc_s)

##do all scaling in full df prior to stratfication

d_black <- d_black %>% 
  mutate(
    sqrt_acestot = sqrt(acestot),
    sqrt_tleqtot = sqrt(tleqtot),
    sqrt_psletot = sqrt(psletot),
    age_s = scale(mage),
    logit_ICErace_s = logit(ICErace_s),
    logit_ICEraceinc_s = logit(ICEraceinc_s),
    bmi_s = scale(bmi),
    mage_s = scale(mage),
    adjusted_income_s = scale(adjusted_income),
    PC1 = scale(CellType_PC1),
    PC2 = scale(CellType_PC2),
    PC3 = scale(CellType_PC3),
    female = ifelse(csex== "Female", 1, 0),
    mtotpreg_s = scale(mtotpreg)
  )



ggpairs(select(d_black, c(age_difference_CPC, sqrt_acestot, sqrt_tleqtot, sqrt_psletot, logit_ICErace_s, mage)))


d_black_complete <- select(d_black,c(age_difference_CPC, sqrt_acestot, sqrt_tleqtot, sqrt_psletot, logit_ICErace_s, logit_ICEraceinc_s, mage_s, bmi_s, female, PC1, PC2, PC3, mtotpreg_s, adjusted_income_s ) )
d_black_complete <- d_black_complete[complete.cases(d_black_complete),]


### Make Black ICErace SEM ###############
model_ICE <- 'age_difference_CPC ~ logit_ICErace_s + mage'

fit_model_ICE <- sem(model_ICE, data=d_black_complete)
summary(fit_model_ICE, standardized=T, fit.measures=TRUE)

lavaanPlot(model = fit_model_ICE, edge_options = list(color="grey"), coefs=T, stand=T)

## Now add trauma measures
model_ICE_trauma <- 'age_difference_CPC ~ logit_ICErace_s + sqrt_acestot + sqrt_tleqtot + sqrt_psletot + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     sqrt_acestot ~ logit_ICErace_s
                     sqrt_tleqtot ~ logit_ICErace_s
                     sqrt_psletot ~ logit_ICErace_s
                     sqrt_acestot ~ adjusted_income_s
                     sqrt_tleqtot ~ adjusted_income_s
                     sqrt_psletot ~ adjusted_income_s
                     # residual correlations
                     sqrt_acestot ~~ sqrt_tleqtot
                     sqrt_acestot ~~ sqrt_psletot
                     sqrt_tleqtot ~~ sqrt_psletot
                     mage_s ~~ 0*logit_ICErace_s
                    '

fit_model_ICE_trauma <- sem(model_ICE_trauma, data=d_black_complete, fixed.x=F)
summary(fit_model_ICE_trauma, standardized=T, fit.measures=TRUE)
fitMeasures(fit_model_ICE_trauma, "cfi")



black_SEM_plot <- lavaanPlot(model = fit_model_ICE_trauma, edge_options = list(color="grey"), coefs=T, sig=0.05)
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
    sqrt_acestot = sqrt(acestot),
    sqrt_tleqtot = sqrt(tleqtot),
    sqrt_psletot = sqrt(psletot),
    age_s = scale(mage),
    logit_ICErace_s = logit(ICErace_s),
    logit_ICEraceinc_s = logit(ICEraceinc_s),
    bmi_s = scale(bmi),
    mage_s = scale(mage),
    adjusted_income_s = scale(adjusted_income),
    PC1 = scale(CellType_PC1),
    PC2 = scale(CellType_PC2),
    PC3 = scale(CellType_PC3),
    female = ifelse(csex== "Female", 1, 0),
    mtotpreg_s = scale(mtotpreg)
  )



d_white_complete <- select(d_white,c(age_difference_CPC, sqrt_acestot, sqrt_tleqtot, sqrt_psletot, logit_ICErace_s, logit_ICEraceinc_s, mage_s, bmi_s, female, PC1, PC2, PC3, mtotpreg_s, adjusted_income_s ) )
d_white_complete <- d_white_complete[complete.cases(d_white_complete),]



### SEM model for ICErace in white women

model_ICE <- 'age_difference_CPC ~ logit_ICErace_s + mage'

fit_model_ICE <- sem(model_ICE, data=d_white_complete)
summary(fit_model_ICE, standardized=T)

lavaanPlot(model = fit_model_ICE, edge_options = list(color="grey"), coefs=T, stand=T)

## Now add trauma measures
model_ICE_trauma <- 'age_difference_CPC ~ logit_ICErace_s + sqrt_acestot + sqrt_tleqtot + sqrt_psletot + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     sqrt_acestot ~ logit_ICErace_s
                     sqrt_tleqtot ~ logit_ICErace_s
                     sqrt_psletot ~ logit_ICErace_s
                     sqrt_acestot ~ adjusted_income_s
                     sqrt_tleqtot ~ adjusted_income_s
                     sqrt_psletot ~ adjusted_income_s
                     # residual correlations
                     sqrt_acestot ~~ sqrt_tleqtot
                     sqrt_acestot ~~ sqrt_psletot
                     sqrt_tleqtot ~~ sqrt_psletot
                     mage_s ~~ 0*logit_ICErace_s
                    '

fit_model_ICE_trauma <- sem(model_ICE_trauma, data=d_white_complete, fixed.x=F)
summary(fit_model_ICE_trauma)

white_SEM_plot <- lavaanPlot(model = fit_model_ICE_trauma, edge_options = list(color="grey"), coefs=T, sig=0.05)
white_SEM_plot

#### SEM ICEraceinc for Black women

### Make Black ICErace SEM ###############
model_ICE <- 'age_difference_CPC ~ logit_ICEraceinc_s + mage'

fit_model_ICE <- sem(model_ICE, data=d_black_complete)
summary(fit_model_ICE, standardized=T)

lavaanPlot(model = fit_model_ICE, edge_options = list(color="grey"), coefs=T, stand=T)

## Now add trauma measures
model_ICE_trauma <- 'age_difference_CPC ~ logit_ICEraceinc_s + sqrt_acestot + sqrt_tleqtot + sqrt_psletot + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     sqrt_acestot ~ logit_ICEraceinc_s
                     sqrt_tleqtot ~ logit_ICEraceinc_s
                     sqrt_psletot ~ logit_ICEraceinc_s
                     sqrt_acestot ~ adjusted_income_s
                     sqrt_tleqtot ~ adjusted_income_s
                     sqrt_psletot ~ adjusted_income_s
                     # residual correlations
                     sqrt_acestot ~~ sqrt_tleqtot
                     sqrt_acestot ~~ sqrt_psletot
                     sqrt_tleqtot ~~ sqrt_psletot
                     mage_s ~~ 0*logit_ICEraceinc_s
                    '

fit_model_ICE_trauma <- sem(model_ICE_trauma, data=d_black_complete, fixed.x=F)
summary(fit_model_ICE_trauma, standardized=T)

black_SEM_plot2 <- lavaanPlot(model = fit_model_ICE_trauma, edge_options = list(color="grey"), coefs=T, sig=0.05)
black_SEM_plot2



###ICEraceinc SEM for white women


model_ICE <- 'age_difference_CPC ~ logit_ICEraceinc_s + mage'

fit_model_ICE <- sem(model_ICE, data=d_white_complete)
summary(fit_model_ICE, standardized=T)

lavaanPlot(model = fit_model_ICE, edge_options = list(color="grey"), coefs=T, stand=T)

## Now add trauma measures
model_ICE_trauma <- 'age_difference_CPC ~ logit_ICEraceinc_s + sqrt_acestot + sqrt_tleqtot + sqrt_psletot + mage_s + bmi_s + adjusted_income_s + female + mtotpreg_s + PC1 + PC2 + PC3
                     sqrt_acestot ~ logit_ICEraceinc_s
                     sqrt_tleqtot ~ logit_ICEraceinc_s
                     sqrt_psletot ~ logit_ICEraceinc_s
                     sqrt_acestot ~ adjusted_income_s
                     sqrt_tleqtot ~ adjusted_income_s
                     sqrt_psletot ~ adjusted_income_s
                     # residual correlations
                     sqrt_acestot ~~ sqrt_tleqtot
                     sqrt_acestot ~~ sqrt_psletot
                     sqrt_tleqtot ~~ sqrt_psletot
                     mage_s ~~ 0*logit_ICEraceinc_s
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
