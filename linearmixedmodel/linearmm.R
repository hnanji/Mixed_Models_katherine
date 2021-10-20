#==========================================================================
# Researcher: katherine H
# Project Title: Applying Linear Mixed Models
# Name: Henry Nanji
# Date: Monday 06.09.21
#=========================================================================
# Environment set up and dependencies
dir.create("data")
dir.create("data_output")
dir.create("fig_output")
dir.create("report_output")

# Loading Libraries
library(dplyr)
library(tidyverse)
library(readr)
library(caret)
library(gtsummary)
library(ggplot2)
library(stringr)
library(gplots)
library(broom.mixed)
library(lme4)
library(multilevel)
library(jtools)
library(performance) # ised to extract ICC
# data(sleepstudy)  the sleep data set is part of the lme4 package
#install.packages("sjPlot")
library(sjPlot)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)


# Importing data

load("~/Katherine_Mixed Model/linearmixedmodel/data_output/analysisset_saliva.Rdata")

# repeated measurements made on each subject are likely to be correlated
# so flowrate are likely to be similar if they come from the same subject
# The ICC(Intraclass correlation) is used to measure how similar the flowrate is if they come from the same subject
# # To calculate the ICC, we make a null model

 #   TASK 1 METHOD1: TO INVESTIGATE CLUSTERING AT VISIT AND PATIENT LEVEL USING LMER4 PACKAGE

# visit here is the grouping variable and patno is another grouping variable which are both random
# I am using the lme4 method to investigate between group(SS/US) variation and within group variation
# between group = SS/US. within group = visit
null <-lmer(flowrate ~ 1 +  (1|visit) + (1|patno) , data = salivadata) # random effect of visits grouped under patno
summary(null)
# get ICC for visit
summary(null)
summ(null)
performance::icc(null)
# ICC of 0.463  suggest that we need a random effect model to account for clustering at subject level
# there is a high correlation of outcome at patient level and very small at visit level
#=============================================

# TASK 1: METHOD 2 USING  REPEATED MEASURE ANOVA
# this method assumes that the covariance of within group factor follows a specific form called
# sphericity, variances of any two levels of the within group factor are the equal- not the case in real world

aov.1 <-aov(flowrate ~ visit*treatment + Error(patno/(visit)), salivadata) 


summary(aov.1)
# boader line significance of visit, very significant effect of treatment and no effect of treatment over visit

with(salivadata, interaction.plot(visit,treatment,flowrate,
                                  type = "b",col=c("red","blue"),pch=c(16,18),
                                  main="Interaction plot of treatment and visit"))
boxplot(flowrate ~ treatment*visit, data = salivadata, col=(c("gold","green")),
        main = " SS and US ")

#both graphs shows a higher value of flow rate for SS compared to US
# the mean value of flow rate for SS and US is fairly constant over visits


# also getting ICC ising multilevel package
#ICC1(aov.1)
#ICC2(aov.1)
#null1 <-lmer(flowrate ~ 1 + (1|patno), data = salivadata) 
#performance::icc(null1)
#aov.2 <-aov(flowrate ~ patno, salivadata)
#ICC2(aov.2)
#===============================================

# MIXED MODEL

# random interceopt model allows the line for each group(subjects, visit) to vary
# data has multiple measurements per subject i.e each subject had multIple SS and US measurements
# multiple responses from the same individual are not independent
# every individual has a slightly different  flow rate
# so we are going to add a random effect for each subject which allows a different baseline flowrate
# for each subject
# we are adding a random effect to account for individual differences

# model with  fixed effect = treatment and random effect for patno and visit
# this modeL accounts for the fact that there are differences between individuals and time differences

# check if flow rate has a missing value
sum(is.na(salivadata$flowrate))
# = 0 missing value
flowmodel <-
lmer(flowrate ~ treatment + (1|patno) +  (1|visit), data = salivadata)

# Print the model's output
summary(flowmodel)

# Extract coefficents
#lmer_coef <-
 # tidy(flowmodel, conf.int = TRUE)
#lmer_coef
#======================================================
# From random effect output, there is less variability in visit compared to subjects(patno)
# Residuals: This is variability that is not due to patno and visit
# Fixed effetct
# treatmentSS means that flow rate is higher in SS by 0.75(what are the units of flow rate)compared to US
# intercept is the mean flowrate of the baseline group (US)

# EXPLORING SIGNIFICANCE
# Compare models using likelihood ratio test

null_model <- lmer(flowrate ~ 1  + (1|patno) +  (1|visit), # this is an intercept only model where
                   # we just estimate the mean of the data set
                   data = salivadata, REML = FALSE)
# in this model, treatment is added
full_model <- lmer(flowrate ~ treatment + (1|patno) +  (1|visit),
                   data = salivadata, REML = FALSE)

# performing likelihood ratio test using anova()
# This compares the two models and produce a p value if difference in mean of the two groups are statistically different
anova(null_model, full_model) # p < 0.001

#=========================================================

# adding gender in the model

# generally more females than males. however, median value for F > M

ggplot(data = salivadata,
       aes(x = gender, y = flowrate)) +
  geom_boxplot() 

ggplot(data = salivadata,
       aes(x = treatment, y = flowrate, colour = gender)) +
  geom_boxplot() 

# baseline model just with treatment
flowrate.model1 <-
  lmer(flowrate ~ treatment + (1|patno) +  (1|visit), data = salivadata)

summary(flowrate.model1)

# Adding gender and testing if significant

flowrate_gender <- lmer(flowrate ~ treatment + gender + (1|patno) +  (1|visit),
                   data = salivadata, REML = FALSE)

summary(flowrate_gender)

anova(flowrate.model1, flowrate_gender)  # p > 0.569 ---> gender not statisticaly significant


# testing if ethnicity is a significant predictor
ggplot(data = salivadata,
       aes(x = ethnicity, y = flowrate)) +
  geom_boxplot() 

null_mode_ethnicity <- lmer(flowrate ~ treatment  + (1|patno) +  (1|visit),
                         data = salivadata, REML = FALSE)

full_model_ethnicity <- lmer(flowrate ~ treatment + ethnicity + (1|patno) +  (1|visit),
                          data = salivadata, REML = FALSE)

anova(null_mode_ethnicity, full_model_ethnicity)  # p > 0.07

# Investigating age

# check distribution of age

ggplot(data = salivadata, mapping = aes(x= age)) +
         geom_histogram()

ggplot(salivadata) + geom_density(aes(x=age), bindwith = 5, fill = 'grey')
summary(salivadata$age)
table(salivadata$age)

ggplot(data = salivadata,
       aes(x = treatment, y = age)) +
  geom_boxplot()  # median value for the two groups are similar, this is an indication that age is not predictive


# Three subpolulations clearly visible
# grouping age according to <=26, >27 <=33, >33

salivadata <- salivadata %>%
  mutate(agecat1 = case_when(
    age  <= 26 ~ 0,
    age > 26 & age <= 33 ~ 1,
    age > 33 ~ 2
  ))
salivadata <- salivadata %>% 
  mutate(agecat2 = case_when(
    agecat1 == 0 ~ "<=26",
    agecat1 ==1 ~ ">26 & <=33",
    agecat1 ==2 ~ ">33",
    TRUE ~"m"
  ))

# grouping age according to <=26, >26 

salivadata <- salivadata %>%
  mutate(age1 = case_when(
    age  <= 26 ~ 0,
    age >26 ~ 1
  ))
salivadata <- salivadata %>% 
  mutate(agecat3 = case_when(
    age1 == 0 ~ "<=26",
    age1 ==1 ~ ">26",
    TRUE ~"m"
  ))


# using age categories generated by me for three groupings
null_mode_age <- lmer(flowrate ~ treatment  + (1|patno) +  (1|visit),
                            data = salivadata, REML = FALSE)

# agecat2 has three levels
full_model_agecat2 <- lmer(flowrate ~ treatment + agecat2 + (1|patno) +  (1|visit),
                             data = salivadata, REML = FALSE)
# using the age category generated by me for two groups
# agecat3 has two levels
full_model_agecat3 <- lmer(flowrate ~ treatment + agecat3 + (1|patno) +  (1|visit),
                           data = salivadata, REML = FALSE)

# testing if agecat2 is significant
anova(null_mode_age, full_model_agecat2)
# testing if agecat is significant
anova(null_mode_age, full_model_agecat3)

# con conclusion, both agecat2 and agecat3 are not significant

#==================================================================

# Investigating BMI

ggplot(salivadata) + geom_density(aes(x=bmi), bindwith = 5, fill = 'grey')
summary(salivadata$bmi) # 6 missing values for bmi is not problematic
table(salivadata$bmi)

ggplot(data = salivadata,
       aes(x = treatment, y = bmi)) + # median values for the two groups are similar indicating that bmi is not predictive
  geom_boxplot()

# gropuping bmi according to suitable cut point <=25 and >=26

salivadata <- salivadata %>%
  mutate(bmi1 = case_when(
    bmi  <=25 ~ 0,
    bmi >=26 ~ 1
  ))
salivadata <- salivadata %>% 
  mutate(bmicat1 = case_when(
    bmi1 == 0 ~ "<=25",
    bmi1 ==1 ~ ">=26",
    TRUE ~"m"
  ))

# Investigating bmi categories
null_model_bmi <- lmer(flowrate ~ treatment  + (1|patno) +  (1|visit),
                      data = salivadata, REML = FALSE)

full_model_bmicat1 <- lmer(flowrate ~ treatment + bmicat1 + (1|patno) +  (1|visit),
                           data = salivadata, REML = FALSE)

# testing the effect of bmicat
anova(null_model_bmi, full_model_bmicat1)

# neither gender, bmi anmd age ddoesnt account for any difference observe  between US and SS
#=================================================
# Final flow Model
flowmodel <- lmer(flowrate ~ treatment + (1|patno) +  (1|visit),
                           data = salivadata, REML = FALSE)
# extract parameters
summ(flowmodel, confint = TRUE)
#ICC of 59% mean that 59% of variation in flow rate is explained buy  differences in participants
#and only 1% explained by differences over the different visits

# Assessing varaibility across participants and  visits

summary(flowmodel)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

fl01 <- lmer(flowrate ~ 1  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

fl02 <- lmer(flowrate ~ treatment  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)
fl03 <- lmer(flowrate ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
             #we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

anova(fl01, fl02) # testing for treatment effect
anova(fl02, fl03) # testing for withing subject effect


# Interactions plot as requested

bxp <- 
  ggboxplot(
    salivadata, x = "visit", y = "flowrate",
    color = "treatment", palette = "jco"
  )
bxp


# boader line significance of visit, very significant effect of treatment and no effect of treatment over visit

jpeg(file="./fig_output/flowinteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = flowrate, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "flow rate",
       title = "Interaction plot") 
dev.off()


#  Make predictions and extract residuals
flowsubset <-
  salivadata %>%
  mutate(lm_predict = predict(flowmodel)) %>%
 mutate(resid = (flowrate - lm_predict)) # calculate residuals


# residuals should be normally distributed with a mean of 0

#plot(flowdel) # this plots residuals vs fitted values to check for normality

jpeg(file="./fig_output/flowrateresidual.jpeg")   
ggplot(data = flowsubset,
       aes(x = resid)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot") 
dev.off()

jpeg(file="./fig_output/QQpflowrate.jpeg")   
qqnorm(flowsubset$resid) 
qqline(flowsubset$resid)
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(flowsubset$resid) # p-value = 0.08571


#=================================================================
#  Exploring Total protein Concentration
#================================================================
# ggplot(data = salivadata,
#        aes(x = treatment, y = protenconc)) +
#   geom_boxplot() 
# 
# # check if flow rate has a missing value
# sum(is.na(salivadata$protenconc))
# 
# protein.model <-
#   lmer(protenconc ~ treatment + (1|patno) +  (1|visit), data = salivadata)
# 
# # Print the model's output
# summary(protein.model)
# 
# # Extract coefficents
# lmer_coef <-
#   tidy(protein.model, conf.int = TRUE)
# lmer_coef
# #======================================================
# 
# # treatmentSS means that the protein conc is lower  by 0.l114 in SS compared to US
# 
# 
# # EXPLORING SIGNIFICANCE
# # Compare models using likelihood ratio test
# 
# protein.model.null <- lmer(protenconc ~ 1  + (1|patno) +  (1|visit), # this is an intercept only model where
#                    # we just estimate the mean of the data set
#                    data = salivadata, REML = FALSE)
# # in this model, treatment is added
# protein.model.full <- lmer(protenconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
#                       data = salivadata, REML = FALSE)
# 
# 
# # This compares the two models and produce a p value if difference in mean of the two groups are statistically different
# anova(protein.model.null, protein.model.full) # p < 0.016
# 
# #=========================================================
# 
# # adding gender in the model
# 
# # generally more females than males. however, median value for F > M
# 
# ggplot(data = salivadata,
#        aes(x = gender, y = protenconc)) +
#   geom_boxplot() 
# 
# 
# # baseline model just with treatment
# protein.model.full <- lmer(protenconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
#                            data = salivadata, REML = FALSE)
# 
# # Adding gender and testing if significant
# 
# protein.model.full.gender <- lmer(protenconc ~ treatment + gender + (1|patno) +  (1|visit), # this is an intercept only model where
#                            data = salivadata, REML = FALSE)
# 
# anova(protein.model.full, protein.model.full.gender)  # p > 0.4189 ---> gender not statisticaly significant
# 
# 
# # testing if ethnicity is a significant predictor
# ggplot(data = salivadata,
#        aes(x = ethnicity, y = protenconc)) +
#   geom_boxplot() 
# 
# protein.model.full <- lmer(protenconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
#                            data = salivadata, REML = FALSE)
# 
# protein.model.ethnicity <- lmer(protenconc ~ treatment + ethnicity + (1|patno) +  (1|visit), # this is an intercept only model where
#                            data = salivadata, REML = FALSE)
# 
# anova(protein.model.full, protein.model.ethnicity)  # p > 0.02 which is significant
# 
# # Investigating age
# 
# # check distribution of age
# 
# ggplot(data = salivadata,
#        aes(x = agecat2, y = protenconc)) +
#   geom_boxplot() 
# 
# ggplot(data = salivadata,
#        aes(x = agecat3, y = protenconc)) +
#   geom_boxplot() 
# 
# # using age categories generated by me for three groupings
# protein.model.full <- lmer(protenconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
#                            data = salivadata, REML = FALSE)
# 
# # agecat2 has three levels
# protein.model.full.agecat2 <- lmer(protenconc ~ treatment +agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
#                            data = salivadata, REML = FALSE)
# # using the age category generated by me for two groups
# # agecat3 has two levels
# protein.model.full.agecat3 <- lmer(protenconc ~ treatment +agecat3 + (1|patno) +  (1|visit), # this is an intercept only model where
#                                    data = salivadata, REML = FALSE)
# 
# # testing if agecat2 is significant
# anova(protein.model.full, protein.model.full.agecat2)
# # testing if agecat is significant
# anova(protein.model.full, protein.model.full.agecat3)
# 
# # con conclusion, both agecat2 and agecat3 are not significant
# 
# #==================================================================
# 
# # Investigating BMI
# 
# 
# ggplot(data = salivadata,
#        aes(x = bmicat1, y = protenconc)) + # median values for the two groups are similar indicating that bmi is not predictive
#   geom_boxplot()
# 
# # Investigating bmi categories
# protein.model.full <- lmer(protenconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
#                            data = salivadata, REML = FALSE)
# 
# protein.model.bmicat1 <- lmer(protenconc ~ treatment + bmicat1 + (1|patno) +  (1|visit),
#                            data = salivadata, REML = FALSE)
# 
# # testing the effect of bmicat
# anova(protein.model.full, protein.model.bmicat1)
# 
# # bmi not significant

# Final Model which includes treatment only
proteinmodel <- lmer(protenconc ~ treatment + (1|patno) +  (1|visit),
                    data = salivadata, REML = FALSE)

# extract parameters
summary(proteinmodel)
summ(proteinmodel, confint = TRUE)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

pr01 <- lmer(protenconc ~ 1  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

pr02 <- lmer(protenconc ~ treatment  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)
pr03 <- lmer(protenconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
 #we just estimate the mean of the data set
data = salivadata, REML = FALSE)

anova(pr01, pr02) # testing for treatment effect
anova(pr02, pr03) # testing for withing subject effect(visit effect)


# effect of visit not significant over time


jpeg(file="./fig_output/proteninteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = flowrate, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "Proten conc",
       title = "Interaction plot") 
dev.off()




#  Make predictions and extract residual

proten_residuals <-data.frame(residuals(proteinmodel))


# residuals should be normally distributed with a mean of 0
jpeg(file="./fig_output/proteinresidual.jpeg")   
ggplot(data = proten_residuals,
       aes(x = residuals.proteinmodel.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-protein") 
dev.off()

jpeg(file="./fig_output/QQplotprotein.jpeg")   
qqnorm(proten_residuals$residuals.proteinmodel.) 
qqline(proten_residuals$residuals.proteinmodel.) 
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(proten_residuals$residuals.proteinmodel.) 
# data is very skewed

boxplot(salivadata$protenconc)
####
# identify outliers
#extract the values of the potential outliers based on the IQR
 boxplot.stats(salivadata$protenconc)$out

 #extract the row number corresponding to these outliers:
  
out <- boxplot.stats(salivadata$protenconc)$out
out_ind <- which(salivadata$protenconc %in% c(out))
out_ind
salivadata[out_ind, ]
# create new subset without the outliers and refit the model
#row_no <-c(56, 105, 109, 111, 113, 139, 191, 194, 195, 201)
flow_without_outlier  <-salivadata %>%
  mutate(rowno = row_number()) %>%
  filter(rowno != 56 & rowno != 105 & rowno != 109 & rowno != 111
         & rowno != 113 & rowno != 139 & rowno != 191 & rowno != 194
         & rowno != 195 & rowno != 201) # deleting outliers

# refit the model on  data set without outluiers for protemnconcen

boxplot(flow_without_outlier$protenconc)
proteinM1 <- lmer(protenconc ~ treatment + (1|patno) +  (1|visit),
                     data = data_without_outlier, REML = FALSE)

# extract parameters
summ(proteinM1, confint = TRUE)
# after deleting outliers, treatment effect was not significant

proten_residualsM1 <-data.frame(residuals(proteinM1))

# residuals should be normally distributed with a mean of 0
jpeg(file="./fig_output/flowrateresidual.jpeg")   
ggplot(data = proten_residualsM1,
       aes(x = residuals.proteinM1.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-protein without outliets") 
dev.off()

jpeg(file="./fig_output/QQplot.jpeg")   
qqnorm(proten_residualsM1$residuals.proteinM1.) 
qqline(proten_residualsM1$residuals.proteinM1.) 
dev.off()

shapiro.test(proten_residualsM1$residuals.proteinM1.)

#
# conclusoion. After deleting  outliers for protein concentration, model did not converge

 
#=================================================================
#  Exploring Total soduimconc
#================================================================

# check if sodiumconc has a missing value
sum(is.na(salivadata$sodiumconc))

# EXPLORING SIGNIFICANCE


sodium.model <- lmer(sodiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                    # we just estimate the mean of the data set
                    data = salivadata, REML = FALSE)


summ(sodium.model, confint = TRUE)

# adding gender in the model

 ggplot(data = salivadata,
       aes(x = gender, y = sodiumconc)) +
  geom_boxplot() 

# # Investigating age
 
 sod0 <- lmer(sodiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                      # we just estimate the mean of the data set
                      data = salivadata, REML = FALSE)
 
 # add  agecat2
 
 sod1 <- lmer(sodiumconc ~ treatment  + agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
                      # we just estimate the mean of the data set
                      data = salivadata, REML = FALSE)
 
 sod2 <- lmer(sodiumconc ~ treatment  + agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
                      # we just estimate the mean of the data set
                      data = salivadata, REML = FALSE)

# 
# # testing if agecat2 is significant
 anova(sod0, sod1)
# # testing if agecat is significant
anova(sod0, sod1)
# 
# # con conclusion, both agecat2 and agecat3 are not significant
#
# #==================================================================

bmi0 <- lmer(sodiumconc ~ treatment   + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

bmi1 <- lmer(sodiumconc ~ treatment  + bmicat + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if bmicatis significant
anova(bmi0, bmi1)

# bmi is not significant
# Final sodium model
sodium.model <- lmer(sodiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                     # we just estimate the mean of the data set
                     data = salivadata, REML = FALSE)
#  due to high singularity, vistit is omitted= singularity mean there is 
# a perfect correlation between  two of the 2 IV

sodium.model <- lmer(sodiumconc ~ treatment  + (1|patno), # this is an intercept only model where
                     # we just estimate the mean of the data set
                     data = salivadata, REML = FALSE)
summ(sodium.model, confint = TRUE)


# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

na01 <- lmer(sodiumconc ~ 1  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

na02 <- lmer(sodiumconc ~ treatment  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)
# na03 <- lmer(sodiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             #data = salivadata, REML = FALSE)
anova(na01, na02) # testing for treatment effect
#anova(mg02, mg03) # testing for withing subject effect


# effect of visit not significant over time

jpeg(file="./fig_output/sodiuminteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = sodiumconc, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "Sodium conc",
       title = "Interaction plot") 
dev.off()



#  Make predictions and extract residual

so_residuals <-data.frame(residuals(sodium.model))

# residuals should be normally distributed with a mean of 0
jpeg(file="./fig_output/flowrateresidual.jpeg")   
ggplot(data = so_residuals,
       aes(x = residuals.sodium.model.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-sodium") 
dev.off()

jpeg(file="./fig_output/QQplot.jpeg")   
qqnorm(so_residuals$residuals.sodium.model.) 
qqline(so_residuals$residuals.sodium.model.) 
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(so_residuals$residuals.sodium.model.) 
# data is very skewed

boxplot(salivadata$sodiumconc)
####
#extract the values of the potential outliers based on the IQR
boxplot.stats(salivadata$sodiumconc)$out

#extract the row number corresponding to these outliers:

out1 <- boxplot.stats(salivadata$sodiumconc)$out
out_ind <- which(salivadata$sodiumconc %in% c(out1))
out_ind
salivadata[out_ind, ]
# create new subset without the outliers and refit the model
#row_no <-c(56, 105, 109, 111, 113, 139, 191, 194, 195, 201)
data_without_outlier1  <-salivadata %>%
  mutate(rowno = row_number())%>%
  filter(rowno != 26 & rowno != 122 & rowno != 128 & rowno != 130
         & rowno != 132 & rowno != 150 & rowno != 176 & rowno != 178 & rowno != 180
         & rowno != 182 & rowno != 184 & rowno != 189 & rowno != 200 & rowno != 202) # deleting outliers

# refit the model on  data set without outliers for protenconcen
sum(is.na(data_without_outlier1$sodiumconc))
# deleting the missing value for sodioum
data_without_outlier1 <- data_without_outlier1 %>%
  filter(sodiumconc != "")

boxplot(data_without_outlier1$sodiumconc)
soM1 <- lmer(sodiumconc ~ treatment + (1|patno) +  (1|visit),
                  data = data_without_outlier1, REML = FALSE)

# extract parameters
summ(soM1, confint = TRUE)


so_residualsM1 <-data.frame(residuals(soM1))

# residuals should be normally distributed with a mean of 0
jpeg(file="./fig_output/flowrateresidual.jpeg")   
ggplot(data = so_residualsM1,
       aes(x = residuals.soM1.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-sodium without outliers") 
dev.off()

jpeg(file="./fig_output/QQplot.jpeg")   
qqnorm(so_residualsM1$residuals.soM1.) 
qqline(so_residualsM1$residuals.soM1.) 
dev.off()

shapiro.test(so_residualsM1$residuals.soM1.)
# also performing shapiro test at different visits and treatment
data_without_outlier1 %>%
  group_by(visit, treatment) %>%
  shapiro_test(sodiumconc)

sodium.model <- lmer(sodiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                     # we just estimate the mean of the data set
                     data = data_without_outlier1, REML = FALSE)
summ(sodium.model, confint = TRUE)

# after deleting outliers, the residuals were still not normally distributed

#=================================================================
#  Exploring Total magnesium concentration
#================================================================

# check if sodiumconc has a missing value
sum(is.na(salivadata$magnesiumconc))

# EXPLORING SIGNIFICANCE


mg.model <- lmer(magnesiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                     # we just estimate the mean of the data set
                     data = salivadata, REML = FALSE)


summ(mg.model, confint = TRUE)



# # Investigating age

mg0 <- lmer(magnesiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# add  agecat2

mg1 <- lmer(magnesiumconc ~ treatment  + agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

mg2 <- lmer(magnesiumconc ~ treatment  + agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if agecat2 is significant
anova(mg0, mg1)
# # testing if agecat is significant
anova(mg0, mg2)
# 
# # con conclusion, both agecat2 and agecat3 are not significant
#
# #==================================================================

bmi0 <- lmer(magnesiumconc ~ treatment   + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

bmi1 <- lmer(magnesiumconc ~ treatment  + bmicat + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if bmicatis significant
anova(bmi0, bmi1)

# bmi is boarderline significant
# Final mg model
mg <- lmer(magnesiumconc ~ treatment  + bmicat + (1|patno) +  (1|visit), # this is an intercept only model where
                     # we just estimate the mean of the data set
                     data = salivadata, REML = FALSE)
summ(mg, confint = TRUE)
# remove bmi as it is not significant

mg <- lmer(magnesiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(mg, confint = TRUE)


# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

#aov.4 <-aov(magnesiumconc ~ visit*treatment + Error(patno/(visit)), salivadata) 
#summary(aov.4)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

#aov.5 <-aov(phosphorus ~ visit*treatment + Error(patno/(visit)), salivadata) 
#summary(aov.5)
mg01 <- lmer(magnesiumconc ~ 1  + (1|patno) , # this is an intercept only model where
              # we just estimate the mean of the data set
              data = salivadata, REML = FALSE)

mg02 <- lmer(magnesiumconc ~ treatment  + (1|patno), # this is an intercept only model where
              # we just estimate the mean of the data set
              data = salivadata, REML = FALSE)
mg03 <- lmer(magnesiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
              # we just estimate the mean of the data set
              data = salivadata, REML = FALSE)
anova(mg01, mg02) # testing for treatment effect
anova(mg02, mg03) # testing for withing subject effect

# effect of visit not significant over time

jpeg(file="./fig_output/magnesiuminteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = magnesiumconc, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "magnesium conc",
       title = "Interaction plot") 
dev.off()



#  Make predictions and extract residual
plot(mg)
mg_residuals <-data.frame(residuals(mg))

# residuals should be normally distributed with a mean of 0
jpeg(file="./fig_output/mgresiduals.jpeg")   
ggplot(data = mg_residuals,
       aes(x = residuals.mg.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-mg") 
dev.off()

jpeg(file="./fig_output/mgQQplot.jpeg")   
qqnorm(mg_residuals$residuals.mg) 
qqline(mg_residuals$residuals.mg) 
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(mg_residuals$residuals.mg) 
# data is very skewed

boxplot(salivadata$magnesiumconc)
####
#extract the values of the potential outliers based on the IQR
boxplot.stats(salivadata$magnesiumconc)$out

#extract the row number corresponding to these outliers:

out2 <- boxplot.stats(salivadata$magnesiumconc)$out
out_ind <- which(salivadata$magnesiumconc %in% c(out2))
out_ind
salivadata[out_ind, ]
# create new subset without the outliers and refit the model
#row_no <-c(56, 105, 109, 111, 113, 139, 191, 194, 195, 201)
data_without_outlier2  <-salivadata %>%
  mutate(rowno = row_number()) %>%
  filter(rowno != 39 & rowno != 61 & rowno != 67 & rowno != 99
         & rowno != 110 & rowno != 121 & rowno != 163 & rowno != 187 & rowno != 188
         & rowno != 189 & rowno != 209) # deleting outliers

# refit the model on  data set without outliers for protemnconcen

boxplot(data_without_outlier2$magnesiumconc)
mgM1 <- lmer(magnesiumconc ~ treatment + (1|patno) +  (1|visit),
             data = data_without_outlier2, REML = FALSE)

# extract parameters
summ(mgM1, confint = TRUE)

mg_residualsM1 <-data.frame(residuals(mgM1))

# residuals should be normally distributed with a mean of 
jpeg(file="./fig_output/QQplot.jpeg")   
qqnorm(mg_residualsM1$residuals.mgM1.) 
qqline(mg_residualsM1$residuals.mgM1.) 
dev.off()

shapiro.test(mg_residualsM1$residuals.mgM1.)

magnesium.model <- lmer(magnesiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                     # we just estimate the mean of the data set
                     data = data_without_outlier2, REML = FALSE)
summ(magnesium.model, confint = TRUE)

# After deleting outliers, residuals still not normally distributed


#=================================================================
#  Exploring Total  phosphorus
#================================================================

# check if sodiumconc has a missing value
sum(is.na(salivadata$phosphorus))

# EXPLORING SIGNIFICANCE


ph.model <- lmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                 # we just estimate the mean of the data set
                 data = salivadata, REML = FALSE)


summ(ph.model, confint = TRUE)



# # Investigating age

age0 <- lmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

# add  agecat2

age2 <- lmer(phosphorus ~ treatment  + agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

age3 <- lmer(phosphorus ~ treatment  + agecat3 + (1|patno) +  (1|visit), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

# 
# # testing if agecat2 is significant
anova(age0, age2)
# # testing if agecat is significant
anova(age0, age3)
# 
# # con conclusion, both agecat2 and agecat3 are not significant
#
# #==================================================================

bmi0 <- lmer(phosphorus ~ treatment   + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

bmi1 <- lmer(phosphorus ~ treatment  + bmicat + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if bmicatis significant
anova(bmi0, bmi1)

# bmi is boarderline significant
# Final mg model
ph <- lmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(ph, confint = TRUE)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

#aov.5 <-aov(phosphorus ~ visit*treatment + Error(patno/(visit)), salivadata) 
#summary(aov.5)
pho <- lmer(phosphorus ~ 1  + (1|patno) , # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

ph1 <- lmer(phosphorus ~ treatment  + (1|patno), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)
ph2 <- lmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

anova(pho, ph1)  # testing for treatmemnt effect
anova(ph1, ph2)  # testing  for visitb effect


# effect of visit not significant over time

jpeg(file="./fig_output/phosphurusinteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = phosphorus, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "phosphorus",
       title = "Interaction plot") 
dev.off()



#  Make predictions and extract residual

ph_residuals <-data.frame(residuals(ph))

# residuals should be normally distributed with a mean of 0

jpeg(file="./fig_output/phresiduals.jpeg")   
ggplot(data = ph_residuals,
       aes(x = residuals.ph.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-ph") 
dev.off()

jpeg(file="./fig_output/QQphplot.jpeg")   
qqnorm(ph_residuals$residuals.ph) 
qqline(ph_residuals$residuals.ph) 
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(ph_residuals$residuals.ph) 
# data is very skewed

boxplot(salivadata$phosphorus)
####
#extract the values of the potential outliers based on the IQR
boxplot.stats(salivadata$phosphorus)$out

#extract the row number corresponding to these outliers:

out2 <- boxplot.stats(salivadata$phosphorus)$out
out_ind <- which(salivadata$phosphorus %in% c(out2))
out_ind
#salivadata[out_ind, ]
# create new subset without the outliers and refit the model
#row_no <-c(56, 105, 109, 111, 113, 139, 191, 194, 195, 201)
data_without_outlier2  <-salivadata %>%
  mutate(rowno = row_number()) %>%
  filter(rowno != 37 & rowno != 103 & rowno != 105 & rowno != 107
         & rowno != 109 & rowno != 110 & rowno != 112 & rowno != 120 & rowno != 160
         & rowno != 195) # deleting outliers

# refit the model on  data set without outliers for phosphorus

boxplot(data_without_outlier2$phosphorus)
phM1 <- lmer(phosphorus ~ treatment + (1|patno) +  (1|visit),
             data = data_without_outlier2, REML = FALSE)

# extract parameters
summ(phM1, confint = TRUE)

ph_residualsM1 <-data.frame(residuals(phM1))

# residuals should be normally distributed with a mean of 
jpeg(file="./fig_output/QQplot.jpeg")   
qqnorm(ph_residualsM1$residuals.phM1.) 
qqline(ph_residualsM1$residuals.phM1.) 
dev.off()

shapiro.test(ph_residualsM1$residuals.phM1.)

ph.model <- lmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                        # we just estimate the mean of the data set
                        data = data_without_outlier2, REML = FALSE)
summ(ph.model, confint = TRUE)

# After deleting outliers, residuals still not normally distributed


#=================================================================
#  Exploring Total  potassiom
#================================================================

# check if potassiumn has a missing value
sum(is.na(salivadata$potassiom))

# EXPLORING SIGNIFICANCE


k.model <- lmer(potassiom ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                 # we just estimate the mean of the data set
                 data = salivadata, REML = FALSE)


summ(k.model, confint = TRUE)



# # Investigating age

age0 <- lmer(potassiom ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# add  agecat2

age2 <- lmer(potassiom ~ treatment  + agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

age3 <- lmer(potassiom ~ treatment  + agecat3 + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if agecat2 is significant
anova(age0, age2)
# # testing if agecat is significant
anova(age0, age3)
# 
# # con conclusion, both agecat2 and agecat3 are not significant
#
# #==================================================================

bmi0 <- lmer(potassiom ~ treatment   + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

bmi1 <- lmer(potassiom ~ treatment  + bmicat + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if bmicatis significant
anova(bmi0, bmi1)

# bmi is boarderline significant
# Final k model
k <- lmer(potassiom ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(k, confint = TRUE)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

#aov.5 <-aov(phosphorus ~ visit*treatment + Error(patno/(visit)), salivadata) 
#summary(aov.5)
k01 <- lmer(potassiom ~ 1  + (1|patno) , # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

k02 <- lmer(potassiom ~ treatment  + (1|patno), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)
k03 <- lmer(potassiom ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

anova(k01, k02)  # testing for treatmemnt effect
anova(k02, k03)  # testing  for visitb effect


# effect of visit not significant over time

jpeg(file="./fig_output/potassiuminteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = potassiom, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "potassium",
       title = "Interaction plot") 
dev.off()




#  Make predictions and extract residual

k_residuals <-data.frame(residuals(k))

# residuals should be normally distributed with a mean of 0
jpeg(file="./fig_output/kresiduals.jpeg")   
ggplot(data = k_residuals,
          aes(x = residuals.k.)) +
    geom_density() +
    labs(x = "residuals", y = "density",
                   title = "Residual plot-K") 
dev.off()


jpeg(file="./fig_output/kQQplot.jpeg")   
qqnorm(k_residuals$residuals.k) 
qqline(k_residuals$residuals.k) 
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(k_residuals$residuals.k) 
# data is very skewed

boxplot(salivadata$potassiom)
####
#extract the values of the potential outliers based on the IQR
boxplot.stats(salivadata$potassiom)$out

#extract the row number corresponding to these outliers:

out2 <- boxplot.stats(salivadata$potassiom)$out
out_ind <- which(salivadata$potassiom %in% c(out2))
out_ind
#salivadata[out_ind, ]
# create new subset without the outliers and refit the model
#row_no <-c(56, 105, 109, 111, 113, 139, 191, 194, 195, 201)
data_without_outlier2  <-salivadata %>%
  mutate(rowno = row_number()) %>%
  filter(rowno != 40 & rowno != 71 & rowno != 97 & rowno != 103
         & rowno != 105 & rowno != 109 & rowno != 110 & rowno != 111 & rowno != 112
         & rowno != 113  & rowno != 114 & rowno != 160 & rowno != 188) # deleting outliers

# refit the model on  data set without outluiers for protemnconcen

boxplot(data_without_outlier2$potassiom)

kM1 <- lmer(potassiom ~ treatment + (1|patno) +  (1|visit),
             data = data_without_outlier2, REML = FALSE)

# extract parameters
summ(kM1, confint = TRUE)

k_residualsM1 <-data.frame(residuals(kM1))

# residuals should be normally distributed with a mean of 
jpeg(file="./fig_output/QQplot.jpeg")   
qqnorm(k_residualsM1$residuals.kM1.) 
qqline(k_residualsM1$residuals.kM1.) 
dev.off()

shapiro.test(k_residualsM1$residuals.kM1.)

k.model <- lmer(potassiom ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                 # we just estimate the mean of the data set
                 data = data_without_outlier2, REML = FALSE)
summ(k.model, confint = TRUE)

# After deleting outliers, residuals still not normally distributed



#=================================================================
#  Exploring Total calcium
#================================================================

# check if sodiumconc has a missing value
sum(is.na(salivadata$calcium))

# EXPLORING SIGNIFICANCE


ca.model <- lmer(calcium ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                # we just estimate the mean of the data set
                data = salivadata, REML = FALSE)


summ(ca.model, confint = TRUE)



# # Investigating age

age0 <- lmer(calcium ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# add  agecat2

age2 <- lmer(calcium ~ treatment  + agecat2 + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

age3 <- lmer(calcium ~ treatment  + agecat3 + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if agecat2 is significant
anova(age0, age2)
# # testing if agecat is significant
anova(age0, age3)
# 
# # con conclusion, both agecat2 and agecat3 are not significant
#
# #==================================================================

bmi0 <- lmer(calcium ~ treatment   + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

bmi1 <- lmer(calcium ~ treatment  + bmicat + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

# 
# # testing if bmicatis significant
anova(bmi0, bmi1)


# Final ca model
ca <- lmer(calcium ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
          # we just estimate the mean of the data set
          data = salivadata, REML = FALSE)
summ(ca, confint = TRUE)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

#aov.5 <-aov(phosphorus ~ visit*treatment + Error(patno/(visit)), salivadata) 
#summary(aov.5)
ca01 <- lmer(calcium ~ 1  + (1|patno) , # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

ca02 <- lmer(calcium ~ treatment  + (1|patno), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)
ca03 <- lmer(calcium ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
            # we just estimate the mean of the data set
            data = salivadata, REML = FALSE)

anova(ca01, ca02)  # testing for treatmemnt effect
anova(ca02, ca03)  # testing  for visitb effect

jpeg(file="./fig_output/calciuminteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = calcium, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "Calcium",
       title = "Interaction plot") 
dev.off()



#  Make predictions and extract residual

ca_residuals <-data.frame(residuals(ca))

# residuals should be normally distributed with a mean of 0

jpeg(file="./fig_output/Caresiduals.jpeg")   
ggplot(data = ca_residuals,
       aes(x = residuals.ca.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-Ca") 
dev.off()

jpeg(file="./fig_output/CaQQplot.jpeg")   
qqnorm(ca_residuals$residuals.ca) 
qqline(ca_residuals$residuals.ca) 
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(ca_residuals$residuals.ca) 
# data is very skewed

boxplot(salivadata$calcium)
####
#extract the values of the potential outliers based on the IQR
boxplot.stats(salivadata$calcium)$out

#extract the row number corresponding to these outliers:

out2 <- boxplot.stats(salivadata$calcium)$out
out_ind <- which(salivadata$calcium %in% c(out2))
out_ind
#salivadata[out_ind, ]
# create new subset without the outliers and refit the model
#row_no <-c(56, 105, 109, 111, 113, 139, 191, 194, 195, 201)
data_without_outlier2  <-salivadata %>%
  mutate(rowno = row_number()) %>%
  filter(rowno != 85 & rowno != 87 & rowno !=89 & rowno != 139
         & rowno != 160 & rowno != 207) # deleting outliers

# refit the model on  data set without outluiers for protemnconcen

boxplot(data_without_outlier2$calcium)

caM1 <- lmer(calcium ~ treatment + (1|patno) +  (1|visit),
            data = data_without_outlier2, REML = FALSE)

# extract parameters
summ(caM1, confint = TRUE)

ca_residualsM1 <-data.frame(residuals(caM1))

# residuals should be normally distributed with a mean of 
jpeg(file="./fig_output/QQplot.jpeg")   
qqnorm(ca_residualsM1$residuals.caM1.) 
qqline(ca_residualsM1$residuals.caM1.) 
dev.off()

shapiro.test(ca_residualsM1$residuals.caM1.)

ca.model <- lmer(calcium ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                # we just estimate the mean of the data set
                data = data_without_outlier2, REML = FALSE)
summ(ca.model, confint = TRUE)

# After deleting outliers, residuals still not normally distributed


#=================================================================
#  Exploring Total zinc
#================================================================

# check if sodiumconc has a missing value
sum(is.na(salivadata$zinc))

# EXPLORING SIGNIFICANCE


zn.model <- lmer(zinc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
                 # we just estimate the mean of the data set
                 data = salivadata, REML = FALSE)


summ(zn.model, confint = TRUE)


# Zinc is clearly not statistically significant --> no further action

by(data=salivadata, INDICES=salivadata$treatment, FUN=lmer,
   formula=formula("zinc ~ treatment  + (1|patno) +  (1|visit)"))

#=============================================
# Mdel for sulphure

#==============================================

sul <- lmer(sulphur ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(sul, confint = TRUE)


# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

#aov.5 <-aov(phosphorus ~ visit*treatment + Error(patno/(visit)), salivadata) 
#summary(aov.5)
sul01 <- lmer(sulphur ~ 1  + (1|patno) , # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

sul02 <- lmer(sulphur ~ treatment  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)
sul03 <- lmer(sulphur ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
             # we just estimate the mean of the data set
             data = salivadata, REML = FALSE)

anova(sul01, sul02)  # testing for treatmemnt effect
anova(sul02, sul03)  # testing  for visitb effect

jpeg(file="./fig_output/sulinteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = sulphur, color = visit)) +
  geom_boxplot() +
  labs(x = "treatment", y = "Sulphur",
       title = "Interaction plot") 
dev.off()

#  Make predictions and extract residual
sul_residuals <-data.frame(residuals(sul))

# residuals should be normally distributed with a mean of 0
jpeg(file="./fig_output/sulresiduals.jpeg")   
ggplot(data = sul_residuals,
       aes(x = residuals.sul.)) +
  geom_density() +
  labs(x = "residuals", y = "density",
       title = "Residual plot-S") 
dev.off()

jpeg(file="./fig_output/sulQQplot.jpeg")   
qqnorm(sul_residuals$residuals.sul) 
qqline(sul_residuals$residuals.sul) 
dev.off()

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(sul_residuals$residuals.sul) 


