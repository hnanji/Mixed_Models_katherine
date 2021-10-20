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


# Importing data

load("~/Katherine_Mixed Model/linearmixedmodel/data_output/analysisset_saliva.Rdata")



# Final flow Model
flowmodel <- lmer(flowrate ~ treatment + (1|patno) +  (1|visit),
                           data = salivadata, REML = FALSE)
# extract parameters
summ(flowmodel, confint = TRUE)
#ICC of 59% mean that 59% of variation in flow rate is explained buy  differences in participants
#and only 1% explained by differences over the different visits



#  Make predictions and extract residuals
flowsubset <-
  salivadata %>%
  mutate(lm_predict = predict(flowmodel)) %>%
 mutate(resid = (flowrate - lm_predict)) # calculate residuals




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

# Final Model which includes treatment only
proteinmodel <- lmer(protenconc ~ treatment + (1|patno) +  (1|visit),
                    data = salivadata, REML = FALSE)

# extract parameters
summ(proteinmodel, confint = TRUE)




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

# appplying gamma model

protein_g <- glmer(protenconc ~ treatment  + (1|patno) +  (1|visit), 
                     family = Gamma(link = "identity"),  # link function = identity
                     data = salivadata)

proten_residuals_g <-data.frame(residuals(protein_g))


qqnorm(proten_residuals_g$residuals.protein_g.) 
qqline(proten_residuals_g$residuals.protein_g.) 

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(proten_residuals_g$residuals.protein_g.)

# extract coefficient since residuakls are normnal
summ(protein_g, confint = TRUE)

#==================


# Final sodium model

sodium.model <- lmer(sodiumconc ~ treatment  + (1|patno), # this is an intercept only model where
                     # we just estimate the mean of the data set
                     data = salivadata, REML = FALSE)

summ(sodium.model, confint = TRUE)

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
#=======================================================
# investigating gamma model
na_g <- glmer(sodiumconc ~ treatment  + (1|patno), 
                   family = Gamma(link = "identity"),  # link function = identity
                   data = salivadata)

na_residuals_g <-data.frame(residuals(na_g))


qqnorm(na_residuals_g$residuals.na_g.) 
qqline(na_residuals_g$residuals.na_g.) 

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(na_residuals_g$residuals.na_g.)
# after applyimng gamma model, the resuduals are still not normal


#=================================================================
#  Exploring Total magnesium concentration
#================================================================
# Final mg model
mg <- lmer(magnesiumconc ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(mg, confint = TRUE)


#  Make predictions and extract residual

mg_residuals <-data.frame(residuals(mg))

qqnorm(mg_residuals$residuals.mg) 
qqline(mg_residuals$residuals.mg) 


# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(mg_residuals$residuals.mg) 
#===========================================================================
# investigating gamma model
mg_g <- glmer(magnesiumconc ~ treatment  + (1|patno) +  (1|visit), 
                   family = Gamma(link = "identity"),  # link function = identity
                   data = salivadata)

mg_residuals_g <-data.frame(residuals(mg_g))


qqnorm(mg_residuals_g$residuals.mg_g.) 
qqline(mg_residuals_g$residuals.mg_g.) 

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(mg_residuals_g$residuals.mg_g.)

#============================================================================


#=================================================================
#  Exploring Total  phosphorus
#================================================================



# Final mg model
ph <- lmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(ph, confint = TRUE)

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
#===========================================================================
# investigating gamma model
ph_g <- glmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), 
              family = Gamma(link = "identity"),  # link function = identity
              data = salivadata)

ph_residuals_g <-data.frame(residuals(ph_g))


qqnorm(ph_residuals_g$residuals.ph_g.) 
qqline(ph_residuals_g$residuals.ph_g.) 

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(ph_residuals_g$residuals.ph_g.)

#============================================================================

#=================================================================
#  Exploring Total  potassiom
#================================================================

# Final k model
k <- lmer(potassiom ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(k, confint = TRUE)



#  Make predictions and extract residual

k_residuals <-data.frame(residuals(k))


 
qqnorm(k_residuals$residuals.k) 
qqline(k_residuals$residuals.k) 


# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(k_residuals$residuals.k) 

#===========================================================================
# investigating gamma model
ph_g <- glmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), 
              family = Gamma(link = "identity"),  # link function = identity
              data = salivadata)

ph_residuals_g <-data.frame(residuals(ph_g))


qqnorm(ph_residuals_g$residuals.ph_g.) 
qqline(ph_residuals_g$residuals.ph_g.) 

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(ph_residuals_g$residuals.ph_g.)

#============================================================================







#=================================================================
#  Exploring Total calcium
#================================================================

# Final ca model
ca <- lmer(calcium ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
          # we just estimate the mean of the data set
          data = salivadata, REML = FALSE)
summ(ca, confint = TRUE)

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(ca_residuals$residuals.ca) 
#===========================================================================
# investigating gamma model
ca_g <- glmer(calcium ~ treatment  + (1|patno) +  (1|visit), 
              family = Gamma(link = "identity"),  # link function = identity
              data = salivadata)

ca_residuals_g <-data.frame(residuals(ca_g))


qqnorm(ca_residuals_g$residuals.ca_g.) 
qqline(ca_residuals_g$residuals.ca_g.) 

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(ca_residuals_g$residuals.ca_g.)

#============================================================================

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


#=============================================
# Mdel for sulphure

#==============================================

sul <- lmer(sulphur ~ treatment  + (1|patno) +  (1|visit), # this is an intercept only model where
           # we just estimate the mean of the data set
           data = salivadata, REML = FALSE)
summ(sul, confint = TRUE)

#  Make predictions and extract residual
sul_residuals <-data.frame(residuals(sul))

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(sul_residuals$residuals.sul) 

#===========================================================================
# investigating gamma model
sul_g <- glmer(sulphur ~ treatment  + (1|patno) +  (1|visit), 
              family = Gamma(link = "identity"),  # link function = identity
              data = salivadata)

sul_residuals_g <-data.frame(residuals(sul_g))


qqnorm(sul_residuals_g$residuals.sul_g.) 
qqline(sul_residuals_g$residuals.sul_g.) 

# Shapiro-Wilk test of approximately normally. if P >0.05, normality can be assumed
shapiro.test(sul_residuals_g$residuals.sul_g.)

#============================================================================

