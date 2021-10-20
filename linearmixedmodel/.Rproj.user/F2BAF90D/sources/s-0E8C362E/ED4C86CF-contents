#==========================================================================
# Researcher: katherine H
# Project Title: Correlation Analysis
# Name: Henry Nanji
# Date: Monday 20.09.21
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
library(funModeling)
data(heart_disease)

# https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/
  
rawdatacorr1 <- read_csv("data/darta1.csv")

round(cor(rawdatacorr1 ),digits = 2)

# nornamity checks for each variable using
qqnorm(rawdatacorr1$`logBET DT`) 
qqline(rawdatacorr1$`logBET DT`) 

qqnorm(rawdatacorr1$`logBET RT`) 
qqline(rawdatacorr1$`logBET RT`) 

qqnorm(rawdatacorr1$`logpercieved1 saltiness0.056M`)
qqline(rawdatacorr1$`logpercieved1 saltiness0.056M`) 

qqnorm(rawdatacorr1$logpercieved1saltness0.56M) 
qqline(rawdatacorr1$logpercieved1saltness0.56M) 

qqnorm(rawdatacorr1$flowrate_meanUS) 
qqline(rawdatacorr1$flowrate_meanUS) 

qqnorm(rawdatacorr1$flowrate_meanSS) 
qqline(rawdatacorr1$flowrate_meanSS) 

qqnorm(rawdatacorr1$sodiumconc_meanUS)
qqline(rawdatacorr1$sodiumconc_meanUS) 

qqnorm(rawdatacorr1$sodiumconc_meanSS)
qqline(rawdatacorr1$sodiumconc_meanSS) 

qqnorm(rawdatacorr1$proteinconc_meanUS)
qqline(rawdatacorr1$proteinconc_meanUS) 

qqnorm(rawdatacorr1$proteinconc_meanSS) 
qqline(rawdatacorr1$proteinconc_meanSS)

qqnorm(rawdatacorr1$dailysodiumIntake) 

qqnorm(rawdatacorr1$dailycalorieintake) 

qqnorm(rawdatacorr1$sodiumintakePerKca) 

qqnorm(rawdatacorr1$`sodiumintake ffq`)

library(Hmisc)
#install.packages("correlations")
library(correlation)

res <-rcorr(as.matrix(rawdatacorr1))

round(res$P,3)

corr_results1 <- correlation::correlation(rawdatacorr1, include_factors = TRUE,
                         method = "spearman", p_adjust = "bonferroni")

# correlation
# Extract the p.value
results1 <- corr_results1 %>%
  filter(p <0.05)

cor.test(rawdatacorr1$`logBET DT`,rawdatacorr1$`logBET RT`, method = "spearman")


write.csv(corr_results1,"./data_output/corr_results1.csv")
writeLines(text_lines, "my_file1.txt") 
write.csv(corr_results1,"./data_output/corr_results1.txt")

write.table(corr_results1,"corr_results1.txt",sep="\t",row.names=FALSE)
#=========================================================


rawdatacorr2 <- read_csv("data/darta2.csv")

round(cor(rawdatacorr2 ),digits = 2)

# nornamity checks for each variable using
qqnorm(rawdatacorr2$`logBET DT`) 
qqline(rawdatacorr2$`logBET DT`) 

qqnorm(rawdatacorr2$`logBET RT`) 
qqline(rawdatacorr2$`logBET RT`) 

qqnorm(rawdatacorr2$`logpercieved1 saltiness0.056M`)
qqline(rawdatacorr2$`logpercieved1 saltiness0.056M`) 

qqnorm(rawdatacorr2$logpercieved1saltness0.56M) 
qqline(rawdatacorr2$logpercieved1saltness0.56M) 

qqnorm(rawdatacorr2$flowrate_meanUS) 
qqline(rawdatacorr2$flowrate_meanUS) 

qqnorm(rawdatacorr2$flowrate_meanSS) 
qqline(rawdatacorr2$flowrate_meanSS) 

qqnorm(rawdatacorr2$sodiumconc_meanUS)
qqline(rawdatacorr2$sodiumconc_meanUS) 

qqnorm(rawdatacorr2$sodiumconc_meanSS)
qqline(rawdatacorr2$sodiumconc_meanSS) 

qqnorm(rawdatacorr2$proteinconc_meanUS)
qqline(rawdatacorr2$proteinconc_meanUS) 

qqnorm(rawdatacorr2$proteinconc_meanSS) 
qqline(rawdatacorr2$proteinconc_meanSS)

qqnorm(rawdatacorr2$dailysodiumIntake) 

qqnorm(rawdatacorr2$dailycalorieintake) 

qqnorm(rawdatacorr2$sodiumintakePerKca) 

qqnorm(rawdatacorr2$`sodiumintake ffq`)

library(Hmisc)
install.packages("correlations")
library(correlation)

res2 <-rcorr(as.matrix(rawdatacorr2))

round(res2$P,3)

corr_results2 <- correlation::correlation(rawdatacorr2, include_factors = TRUE,
                                          method = "spearman", p_adjust = "bonferroni")

write.csv(corr_results2,"./data_output/corr_results2.csv")


mydata = read.csv("https://wiki.q-researchsoftware.com/images/b/b9/Ownership.csv", header = TRUE, fileEncoding="latin1")

