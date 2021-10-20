#==========================================================================
# Researcher: katherine H
# Project Title: To apply linear mixed model to explore, quantify and extiomate the difference between simulated and unsimulated saliva
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
library(mice)

# Importing data
salivadata <-read_csv("./data/mmdata.csv")


# Data preparation
str(salivadata)
# converting the relevant variables as a factor for easy analysis
salivadata$visit  <-factor(salivadata$visit, levels = c(1,2,3,4), labels = c("V1", "V2", "V3", "V4"))
salivadata$agecat  <-factor(salivadata$agecat, levels = c(1,2), labels = c("11-30", "31-55"))
salivadata$bmicat  <-factor(salivadata$bmicat, levels = c(1,2), labels = c("<=25", ">25"))
salivadata$treatment <-ifelse(salivadata$treatment == "US",0,1)
salivadata$treatment  <-factor(salivadata$treatment, levels = c(0,1), labels = c("US", "SS"))

# Create ID by group
salivadata <- transform(salivadata,                                 
                        ID = as.numeric(factor(treatment)))

# checking and dealing with missing values
# missing values investigation
summary(salivadata)
# there was one row with missing enteries for all variables that was deleted
salivadata <-salivadata %>%
  filter(patno != "") %>%
  arrange(patno) # arranging dataset according to subject number


# Estimating the mean(sd) of each parameter by treatment group
# also counting the unique subjects per treatment group
summary_by_treatment<-
  salivadata %>%
  group_by(treatment) %>%
  summarize(meanflowrate = round(mean(flowrate),2),
            sdflow = round(sd(flowrate, na.rm = TRUE),3),
            meanage = round(mean(age),2),
            sdage = round(sd(age, na.rm = TRUE),3),
            meanbmi = round(mean((bmi), na.rm = TRUE),2),
            sdbmi = round(sd(bmi, na.rm = TRUE),3),
            meanpro = round(mean((protenconc), na.rm = TRUE),2),
            sdpro = round(sd(protenconc, na.rm = TRUE),3),
            meansod = round(mean((sodiumconc), na.rm = TRUE),2),
            sdsod = round(sd(sodiumconc, na.rm = TRUE),3),
            meanmag = round(mean((magnesiumconc), na.rm = TRUE),2),
            sdmag = round(sd(magnesiumconc, na.rm = TRUE),3),
            meanphs = round(mean((phosphorus), na.rm = TRUE),2),
            sdphs = round(sd(phosphorus, na.rm = TRUE),3),
            meansul = round(mean((sulphur), na.rm = TRUE),2),
            sdsul = round(sd(sulphur, na.rm = TRUE),3), 
            meanpot = round(mean((potassiom), na.rm = TRUE),2),
            sdpot = round(sd(potassiom, na.rm = TRUE),3),
            meancal = round(mean((calcium), na.rm = TRUE),2),
            sdcal = round(sd(calcium, na.rm = TRUE),3),
            meanzinc = round(mean((zinc), na.rm = TRUE),2),
            sdzinc = round(sd(zinc, na.rm = TRUE),3),
            n_pop  = n_distinct(patno), .groups = "keep")
# saving results
write.csv(summary_by_treatment,"./data_output/summary_by_treatment.csv")

# Estimating the mean(sd) of each parameter by treatment group and visit

summary_by_treatment_by_visit<-
  salivadata %>%
  group_by(treatment,visit ) %>%
  summarize(meanflowrate = round(mean(flowrate),2),
            sdflow = round(sd(flowrate, na.rm = TRUE),3),
            meanage = round(mean(age),2),
            sdage = round(sd(age, na.rm = TRUE),3),
            meanbmi = round(mean((bmi), na.rm = TRUE),2),
            sdbmi = round(sd(bmi, na.rm = TRUE),3),
            meanpro = round(mean((protenconc), na.rm = TRUE),2),
            sdpro = round(sd(protenconc, na.rm = TRUE),3),
            meansod = round(mean((sodiumconc), na.rm = TRUE),2),
            sdsod = round(sd(sodiumconc, na.rm = TRUE),3),
            meanmag = round(mean((magnesiumconc), na.rm = TRUE),2),
            sdmag = round(sd(magnesiumconc, na.rm = TRUE),3),
            meanphs = round(mean((phosphorus), na.rm = TRUE),2),
            sdphs = round(sd(phosphorus, na.rm = TRUE),3),
            meansul = round(mean((sulphur), na.rm = TRUE),2),
            sdsul = round(sd(sulphur, na.rm = TRUE),3), 
            meanpot = round(mean((potassiom), na.rm = TRUE),2),
            sdpot = round(sd(potassiom, na.rm = TRUE),3),
            meancal = round(mean((calcium), na.rm = TRUE),2),
            sdcal = round(sd(calcium, na.rm = TRUE),3),
            meanzinc = round(mean((zinc), na.rm = TRUE),2),
            sdzinc = round(sd(zinc, na.rm = TRUE),3),
            n_pop  = n_distinct(patno), .groups = "keep")

write.csv(summary_by_treatment_by_visit,"./data_output/summary_by_treatment_by_visit.csv")

# this is a summary of the mean by each subject
mean_by_patno<-
  salivadata %>%
  group_by(patno) %>%
  summarize(meanflowrate = round(mean(flowrate),2),
            sdflow = round(sd(flowrate, na.rm = TRUE),3),
            meanage = round(mean(age),2),
            sdage = round(sd(age, na.rm = TRUE),3),
            meanbmi = round(mean((bmi), na.rm = TRUE),2),
            sdbmi = round(sd(bmi, na.rm = TRUE),3),
            meanpro = round(mean((protenconc), na.rm = TRUE),2),
            sdpro = round(sd(protenconc, na.rm = TRUE),3),
            meansod = round(mean((sodiumconc), na.rm = TRUE),2),
            sdsod = round(sd(sodiumconc, na.rm = TRUE),3),
            meanmag = round(mean((magnesiumconc), na.rm = TRUE),2),
            sdmag = round(sd(magnesiumconc, na.rm = TRUE),3),
            meanphs = round(mean((phosphorus), na.rm = TRUE),2),
            sdphs = round(sd(phosphorus, na.rm = TRUE),3),
            meansul = round(mean((sulphur), na.rm = TRUE),2),
            sdsul = round(sd(sulphur, na.rm = TRUE),3), 
            meanpot = round(mean((potassiom), na.rm = TRUE),2),
            sdpot = round(sd(potassiom, na.rm = TRUE),3),
            meancal = round(mean((calcium), na.rm = TRUE),2),
            sdcal = round(sd(calcium, na.rm = TRUE),3),
            meanzinc = round(mean((zinc), na.rm = TRUE),2),
            sdzinc = round(sd(zinc, na.rm = TRUE),3),
            n_pop  = n_distinct(patno), .groups = "keep")

write.csv(mean_by_patno,"./data_output/mean_by_patno.csv")


# counting the number of repeated measurements by visit
visit_count<-
  salivadata %>%
  group_by(visit) %>%
  summarize(count = n())

write.csv(visit_count,"./data_output/visit_count.csv")

# Summarizing categorical variables,gender and ethnicity
# this will be done per group due to the repeated nature of the data set
# For ss group
# This code calculate the stats for  gender by treatment group
ss_gender <- salivadata %>%
  filter(treatment =="SS") %>%
  mutate(tag = as.numeric(duplicated(patno)))%>%
  filter(tag == 0)%>%
  group_by(treatment, gender) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100),0))
# This code calculate the stats for  ethnicity by treatment group
ss_ethnicity <- salivadata %>%
  filter(treatment =="SS") %>%
  mutate(tag = as.numeric(duplicated(patno)))%>%
  filter(tag == 0)%>%
  group_by(treatment, ethnicity) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100),0))
  

# For US group

# This code calculate the stats for  gender by treatment group
us_gender <- salivadata %>%
  filter(treatment =="US") %>%
  mutate(tag = as.numeric(duplicated(patno)))%>%
  filter(tag == 0)%>%
  group_by(treatment, gender) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100),0))

# This code calculate the stats for  ethnicity by treatment group
us_ethnicity <- salivadata %>%
  filter(treatment =="US") %>%
  mutate(tag = as.numeric(duplicated(patno)))%>%
  filter(tag == 0)%>%
  group_by(treatment, ethnicity) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100),0))


# combing the gender data set
gender<- rbind(ss_gender, us_gender)
write.csv(gender,"./data_output/gender.csv")
# combing the ethnicity data set
ethnicity<- rbind(ss_ethnicity, us_ethnicity)
write.csv(ethnicity,"./data_output/ethynicity.csv")


# reveal variables with missing values
missing_data <-data.frame(md.pattern((salivadata)))
write.csv(missing_data,"./data_output/missing_data.csv")
#apply(is.na(salivadata), 2, which)


# Data Visualisation 

#  by treatment
jpeg(file="./fig_output/group_differences.jpeg")
ggplot(data = salivadata, aes(x = treatment, y = flowrate, group = patno)) +
  geom_point() +
  geom_line() +
  labs(x = "treatment", y = "flowrate",
       title = "Group differences") +
  theme_minimal() 
# this shows an increase in flow rate for SS compared to US
dev.off()


# plot the raw data for flowrate over visit
jpeg(file="./fig_output/visitdifferences.jpeg")
ggplot(data = salivadata, aes(x = visit, y = flowrate, color = treatment)) +
  geom_point() +
  geom_line() +
  labs(x = "visit", y = "flowrate",
       title =" Variation across visits") +
  theme_minimal() 
dev.off()


# Plot the mean of SS compared to US for flowrate


# this graph plots the means for each individual and show the 95% CI
jpeg(file="./fig_output/subjectdifferences.jpeg")
plotmeans(flowrate ~ patno, main= "Hererogeneity across individual(mean values)", data = salivadata)
dev.off()

jpeg(file="./fig_output/visitdifferences.jpeg")
plotmeans(flowrate ~ visit, main= "Hererogeneity across visit(mean values)", data = salivadata)
dev.off()


jpeg(file="./fig_output/scatter_flowrate4.jpeg")
ggplot(data = summary_by_treatment_by_visit, aes(x = visit, y = meanflowrate, color = treatment)) +
  geom_point() +
  geom_line() +
  labs(x = "visit", y = "meanflowrate",
       title = "Hererogeneity across visit")
dev.off()

jpeg(file="./fig_output/scatter_flowrate5.jpeg")   
ggplot(data = salivadata,
       aes(x = treatment, y = flowrate)) +
  geom_boxplot()
dev.off()

jpeg(file="./fig_output/scatter_flowrate6.jpeg")
ggplot(data = salivadata,
       aes(x = treatment, y = flowrate, colour = gender)) +
  geom_boxplot() +
  labs(x = "treatment", y = "flowrate",
       title = "Gender Differences")
dev.off()

jpeg(file="./fig_output/scatter_flowrate7.jpeg")
ggplot(data = salivadata,
       aes(x = ethnicity, y = flowrate, colour = treatment)) +
  geom_boxplot() +
  labs(x = "treatment", y = "flowrate",
       title = "Ethnicity Differences")
dev.off()
 


# plot the subject mean
jpeg(file="./fig_output/sunject_mean.jpeg")
ggplot(data = mean_by_patno, aes(x = patno, y = meanflowrate)) +
  geom_point() +
  geom_line() +
  labs(x = "subject", y = "meanflowrate",
       title = "Individual differences") +
  theme_minimal() 
# this shows an increase in flow rate for SS compared to US
dev.off()


jpeg(file="./fig_output/scatter_flowrate8.jpeg")   
ggplot(data = salivadata,
       aes(x = as.factor(visit), y = flowrate)) +
  geom_boxplot() +
  labs(x = "visit", y = "flowrate",
       title = "Visit differences") 
dev.off()

# 

jpeg(file="./fig_output/scatter_flowrate9.jpeg")   
ggplot(data = salivadata,
       aes(x = as.factor(patno), y = flowrate)) +
  geom_boxplot() +
  labs(x = "patno", y = "flowrate",
       title = "Subject differences") 
dev.off()

# The results below shows that there is a significant difference in flow rate for SS and US, but no significant difference over time

with(salivadata, interaction.plot(visit, treatment, flowrate,
                                  ylab = "mean flowrate", xlab = "visit", trace.label = "treatment"))


salivadata.aov <- aov(flowrate ~ treatment * visit + Error(patno), data = salivadata)
summary(salivadata.aov) # there is a statistically significant difference between US and SS, but no difference in patno and visit

# saving data in ouput file  for later use
save(salivadata, file = "./data_output/analysisset_saliva.Rdata") 

#==============================================
# this is a sctatter plot of flow rate of each individual
jpeg(file="./fig_output/scatter_flowrate10.jpeg") 
ggplot(salivadata, aes(x = visit, y = flowrate)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "black") + 
  theme_bw() + facet_wrap(~patno)
# from the plot, , each dot is a single measurement, there is no  clear pattern of flow rate of each subject across visits
dev.off()




saliva <-salivadata %>%
  group_by(treatment,visit) %>%
  summarise(Y.bar=mean(flowrate),SD=sd(flowrate),N=n(),SE=SD/sqrt(N))

dodge <- position_dodge(.1)
ggplot(salivadata,aes(x=visit,y=flowrate,group=treatment,color=treatment,shape=treatment)) +   theme_bw() +
  stat_summary(position=dodge,fun.data=mean_se) +
  stat_summary(geom="line")


