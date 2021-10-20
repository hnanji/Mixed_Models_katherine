# Set the seed to be 345659
set.seed(345659)

# Model 10 individuals 
n_ind <- 10

# simulate before with mean of 0 and sd of 0.5
before <- rnorm(n_ind, mean = 0, sd = 0.5)
# simulate after with mean effect of 4.5 and standard devation of 5
after  <- before + rnorm(n_ind, mean = 4.5, sd = 5)


# Run a standard, non-paired t-test
t.test(x = before, y = after, paired = FALSE)

# Run a standard, paired t-test
t.test(x = before, y = after, paired = TRUE)

# unequal variance (that is, the standard deviations will be different).
# The paired t-test is more powerful because it accounts for individual variability
#==================================================================================

# Create the data.frame, using the variables from the previous exercise. 
# y is the joined vectors before and after.
# trial is the repeated names before and after, each one repeated n_ind
# ind is the letter of the individual, repeated 2 times (because there were two observations)
dat <- data.frame(y = c(before, after), 
                  trial = rep(c("before", "after"), each = n_ind),
                  ind = rep(letters[1:n_ind], times = 2))

# installing the package to use
install.packages("lmerTest")
library(lmerTest)

# Run a standard, paired t-test
t.test( before, after, paired = TRUE)

# Run a lmer and save it as lmer_out 
# this applies ind as a random-effect intercept
lmer_out <- lmer(y ~ trial + (1 | ind), data = dat)

# Look at the summary() of lmer_out
summary(lmer_out)

# above : The main purpose of this exercise was to show you a paired 
# t-test is a special case of repeated measures ANOVA and a repeated
# measures ANOVA is simply a special case of a linear mixed-effects model
#==============================================================================================

# the null hupothesis is that thee is no difference in the variability of sleep(the outcome) whether drug 1 or 2 was used

# the alternative hypothesis is that the drug affetcs the variability of sleep

# steps, visualizse the data
# build simple model
# build model of interest
# extact information  of interest
# visalise results

sleep <-read_csv("./data/sleep.csv")

# The ID of each patient allows us to do a repeated 
#measures analysis. This is a random-effect intercept 
#and corresponds to the baseline effect of giving a person a sleeping drug.

# plot the raw data
ggplot(data = sleep, aes(x = group, y = extra)) +
  geom_point()

# Replace geom_point() with geom_line() because you 
#are plotting results from the same individuals.
# Add group = ID in the aes() function

# replace geom_point with geom_line
ggplot(data = sleep, aes(x = group, y = extra, group = id)) +
  geom_line() +
  xlab(label = "Drug") +
  ylab(label = "Extra sleep") + 
  theme_minimal() 

# Also, in this figure, notice how almost all individuals 
#had a greater increase in sleep with the second drug 
# compared to the first. This suggests the second drug is more effective.

# step 2: Build a linear model using lm(). The goal of this step is to simply 
# make sure the model builds without errors or warnings'

# Build a simple linear model 
lm(extra ~ group + id, data = sleep)

# Build a mixed-effects regression
# Build a lmer() model with extra predicted by the fixed-effect group 
#and random-effect intercept ID using the sleep data.
lmer_out <- lmer(extra ~ group + (1 | id), data = sleep)  # Your formula should include the random-effect intercept (1 | ID)


# First, use the lmer_out model you built to run an anova() on it to see if group explains 
# a significant amount of variability. Second, you will examine the 
# regression coefficient from group to see if it significantly differs from zero.

# Run an anova() on lmer_out
anova(lmer_out)

# Look at the summary() of lmer_out
summary(lmer_out)

# notice that  the p values in both cases are exactly the same

#  the ANOVA tells us which variables explain a significant amount 
#of variability, but the ANOVA does not tell us how things are #
#different. Conversely, the regression tells us how much one #
#unit of input would be expected to change the model's output.

#=====================================================================

# VISALISATION OF RESULTS

#  In the previous exercises, you have examined the raw data,
# used the data to build a model, and applied the model for statistical
# inferences. You found drug 2 increased the amount of extra sleep compared to drug 1.
# During this exercise, you will plot the results to see how much drug 2 increased extra sleep.

# First, wrangle the data by using pivot_wider() function from the tidyr package.
# Then, calculate the difference in extra sleep for each individual. Last, plotthis 
# difference as a histogram

# Load the tidyr package
library(tidyr)

# Make the data wider
sleep_wide <- 
  pivot_wider(sleep, 
              names_from = group, values_from = extra)

# Calculate the difference 
sleep_wide$diff <- sleep_wide$`2` - sleep_wide$`1`


# Use the data sleep_wide and diff for the aesthetic x  
ggplot(sleep_wide, aes(x = diff)) + 
  geom_histogram() +
  xlab(label = "Extra sleep from drug 2") +
  ylab(label = "Count") +
  theme_bw()

#=========================================================

# Build a liner model including group as fixed-effect model
install.packages("broom.mixed")
library(broom.mixed)
lm_out1 <- lm(extra ~ group , data = sleep)

# Build a mixed-effect model including class id as a random-effect
lmer_out2 <- lmer(extra ~ group + (1 | id), data = sleep)

# Extract out the slope estimate for mathkind
tidy(lm_out1) %>%
  filter(term == "group")

tidy(lmer_out2) %>%
  filter(term == "group")


#==================================================================#

# Re-run the models to load their outputs
lm_out1 <- lm(extra ~ group , data = sleep)
lmer_out2 <- lmer(extra ~ group + (1 | id), data = sleep)

# Add the predictions to the original data
sleep_data_subset <-
  sleep %>%
  mutate(lm_predict = predict(lm_out1),
         lmer_predict = predict(lmer_out2))
  #filter(schoolid == "1")

# Plot the predicted values. examine this carefully and use it to plot residuals

ggplot(sleep_data_subset,
       aes(x = group, y = extra, color = id)) +
  geom_point() +
  geom_line(aes(x = group, y = lm_predict)) +
  geom_line(aes(x = group, y = lmer_predict), linetype = 'dashed') +
  xlab("Kindergarten math score") +
  ylab("Math gain later in school") +
  theme_bw() +
  scale_color_manual("Class ID", values = c("red", "blue"))

#===============================================
# thhis code is adding other predictors

# Build the model
lmer_classroom <-
  lmer(mathgain ~
         mathknow + mathprep + sex + mathkind + ses +
         (1 | classid),
       data = student_data)

# Print the model's output
print(lmer_classroom)

# Extract coefficents
lmer_coef <-
  tidy(lmer_classroom, conf.int = TRUE)

# Print coefficents
print(lmer_coef)

# Extract coefficents
lmer_coef <-
  tidy(lmer_classroom, conf.int = TRUE)

# Plot results
lmer_coef %>%
  filter(effect == "fixed" & term != "(Intercept)") %>%
  ggplot(., aes(x = term, y = estimate,
                ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = 'red') + 
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate and 95% CI") +
  xlab("Regression coefficient")



