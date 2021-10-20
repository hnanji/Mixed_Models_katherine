# Build the model
lmer_classroom <-
  lmer(mathgain ~
         mathknow + mathprep + sex + mathkind + ses +
         (1 | classid),
       data = student_data)

# Print the model's output
print(lmer_classroom)



lm_out1 <- lm(extra ~ group , data = sleep)

# Build a mixed-effect model including class id as a random-effect
lmer_out2 <- lmer(extra ~ group + (1 | id), data = sleep)




meanflow_bytreatment<-
  salivadata %>%
  group_by(treatment) %>%
  summarize(meanflowrate = round(mean(flowrate),2),
            n_pop  = n(), .groups = "keep")


politeness=
  read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

