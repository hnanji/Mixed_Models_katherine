

GLMMs allow researchers to use
the dependent variable most appropriate to their research
question, while simultaneously meeting the mathematical
criterion of normalized, homoscedastic residuals in linear
regression. 
GLMMs combine and extend the properties of LMM and
generalized linear model (GLM) approaches, by relaxing LMM’s
assumption that the dependent variable (and the residuals) follow
a normal (Gaussian) distribution, and extending GLM’s scope of
inference to extend beyond a single random population. Rather
than making the default assumptions of LMM methods, GLMM
requires researchers to specify a number of components of their
data and design:
   (1) the explanatory variables responsible for systematic variation
in responses: referred to as the fixed factors;

(2) the sampling structure of the design contributing to random
variability in responses: the random factors;
(3) the probability distribution describing the plausible
processes underlying the observed data: the distribution of
the dependent variable; and
(4) the mathematical function characterizing the relationship
between the fixed factors and the dependent variable: the link
function.

Using GLMM to Avoid the Need for
Transformation of Skewed RT Data


agam <− glm ( yobs ∼ I ( 1 / x seq ) , f amil y = Gamma( l i n k = ” i d e n t i t y ”) , c o n t r o l =
                 glm. c o n t r ol ( maxit =100) , s t a r t=c ( 1 , 6 5 ) )
l i b r a r y (MASS)
myshape <− gamma.shape ( agam )
gampred <− p r e d i c t ( agam , type=” r e s p o n s e ” , s e=T, d i s p e r s i o n=1/myshape$ alph a )

#Gamma(link = "inverse")
#inverse.gaussian(link = "1/mu^2")



# Try fitting a gamma generalized linear model via the glmer() function from the lme4 package:
#==============================================================================
# LMM for flowrate
# using the Gamma(link = "inverse")
flow <-  lmer(flowrate ~ treatment + (1|patno) +  (1|visit),
              data = salivadata, REML = FALSE)

#plot(resid(pro, type = "pearson") ~ fitted(pro))
#qqnorm(resid(pro, type = "pearson"))
#qqline(resid(pro, type = "pearson"))

flow.residuals <-data.frame(residuals(flow))
jpeg(file="./fig_output/protenresid.jpeg")
qqnorm(flow.residuals$residuals.flow.) 
qqline(flow.residuals$residuals.flow.)
dev.off()
shapiro.test(flow.residuals$residuals.flow.)


summ(flow, confint = TRUE)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

flow1 <- lmer(flowrate ~ 1  + (1|patno),
               data = salivadata, REML = FALSE)

flow2 <- lmer(flowrate ~ treatment  + (1|patno),
              data = salivadata, REML = FALSE)
flow3 <- lmer(flowrate ~ treatment  + (1|patno) +  (1|visit),
               data = salivadata, REML = FALSE)

anova(flow1, flow2) # testing for treatment effect
anova(flow2, flow3) # testing for withing subject effect(visit effect)

jpeg(file="./fig_output/proteninteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = flowrate, color = visit)) +
   geom_boxplot() +
   labs(x = "treatment", y = "Proten conc",
        title = "Interaction plot") 
dev.off()




#==============================================================================
# LMM for protein
# using the Gamma(link = "inverse")
pro <- glmer(protenconc ~ treatment  + (1|patno) +  (1|visit), 
             family = Gamma(link = "inverse"),
             data = salivadata)

#plot(resid(pro, type = "pearson") ~ fitted(pro))
qqnorm(resid(pro, type = "pearson"))
qqline(resid(pro, type = "pearson"))

pro.residuals <-data.frame(residuals(pro))
jpeg(file="./fig_output/protenresid.jpeg")
qqnorm(pro.residuals$residuals.pro.) 
qqline(pro.residuals$residuals.pro.)
dev.off()
shapiro.test(pro.residuals$residuals.pro.)


summ(pro, confint = TRUE)

# TESTING EFFECT OF VISIT AND TREATMENT FOR SS AND US

pr01 <- glmer(protenconc ~ 1  + (1|patno), # this is an intercept only model where
             # we just estimate the mean of the data set
             family = Gamma(link = "inverse"),data = salivadata)

pr02 <- glmer(protenconc ~ treatment  + (1|patno), 
              family = Gamma(link = "inverse"),data = salivadata)
pr03 <- glmer(protenconc ~ treatment  + (1|patno) + (1|visit),
              family = Gamma(link = "inverse"),data = salivadata)

anova(pr01, pr02) # testing for treatment effect
anova(pr02, pr03) # testing for withing subject effect(visit effect)

jpeg(file="./fig_output/proteninteraction.jpeg")
ggplot(data = salivadata,
       aes(x = as.factor(treatment), y = flowrate, color = visit)) +
   geom_boxplot() +
   labs(x = "treatment", y = "Proten conc",
        title = "Interaction plot") 
dev.off()

#===========================================================
# LMM for magnesium
# using the Gamma(link = "inverse")
mg <- glmer(magnesiumconc ~ treatment  + (1|patno), 
             family = Gamma(link = "inverse"),
             data = salivadata)
summ(mg, confint = TRUE)
# NB using the gamma model, this model didnt convergre, so so updated report is based on normal molde on previous script




mg.residuals <-data.frame(residuals(mg))
jpeg(file="./fig_output/protenresid.jpeg")
qqnorm(mg.residuals$residuals.mg.) 
qqline(mg.residuals$residuals.mg.)
dev.off()
shapiro.test(mg.residuals$residuals.mg.)

# The gamma model didnt impriove the fit, so progress stopped here

#===============================================

# LMM for phosphorus
# using the Gamma(link = "inverse")
p <- glmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), 
             family = Gamma(link = "inverse"),
             data = salivadata)


p1 <- glmer(phosphorus ~ treatment  + (1|patno) +  (1|visit), 
           inverse.gaussian(link = "1/mu^2"),
           data = salivadata)
# for this two distribution, the model didnt converge, so progress weas stopped


#=====================================

# LMM for potassium
# using the Gamma(link = "inverse")
k <- glmer(potassiom ~ treatment  + (1|patno) +  (1|visit), 
             family = Gamma(link = "inverse"),
             data = salivadata)
# gamma model produces and arror and cannot be estimated
#====================================================
# LMM for calcuim
# using the Gamma(link = "inverse")
ca <- glmer(calcium ~ treatment  + (1|patno) +  (1|visit), 
           family = Gamma(link = "inverse"),
           data = salivadata)
# gamma model produces and arror and cannot be estimated

#====================================================
# LMM for calcuim
# using the Gamma(link = "inverse")
zn <- glmer(zinc ~ treatment  + (1|patno) +  (1|visit), 
            family = Gamma(link = "inverse"),
            data = salivadata)

# gamma model produces and arror and cannot be estimated


# LMM for calcuim
# using the Gamma(link = "inverse")
sol <- glmer(sulphur ~ treatment  + (1|patno) +  (1|visit), 
            family = Gamma(link = "inverse"),
            data = salivadata)

# gamma model produces and arror and cannot be estimated


