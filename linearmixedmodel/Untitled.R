install.packages("foreign")
library(foreign)
Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")

coplot(y ~ year|country, type="l", data=Panel) 

coplot(y ~ year|country, type="b", data=Panel) 

# Fixed effect: exploring heteroigeneity across countries(individuals)
install.packages("gplots")
library(gplots)
library(broom.mixed)

# this graph plots the means for each individual and show the 95% CI

plotmeans(y ~ country, main= "Hererogeneity across countrie", data = Panel)

# this graph plots the means for eachyear and show the 95% CI
plotmeans(y ~ year, main= "Hererogeneity across year", data = Panel)



demo1  <- read.csv("https://stats.idre.ucla.edu/stat/data/demo1.csv")
## Convert variables to factor
demo1 <- within(demo1, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})

par(cex = .6)

with(demo1, interaction.plot(time, group, pulse,
                             ylim = c(5, 20), lty= c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))






