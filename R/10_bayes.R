###
### 10_bayes.R
###
### HCI Handbook, 4th Edition
### Copyright (C) 2025 CRC Press
###
### Jacob O. Wobbrock, Ph.D.
### University of Washington
### wobbrock@uw.edu
###
### Last Updated: 02/21/2025
###

### BSD 2-Clause License
###
### Copyright (c) 2024, Jacob O. Wobbrock
### 
### Redistribution and use in source and binary forms, with or without
### modification, are permitted provided that the following conditions are met:
### 
### 1. Redistributions of source code must retain the above copyright notice, this
###    list of conditions and the following disclaimer.
###
### 2. Redistributions in binary form must reproduce the above copyright notice,
###    this list of conditions and the following disclaimer in the documentation
###    and/or other materials provided with the distribution.
###
### THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
### AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
### IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
### DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
### FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
### DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
### SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
### CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
### OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
### OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


library(plyr) # for ddply, laply
library(reshape2) # for dcast
library(EnvStats) # for gofTest
library(BayesFactor) # for ttestBF, anoveBF, posterior 
library(bayestestR) # for hdi
library(afex) # for aov_ez
library(performance) # for check_*
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova
library(emmeans) # for emmeans


##
#### 02_one_sample.csv ####
## one-sample t-test
##

# prepare data table
df <- read.csv(".\\data\\02_one_sample.csv")
df$PId = factor(df$PId)
View(df)

# descriptive statistics
summary(df)

ddply(df, .(), function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Hours),
  "Mean"=mean(data$Hours), 
  "SD"=sd(data$Hours),
  "Median"=median(data$Hours),
  "IQR"=IQR(data$Hours),
  "Max"=max(data$Hours)
))

# verify mean and sd
mean(df$Hours) # 22.5
sd(df$Hours)   # 5.0

# make a histogram
hist(df$Hours, 
     main="Distribution of CS Programming Hours", 
     xlab="Hours",
     ylab="Students",
     xlim=c(10,40),
     ylim=c(0,.1),
     freq=FALSE)
f = gofTest(df$Hours, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE)
print(f)

# normality test
shapiro.test(df$Hours)

# one-sample t.test
t.test(df$Hours, mu=20.0)

## Bayesian approach
bf = ttestBF(df$Hours, mu=20.0)
print(bf)  # BF = 1.244134, "anecdotal evidence for H1"

chains = posterior(bf, iterations=1000)
summary(chains)
head(chains)
plot(chains[,1]) # "mu"
mean(chains[,1])
hdi(chains[,1])



##
#### 03a_independent_samples.csv ####
## independent-samples t-test
##

# prepare data table
df <- read.csv(".\\data\\03a_independent_samples.csv")
df$PId = factor(df$PId)
df$Engine = factor(df$Engine)
contrasts(df$Engine) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Engine, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Minutes),
  "Mean"=mean(data$Minutes), 
  "SD"=sd(data$Minutes),
  "Median"=median(data$Minutes),
  "IQR"=IQR(data$Minutes),
  "Max"=max(data$Minutes)
))

# boxplot
boxplot(Minutes ~ Engine,
        main="Minutes by Search Engine",
        xlab="Search Engine",
        ylab="Minutes",
        col=c("lightblue","lightgreen"),
        data=df)

# build an ANOVA model and test normality and homoscedasticity
m = aov_ez(dv="Minutes", between="Engine", id="PId", type=3, data=df)
print(check_normality(m))   # Shapiro-Wilk
print(check_homogeneity(m)) # Levene's test

# independent-samples t-test
t.test(Minutes ~ Engine, var.equal=TRUE, data=df) # Student's

## Bayesian approach
bf = ttestBF(formula = Minutes ~ Engine, data=df)
print(bf)  # BF = 1.28696, "anecdotal evidence for H1"

chains = posterior(bf, iterations=1000)
summary(chains)
head(chains)
plot(chains[,2]) # "beta"
mean(chains[,2])
hdi(chains[,2])



##
#### 04a_dependent_samples.csv ####
## paired-samples t-test
##

# prepare data table
df <- read.csv(".\\data\\04a_dependent_samples.csv")
df$PId = factor(df$PId)
df$Mouse = factor(df$Mouse)
contrasts(df$Mouse) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Mouse, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Throughput),
  "Mean"=mean(data$Throughput), 
  "SD"=sd(data$Throughput),
  "Median"=median(data$Throughput),
  "IQR"=IQR(data$Throughput),
  "Max"=max(data$Throughput)
))

# boxplot
boxplot(Throughput ~ Mouse,
        main="Throughput by Mouse",
        xlab="Mouse",
        ylab="Throughput (bits/s)",
        col=c("gray","lightblue"),
        data=df)

# build an ANOVA model and check normality
m = aov_ez(dv="Throughput", within="Mouse", id="PId", type=3, data=df)
print(check_normality(m))

# paired-samples t-test
df2 <- dcast(df, PId ~ Mouse, value.var="Throughput")  # make wide-format table
View(df2)  # view wide-format table
t.test(df2$Logitech, df2$Microsoft, paired=TRUE)

## Bayesian approach
bf = ttestBF(df2$Logitech, df2$Microsoft, paired=TRUE)
print(bf)  # BF = 0.5299686, "anecdotal evidence for H0"

chains = posterior(bf, iterations=1000)
summary(chains)
head(chains)
plot(chains[,1]) # "mu"
mean(chains[,1])
hdi(chains[,1])



##
#### 05a_factorial.csv ####
## two-way ANOVA
##

# prepare data table
df <- read.csv(".\\data\\05a_factorial.csv")
df$PId = factor(df$PId)
df$Keyboard = factor(df$Keyboard)
df$Posture = factor(df$Posture)
contrasts(df$Keyboard) <- "contr.sum"
contrasts(df$Posture) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Keyboard + Posture, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$WPM),
  "Mean"=mean(data$WPM), 
  "SD"=sd(data$WPM),
  "Median"=median(data$WPM),
  "IQR"=IQR(data$WPM),
  "Max"=max(data$WPM)
))

# interaction plot with error bars
with(df, 
     interaction.plot(
       Posture, 
       Keyboard, 
       WPM, 
       ylim=c(min(WPM), max(WPM)), 
       ylab="WPM",
       main="WPM by Keyboard, Posture",
       lty=c(2,1), 
       lwd=c(3,3), 
       col=c("darkgreen","darkgray")
))
msd <- ddply(df, ~ Posture + Keyboard, function(data) c(
  "Mean"=mean(data$WPM), 
  "SD"=sd(data$WPM)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=2, lwd=3, length=0.2, col="darkgreen")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="darkgray")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=2, lwd=3, length=0.2, col="darkgreen")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="darkgray")

# build an ANOVA model and test normality and homoscedasticity
m = aov_ez(dv="WPM", between=c("Keyboard","Posture"), id="PId", type=3, data=df)
print(check_normality(m))   # Shapiro-Wilk test
print(check_homogeneity(m)) # Levene's test

# two-way ANOVA
anova(m)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Keyboard*Posture, adjust="holm")

## Bayesian approach
bf = anovaBF(formula = WPM ~ Keyboard*Posture, data=df)
print(bf)

# to sample from the posterior, provide an index 1-4
chains = posterior(bf, index=4, iterations=1000) # index=4 for Keyboard*Posture
summary(chains)
head(chains)
plot(chains[,6:9])
laply(6:9, \(x) mean(chains[,x]))
hdi(chains[,6:9])

# manual post hoc pairwise comparisons
dv1 = df[df$Keyboard == "Android" & df$Posture == "standing",]$WPM
dv2 = df[df$Keyboard == "Android" & df$Posture == "walking",]$WPM
ttestBF(dv1, dv2)

dv1 = df[df$Keyboard == "Android" & df$Posture == "standing",]$WPM
dv2 = df[df$Keyboard == "iPhone" & df$Posture == "standing",]$WPM
ttestBF(dv1, dv2)

dv1 = df[df$Keyboard == "Android" & df$Posture == "standing",]$WPM
dv2 = df[df$Keyboard == "iPhone" & df$Posture == "walking",]$WPM
ttestBF(dv1, dv2)

dv1 = df[df$Keyboard == "Android" & df$Posture == "walking",]$WPM
dv2 = df[df$Keyboard == "iPhone" & df$Posture == "standing",]$WPM
ttestBF(dv1, dv2)

dv1 = df[df$Keyboard == "Android" & df$Posture == "walking",]$WPM
dv2 = df[df$Keyboard == "iPhone" & df$Posture == "walking",]$WPM
ttestBF(dv1, dv2)

dv1 = df[df$Keyboard == "iPhone" & df$Posture == "standing",]$WPM
dv2 = df[df$Keyboard == "iPhone" & df$Posture == "walking",]$WPM
ttestBF(dv1, dv2)



##
#### 05b_factorial.csv ####
## linear mixed model
##

# prepare data table
df <- read.csv(".\\data\\05b_factorial.csv")
df$PId = factor(df$PId)
df$Keyboard = factor(df$Keyboard)
df$Posture = factor(df$Posture)
contrasts(df$Keyboard) <- "contr.sum"
contrasts(df$Posture) <- "contr.sum"
View(df)

# The data is the same as 05a_factorial.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# linear mixed model (LMM)
m = lmer(WPM ~ Keyboard*Posture + (1|PId), data=df)
print(check_normality(m)) # Shapiro-Wilk
# no sphericity assumption for LMMs!

# analysis of variance
Anova(m, type=3, test.statistic="F")

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Keyboard*Posture, adjust="holm")

## Bayesian approach
bf = anovaBF(formula = WPM ~ Keyboard*Posture + PId, whichRandom="PId", data=df)
print(bf)

# to sample from the posterior, provide an index 1-4
chains = posterior(bf, index=4, iterations=1000) # index=4 for Keyboard*Posture + PId
summary(chains)
head(chains)
plot(chains[,18:21])
laply(18:21, \(x) mean(chains[,x]))
hdi(chains[,18:21])

# manual post hoc pairwise comparisons
df2 <- dcast(df, PId ~ Keyboard + Posture, value.var="WPM") # go wide
View(df2)

ttestBF(df2$Android_standing, df2$Android_walking, paired=TRUE)
ttestBF(df2$Android_standing, df2$iPhone_standing, paired=TRUE)
ttestBF(df2$Android_standing, df2$iPhone_walking,  paired=TRUE)
ttestBF(df2$Android_walking,  df2$iPhone_standing, paired=TRUE)
ttestBF(df2$Android_walking,  df2$iPhone_walking,  paired=TRUE)
ttestBF(df2$iPhone_standing,  df2$iPhone_walking,  paired=TRUE)


