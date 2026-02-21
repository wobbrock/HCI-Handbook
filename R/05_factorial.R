###
### 05_factorial.R
###
### HCI Handbook, 4th Edition
### Copyright (C) 2026 CRC Press
###
### Jacob O. Wobbrock, Ph.D.
### University of Washington
### wobbrock@uw.edu
###
### Last Updated: 02/21/2026
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


library(plyr) # for ddply
library(dplyr) # for case_match
library(EnvStats) # for gofTest
library(car) # for leveneTest, Anova
library(afex) # for for aov_ez
library(performance) # for check_*
library(effectsize) # for eta_squared
library(emmeans) # for emmeans


##
#### 05a_factorial.csv ####
## two between-Ss. factors
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

# boxplot
boxplot(
  WPM ~ Keyboard + Posture,
  main="WPM by Keyboard, Posture",
  xlab="Keyboard.Posture",
  ylab="WPM",
  col=c("lightgreen","lightgray","darkgreen","darkgray"),
  data=df
)

# make four stacked histograms
par(mfrow=c(4,1))
  hist(df[df$Keyboard == "Android" & df$Posture == "standing",]$WPM, 
       main="Distribution of Android standing WPM", 
       xlab="WPM",
       ylab="Frequency",
       xlim=c(25,70),
       ylim=c(0,5),
       breaks=seq(25,70,5),
       col="lightgreen")
  hist(df[df$Keyboard == "iPhone" & df$Posture == "standing",]$WPM, 
       main="Distribution of iPhone standing WPM", 
       xlab="WPM",
       ylab="Frequency",
       xlim=c(25,70),
       ylim=c(0,5),
       breaks=seq(25,70,5),
       col="lightgray")
  hist(df[df$Keyboard == "Android" & df$Posture == "walking",]$WPM, 
       main="Distribution of Android walking WPM", 
       xlab="WPM",
       ylab="Frequency",
       xlim=c(25,70),
       ylim=c(0,5),
       breaks=seq(25,70,5),
       col="darkgreen")
  hist(df[df$Keyboard == "iPhone" & df$Posture == "walking",]$WPM, 
       main="Distribution of iPhone walking WPM", 
       xlab="WPM",
       ylab="Frequency",
       xlim=c(25,70),
       ylim=c(0,5),
       breaks=seq(25,70,5),
       col="darkgray")
par(mfrow=c(1,1))

# interaction plot with error bars
with(df, interaction.plot(
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

# normality of conditional response
shapiro.test(df[df$Keyboard == "Android" & df$Posture == "standing",]$WPM)
shapiro.test(df[df$Keyboard == "iPhone" & df$Posture == "standing",]$WPM)
shapiro.test(df[df$Keyboard == "Android" & df$Posture == "walking",]$WPM)
shapiro.test(df[df$Keyboard == "iPhone" & df$Posture == "walking",]$WPM)

# normality of residuals
Android.standing.mean = mean(df[df$Keyboard == "Android" & df$Posture == "standing",]$WPM)
iPhone.standing.mean = mean(df[df$Keyboard == "iPhone" & df$Posture == "standing",]$WPM)
Android.walking.mean = mean(df[df$Keyboard == "Android" & df$Posture == "walking",]$WPM)
iPhone.walking.mean = mean(df[df$Keyboard == "iPhone" & df$Posture == "walking",]$WPM)
df$Residuals = case_match(
  interaction(df$Keyboard, df$Posture),
  "Android.standing" ~ df$WPM - Android.standing.mean,
  "iPhone.standing" ~ df$WPM - iPhone.standing.mean,
  "Android.walking" ~ df$WPM - Android.walking.mean,
  "iPhone.walking" ~ df$WPM - iPhone.walking.mean
)
shapiro.test(df$Residuals)

# we can build an ANOVA model and test residuals this way
m = aov_ez(dv="WPM", between=c("Keyboard","Posture"), id="PId", type=3, data=df)
r = residuals(m$lm)  # extract residuals
mean(r); sum(r) # should be ~0

plot(r[1:length(r)], main="Plot of Residuals"); abline(h=0)
qqnorm(r); qqline(r)

hist(r, xlim=c(-20,+20), ylim=c(0,16), main="Histogram of Residuals", freq=TRUE)      # frequency (counts)
hist(r, xlim=c(-20,+20), ylim=c(0,0.0665), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f) # Shapiro-Wilk test
shapiro.test(r) # same
check_normality(m)[1] # same

# homogeneity of variance, a/k/a homoscedasticity
leveneTest(WPM ~ Keyboard*Posture, data=df, center=median) # Brown-Forstye test (default)
leveneTest(WPM ~ Keyboard*Posture, data=df, center=mean)   # Levene's test
check_homogeneity(m)                                       # Levene's test

# two-way ANOVA
anova(m)

# if Levene's test had shown heteroscedasticity (p<.05), use a White-adjusted ANOVA
Anova(m$lm, type=3, white.adjust=TRUE)
eta_squared(m, generalized=TRUE)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Keyboard*Posture, adjust="holm")



##
#### 05b_factorial.csv ####
## two within-Ss. factors
##

# prepare data table
df <- read.csv(".\\data\\05b_factorial.csv")
df$PId = factor(df$PId)
df$Keyboard = factor(df$Keyboard)
df$Posture = factor(df$Posture)
contrasts(df$Keyboard) <- "contr.sum"
contrasts(df$Posture) <- "contr.sum"
View(df)

# This data is the same as 05a_factorial.csv except that was a between-Ss.
# data table and this is a within-Ss. data table. So, we omit repeating 
# the data visualization steps here; they would be unchanged from above.
summary(df)

# we can build an ANOVA model and test residuals this way
m = aov_ez(dv="WPM", within=c("Keyboard","Posture"), id="PId", type=3, data=df)
r = residuals(m$lm)  # extract residuals
mean(r); sum(r) # should be ~0

plot(r[1:length(r)], main="Plot of Residuals"); abline(h=0)
qqnorm(r); qqline(r)

hist(r, xlim=c(-20,+20), ylim=c(0,16), main="Histogram of Residuals", freq=TRUE)      # frequency (counts)
hist(r, xlim=c(-20,+20), ylim=c(0,0.0665), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f) # Shapiro-Wilk test
shapiro.test(r) # same
check_normality(m)[1] # same

# sphericity assumption
summary(m)$sphericity.tests # Mauchly's test of sphericity
check_sphericity(m) # same

# two-way repeated measures ANOVA
anova(m, correction="none") # use if p≥.05, no violation of sphericity
anova(m, correction="GG")   # use if p<.05, sphericity violation

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Keyboard*Posture, adjust="holm")



##
#### 05c_factorial.csv ####
## two factors, one between-Ss. and one within-Ss.
## ("split-plot design")
##

# prepare data table
df <- read.csv(".\\data\\05c_factorial.csv")
df$PId = factor(df$PId)
df$Keyboard = factor(df$Keyboard)
df$Posture = factor(df$Posture)
contrasts(df$Keyboard) <- "contr.sum"
contrasts(df$Posture) <- "contr.sum"
View(df)

# This data is the same as 05a_factorial.csv except that was a between-Ss.
# data table and this is a mixed factorial data table. So, we omit repeating 
# the data visualization steps here; they would be unchanged from above.
summary(df)

# we can build an ANOVA model and test residuals this way
m = aov_ez(dv="WPM", between="Keyboard", within="Posture", id="PId", type=3, data=df)
r = residuals(m$lm)  # extract residuals
mean(r); sum(r) # should be ~0

plot(r[1:length(r)], main="Plot of Residuals"); abline(h=0)
qqnorm(r); qqline(r)

hist(r, xlim=c(-20,+20), ylim=c(0,16), main="Histogram of Residuals", freq=TRUE)      # frequency (counts)
hist(r, xlim=c(-20,+20), ylim=c(0,0.0665), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f) # Shapiro-Wilk test
shapiro.test(r) # same
check_normality(m)[1] # same

# homogeneity of variance, a/k/a homoscedasticity
check_homogeneity(m)

# sphericity assumption
summary(m)$sphericity.tests # Mauchly's test of sphericity
check_sphericity(m) # same

# two-way mixed factorial ANOVA
anova(m, correction="none") # use if p≥.05, no violation of sphericity
anova(m, correction="GG")   # use if p<.05, sphericity violation

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Keyboard*Posture, adjust="holm")


