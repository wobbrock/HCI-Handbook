###
### 03_independent_samples.R
###
### HCI Handbook, 4th Edition
### Copyright (C) 2025 CRC Press
###
### Jacob O. Wobbrock, Ph.D.
### University of Washington
### wobbrock@uw.edu
###
### Last Updated: 12/03/2024
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
library(performance) # for check_homogeneity
library(emmeans) # for emmeans
library(effectsize) # for cohens_d, eta_squared


##
#### 03a_independent_samples.csv ####
## two independent samples
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

# make two stacked histograms
par(mfrow=c(2,1))
  hist(df[df$Engine == "Bing",]$Minutes, 
     main="Distribution of Bing Minutes", 
     xlab="Minutes",
     ylab="Frequency",
     xlim=c(9,14),
     ylim=c(0,3),
     breaks=seq(9,14,0.5),
     col="lightblue")
  hist(df[df$Engine == "Google",]$Minutes, 
     main="Distribution of Google Minutes", 
     xlab="Minutes",
     ylab="Frequency",
     xlim=c(9,14),
     ylim=c(0,3),
     breaks=seq(9,14,0.5),
     col="lightgreen")
par(mfrow=c(1,1))

# normality of conditional response
shapiro.test(df[df$Engine == "Bing",]$Minutes)
shapiro.test(df[df$Engine == "Google",]$Minutes)

# normality of residuals
Bing.mean = mean(df[df$Engine == "Bing",]$Minutes)
Google.mean = mean(df[df$Engine == "Google",]$Minutes)
df$Residuals = ifelse(
  df$Engine == "Bing", 
  df$Minutes - Bing.mean,
  df$Minutes - Google.mean
)
shapiro.test(df$Residuals)

# we can build an ANOVA model and test residuals this way
m = aov_ez(dv="Minutes", between="Engine", id="PId", type=3, data=df)
r = residuals(m$lm)  # extract residuals

hist(r, xlim=c(-2,+2), main="Histogram of Residuals", freq=TRUE)  # frequency (counts)
hist(r, xlim=c(-2,+2), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

plot(r[1:length(r)], main="Plot of Residuals"); abline(h=0)
qqnorm(r); qqline(r)

shapiro.test(r)

# homogeneity of variance, a/k/a homoscedasticity
leveneTest(Minutes ~ Engine, center=median, data=df) # Brown-Forstye test (default)
leveneTest(Minutes ~ Engine, center=mean, data=df)   # Levene's test
print(check_homogeneity(m))                          # Levene's test

# independent-samples t-test
t.test(Minutes ~ Engine, var.equal=TRUE, data=df)  # Student's
t.test(Minutes ~ Engine, var.equal=FALSE, data=df) # Welch
cohens_d(Minutes ~ Engine, data=df)



##
#### 03b_independent_samples.csv ####
## three independent samples
##

# prepare data table
df <- read.csv(".\\data\\03b_independent_samples.csv")
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
        col=c("lightblue","tan1","lightgreen"),
        data=df)

# make three stacked histograms
par(mfrow=c(3,1))
  hist(df[df$Engine == "Bing",]$Minutes, 
       main="Distribution of Bing Minutes", 
       xlab="Minutes",
       ylab="Frequency",
       xlim=c(9,14),
       ylim=c(0,3),
       col="lightblue")
  hist(df[df$Engine == "Duck",]$Minutes, 
       main="Distribution of DuckDuckGo Minutes", 
       xlab="Minutes",
       ylab="Frequency",
       xlim=c(9,14),
       ylim=c(0,3),
       col="tan1")
  hist(df[df$Engine == "Google",]$Minutes, 
       main="Distribution of Google Minutes", 
       xlab="Minutes",
       ylab="Frequency",
       xlim=c(9,14),
       ylim=c(0,3),
       col="lightgreen")
par(mfrow=c(1,1))

# normality of conditional response
shapiro.test(df[df$Engine == "Bing",]$Minutes)
shapiro.test(df[df$Engine == "Duck",]$Minutes)
shapiro.test(df[df$Engine == "Google",]$Minutes)

# normality of residuals
Bing.mean = mean(df[df$Engine == "Bing",]$Minutes)
Duck.mean = mean(df[df$Engine == "Duck",]$Minutes)
Google.mean = mean(df[df$Engine == "Google",]$Minutes)
df$Residuals = case_match(
  df$Engine,
  "Bing" ~ df$Minutes - Bing.mean,
  "Duck" ~ df$Minutes - Duck.mean,
  "Google" ~ df$Minutes - Google.mean
)
shapiro.test(df$Residuals)

# we can build an ANOVA model and test residuals this way
m = aov_ez(dv="Minutes", between="Engine", id="PId", type=3, data=df)
r = residuals(m$lm)  # extract residuals

hist(r, xlim=c(-2,+2), ylim=c(0,6), main="Histogram of Residuals", freq=TRUE)    # frequency (counts)
hist(r, xlim=c(-2,+2), ylim=c(0,0.5), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

plot(r[1:length(r)], main="Plot of Residuals"); abline(h=0)
qqnorm(r); qqline(r)

shapiro.test(r)

# homogeneity of variance, a/k/a homoscedasticity
leveneTest(Minutes ~ Engine, center=median, data=df) # Brown-Forstye test (default)
leveneTest(Minutes ~ Engine, center=mean, data=df)   # Levene's test
print(check_homogeneity(m))                          # Levene's test

# one-way ANOVA
anova(m)

# if Levene's test had shown heteroscedasticity (p<.05), use a White-adjusted ANOVA
Anova(m$lm, type=3, white.adjust=TRUE)
eta_squared(m, generalized=TRUE)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Engine, adjust="holm")


