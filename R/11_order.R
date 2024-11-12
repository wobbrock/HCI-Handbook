###
### 11_order.R
###
### HCI Handbook, 4th Edition
### Copyright (C) 2025 CRC Press
###
### Jacob O. Wobbrock, Ph.D.
### University of Washington
### wobbrock@uw.edu
###
### Last Updated: 11/11/2024
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
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova
library(emmeans) # for emmeans
library(effectsize) # for eta_squared


##
#### 04c_dependent_samples.csv ####
## testing for order effects
##

# prepare data table
df <- read.csv("04c_dependent_samples.csv")
df$PId = factor(df$PId)
df$Mouse = factor(df$Mouse)
df$Order = factor(df$Order)
contrasts(df$Mouse) <- "contr.sum"
contrasts(df$Order) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Order, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Throughput),
  "Mean"=mean(data$Throughput), 
  "SD"=sd(data$Throughput),
  "Median"=median(data$Throughput),
  "IQR"=IQR(data$Throughput),
  "Max"=max(data$Throughput)
))

# boxplot
boxplot(Throughput ~ Order,
        main="Throughput by Order",
        xlab="Order",
        ylab="Throughput (bits/s)",
        data=df)

# make three stacked histograms
par(mfrow=c(3,1))
  hist(df[df$Order == "1",]$Throughput, 
       main="Distribution of First Throughput", 
       xlab="Throughput (bits/s)",
       ylab="Frequency",
       xlim=c(2.5,6.0),
       ylim=c(0,8),
       breaks=seq(2.5,6.0,0.5))
  hist(df[df$Order == "2",]$Throughput, 
       main="Distribution of Second Throughput", 
       xlab="Throughput (bits/s)",
       ylab="Frequency",
       xlim=c(2.5,6.0),
       ylim=c(0,8),
       breaks=seq(2.5,6.0,0.5))
  hist(df[df$Order == "3",]$Throughput, 
       main="Distribution of Third Throughput", 
       xlab="Throughput (bits/s)",
       ylab="Frequency",
       xlim=c(2.5,6.0),
       ylim=c(0,8),
       breaks=seq(2.5,6.0,0.5))
par(mfrow=c(1,1))

# normality of conditional response
shapiro.test(df[df$Order == "1",]$Throughput)
shapiro.test(df[df$Order == "2",]$Throughput)
shapiro.test(df[df$Order == "3",]$Throughput)

# normality of residuals
Order1.mean = mean(df[df$Order == "1",]$Throughput)
Order2.mean = mean(df[df$Order == "2",]$Throughput)
Order3.mean = mean(df[df$Order == "3",]$Throughput)
df$Residuals = case_match(
  df$Order,
  "1" ~ df$Throughput - Order1.mean,
  "2" ~ df$Throughput - Order2.mean,
  "3" ~ df$Throughput - Order3.mean
)
shapiro.test(df$Residuals)

# check for an Order effect
m = lmer(Throughput ~ Order + (1|PId), data=df)
r = residuals(m) # extract residuals

hist(r, xlim=c(-2,+2), ylim=c(0,14), main="Histogram of Residuals", freq=TRUE) # frequency (counts)
hist(r, xlim=c(-2,+2), ylim=c(0,0.6), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

plot(r[1:length(r)], main="Plot of Residudals"); abline(h=0)
qqnorm(r); qqline(r)

shapiro.test(r)

# analysis of variance
Anova(m, type=3, test.statistic="F")

# partial eta-squared effect size
eta_squared(m, partial=TRUE)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Order, adjust="holm")

# check the Mouse x Order interaction
m = lmer(Throughput ~ Mouse*Order + (1|PId), data=df)
r = residuals(m) # extract residuals

hist(r, xlim=c(-1.5,+1.5), ylim=c(0,18), main="Histogram of Residuals", freq=TRUE) # frequency (counts)
hist(r, xlim=c(-1.5,+1.5), ylim=c(0,0.8), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

plot(r[1:length(r)], main="Plot of Residudals"); abline(h=0)
qqnorm(r); qqline(r)

shapiro.test(r)

# analysis of variance
Anova(m, type=3, test.statistic="F")

# partial eta-squared effect size
eta_squared(m, partial=TRUE)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Order, adjust="holm")


