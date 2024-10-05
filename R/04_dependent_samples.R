###
### 04_dependent_samples.R
###
### HCI Handbook, 4th Edition
### Copyright (C) 2025 CRC Press
###
### Jacob O. Wobbrock, Ph.D.
### University of Washington
### wobbrock@uw.edu
###
### Last Updated: 10/05/2024
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
library(afex) # for for aov_ez
library(performance) # for check_sphericity
library(reshape2) # for dcast
library(emmeans) # for emmeans
library(effectsize) # for cohens_d


##
## 04a_dependent_samples.csv
## two dependent samples
##

# prepare data table
df <- read.csv("04a_dependent_samples.csv")
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

# make two stacked histograms
par(mfrow=c(2,1))
  hist(df[df$Mouse == "Logitech",]$Throughput, 
     main="Distribution of Logitech Throughput", 
     xlab="Throughput (bits/s)",
     ylab="Frequency",
     xlim=c(3,5),
     ylim=c(0,8),
     breaks=seq(3,5,0.25),
     col="gray")
  hist(df[df$Mouse == "Microsoft",]$Throughput, 
     main="Distribution of Microsoft Throughput", 
     xlab="Throughput (bits/s)",
     ylab="Frequency",
     xlim=c(3,5),
     ylim=c(0,8),
     breaks=seq(3,5,0.25),
     col="lightblue")
par(mfrow=c(1,1))

# normality of conditional response
shapiro.test(df[df$Mouse == "Logitech",]$Throughput)
shapiro.test(df[df$Mouse == "Microsoft",]$Throughput)

# normality of residuals
Logitech.mean = mean(df[df$Mouse == "Logitech",]$Throughput)
Microsoft.mean = mean(df[df$Mouse == "Microsoft",]$Throughput)
df$Residuals = ifelse(
  df$Mouse == "Logitech", 
  df$Throughput - Logitech.mean,
  df$Throughput - Microsoft.mean
)
shapiro.test(df$Residuals)

# we can build an ANOVA model and test residuals this way
m = aov_ez(dv="Throughput", within="Mouse", id="PId", type=3, data=df)
r = residuals(m$lm)  # extract residuals

hist(r,xlim=c(-2,+2), ylim=c(0,12.8), main="Histogram of Residuals", freq=TRUE)  # frequency (counts)
hist(r, xlim=c(-2,+2), ylim=c(0,0.8), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

plot(r[1:length(r)], main="Plot of Residudals"); abline(h=0)
qqnorm(r); qqline(r)

shapiro.test(r)

# paired-samples t-test
df2 <- dcast(df, PId ~ Mouse, value.var="Throughput")  # make wide-format table
View(df2)  # view wide-format table
t.test(df2$Logitech, df2$Microsoft, paired=TRUE)
cohens_d(df2$Logitech, df2$Microsoft, paired=TRUE)



##
## 04b_dependent_samples.csv
## three dependent samples
##

# prepare data table
df <- read.csv("04b_dependent_samples.csv")
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
        col=c("gray","lightblue","lightgreen"),
        data=df)

# make three stacked histograms
par(mfrow=c(3,1))
  hist(df[df$Mouse == "Logitech",]$Throughput, 
     main="Distribution of Logitech Throughput", 
     xlab="Throughput (bits/s)",
     ylab="Frequency",
     xlim=c(3,6),
     ylim=c(0,8),
     breaks=seq(3,6,0.25),
     col="gray")
  hist(df[df$Mouse == "Microsoft",]$Throughput, 
     main="Distribution of Microsoft Throughput", 
     xlab="Throughput (bits/s)",
     ylab="Frequency",
     xlim=c(3,6),
     ylim=c(0,8),
     breaks=seq(3,6,0.25),
     col="lightblue")
  hist(df[df$Mouse == "Razer",]$Throughput, 
       main="Distribution of Razer Throughput", 
       xlab="Throughput (bits/s)",
       ylab="Frequency",
       xlim=c(3,6),
       ylim=c(0,8),
       breaks=seq(3,6,0.25),
       col="lightgreen")
par(mfrow=c(1,1))

# normality of conditional response
shapiro.test(df[df$Mouse == "Logitech",]$Throughput)
shapiro.test(df[df$Mouse == "Microsoft",]$Throughput)
shapiro.test(df[df$Mouse == "Razer",]$Throughput)

# normality of residuals
Logitech.mean = mean(df[df$Mouse == "Logitech",]$Throughput)
Microsoft.mean = mean(df[df$Mouse == "Microsoft",]$Throughput)
Razer.mean = mean(df[df$Mouse == "Razer",]$Throughput)
df$Residuals = case_match(
  df$Mouse,
  "Logitech" ~ df$Throughput - Logitech.mean,
  "Microsoft" ~ df$Throughput - Microsoft.mean,
  "Razer" ~ df$Throughput - Razer.mean
)
shapiro.test(df$Residuals)

# we can build an ANOVA model and test residuals this way
m = aov_ez(dv="Throughput", within="Mouse", id="PId", type=3, data=df)
r = residuals(m$lm)  # extract residuals

hist(r, xlim=c(-2,+2), ylim=c(0,16.8), main="Histogram of Residuals", freq=TRUE) # frequency (counts)
hist(r, xlim=c(-2,+2), ylim=c(0,0.7), main="Histogram of Residuals", freq=FALSE) # density (area sums to 1.00)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

plot(r[1:length(r)], main="Plot of Residudals"); abline(h=0)
qqnorm(r); qqline(r)

shapiro.test(r)

# sphericity assumption
summary(m)                  # Mauchly's test for sphericity
print(check_sphericity(m))  # Mauchly's test for sphericity

# one-way repeated measures ANOVA
anova(m, correction="none") # use if p>.05, no violation of sphericity
anova(m, correction="GG")   # use if p<.05, sphericity violation

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Mouse, adjust="holm")


