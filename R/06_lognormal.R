###
### 06_lognormal.R
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
library(EnvStats) # for gofTest
library(car) # for leveneTest, Anova
library(afex) # for for aov_ez
library(performance) # for check_*
library(effectsize) # for cohens_d


##
#### 06_lognormal.csv ####
## log-transform of the D.V.
##

# prepare data table
df <- read.csv(".\\data\\06_lognormal.csv")
df$PId = factor(df$PId)
df$IDE = factor(df$IDE)
contrasts(df$IDE) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ IDE, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Hours),
  "Mean"=mean(data$Hours), 
  "SD"=sd(data$Hours),
  "Median"=median(data$Hours),
  "IQR"=IQR(data$Hours),
  "Max"=max(data$Hours)
))

# boxplot
boxplot(
  Hours ~ IDE,
  main="Hours by IDE",
  xlab="IDE",
  ylab="Hours",
  ylim=c(0, max(df$Hours)),
  col=c("pink","lightblue"),
  data=df
)

# make two stacked histograms
par(mfrow=c(2,1))
  hist(df[df$IDE == "Nobugs",]$Hours, 
     main="Distribution of Nobugs Hours", 
     xlab="Hours",
     ylab="Frequency",
     xlim=c(0,30),
     ylim=c(0,20),
     breaks=seq(0,30,3),
     col="pink")
  hist(df[df$IDE == "VStudio",]$Hours, 
     main="Distribution of Microsoft Visual Studio Hours", 
     xlab="Hours",
     ylab="Frequency",
     xlim=c(0,30),
     ylim=c(0,20),
     breaks=seq(0,30,3),
     col="lightblue")
par(mfrow=c(1,1))


# normality of conditional response
shapiro.test(df[df$IDE == "Nobugs",]$Hours)
shapiro.test(df[df$IDE == "VStudio",]$Hours)

# fit normal and lognormal distributions to the conditional responses
hist(df[df$IDE == "Nobugs",]$Hours, main="Nobugs Hours", xlab="Hours", col="pink", freq=FALSE)
f = gofTest(df[df$IDE == "Nobugs",]$Hours, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)
f = gofTest(df[df$IDE == "Nobugs",]$Hours, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # lognormal curve
print.gof(f)

hist(df[df$IDE == "VStudio",]$Hours, main="Microsoft Visual Studio Hours", xlab="Hours", col="lightblue", freq=FALSE)
f = gofTest(df[df$IDE == "VStudio",]$Hours, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)
f = gofTest(df[df$IDE == "VStudio",]$Hours, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # lognormal curve
print.gof(f)

# normality of residuals
Nobugs.mean = mean(df[df$IDE == "Nobugs",]$Hours)
VStudio.mean = mean(df[df$IDE == "VStudio",]$Hours)
df$Residuals = ifelse(
  df$IDE == "Nobugs", 
  df$Hours - Nobugs.mean,
  df$Hours - VStudio.mean
)
shapiro.test(df$Residuals)

# plot and test the residuals calculated above
hist(df$Residuals, main="Histogram of Residuals", freq=FALSE)
df$Residuals.10 = df$Residuals + 10  # shift by +10 to make non-negative for lognormal fit

hist(df$Residuals.10, main="Histogram of Residuals", freq=FALSE)
f = gofTest(df$Residuals.10, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)
f = gofTest(df$Residuals.10, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)

# we can also build an ANOVA model and test residuals this way
m = aov_ez(dv="Hours", between="IDE", id="PId", type=3, data=df)
check_normality(m)[1] # violation!

r = residuals(m$lm) # extract residuals
plot(r[1:length(r)], main="Plot of Residuals"); abline(h=0)
qqnorm(r); qqline(r)
hist(r, main="Histogram of Residuals")
shapiro.test(r)

r.10 = r + 10  # shift by +10 to make non-negative for lognormal fit
hist(r.10, main="Histogram of Residuals", freq=FALSE) # density
f = gofTest(r.10, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)
f = gofTest(r.10, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)

# take the log of the residuals and fit a normal curve to it
log.r.10 = log(r.10)
plot(log.r.10[1:length(log.r.10)], main="Plot of log(Residuals)"); abline(h=mean(log.r.10))
qqnorm(log.r.10); qqline(log.r.10)
hist(log.r.10, main="Histogram of log(Residuals)", freq=FALSE)
f = gofTest(log.r.10, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)
shapiro.test(log.r.10)

# make a log-transformed dependent variable and re-test normality
df$logHours = log(df$Hours)  # create new D.V.
hist(df$logHours, main="Histogram of log(Hours)")

shapiro.test(df[df$IDE == "Nobugs",]$logHours)
shapiro.test(df[df$IDE == "VStudio",]$logHours)

m = aov_ez(dv="logHours", between="IDE", id="PId", type=3, data=df)
check_normality(m)[1]

r = residuals(m$lm) # extract residuals
plot(r[1:length(r)], main="Plot of log(Hours) Residuals"); abline(h=0)
qqnorm(r); qqline(r)
hist(r, main="Histogram of log(Hours) Residuals", freq=FALSE)
shapiro.test(r)

r.1 = r + 1  # shift by +1 to enable lognormal fit
hist(r.1, main="Histogram of log(Hours) Residuals", freq=FALSE)

f = gofTest(r.1, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)
f = gofTest(r.1, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # normal curve
print.gof(f)

# visualize logHours before testing
boxplot(
  logHours ~ IDE,
  main="log(Hours) by IDE",
  xlab="IDE",
  ylab="log(Hours)",
  col=c("pink","lightblue"),
  data=df
)

# homogeneity of variance, a/k/a homoscedasticity
leveneTest(logHours ~ IDE, data=df, center=median) # Brown-Forstye test (default)
leveneTest(logHours ~ IDE, data=df, center=mean)   # Levene's test
check_homogeneity(m)                               # same

# independent-samples t-test
t.test(logHours ~ IDE, var.equal=TRUE, data=df)    # Student's t-test
t.test(logHours ~ IDE, var.equal=FALSE, data=df)   # Welch's t-test

# Cohen's d effect size
cohens_d(logHours ~ IDE, data=df)


