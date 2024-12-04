###
### 08_lmm.R
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


library(plyr) # for ddply, l_ply
library(lme4) # for lmer
library(lmerTest) # for lmer
library(nlme) # for lme
library(emmeans) # for emmeans
library(car) # for leveneTest, Anova
library(afex) # for for aov_ez
library(performance) # for check_homogeneity, check_sphericity
library(effectsize) # for eta_squared
library(EnvStats) # for gofTest


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

# fixed-effects ANOVA
m0 = aov_ez(dv="WPM", within=c("Keyboard","Posture"), id="PId", type=3, data=df)

print(check_normality(m0))  # normality
print(check_sphericity(m0)) # sphericity

# two-way repeated measures ANOVA
anova(m0, correction="none")

# post hoc pairwise comparisons
emmeans(m0, pairwise ~ Keyboard*Posture, adjust="holm")

# linear mixed model (LMM)
m = lmer(WPM ~ Keyboard*Posture + (1|PId), data=df)

# normality
r = residuals(m)
qqnorm(r); qqline(r)
shapiro.test(r)

# no sphericity assumption for LMMs!

# analysis of variance
Anova(m, type=3, test.statistic="F")

# partial eta-squared effect size
eta_squared(m, partial=TRUE)

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

# fixed-effects ANOVA
m0 = aov_ez(dv="WPM", between="Keyboard", within="Posture", id="PId", type=3, data=df)
print(check_normality(m0))   # Shapiro-Wilk
print(check_homogeneity(m0)) # Levene's test
print(check_sphericity(m0))  # Mauchly's test for sphericity

# two-way repeated measures ANOVA
anova(m0, correction="none")

# post hoc pairwise comparisons
emmeans(m0, pairwise ~ Keyboard*Posture, adjust="holm")

# linear mixed model (LMM)
m = lmer(WPM ~ Keyboard*Posture + (1|PId), data=df)

# normality
r = residuals(m)
qqnorm(r); qqline(r)
shapiro.test(r)

# homoscedasticity
print(check_homogeneity(m))  # Bartlett's test

# no sphericity assumption for LMMs!

# analysis of variance
Anova(m, type=3, test.statistic="F")

# partial eta-squared effect size
eta_squared(m, partial=TRUE)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Keyboard*Posture, adjust="holm")



##
#### 08a_lmm.csv ####
## nested random factor in a random factor
##

# prepare data table
df <- read.csv(".\\data\\08a_lmm.csv")
df$PId = factor(df$PId)
df$University = factor(df$University)
df$IDE = factor(df$IDE)
df$App = factor(df$App)
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

ddply(df, ~ University, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Hours),
  "Mean"=mean(data$Hours), 
  "SD"=sd(data$Hours),
  "Median"=median(data$Hours),
  "IQR"=IQR(data$Hours),
  "Max"=max(data$Hours)
))

# boxplot
boxplot(Hours ~ IDE,
        main="Hours by IDE",
        xlab="IDE",
        ylab="Hours",
        ylim=c(0, max(df$Hours)),
        col=c("pink","lightblue"),
        data=df)

# make two stacked histograms
par(mfrow=c(2,1))
  hist(df[df$IDE == "Nobugs",]$Hours, 
     main="Distribution of Nobugs Hours", 
     xlab="Hours",
     ylab="Frequency",
     xlim=c(0,30),
     ylim=c(0,90),
     breaks=seq(0,30,3),
     col="pink")
  hist(df[df$IDE == "VStudio",]$Hours, 
     main="Distribution of Microsoft Visual Studio Hours", 
     xlab="Hours",
     ylab="Frequency",
     xlim=c(0,30),
     ylim=c(0,90),
     breaks=seq(0,30,3),
     col="lightblue")
par(mfrow=c(1,1))

# normality of conditional response
shapiro.test(df[df$IDE == "Nobugs",]$Hours)
shapiro.test(df[df$IDE == "VStudio",]$Hours)

# fit normal and lognormal distributions to the conditional response
hist(df[df$IDE == "Nobugs",]$Hours, main="Nobugs Hours", xlab="Hours", ylim=c(0,0.2), col="pink", freq=FALSE)
f = gofTest(df[df$IDE == "Nobugs",]$Hours, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)
f = gofTest(df[df$IDE == "Nobugs",]$Hours, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # lognormal curve
print(f)

hist(df[df$IDE == "VStudio",]$Hours, main="Microsoft Visual Studio Hours", xlab="Hours", ylim=c(0,0.125), col="lightblue", freq=FALSE)
f = gofTest(df[df$IDE == "VStudio",]$Hours, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)
f = gofTest(df[df$IDE == "VStudio",]$Hours, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # lognormal curve
print(f)

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
hist(df$Residuals, main="Histogram of Residuals", ylim=c(0,0.15), freq=FALSE)

df$Residuals.10 = df$Residuals + 10  # shift by +10 to make non-negative for lognormal fit
hist(df$Residuals.10, main="Histogram of Residuals", ylim=c(0,0.15), freq=FALSE)
f = gofTest(df$Residuals.10, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)
f = gofTest(df$Residuals.10, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

# take the log of the residuals and fit a normal curve to it
df$log.Residuals.10 = log(df$Residuals.10)
hist(df$log.Residuals.10, main="Histogram of log(Residuals)", freq=FALSE)
f = gofTest(df$log.Residuals.10, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)

plot(df$Residuals, main="Plot of Residuals"); abline(h=0)
qqnorm(df$Residuals); qqline(df$Residuals)

plot(df$log.Residuals.10, main="Plot of log(Residuals)"); abline(h=mean(df$log.Residuals.10))
qqnorm(df$log.Residuals.10); qqline(df$log.Residuals.10)

shapiro.test(df$Residuals)
shapiro.test(df$log.Residuals.10)

# make a log-transformed dependent variable and re-test normality
df$logHours = log(df$Hours)  # create new D.V.
hist(df$logHours, main="Histogram of log(Hours)")

shapiro.test(df[df$IDE == "Nobugs",]$logHours)
shapiro.test(df[df$IDE == "VStudio",]$logHours)

# visualize before testing
boxplot(logHours ~ IDE,
        main="log(Hours) by IDE",
        xlab="IDE",
        ylab="log(Hours)",
        col=c("pink","lightblue"),
        data=df)

# build our LMM
m = lmer(logHours ~ IDE + (1|App) + (1|University/PId), data=df)
r = residuals(m)

# normality
hist(r, xlim=c(-1.5,+1.5), main="Histogram of Residuals", freq=FALSE)
f = gofTest(r, distribution="norm")
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # normal curve
print(f)
plot(r[1:length(r)], main="Plot of Residuals"); abline(h=0)
qqnorm(r); qqline(r)
shapiro.test(r)
print(check_normality(m))

# homoscedasticity
print(check_homogeneity(m))  # Bartlett's test

# analysis of variance
Anova(m, type=3, test.statistic="F", white.adjust=TRUE)

# partial eta-squared effect size
eta_squared(m, partial=TRUE)



##
#### 08b_lmm.csv ####
## nested random factor in a fixed factor
## 

# prepare data table
df <- read.csv(".\\data\\08b_lmm.csv")
df$PId = factor(df$PId)
df$Company = factor(df$Company)
df$UI = factor(df$UI)
contrasts(df$Company) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Company, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Problems),
  "Mean"=mean(data$Problems), 
  "SD"=sd(data$Problems),
  "Median"=median(data$Problems),
  "IQR"=IQR(data$Problems),
  "Max"=max(data$Problems)
))

# boxplot
boxplot(Problems ~ Company,
        main="Usability Problems Found by Company",
        xlab="Company",
        ylab="Problems",
        ylim=c(0, max(df$Problems)),
        col=c("tan1","lightgray","pink","lightgreen","lightblue","lightyellow"),
        data=df)

# make 3x2 stacked histograms
par(mfrow=c(3,2))
  hist(df[df$Company == "Amazon",]$Problems, 
       main="Amazon Problems", 
       xlab="Problems",
       ylab="Count",
       xlim=c(0,20),
       ylim=c(0,12),
       breaks=seq(0,20,2),
       col="tan1")
  hist(df[df$Company == "Apple",]$Problems, 
       main="Apple Problems", 
       xlab="Problems",
       ylab="Count",
       xlim=c(0,20),
       ylim=c(0,12),
       breaks=seq(0,20,2),
       col="lightgray")
  hist(df[df$Company == "Baidu",]$Problems, 
       main="Baidu Problems", 
       xlab="Problems",
       ylab="Count",
       xlim=c(0,20),
       ylim=c(0,12),
       breaks=seq(0,20,2),
       col="pink")
  hist(df[df$Company == "Google",]$Problems, 
       main="Google Problems", 
       xlab="Problems",
       ylab="Count",
       xlim=c(0,20),
       ylim=c(0,12),
       breaks=seq(0,20,2),
       col="lightgreen")
  hist(df[df$Company == "Meta",]$Problems, 
       main="Meta Problems", 
       xlab="Problems",
       ylab="Count",
       xlim=c(0,20),
       ylim=c(0,12),
       breaks=seq(0,20,2),
       col="lightblue")
  hist(df[df$Company == "Microsoft",]$Problems, 
       main="Microsoft Problems", 
       xlab="Problems",
       ylab="Count",
       xlim=c(0,20),
       ylim=c(0,12),
       breaks=seq(0,20,2),
       col="lightyellow")
par(mfrow=c(1,1))

# these would be incorrect LMM formulations
m0 = lmer(Problems ~ Company + (1|Company/PId) + (1|UI), data=df)  # wrong!
m1 = lmer(Problems ~ Company + (1|Company) + (1|Company:PId) + (1|UI), data=df)  # no, same!

# this is the correct LMM formulation
m = lmer(Problems ~ Company + (1|Company:PId) + (1|UI), data=df)  # correct!

# normality
r = residuals(m)
qqnorm(r); qqline(r)
shapiro.test(r)
print(check_normality(m))

# homoscedasticity
print(check_homogeneity(m)) # Bartlett's test

# analysis of variance
Anova(m, type=3, test.statistic="F", white.adjust=TRUE)

# partial eta-squared effect size
eta_squared(m, partial=TRUE)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Company, adjust="holm")



##
#### 08c_lmm.csv ####
## covariance structures
##

# prepare data table
df <- read.csv(".\\data\\08c_lmm.csv")
df$PId = factor(df$PId)
df$Week = factor(df$Week)
contrasts(df$Week) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Week, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Hours),
  "Mean"=mean(data$Hours), 
  "SD"=sd(data$Hours),
  "Median"=median(data$Hours),
  "IQR"=IQR(data$Hours),
  "Max"=max(data$Hours)
))

# boxplot
boxplot(Hours ~ Week,
        main="Videogame Hours per Week",
        xlab="Week",
        ylab="Hours",
        ylim=c(0, max(df$Hours)),
        data=df)

# regression plot
plot(Hours ~ as.numeric(Week), data=df, main="Videogame Hours per Week", xlab="Week", ylab="Hours")
m.line = lm(Hours ~ as.numeric(Week), data=df) # line
abline(coef(m.line), lty=1, lwd=3, col="red")  # graph the model line

# connected segments plot
msd <- ddply(df, ~ Week, function(data) c(
  "Mean"=mean(data$Hours), 
  "SD"=sd(data$Hours)
))
segments(x0=1, y0=msd[1,]$Mean, x1=2, y1=msd[2,]$Mean, lty=1, lwd=3, col="blue")
segments(x0=2, y0=msd[2,]$Mean, x1=3, y1=msd[3,]$Mean, lty=1, lwd=3, col="blue")
arrows(x0=1, y0=msd[1,]$Mean - msd[1,]$SD, x1=1, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2, y0=msd[2,]$Mean - msd[2,]$SD, x1=2, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=3, y0=msd[3,]$Mean - msd[3,]$SD, x1=3, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

# manually calculate variances and covariances
var(df[df$Week == 1,]$Hours) # 25.93762
var(df[df$Week == 2,]$Hours) # 18.01613
var(df[df$Week == 3,]$Hours) # 35.21805

cov(df[df$Week == 1,]$Hours, df[df$Week == 2,]$Hours) # 12.98577
cov(df[df$Week == 1,]$Hours, df[df$Week == 3,]$Hours) # 14.9071
cov(df[df$Week == 2,]$Hours, df[df$Week == 3,]$Hours) # 19.68331

# first fit an LMM with unstructured (UN) covariance matrix
m = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corSymm(form=~1|PId), weights=varIdent(form=~1|Week)) #UN
getVarCov(m, type="marginal")
# Marginal variance covariance matrix
#        1      2      3
# 1 25.938 12.986 14.907
# 2 12.986 18.016 19.683
# 3 14.907 19.683 35.218

# analysis of variance
anova(m, type="marginal")    # F(2,18) = 4.69, p=0.023
eta_squared(m, partial=TRUE) # 0.34
emmeans(m, pairwise ~ Week, adjust="holm")
BIC(m) # 193.5247

## Now let's try other covariance structures:
#  1. Scaled Identity (ID)
m.ID = lme(Hours ~ Week, random=~1|PId, data=df) #ID
getVarCov(m.ID, type="marginal")
anova(m.ID, type="marginal")    # F(2,18) = 7.03, p = .006
eta_squared(m.ID, partial=TRUE) # 0.44
emmeans(m.ID, pairwise ~ Week, adjust="holm")
BIC(m.ID) # 181.1469

#  2. Diagonal (DIAG)
m.DIAG = lme(Hours ~ Week, random=~1|PId, data=df, weights=varIdent(form=~1|Week)) #DIAG
getVarCov(m.DIAG, type="marginal")
anova(m.DIAG, type="marginal")    # F(2,18) = 4.90, p = .020
eta_squared(m.DIAG, partial=TRUE) # 0.35
emmeans(m.DIAG, pairwise ~ Week, adjust="holm")
BIC(m.DIAG) # 184.4128

#  3. Compound Symmetry (CS)
m.CS = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corCompSymm(form=~1|PId)) #CS
getVarCov(m.CS, type="marginal")
anova(m.CS, type="marginal")    # F(2,18) = 7.03, p = .006
eta_squared(m.CS, partial=TRUE) # 0.44
emmeans(m.CS, pairwise ~ Week, adjust="holm")
BIC(m.CS) # 184.4427

#  4. Heterogeneous Compound Symmetry (CSH)
m.CSH = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corCompSymm(form=~1|PId), weights=varIdent(form=~1|Week)) #CSH
getVarCov(m.CSH, type="marginal")
anova(m.CSH, type="marginal")    # F(2,18) = 4.70, p = .023
eta_squared(m.CSH, partial=TRUE) # 0.34
emmeans(m.CSH, pairwise ~ Week, adjust="holm")
BIC(m.CSH) # 187.6678

#  5. First-order autoregressive (AR1)
m.AR1 = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corAR1(form=~1|PId)) #AR1
getVarCov(m.AR1, type="marginal")
anova(m.AR1, type="marginal")    # F(2,18) = 5.63, p = .013
eta_squared(m.AR1, partial=TRUE) # 0.38
emmeans(m.AR1, pairwise ~ Week, adjust="holm")
BIC(m.AR1) # 182.9592

#  6. Heterogeneous first-order autoregressive (ARH1)
m.ARH1 = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corAR1(form=~1|PId), weights=varIdent(form=~1|Week)) #ARH1
getVarCov(m.ARH1, type="marginal")
anova(m.ARH1, type="marginal")    # F(2,18) = 4.88, p = .020
eta_squared(m.ARH1, partial=TRUE) # 0.35
emmeans(m.ARH1, pairwise ~ Week, adjust="holm")
BIC(m.ARH1) # 187.705

#  7. Autoregressive moving average (ARMA11)
m.ARMA11 = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corARMA(form=~1|PId, p=1, q=1)) #ARMA11
getVarCov(m.ARMA11, type="marginal")
anova(m.ARMA11, type="marginal")    # F(2,18) = 5.63, p = .013
eta_squared(m.ARMA11, partial=TRUE) # 0.38
emmeans(m.ARMA11, pairwise ~ Week, adjust="holm")
BIC(m.ARMA11) # 186.2551

#  8. Toeplitz (TP)
m.TP = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corARMA(form=~1|PId, p=2, q=0)) #TP
getVarCov(m.TP, type="marginal")
anova(m.TP, type="marginal")    # F(2,18) = 5.63, p= .013
eta_squared(m.TP, partial=TRUE) # 0.38
emmeans(m.TP, pairwise ~ Week, adjust="holm")
BIC(m.TP) # 186.2551

#  9. Heterogeneous Toeplitz (TPH)
m.TPH = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corARMA(form=~1|PId, p=2, q=0), weights=varIdent(form=~1|Week), control=lmeControl(opt="optim")) #TPH
getVarCov(m.TPH, type="marginal")
anova(m.TPH, type="marginal")    # F(2,18) = 4.72, p = .023
eta_squared(m.TPH, partial=TRUE) # 0.34
emmeans(m.TPH, pairwise ~ Week, adjust="holm")
BIC(m.TPH) # 190.9491

#  10. Unstructured (UN)
m.UN = lme(Hours ~ Week, random=~1|PId, data=df, correlation=corSymm(form=~1|PId), weights=varIdent(form=~1|Week)) #UN
getVarCov(m.UN, type="marginal")
anova(m.UN, type="marginal")    # F(2,18) = 4.69, p = .023
eta_squared(m.UN, partial=TRUE) # 0.34
emmeans(m.UN, pairwise ~ Week, adjust="holm")
BIC(m.UN) # 193.5247

# compare to lme4::lmer
m.lmer = lmer(Hours ~ Week + (1|PId), data=df)
Anova(m.lmer, type=3, test.statistic="F") # F(2,18) = 7.03, p = .006
eta_squared(m.lmer, partial=TRUE)         # 0.44
emmeans(m.lmer, pairwise ~ Week, adjust="holm")
BIC(m.lmer) # 181.6737



##
#### Random intercepts vs. random slopes ####
##

## Still using 08c_lmm.csv from above...

colors = hcl.colors(10, palette="Dark 3") # store 10 colors

# interaction plot with participants as the traces
with(df, 
  interaction.plot(
    Week, 
    PId, 
    Hours, 
    ylim=c(min(Hours), max(Hours)), 
    ylab="Hours",
    main="Videogame Hours per Week by Participant",
    lty=1, 
    lwd=3, 
    col=colors)
)

# turn Week to numeric for exploring intercepts and slopes
df$Week = as.numeric(df$Week)
is.numeric(df$Week) # verify

# plot per participant regression models
plot(Hours ~ Week, data=df, main="Participant Regression Lines", pch=16, cex=1.3, col=colors)
for (i in 1:length(levels(df$PId))) {
  m.PId = lm(Hours ~ Week, data=df[df$PId == i,])
  abline(m.PId, lwd=2, col=colors[i])
}
# plot the average line
m.avg = lm(Hours ~ Week, data=df)
abline(m.avg, lwd=5, col="black")

# build random intercept and random slope models
m.int = lmer(Hours ~ Week + (1|PId), data=df)
m.slp = lmer(Hours ~ Week + (Week|PId), data=df)

# information criteria
BIC(m.int) # 180.0016
BIC(m.slp) # 183.5065

# analyses of variance
Anova(m.int, type=3, test.statistic="F") # F(1,19) = 14.36, p = .001
Anova(m.slp, type=3, test.statistic="F") # F(1,9) = 9.27, p = .014

# plot the resulting models
par(mfrow=c(1,2))
  # random intercepts
  fe = fixef(m.int)      # fixed effects model
  re = ranef(m.int)$PId  # random intercepts
  plot(Hours ~ Week, data=df, main="Random Intercept Model", xlab="Week", pch=16, cex=1.3, col=colors[as.numeric(PId)])
  l_ply(seq_len(nrow(re)), \(x) abline(fe[1] + re[x,1], fe[2], col=colors[x]))
  abline(fe[1], fe[2], lwd=5, col="black")

  # random slopes
  fe = fixef(m.slp)     # fixed effects model
  re = ranef(m.slp)$PId # random intercepts and slopes
  plot(Hours ~ Week, data=df, main="Random Slope Model", xlab="Week", pch=16, cex=1.3, col=colors[as.numeric(PId)])
  l_ply(seq_len(nrow(re)), \(x) abline(fe[1] + re[x,1], fe[2] + re[x,2], col=colors[x]))
  abline(fe[1], fe[2], lwd=5, col="black")
par(mfrow=c(1,1))


