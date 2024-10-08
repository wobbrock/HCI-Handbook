###
### 09_glm(m).R
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
library(dplyr) # for mutate
library(car) # for Anova
library(emmeans) # for emmeans
library(lme4) # for glmer
library(lmerTest)
library(nnet) # for multinom
library(MASS) # for polr, fitdistr, glm.nb
library(ordinal) # for clmm
library(RVAideMemoire) # for Anova.clmm
library(fitdistrplus) # for gofstat, fitdist
library(performance) # for check_overdispersion
library(pscl) # for zeroinfl
library(glmmTMB) # for glmmTMB
library(EnvStats) # for gofTest


##
## 09a_glm.csv
## between-Ss. dichotomous D.V.
##   ..logistic regression
##

# prepare data table
df <- read.csv("09a_glm.csv")
df$PId = factor(df$PId)
df$Interface = factor(df$Interface)
df$Activity = factor(df$Activity)
df$Adoption = factor(df$Adoption) # D.V.
contrasts(df$Interface) <- "contr.sum"
contrasts(df$Activity) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Interface + Activity, function(data) c(
  "Nrows"=nrow(data),
  "Adoption='yes'"=sum(data$Adoption == 'yes'),
  "Adoption='no'"=sum(data$Adoption == 'no')
))

par(mfrow=c(1,2))
  plot(Adoption ~ Interface, data=df, main="Adoption by Interface",col=c("lightgreen","pink"))
  plot(Adoption ~ Activity, data=df, main="Adoption by Activity", col=c("lightgreen","pink"))
par(mfrow=c(1,1))

xt = xtabs( ~ Interface + Activity + Adoption, data=df)
mosaicplot(xt, main="Adoption by Interface, Activity", cex=0.8, col=c("pink","lightgreen"))

# logistic regression
m = glm(Adoption ~ Interface * Activity, data=df, family=binomial)
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Interface*Activity, adjust="holm")

# We can also run this using the multinomial-Poisson trick. The
# *.mp functions are defined in multpois.R and must each be loaded 
# into memory first. We can also get our post hoc pairwise comparisons 
# this way.
m0 = glm.mp(Adoption ~ Interface * Activity, data=df)
Anova.mp(m0, type=3)
glm.mp.con(m0, pairwise ~ Interface*Activity, adjust="holm")



##
## 09a_glmm.csv
## within-Ss. dichotomous D.V.
##   ..mixed logistic regression
##

# prepare data table
df <- read.csv("09a_glmm.csv")
df$PId = factor(df$PId)
df$Interface = factor(df$Interface)
df$Activity = factor(df$Activity)
df$Adoption = factor(df$Adoption) # D.V.
contrasts(df$Interface) <- "contr.sum"
contrasts(df$Activity) <- "contr.sum"
View(df)

# The data is the same as 09a_glm.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# mixed logistic regression
m = glmer(Adoption ~ Interface*Activity + (1|PId), data=df, family=binomial)
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Interface*Activity, adjust="holm")

# We can also run this using the multinomial-Poisson trick. The
# *.mp functions are defined in multpois.R and must each be loaded 
# into memory first. We can also get our post hoc pairwise comparisons 
# this way.
m0 = glmer.mp(Adoption ~ Interface*Activity + (1|PId), data=df)
Anova.mp(m0, type=3)
glmer.mp.con(m0, pairwise ~ Interface * Activity, adjust="holm")



##
## 09b_glm.csv
## between-Ss. polytomous D.V.
##   ..multinomial logistic regression
##

# prepare data table
df <- read.csv("09b_glm.csv")
df$PId = factor(df$PId)
df$Interface = factor(df$Interface)
df$Activity = factor(df$Activity)
df$Adoption = factor(df$Adoption) # D.V.
contrasts(df$Interface) <- "contr.sum"
contrasts(df$Activity) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Interface + Activity, function(data) c(
  "Nrows"=nrow(data),
  "Adoption='yes'"=sum(data$Adoption == 'yes'),
  "Adoption='no'"=sum(data$Adoption == 'no'),
  "Adoption='maybe'"=sum(data$Adoption == 'maybe')
))

par(mfrow=c(1,2))
  plot(Adoption ~ Interface, data=df, main="Adoption by Interface", col=c("lightgreen","pink","lightyellow"))
  plot(Adoption ~ Activity, data=df, main="Adoption by Activity", col=c("lightgreen","pink","lightyellow"))
par(mfrow=c(1,1))

xt = xtabs( ~ Interface + Activity + Adoption, data=df)
mosaicplot(xt, main="Adoption by Interface, Activity", col=c("lightyellow","pink","lightgreen"))

# Unfortunately, there is no family=multinomial option for glm. But
# fortunately, nnet::multinom offers an equivalent we can use.
m = multinom(Adoption ~ Interface * Activity, data=df, trace=FALSE)
Anova(m, type=3)

# We have to do a bit more to make a multinom model work with emmeans.
e0 = emmeans(m, ~ Interface*Activity | Adoption, mode="latent")
c0 = contrast(e0, method="pairwise", ref=1) 
test(c0, joint=TRUE, by="contrast")

# We can also run this using the multinomial-Poisson trick. The
# *.mp functions are defined in multpois.R and must each be loaded 
# into memory first. We can also get our post hoc pairwise comparisons 
# this way.
m0 = glm.mp(Adoption ~ Interface * Activity, data=df)
Anova.mp(m0, type=3)
glm.mp.con(m0, pairwise ~ Interface*Activity, adjust="holm")



##
## 09b_glmm.csv
## within-Ss. polytomous D.V.
##   ..mixed multinomial logistic regression
##     via the multinomial-Poisson trick
##

# prepare data table
df <- read.csv("09b_glmm.csv")
df$PId = factor(df$PId)
df$Interface = factor(df$Interface)
df$Activity = factor(df$Activity)
df$Adoption = factor(df$Adoption) # D.V.
contrasts(df$Interface) <- "contr.sum"
contrasts(df$Activity) <- "contr.sum"
View(df)

# The data is the same as 09b_glm.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# Unfortunately, there is no family=multinomial option for lme4::glmer, and 
# nnet::multinom cannot model random factors, e.g., (1|PId).

# Happily, we can run this using the multinomial-Poisson trick. The
# *.mp functions are defined in multpois.R and must each be loaded 
# into memory first. We can also get our post hoc pairwise comparisons 
# this way.
m = glmer.mp(Adoption ~ Interface*Activity + (1|PId), data=df)
Anova.mp(m, type=3)
glmer.mp.con(m, pairwise ~ Interface*Activity, adjust="holm")



##
## 09c_glm.csv
## between-Ss. ordinal D.V.
##   ..ordinal logistic regression
##

# prepare data table
df <- read.csv("09c_glm.csv")
df$PId = factor(df$PId)
df$Technique = factor(df$Technique)
df$Hands = factor(df$Hands)
df$Agreement = ordered(df$Agreement) # D.V.
contrasts(df$Technique) <- "contr.sum"
contrasts(df$Hands) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Technique + Hands, function(data) c(
  "Nrows"=nrow(data),
  "1"=sum(data$Agreement == 1),
  "2"=sum(data$Agreement == 2),
  "3"=sum(data$Agreement == 3),
  "4"=sum(data$Agreement == 4),
  "5"=sum(data$Agreement == 5),
  "6"=sum(data$Agreement == 6),
  "7"=sum(data$Agreement == 7)
))

boxplot(Agreement ~ Technique + Hands, data=df, main="Agreement by Technique, Hands")

par(mfrow=c(2,2))
  hist(as.numeric(df[df$Technique == "pinch" & df$Hands == 1,]$Agreement), xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), main="Pinch, 1 Hand", xlab="Agreement", ylab="Count")
  hist(as.numeric(df[df$Technique == "pinch" & df$Hands == 2,]$Agreement), xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), main="Pinch, 2 Hands", xlab="Agreement", ylab="Count")
  hist(as.numeric(df[df$Technique == "press" & df$Hands == 1,]$Agreement), xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), main="Press, 1 Hand", xlab="Agreement", ylab="Count")
  hist(as.numeric(df[df$Technique == "press" & df$Hands == 2,]$Agreement), xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), main="Press, 2 Hands", xlab="Agreement", ylab="Count")
par(mfrow=c(1,1))

with(df, interaction.plot(
  Technique, Hands, as.numeric(Agreement), 
  ylim=c(1,7),
  lwd=3,
  lty=1,
  main="Agreement by Technique, Hands",
  ylab="Agreement",
  col=c("darkgreen","blue")
))
msd <- ddply(df, ~ Technique + Hands, function(data) c(
  "Mean"=mean(as.numeric(data$Agreement)),
  "SD"=sd(as.numeric(data$Agreement))
)) 
dx = 0.0025  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")

# ordinal logistic regression
m = polr(Agreement ~ Technique * Hands, data=df, Hess=TRUE)
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Technique*Hands, adjust="holm", mode="cum.prob")



##
## 09c_glmm.csv
## within-Ss. ordinal D.V.
##   ..mixed ordinal logistic regression
##

# prepare data table
df <- read.csv("09c_glmm.csv")
df$PId = factor(df$PId)
df$Technique = factor(df$Technique)
df$Hands = factor(df$Hands)
df$Agreement = ordered(df$Agreement) # D.V.
contrasts(df$Technique) <- "contr.sum"
contrasts(df$Hands) <- "contr.sum"
View(df)

# The data is the same as 09c_glm.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# mixed ordinal logistic regression
df2 <- as.data.frame(df) # quirk enabling Anova.clmm to work
m = clmm(Agreement ~ Technique*Hands + (1|PId), data=df2)
Anova.clmm(m)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Technique*Hands, adjust="holm", mode="cum.prob")



##
## 09d_glm.csv
## between-Ss. count D.V.
##   ..Poisson regression
##

# prepare data table
df <- read.csv("09d_glm.csv")
df$PId = factor(df$PId)
df$Recognizer = factor(df$Recognizer)
df$Device = factor(df$Device)
contrasts(df$Recognizer) <- "contr.sum"
contrasts(df$Device) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Recognizer + Device, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Errors),
  "Mean"=mean(data$Errors), 
  "SD"=sd(data$Errors),
  "Median"=median(data$Errors),
  "IQR"=IQR(data$Errors),
  "Max"=max(data$Errors)
))

boxplot(Errors ~ Recognizer + Device, data=df, main="Errors by Recognizer, Device", xlab="Recognizer.Device")

with(df, interaction.plot(
  Recognizer, Device, Errors, 
  ylim=c(2,8),
  lwd=3,
  lty=1,
  main="Errors by Recognizer, Device",
  ylab="Errors",
  col=c("darkgreen","blue")
))
msd <- ddply(df, ~ Recognizer + Device, function(data) c(
  "Mean"=mean(data$Errors),
  "SD"=sd(data$Errors)
)) 
dx = 0.0025  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")

# fit a Poisson distribution to each condition
x = seq(floor(min(df$Errors)), ceiling(max(df$Errors)), by=1)

dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
hist(dv, main="$1, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
hist(dv, main="$1, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
hist(dv, main="Rubine, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
hist(dv, main="Rubine, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

# fit a Poisson regression model
m = glm(Errors ~ Recognizer * Device, data=df, family=poisson)

# check for overdispersion
print(check_overdispersion(m))

# check for overdispersion by condition
dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
var(dv) / abs(mean(dv)) > 1.15

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Recognizer*Device, adjust="holm")



##
## 09d_glmm.csv
## within-Ss. count D.V.
##   ..mixed Poisson regression
##

# prepare data table
df <- read.csv("09d_glmm.csv")
df$PId = factor(df$PId)
df$Recognizer = factor(df$Recognizer)
df$Device = factor(df$Device)
contrasts(df$Recognizer) <- "contr.sum"
contrasts(df$Device) <- "contr.sum"
View(df)

# The data is the same as 09d_glm.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# fit a mixed Poisson regression model
m = glmer(Errors ~ Recognizer*Device + (1|PId), data=df, family=poisson)

# check for overdispersion
print(check_overdispersion(m))

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Recognizer*Device, adjust="holm")



##
## 09e_glm.csv
## between-Ss. overdispersed count D.V.
##   ..quasi-Poisson regression
##   ..negative binomial regression
##

# prepare data table
df <- read.csv("09e_glm.csv")
df$PId = factor(df$PId)
df$Recognizer = factor(df$Recognizer)
df$Device = factor(df$Device)
contrasts(df$Recognizer) <- "contr.sum"
contrasts(df$Device) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Recognizer + Device, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Errors),
  "Mean"=mean(data$Errors), 
  "SD"=sd(data$Errors),
  "Median"=median(data$Errors),
  "IQR"=IQR(data$Errors),
  "Max"=max(data$Errors)
))

boxplot(Errors ~ Recognizer + Device, data=df, main="Errors by Recognizer, Device", xlab="Recognizer.Device")

with(df, interaction.plot(
  Recognizer, Device, Errors, 
  ylim=c(1,9),
  lwd=3,
  lty=1,
  main="Errors by Recognizer, Device",
  ylab="Errors",
  col=c("darkgreen","blue")
))
msd <- ddply(df, ~ Recognizer + Device, function(data) c(
  "Mean"=mean(data$Errors),
  "SD"=sd(data$Errors)
)) 
dx = 0.0025  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")

# fit a Poisson distribution to each condition
x = seq(floor(min(df$Errors)), ceiling(max(df$Errors)), by=1)

dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
hist(dv, main="$1, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
hist(dv, main="$1, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
hist(dv, main="Rubine, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
hist(dv, main="Rubine, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

# fit a regular Poisson regression model
m0 = glm(Errors ~ Recognizer * Device, data=df, family=poisson)

# check for overdispersion
print(check_overdispersion(m0))

# check for overdispersion by condition
dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
var(dv) / abs(mean(dv)) > 1.15

# fit a quasi-Poisson model to address overdispersion
m1 = glm(Errors ~ Recognizer * Device, data=df, family=quasipoisson)

# compare analyses of variance
Anova(m0, type=3) # family=poisson
Anova(m1, type=3) # family=quasipoisson

# post hoc pairwise comparisons
emmeans(m1, pairwise ~ Recognizer*Device, adjust="holm")

# now fit a negative binomial distribution to each condition
x = seq(floor(min(df$Errors)), ceiling(max(df$Errors)), by=1)

dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
hist(dv, main="$1, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
hist(dv, main="$1, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
hist(dv, main="Rubine, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
hist(dv, main="Rubine, Mouse Errors", xlab="Errors", ylim=c(0,0.16), freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

# fit a negative binomial regression model
m = glm.nb(Errors ~ Recognizer * Device, data=df)
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Recognizer*Device, adjust="holm")



##
## 09e_glmm.csv
## within-Ss. overdispersed count D.V.
##   ..mixed negative binomial regression
##

# prepare data table
df <- read.csv("09e_glmm.csv")
df$PId = factor(df$PId)
df$Recognizer = factor(df$Recognizer)
df$Device = factor(df$Device)
contrasts(df$Recognizer) <- "contr.sum"
contrasts(df$Device) <- "contr.sum"
View(df)

# The data is the same as 09e_glm.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# fit a mixed Poisson regression model
m0 = glmer(Errors ~ Recognizer*Device + (1|PId), data=df, family=poisson)

# check for overdispersion
print(check_overdispersion(m0))
# p = .085, so we are very close to having overdispersion

# check for overdispersion by condition
dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
var(dv) / abs(mean(dv)) > 1.15

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
var(dv) / abs(mean(dv)) > 1.15

# fit a mixed negative binomial regression model
m1 = glmer.nb(Errors ~ Recognizer*Device + (1|PId), data=df)

# compare analyses of variance
Anova(m0, type=3) # Poisson
Anova(m1, type=3) # negative binomial

# post hoc pairwise comparisons
emmeans(m1, pairwise ~ Recognizer*Device, adjust="holm")



##
## 09f_glm.csv
## between-Ss. zero-inflated count D.V.
##   ..zero-inflated Poisson regression
##   ..zero-inflated negative binomial regression
##

# prepare data table
df <- read.csv("09f_glm.csv")
df$PId = factor(df$PId)
df$Recognizer = factor(df$Recognizer)
df$Device = factor(df$Device)
contrasts(df$Recognizer) <- "contr.sum"
contrasts(df$Device) <- "contr.sum"
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Recognizer + Device, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Errors),
  "Mean"=mean(data$Errors), 
  "SD"=sd(data$Errors),
  "Median"=median(data$Errors),
  "IQR"=IQR(data$Errors),
  "Max"=max(data$Errors)
))

boxplot(Errors ~ Recognizer + Device, data=df, main="Errors by Recognizer, Device", xlab="Recognizer.Device")

with(df, interaction.plot(
  Recognizer, Device, Errors, 
  ylim=c(-1,11),
  lwd=3,
  lty=1,
  main="Errors by Recognizer, Device",
  ylab="Errors",
  col=c("darkgreen","blue")
))
msd <- ddply(df, ~ Recognizer + Device, function(data) c(
  "Mean"=mean(data$Errors),
  "SD"=sd(data$Errors)
)) 
dx = 0.0025  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="darkgreen")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=2, length=0.3, col="blue")

# fit a Poisson distribution to each condition
x = seq(floor(min(df$Errors)), ceiling(max(df$Errors)), by=1)

dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
hist(dv, main="$1, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
hist(dv, main="$1, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
hist(dv, main="Rubine, Stylus Errors", xlab="Errors", ylim=c(0,0.2), freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
hist(dv, main="Rubine, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "Poisson")$estimate
curve(dpois(round(x,0), lambda=f[1]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dpois(x, lambda=f[1]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "pois"))

# fit a regular Poisson regression model
m0 = glm(Errors ~ Recognizer * Device, data=df, family=poisson)

# check for overdispersion
print(check_overdispersion(m0))

# check for zero-inflation
print(check_zeroinflation(m0))

# fit a zero-inflated Poisson regression model
m = zeroinfl(Errors ~ Recognizer * Device, data=df, dist="poisson")

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Recognizer*Device, adjust="holm")

# now fit a negative binomial distribution to each condition
x = seq(floor(min(df$Errors)), ceiling(max(df$Errors)), by=1)

dv = df[df$Recognizer == "dollar" & df$Device == "stylus",]$Errors
hist(dv, main="$1, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

dv = df[df$Recognizer == "dollar" & df$Device == "mouse",]$Errors
hist(dv, main="$1, Mouse Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

dv = df[df$Recognizer == "rubine" & df$Device == "stylus",]$Errors
hist(dv, main="Rubine, Stylus Errors", xlab="Errors", freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

dv = df[df$Recognizer == "rubine" & df$Device == "mouse",]$Errors
hist(dv, main="Rubine, Mouse Errors", xlab="Errors", ylim=c(0,0.16), freq=FALSE)
f = fitdistr(dv, "negative binomial", lower=1e-6)$estimate
curve(dnbinom(round(x,0), size=f[1], mu=f[2]), lty=1, lwd=3, col="blue", add=TRUE)
lines(x, dnbinom(x, size=f[1], mu=f[2]), lwd=3, col="darkgreen")
gofstat(fitdist(dv, "nbinom"))

# fit a regular negative binomial regression model
m0 = glm.nb(Errors ~ Recognizer * Device, data=df)

# check for overdispersion
print(check_overdispersion(m0))

# check for zero-inflation
print(check_zeroinflation(m0))

# fit a zero-inflated negative binomial regression model
m = zeroinfl(Errors ~ Recognizer * Device, data=df, dist="negbin")

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Recognizer*Device, adjust="holm")



##
## 09f_glmm.csv
## within-Ss. zero-inflated count D.V.
##   ..mixed zero inflated Poisson regression
##   ..mixed zero-inflated negative binomial regression
##

# prepare data table
df <- read.csv("09f_glmm.csv")
df$PId = factor(df$PId)
df$Recognizer = factor(df$Recognizer)
df$Device = factor(df$Device)
contrasts(df$Recognizer) <- "contr.sum"
contrasts(df$Device) <- "contr.sum"
View(df)

# The data is the same as 09f_glm.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# fit a mixed zero-inflated Poisson regression model
m = glmmTMB(Errors ~ Recognizer*Device + (1|PId), data=df, family=poisson, ziformula=~.)

# check overdispersion
print(check_overdispersion(m))

# check zero-inflation
print(check_zeroinflation(m))

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Recognizer*Device, adjust="holm")

# fit a mixed zero-inflated negative binomial regression model
m = glmmTMB(Errors ~ Recognizer*Device + (1|PId), data=df, family=nbinom2, ziformula=~.)

# check overdispersion
print(check_overdispersion(m))

# check zero-inflation
print(check_zeroinflation(m))

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Recognizer*Device, adjust="holm")



##
## 06_lognormal.csv
## between-Ss. time-based D.V.
##   ..Gamma regression
##

# prepare data table
df <- read.csv("06_lognormal.csv")
df$PId = factor(df$PId)
df$IDE = factor(df$IDE)
contrasts(df$IDE) <- "contr.sum"
View(df)

# fit Gamma and lognormal distributions to the conditional response
dv = df[df$IDE == "Nobugs",]$Hours
hist(dv, main="Nobugs Hours", xlab="Hours", freq=FALSE)
f = gofTest(dv, distribution="gamma")
curve(dgamma(x, shape=f$distribution.parameters[1], scale=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # Gamma curve
print(f) # Shapiro-Wilk: p = .223
f = gofTest(dv, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=2, lwd=3, add=TRUE) # lognormal curve
print(f) # Shapiro-Wilk: p = .471

dv = df[df$IDE == "VStudio",]$Hours
hist(dv, main="Visual Studio Hours", xlab="Hours", freq=FALSE)
f = gofTest(dv, distribution="gamma")
curve(dgamma(x, shape=f$distribution.parameters[1], scale=f$distribution.parameters[2]), col="blue", lty=1, lwd=3, add=TRUE) # Gamma curve
print(f) # Shapiro-Wilk: p = .145
f = gofTest(dv, distribution="lnorm")
curve(dlnorm(x, meanlog=f$distribution.parameters[1], sdlog=f$distribution.parameters[2]), col="darkgreen", lty=2, lwd=3, add=TRUE) # lognormal curve
print(f) # Shapiro-Wilk: p = .675

# visualize before testing
boxplot(Hours ~ IDE,
        main="Hours by IDE",
        xlab="IDE",
        ylab="Hours",
        col=c("pink","lightblue"),
        data=df)

# build Gamma regression model
m = glm(Hours ~ IDE, data=df, family=Gamma)

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ IDE, adjust="holm")



##
## 09g_glmm.csv
## within-Ss. time-based D.V.
##   ..mixed Gamma regression
##

# prepare data table
df <- read.csv("09g_glmm.csv")
df$PId = factor(df$PId)
df$IDE = factor(df$IDE)
contrasts(df$IDE) <- "contr.sum"
View(df)

# The data is the same as 06_lognormal.csv, but now within-subjects, not between-
# subjects; therefore, we skip the descriptive statistics and visualizations.

# build a mixed Gamma regression model
m = glmer(Hours ~ IDE + (1|PId), data=df, family=Gamma)

# analysis of variance
Anova(m, type=3)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ IDE, adjust="holm")


