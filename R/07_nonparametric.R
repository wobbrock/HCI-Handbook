###
### 07_nonparametric.R
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
library(effectsize) # for cohens_d
library(afex) # for aov_ez
library(emmeans) # for emmeans
library(coin) # for median_test, 
              #     wilcox_test, 
              #     kruskal_test, 
              #     sign_test, 
              #     wilcoxsign_test, 
              #     friedman_test
library(rcompanion) # for wilcoxonZ
library(reshape2) # for dcast
library(performance) # for check_sphericity
library(ARTool) # for art, art.con


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

# independent-samples t-test
t.test(Minutes ~ Engine, var.equal=TRUE, data=df)  # Student's
cohens_d(Minutes ~ Engine, data=df)

# Brown-Mood median test
median_test(Minutes ~ Engine, data=df)

# Mann-Whitney U test
wilcox_test(Minutes ~ Engine, data=df)



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
     breaks=seq(9,14,0.5),
     col="lightblue")
  hist(df[df$Engine == "Duck",]$Minutes, 
     main="Distribution of DuckDuckGo Minutes", 
     xlab="Minutes",
     ylab="Frequency",
     xlim=c(9,14),
     ylim=c(0,3),
     breaks=seq(9,14,0.5),
     col="tan1")
  hist(df[df$Engine == "Google",]$Minutes, 
     main="Distribution of Google Minutes", 
     xlab="Minutes",
     ylab="Frequency",
     xlim=c(9,14),
     ylim=c(0,3),
     breaks=seq(9,14,0.5),
     col="lightgreen")
par(mfrow=c(1,1))

# one-way ANOVA
m = aov_ez(dv="Minutes", between="Engine", id="PId", type=3, data=df)
anova(m)

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Engine, adjust="holm")  # post hoc pairwise comparisons

# Kruskal-Wallis test
kruskal_test(Minutes ~ Engine, data=df)

# Mann-Whitney post hoc pairwise comparisons
bd = wilcox.test(df[df$Engine == "Bing",]$Minutes, df[df$Engine == "Duck",]$Minutes, exact=FALSE)   # Bing vs. DuckDuckGo
bg = wilcox.test(df[df$Engine == "Bing",]$Minutes, df[df$Engine == "Google",]$Minutes, exact=FALSE) # Bing vs. Google
dg = wilcox.test(df[df$Engine == "Duck",]$Minutes, df[df$Engine == "Google",]$Minutes, exact=FALSE) # DuckDuckGo vs. Google
p.adjust(c(bd$p.value, bg$p.value, dg$p.value), method="holm") # p-values

wilcoxonZ(df[df$Engine == "Bing",]$Minutes, df[df$Engine == "Duck",]$Minutes)
wilcoxonZ(df[df$Engine == "Bing",]$Minutes, df[df$Engine == "Google",]$Minutes)
wilcoxonZ(df[df$Engine == "Duck",]$Minutes, df[df$Engine == "Google",]$Minutes)



##
#### 04a_dependent_samples.csv ####
## two dependent samples
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

# paired-samples t-test
df2 <- dcast(df, PId ~ Mouse, value.var="Throughput")  # make wide-format table
View(df2)  # view wide-format table
t.test(df2$Logitech, df2$Microsoft, paired=TRUE)

# Cohen's d effect size
cohens_d(df2$Logitech, df2$Microsoft, paired=TRUE)

# sign test
sign_test(Throughput ~ Mouse | PId, data=df)

# Wilcoxon signed-rank test
wilcoxsign_test(Throughput ~ Mouse | PId, data=df)



##
#### 04b_dependent_samples.csv ####
## three dependent samples
##

# prepare data table
df <- read.csv(".\\data\\04b_dependent_samples.csv")
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

# repeated measures ANOVA
m = aov_ez(dv="Throughput", within="Mouse", id="PId", type=3, data=df)
print(check_sphericity(m))

anova(m, correction="none")

# post hoc pairwise comparisons
emmeans(m, pairwise ~ Mouse, adjust="holm")

# Friedman test
friedman_test(Throughput ~ Mouse | PId, data=df)

# Wilcoxon signed-rank post hoc pairwise comparisons
df2 <- dcast(df, PId ~ Mouse, value.var="Throughput")  # make wide-format table
View(df2)  # view wide-format table

lm = wilcox.test(df2$Logitech, df2$Microsoft, paired=TRUE, exact=FALSE) # Logitech vs. Microsoft
lr = wilcox.test(df2$Logitech, df2$Razer, paired=TRUE, exact=FALSE)     # Logitech vs. Razer
mr = wilcox.test(df2$Microsoft, df2$Razer, paired=TRUE, exact=FALSE)    # Microsoft vs. Razer
p.adjust(c(lm$p.value, lr$p.value, mr$p.value), method="holm") # p-values

wilcoxonZ(df2$Logitech, df2$Microsoft, paired=TRUE)
wilcoxonZ(df2$Logitech, df2$Razer, paired=TRUE)
wilcoxonZ(df2$Microsoft, df2$Razer, paired=TRUE)



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
boxplot(WPM ~ Keyboard + Posture,
        main="WPM by Keyboard, Posture",
        xlab="Keyboard.Posture",
        ylab="WPM",
        col=c("lightgreen","lightgray","darkgreen","darkgray"),
        data=df)

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
       col=c("darkgreen","darkgray"))
)
msd <- ddply(df, ~ Posture + Keyboard, function(data) c(
  "Mean"=mean(data$WPM), 
  "SD"=sd(data$WPM)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=2, lwd=3, length=0.2, col="darkgreen")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="darkgray")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=2, lwd=3, length=0.2, col="darkgreen")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="darkgray")

# two-way ANOVA
m0 = aov_ez(dv="WPM", between=c("Keyboard","Posture"), id="PId", type=3, data=df)
anova(m0)

# post hoc pairwise comparisons
emmeans(m0, pairwise ~ Keyboard*Posture, adjust="holm")

# ART
m = art(WPM ~ Keyboard * Posture, data=df)
anova(m)

# ART-C post hoc pairwise comparisons
art.con(m, ~ Keyboard*Posture, adjust="holm")



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

# build an ANOVA model
m0 = aov_ez(dv="WPM", within=c("Keyboard","Posture"), id="PId", type=3, data=df)
print(check_sphericity(m0))

anova(m0, correction="none")

# post hoc pairwise comparisons
emmeans(m0, pairwise ~ Keyboard*Posture, adjust="holm")

# ART
m = art(WPM ~ Keyboard*Posture + (1|PId), data=df)
anova(m)

# ART-C post hoc pairwise comparisons
art.con(m, ~ Keyboard*Posture, adjust="holm")



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

# build an ANOVA model
m0 = aov_ez(dv="WPM", between="Keyboard", within="Posture", id="PId", type=3, data=df)
print(check_sphericity(m0))

anova(m0, correction="none")

# post hoc pairwise comparisons
emmeans(m0, pairwise ~ Keyboard*Posture, adjust="holm")

# ART
m = art(WPM ~ Keyboard*Posture + (1|PId), data=df)
anova(m)

# ART-C post hoc pairwise comparisons
art.con(m, ~ Keyboard*Posture, adjust="holm")


