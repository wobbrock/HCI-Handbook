###
### 01_categories.R
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
library(XNomial) # for xmulti
library(RVAideMemoire) # for G.test
library(coin) # for symmetry_test, sign_test


##
#### 01a_categories.csv ####
## one sample, two categories
##

# prepare data table
df <- read.csv(".\\data\\01a_categories.csv")
df$PId = factor(df$PId)
df$Mobile = factor(df$Mobile)
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Mobile, function(data) c(
  "Nrows"=nrow(data)
))

# make a barplot
m <- ddply(df, ~ Mobile, function(data) c(
  "Nrows"=nrow(data)
))
barplot(
  m[,2] ~ m[,1], 
  beside=TRUE,
  legend=FALSE,
  main="Mobile Phone",
  xlab="Phone",
  ylab="Count",
  ylim=c(0,60),
  col=c("lightgreen","lightgray")
)

# make a crosstabs
xt = xtabs( ~ Mobile, data=df)
View(xt)

# binomial test
binom.test(xt, p=1/2)

# proportions test
prop.test(xt)

# chi-squared test
chisq.test(xt)

# G test
G.test(xt)



##
#### 01b_categories.csv ####
## one sample, three categories
##

# prepare data table
df <- read.csv(".\\data\\01b_categories.csv")
df$PId = factor(df$PId)
df$Desktop = factor(df$Desktop)
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Desktop, function(data) c(
  "Nrows"=nrow(data)
))

# make a barplot
m <- ddply(df, ~ Desktop, function(data) c(
  "Nrows"=nrow(data)
))
barplot(
  m[,2] ~ m[,1],
  beside=TRUE,
  legend=FALSE,
  main="Desktop OS",
  xlab="Desktop",
  ylab="Count",
  ylim=c(0,50),
  col=c("lightgreen","lightgray", "lightblue")
)

# make a crosstabs
xt = xtabs( ~ Desktop, data=df)
View(xt)

# multinomial test
xmulti(xt, rep(1/length(xt), length(xt)), statName="Prob")

# chi-squared test
chisq.test(xt)

# G-test
G.test(xt)

# comparing each category to chance
binom.test(sum(df$Desktop == "Linux"), nrow(df), p=1/3)
binom.test(sum(df$Desktop == "macOS"), nrow(df), p=1/3)
binom.test(sum(df$Desktop == "Windows"), nrow(df), p=1/3)

# adjustment for multiple comparisons
p.adjust(c(0.004036, 0.5962, 0.02597), method="holm")



##
#### 01c_categories.csv ####
## two samples
##

# prepare data table
df <- read.csv(".\\data\\01c_categories.csv")
df$PId = factor(df$PId)
df$Mobile = factor(df$Mobile)
df$Desktop = factor(df$Desktop)
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Mobile + Desktop, function(data) c(
  "Nrows"=nrow(data)
))

# make a barplot
m <- ddply(df, ~ Mobile + Desktop, function(data) c(
  "Nrows"=nrow(data)
))
barplot(
  m[,3] ~ m[,1] + m[,2],
  beside=TRUE,
  legend=FALSE,
  main="Desktop UI Preference by Mobile Phone Ownership",
  xlab="Desktop Preference",
  ylab="Count",
  ylim=c(0,50),
  col=c("lightgreen","lightgray")
)
legend(
  "topright", 
  c("Android","iPhone"), 
  title="Mobile Phone", 
  fill=c("lightgreen","lightgray")
)

# make a crosstabs
xt = xtabs( ~ Mobile + Desktop, data=df)
View(xt)

# Fisher's exact test
fisher.test(xt)

# chi-squared test
chisq.test(xt)

# G-test
G.test(xt)

# comparing Samsung Android vs. Apple iPhone...
binom.test(xt[,1], p=1/2) # within Linux
binom.test(xt[,2], p=1/2) # within Apple macOS
binom.test(xt[,3], p=1/2) # within Microsoft Windows

# adjustment for multiple comparisons
p.adjust(c(0.8238, 6.96e-05, 0.1742), method="holm")



##
#### 01d_categories.csv ####
## dependent samples
##

# prepare data table
df <- read.csv(".\\data\\01d_categories.csv")
df$PId = factor(df$PId)
df$Mobile = factor(df$Mobile)
df$Desktop = factor(df$Desktop)
View(df)

# descriptive statistics
summary(df)

ddply(df, ~ Mobile + Desktop, function(data) c(
  "Nrows"=nrow(data)
))

# make a barplot
m <- ddply(df, ~ Mobile + Desktop, function(data) c(
  "Nrows"=nrow(data)
))
barplot(
  m[,3] ~ m[,1] + m[,2],
  beside=TRUE,
  legend=FALSE,
  main="Desktop UI Preference by Mobile Phone Ownership",
  xlab="Desktop Preference",
  ylab="Count",
  ylim=c(0,50),
  col=c("lightgreen","lightgray")
)
legend(
  "topright", 
  c("Android","iPhone"), 
  title="Mobile Phone", 
  fill=c("lightgreen","lightgray")
)

# make a crosstabs
xt = xtabs( ~ Mobile + Desktop, data=df)
View(xt)

# symmetry test
symmetry_test(Desktop ~ Mobile | PId, data=df)

# compare Android vs. iPhone within Linux
df$Chose.Linux.w.Android = ifelse(df$Desktop == "Linux" & df$Mobile == "Android", 1, 0)
df$Chose.Linux.w.iPhone = ifelse(df$Desktop == "Linux" & df$Mobile == "iPhone", 1, 0)
sum(df$Chose.Linux.w.Android) # 23
sum(df$Chose.Linux.w.iPhone)  # 15
sign_test(Chose.Linux.w.Android ~ Chose.Linux.w.iPhone, data=df)

# compare Android vs. iPhone within Apple macOS
df$Chose.macOS.w.Android = ifelse(df$Desktop == "macOS" & df$Mobile == "Android", 1, 0)
df$Chose.macOS.w.iPhone = ifelse(df$Desktop == "macOS" & df$Mobile == "iPhone", 1, 0)
sum(df$Chose.macOS.w.Android) # 29
sum(df$Chose.macOS.w.iPhone)  # 49
sign_test(Chose.macOS.w.Android ~ Chose.macOS.w.iPhone, data=df)

# compare Android vs. iPhone within Microsoft Windows
df$Chose.Windows.w.Android = ifelse(df$Desktop == "Windows" & df$Mobile == "Android", 1, 0)
df$Chose.Windows.w.iPhone = ifelse(df$Desktop == "Windows" & df$Mobile == "iPhone", 1, 0)
sum(df$Chose.Windows.w.Android) # 48
sum(df$Chose.Windows.w.iPhone)  # 36
sign_test(Chose.Windows.w.Android ~ Chose.Windows.w.iPhone, data=df)

# adjustment for multiple comparisons
p.adjust(c(0.1944, 0.02354, 0.1904), method="holm")



