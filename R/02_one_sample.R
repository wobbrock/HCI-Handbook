###
### 02_one_sample.R
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
library(BSDA) # for z.test
library(effectsize) # for cohens_d


##
## 02_one_sample.csv
## continuous D.V. vs. hypothesized mu
##

# prepare data table
df <- read.csv("02_one_sample.csv")
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
     ylab="Count"
)

# normality test of response
shapiro.test(df$Hours)

# also compute each measure's residual
df$Residual = df$Hours - mean(df$Hours)
hist(df$Residual,
     main="Programming Hours Residuals",
     xlab="Residual",
     ylab="Count",
     xlim=c(-15,+15)
)

# for a single sample, the normality of the response
# *is* the same as the normality of the residuals
shapiro.test(df$Residual)

# one-sample z.test
z.test(df$Hours, mu=20.0, sigma.x=sd(df$Hours))

# we can also manually compute the Z-score and p-value this way
ca = qnorm(1 - .05/2)                    # critical value
moe = ca * sd(df$Hours) / sqrt(nrow(df)) # margin of error
Bl = mean(df$Hours) - moe                # CI lower bound
Bu = mean(df$Hours) + moe                # CI upper bound
dx = mean(df$Hours) - 20.0               # delta x
SE = (Bu - Bl) / (2 * ca)                # standard error
Z = abs(dx / SE)                         # absolute Z-score
p = exp(-0.717*Z - 0.416*Z*Z)            # p-value

print(Bl) # lower bound
print(Bu) # upper bound
print(Z)  # Z-score
print(p)  # ~p-value

# or, we can just compute the Z-score and ask R for the p-value
Z = (mean(df$Hours) - 20.0) / (sd(df$Hours)/ sqrt(nrow(df)))
(1 - pnorm(Z))*2  # p-value

# one-sample t.test
t.test(df$Hours, mu=20.0)

# calculate the t-test's p-value directly
(1 - pt(Z, df=15))*2  # p-value

# in the limit, the t-test converges to the Z-test
(1 - pt(Z, df=Inf))*2  # p-value

# calculate Cohen's d effect size
cohens_d(df$Hours, mu=20.0)

# Cohen's d can be calculated directly
d = (mean(df$Hours) - 20.0) / sd(df$Hours)
print(d)


