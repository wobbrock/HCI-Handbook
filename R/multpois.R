###
### multpois.R
###
### "multinomial-Poisson trick"
###
### Jacob O. Wobbrock, Ph.D.
### University of Washington
### wobbrock@uw.edu
###
### Last updated: 10/10/2024
###
### Implements the multinomial-Poisson trick for multinomial
### and mixed multinomial regression models. For between-Ss.
### designs with a nominal response, use glm.mp. For within-Ss.
### designs or mixed factorial designs, use glmer.mp. Both 
### offer post hoc pairwise comparison functions as *.con.
### To get ANOVA-style results, use Anova.mp.
###
### To verify correctness:
###
###  1. For between-Ss. data with dichotomous response, compare
###     glm.mp to stats::glm with family=binomial.
###
###  2. For within-Ss. data with dichotomous response, compare 
###     glmer.mp to lme4::glmer with family=binomial.
###
###  3. For between-Ss. data with polytomous response, compare
###     glm.mp to nnet::multinom.
###
###  4. (There is no easy comparison for within-Ss. data with
###     polytomous response, which is the primary motivation
###     for creating multpois.R in the first place. Use glmer.mp
###     to get such results!)
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


library(plyr)  # for laply
library(dplyr) # for mutate
library(dfidx) # for dfidx
library(lme4)  # for glmer
library(lmerTest)
library(car)   # for Anova


##
#### glm.mp ####
##
glm.mp <- function(formula, data)
{
  # ensure there is only one D.V.
  t = terms(formula)
  if (attr(t, "response") != 1) {
    stop("glm.mp is only valid for one dependent variable. You have ", attr(t, "response"), ".")
  }
  
  # get the one D.V.
  DV = formula[[2]] # D.V.
  
  # ensure D.V. is nominal
  if (!is.factor(data[[DV]])) {
    stop("glm.mp is only valid for nominal dependent variables (i.e., factors).\n\t", DV, " is of type ", class(data[[DV]]))
  }
  
  # get the independent variables from the formula
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]
  
  # ensure there are no random factors in the formula
  hasrnd = laply(IVs, function(term) as.list(term)[[1]] == quote(`|`))
  if (any(hasrnd)) {
    stop("glm.mp is only valid for formulas without random factors.")
  }
  
  # transform data table
  df = dfidx(data, choice=DV, shape="wide", drop.index=FALSE, idnames=c("chid","alt"))

  # copy over the factor contrasts from the source table to the new table
  for (i in 1:length(colnames(data))) {
    if (is.factor(data[[i]]) & colnames(data)[i] != DV) {
      contrasts(df[[i]]) <- contrasts(data[[i]])
    }
  }
  # also set the new "alt" factor contrasts
  contrasts(df$alt) <- "contr.sum"

  # create the new formula with the "alt" factor
  f = update.formula(formula, . ~ . * alt)

  # build and return our model
  m = glm(f, data=df, family=poisson) # m-P trick
  return (m)
}


##
#### glmer.mp ####
##
glmer.mp <- function(formula, data)
{
  # ensure there is only one D.V.
  t = terms(formula)
  if (attr(t, "response") != 1) {
    stop("glmer.mp is only valid for one dependent variable. You have ", attr(t, "response"), ".")
  }
  
  # get the one D.V.
  DV = formula[[2]] # D.V.
  
  # ensure D.V. is nominal
  if (!is.factor(data[[DV]])) {
    stop("glmer.mp is only valid for nominal dependent variables (i.e., factors).\n\t", DV, " is of type ", class(data[[DV]]))
  }
  
  # get the independent variables from the formula
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]

  # ensure there is a random factor in the formula
  hasrnd = laply(IVs, function(term) as.list(term)[[1]] == quote(`|`))
  if (!any(hasrnd)) {
    stop("glmer.mp is only valid for formulas with random factors, e.g., (1|S) or (X|S).")
  }

  # transform data table
  df = dfidx(data, choice=DV, shape="wide", drop.index=FALSE, idnames=c("chid","alt"))

  # copy over the factor contrasts from the source table to the new table
  for (i in 1:length(colnames(data))) {
    if (is.factor(data[[i]]) & colnames(data)[i] != DV) {
      contrasts(df[[i]]) <- contrasts(data[[i]])
    }
  }
  # also set the new "alt" factor contrasts
  contrasts(df$alt) <- "contr.sum"

  # add the "alt" factor and ":alt" interactions to only the fixed effects
  f = formula
  tlabs = attr(terms(f), "term.labels")
  for (i in 1:length(tlabs)) {
    if (!grepl("|", tlabs[i], fixed=TRUE)) {
      f = update.formula(f, paste0(". ~ . + ", tlabs[i], ":alt"))
    }
  }
  f = update.formula(f, . ~ . + alt) # add "alt" main effect

  # build and return our model
  m = glmer(f, data=df, family=poisson) # m-P trick
  return (m)
}


##
#### glm.mp.con ####
##
glm.mp.con <- function(model, formula, adjust=c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"))
{
  # require the pairwise keyword
  if (formula[[2]] != "pairwise") {
    stop("glm.mp.con requires the 'pairwise' keyword on the left hand side of the ~ .")
  }
  
  # ensure the model is of class "glm"
  mtype = as.list(class(model))
  if (!any(mtype == "glm")) {
    stop("glm.mp.con requires a model created by glm.mp.")
  }

  # get the data frame used for the model
  df = model.frame(model)

  # df must contain an "alt" factor column or this isn't a model built by glm.mp
  if (!exists("alt", df)) {
    stop("glm.mp.con requires a model created by glm.mp.")
  }

  # ensure there are no random factors in the original model formula
  f0 = formula(model)
  t0 = terms(f0)
  iv0 = as.list(attr(t0, "variables"))[c(-1,-2)]
  hasrnd = laply(iv0, function(term) as.list(term)[[1]] == quote(`|`))
  if (any(hasrnd)) {
    stop("glm.mp.con requires a model without random factors.")
  }

  # get our contrast formula I.V.s
  t = terms(formula)
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]
  
  # ensure all contrast I.V.s were in the original model formula
  if (!any(IVs %in% iv0)) {
    stop("glm.mp.con requires formula terms to be present in the model.")
  }
  
  # warn if any contrast formula I.V.s are not factors
  ivnotfac = laply(IVs, function(term) !is.factor(df[[term]]))
  if (any(ivnotfac)) {
    snf = ""
    for (i in 1:length(ivnotfac)) {
      if (ivnotfac[i]) {
        snf = paste0(snf, '\n\t', IVs[[i]], " is of type ", class(df[[ IVs[[i]] ]]))
      }
    }
    warning("glm.mp.con makes little sense for terms that are not factors:", snf, immediate.=TRUE)
  }

  # build our new composite factor name and column values
  facname = IVs[[1]]
  facvals = df[[ IVs[[1]] ]]
  if (length(IVs) > 1) {
    for (i in 2:length(IVs)) {
      facname = paste0(facname, ".", IVs[[i]])
      facvals = paste0(facvals, ".", df[[ IVs[[i]] ]])
    }
  }

  # set the new column and its values as a factor and get its levels
  df[[facname]] = as.factor(facvals)
  lvls = levels(df[[facname]])

  # get our model's dependent variable
  DV = formula(model)[[2]]

  # now do each of the pairwise comparisons and store them in a table
  resdf <- data.frame(Contrast=character(), Chisq=numeric(), Df=numeric(), N=integer(), p.value=numeric())

  for (i in 1:(length(lvls) - 1))
  {
    for (j in (i + 1):length(lvls))
    {
      # get the relevant subset of rows for this comparison
      d0 = df[df[[facname]] == lvls[i] | df[[facname]] == lvls[j],]

      # update factor levels and contrasts for subset
      d0[[facname]] = factor(d0[[facname]])
      contrasts(d0[[facname]]) <- "contr.sum"

      # create our model formula for this comparison
      s = paste0(DV, " ~ ", facname, " + alt + ", facname, ":alt")
      f = as.formula(s) # convert to formula

      # finally, create our model and examine its effects
      m = glm(f, data=d0, family=poisson) # m-P trick
      a = Anova(m, type=3)
      a = a[grep(":alt", rownames(a)),] # get relevant entry

      # improve our row
      rownames(a)[1] = paste0(lvls[i], " - ", lvls[j]) # update contrast label
      colnames(a)[3] = "p.value" # simplify p-value label
      a = mutate(a, .after=Df, N=nrow(d0)/length(levels(df$alt))) # insert N

      # create a new output row entry and add it to our output table
      r = list(Contrast = rownames(a)[1],
               Chisq = round(as.numeric(format(a$`LR Chisq`, scientific=FALSE)), 6),
               Df = a$Df,
               N = a$N,
               p.value=round(a$p.value, 6)
      )
      resdf = rbind(resdf, r, stringsAsFactors=FALSE) # add row to table
    }
  }

  # apply p-value adjustment
  adjust = match.arg(adjust)
  resdf$p.value = p.adjust(resdf$p.value, method=adjust)

  # assemble our return value as a list
  retval = list(
    heading = "Pairwise comparisons via the multinomial-Poisson trick",
    contrasts = resdf,
    notes = paste0("P value adjustment: ", adjust, " method for ", length(rownames(resdf)), " tests")
  )

  return (retval)
}


##
#### glmer.mp.con ####
##
glmer.mp.con <- function(model, formula, adjust=c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"))
{
  # require the pairwise keyword
  if (formula[[2]] != "pairwise") {
    stop("glmer.mp.con requires the 'pairwise' keyword on the left hand side of the ~ .")
  }
  
  # ensure the model is of class "glmerMod"
  mtype = as.list(class(model))
  if (!any(mtype == "glmerMod")) {
    stop("glmer.mp.con requires a model created by glmer.mp.")
  }
  
  # get the data frame used for the model
  df = model.frame(model)
  
  # df must contain an "alt" factor column or this isn't a model built by glmer.mp
  if (!exists("alt", df)) {
    stop("glmer.mp.con requires a model created by glmer.mp.")
  }
  
  # ensure there is a random factor in the original model
  f0 = formula(model)
  t0 = terms(f0)
  iv0 = as.list(attr(t0, "variables"))[c(-1,-2)]
  hasrnd = laply(iv0, function(term) as.list(term)[[1]] == quote(`|`))
  if (!any(hasrnd)) {
    stop("glmer.mp.con requires a model with a random factor, e.g., (1|S) or (X|S).")
  }
  
  # get our contrast formula I.V.s
  t = terms(formula)
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]
  
  # ensure all contrast I.V.s were in the original model formula
  if (!any(IVs %in% iv0)) {
    stop("glmer.mp.con requires formula terms to be present in the model.")
  }
  
  # warn if any contrast formula I.V.s are not factors
  ivnotfac = laply(IVs, function(term) !is.factor(df[[term]]))
  if (any(ivnotfac)) {
    snf = ""
    for (i in 1:length(ivnotfac)) {
      if (ivnotfac[i]) {
        snf = paste0(snf, '\n\t', IVs[[i]], " is of type ", class(df[[ IVs[[i]] ]]))
      }
    }
    warning("glmer.mp.con makes little sense for terms that are not factors:", snf, immediate.=TRUE)
  }
  
  # build our new composite factor name and column values
  facname = IVs[[1]]
  facvals = df[[ IVs[[1]] ]]
  if (length(IVs) > 1) {
    for (i in 2:length(IVs)) {
      facname = paste0(facname, ".", IVs[[i]])
      facvals = paste0(facvals, ".", df[[ IVs[[i]] ]])
    }
  }
  
  # set the new column and its values as a factor and get its levels
  df[[facname]] = as.factor(facvals)
  lvls = levels(df[[facname]])
  
  # get our dependent variable
  DV = f0[[2]]

  # get our model's original terms so we can preserve random effects
  tlabs = attr(t0, "term.labels")
  
  # now do each of the pairwise comparisons and store them in an output table
  resdf <- data.frame(Contrast=character(), Chisq=numeric(), Df=numeric(), N=integer(), p.value=numeric())

  for (i in 1:(length(lvls) - 1)) 
  {
    for (j in (i + 1):length(lvls)) 
    {
      # get the relevant subset of rows for this comparison
      d0 = df[df[[facname]] == lvls[i] | df[[facname]] == lvls[j],]

      # update factor levels and contrasts for subset
      d0[[facname]] = factor(d0[[facname]])
      contrasts(d0[[facname]]) <- "contr.sum"

      # create our model formula for this comparison
      s = paste0(DV, " ~ ", facname, " + alt + ", facname, ":alt")
      for (k in 1:length(tlabs)) {
        if (grepl("|", tlabs[k], fixed=TRUE)) {
          s = paste0(s, " + (", tlabs[k], ")")
        }
      }
      f = as.formula(s) # convert to formula
      
      # finally, create our model and examine its effects
      m = glmer(f, data=d0, family=poisson) # m-P trick
      a = Anova(m, type=3)
      a = a[grep(":alt", rownames(a)),] # get relevant entry

      # improve our row
      rownames(a)[1] = paste0(lvls[i], " - ", lvls[j]) # update contrast label
      colnames(a)[3] = "p.value" # simplify p-value label
      a = mutate(a, .after=Df, N=nrow(d0)/length(levels(df$alt))) # insert N

      # create a new output row entry and add it to our output table
      r = list(Contrast = rownames(a)[1], 
               Chisq = round(as.numeric(format(a$Chisq, scientific=FALSE)), 6),
               Df = a$Df, 
               N = a$N, 
               p.value = round(a$p.value, 6)
      )
      resdf = rbind(resdf, r, stringsAsFactors=FALSE) # add row to table
    }
  }

  # apply p-value adjustment
  adjust = match.arg(adjust)
  resdf$p.value = p.adjust(resdf$p.value, method=adjust)
  
  # assemble our return value as a list
  retval = list(
    heading = "Pairwise comparisons via the multinomial-Poisson trick",
    contrasts = resdf,
    notes = paste0("P value adjustment: ", adjust, " method for ", length(rownames(resdf)), " tests")
  )
  
  return (retval)
}


##
#### Anova.mp ####
##
Anova.mp <- function(model, type=c(3, 2, "III", "II")) 
{
  # ensure the model is of class "glm" or "glmerMod"
  mtype = as.list(class(model))
  if (!any(mtype == "glm" | mtype == "glmerMod")) {
    stop("Anova.mp requires a model created by glm.mp or glmer.mp.")
  }
  
  # get the data frame used to create the model
  df = model.frame(model)
  
  # df must contain an "alt" factor column or this isn't a model built by glm.mp or glmer.mp
  if (!exists("alt", df)) {
    stop("Anova.mp requires a model created by glm.mp or glmer.mp.")
  }
  
  # get the Anova type
  type = as.character(type)
  type = match.arg(type)
  
  # run the Anova
  a = Anova(model, type)

  # update our output heading
  attr(a, "heading")[3] = "via the multinomial-Poisson trick"
  h = attr(a, "heading") # save

  # insert N for chisq result
  a = mutate(.data=a, .after=Df, N=nrow(df)/length(levels(df$alt)))

  # extract relevant effect entries
  a = a[grep(":alt", rownames(a), fixed=TRUE),]
  rownames(a) = sub(":alt", "", rownames(a))

  # make consistent
  colnames(a)[1] = "Chisq"

  attr(a, "heading") = h # restore
  return (a)
}


