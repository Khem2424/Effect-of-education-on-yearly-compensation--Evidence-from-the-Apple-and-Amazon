#Final Project
#----------------------------------------------------------------
# Clear environment, which means totally clear R environment
# of objects and loaded packages
rm(list=ls())

# Specifying a display option
options("jtools-digits"=4) 
options(scipen = 999, digits= 3)

#----------------------------------------------------------------
# List of packages 
packages <- c("AER","car","ggplot2","gmodels", "haven", "jtools", "pastecs", "psych", "skedastic",
              "stargazer", "summarytools","tidyverse", "corrplot", "olsrr", "RNHANES", "dplyr", 
              "gridExtra", "dynlm", "ggthemes","shiny","urca","vars","shinydashboard","readxl", "dlookr", "gtools", "scales", "caret", "rvest","xml2","readr")

# Install the packages
# Run this code to install packages you do not have installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) 
{install.packages(packages[!installed_packages])}

# Load Data
#--------------------------------------------------------
data = read.csv("final.csv",header=TRUE)

## Data modeling 
compensation= data$totalyearlycompensation
exper=data$yearsofexperience

##Running regression 
reg1<-lm(compensation~Educ+exper+Gender+Company+Company* Educ, data=data)
summary(reg1)

library(car)
vif(reg1)
vif(reg1, type='predictor')
## Partailing out other explanatory variable in order to know the effect of education
reg2<-lm(Educ~exper+Gender+Company+Company* Educ, data=data)
summary(reg2)
data$Educ_res<-reg2$resid

reg3<-lm(compensation~Educ_res, data=data)
summary(reg3)

## Heteroskedasticity Tests
#install.packages("lmtest")
library(lmtest)
library(sandwich)

#(1) Breusch-Pagan Test
bptest(reg1, data=data)

#(2) White test
data$residualSQ<-reg1$residuals^2
data$fit<-reg1$fitted.values
bptest(reg1, ~fit+I(fit^2), data=data)


##For removing Heteroskedasticity using LPMWLS
robustse<-coeftest(reg1, vcov = vcovHC(reg1, "HC0"))
robustse

data<-transform(data, PC_fitted = fitted.values(reg1))
sum(data$PC_fitted>1)
sum(data$PC_fitted<0)


# FWLS
#step 1. save u_hat from LPM
data<-transform(data, u_hat_sq=residuals(reg1)^2)
#step 2. generate log u_hat
data<-transform(data, lu_hat_sq=log(u_hat_sq))
#step 3. regress log u_hat on x
reg9=lm(lu_hat_sq~Educ+exper+Gender+Company+Company* Educ, data=data)
data<-transform(data, fv=fitted.values(reg9))
#step 4. exponentiate the fitted values
data<-transform(data, efv=exp(fv))
#step 5. WLS
reg10<-lm(compensation~Educ+exper+Gender+Company+Company* Educ, weights=1/(efv), data=data)
summary(reg10)

#Testing Heteroskedasticity using BP test
bptest(reg10, data=data)
#(2) White test
data$residualSQ<-reg10$residuals^2
data$fit<-reg10$fitted.values
bptest(reg10, ~fit+I(fit^2), data=data)

##Checking over-specification
##With Gender
summary(reg10)

##With gender
reg11<-lm(compensation~Educ+exper+Company+Company* Educ, weights=1/(efv), data=data)
summary(reg11)

library(stargazer)
stargazer(reg10,reg11, type="text")
## since there is no diiference in adjusted R square, so using Gender variable is over-specified in the model.
#hence, our orginal model will be constucted withour gender variable.
##Endogeneity 

# Load Data for amazon 
#--------------------------------------------------------
data = read.csv("amazon_final (1).csv",header=TRUE)
## Data modeling 
compensation= data$totalyearlycompensation
exper=data$yearsofexperience
reg12<-lm(compensation~Educ+exper+, data=data)
summary(reg12)

# Load Data for Apple
#--------------------------------------------------------
data = read.csv("apple_final (1).csv",header=TRUE)
compensation= data$totalyearlycompensation
exper=data$yearsofexperience
reg13<-lm(compensation~Educ+exper, data=data)
summary(reg13)

stargazer(reg12,reg13, type="text")
