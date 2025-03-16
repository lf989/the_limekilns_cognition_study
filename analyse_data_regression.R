##The Limekilns Cognition Study- an R tutorial 
#analyse the data - regression
#© copyright of Lachlan Fotheringham
#This file is part of The Limekilns Cognition Study- an R tutorial
#The Limekilns Cognition Study- an R tutorial is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#The Limekilns Cognition Study- an R tutorial is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#You should have received a copy of the GNU General Public License along with this file. If not, see <https://www.gnu.org/licenses/>.

#initialise workspace
rm(list = ls());
cat("\14");
setwd("/Users/lfoth/Documents/R/tutorial")#set to whatever your working directory is - probably easiest to have it wherever you have put the .R file

#load fav libraries
library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)#
library(rstatix)

#bespoke functions
format_p_value <- function(p_value){
  #format the p value if it's really low
  #there is also format.pval but it's not quite what I wanted
  
  #roundUp <- function(x) 10^ceiling(log10(x))#finds the next power of 10 above, for use with < symbol
  
  if(p_value <0.001){
    return("<0.001")
  }else{
    p_value %<>% round(3)%>%
      as.character()
    return(p_value)
  }
}

get_coef <- function(m, outcome){
  
  #states for testing
  #m <- m_testA
  #outcome <- "testA"
  
  ci <- confint(m, method = "Wald")
  est <- summary(m)%>%coef()
  
  cat("\n")
  cat("getting coefs for... ", outcome)
  cat("\n")
  
  
  test_coefs <- tibble(
    test = outcome,
    beta = est[outcome,"Estimate"],
    SE = est[outcome,"Std. Error"],
    OR = est[outcome,"Estimate"]%>%exp(),
    OR_CI_lower = ci[outcome,"2.5 %"]%>%exp(),
    OR_CI_upper = ci[outcome,"97.5 %"]%>%exp(),
    p = est[outcome,"Pr(>|z|)"]%>%format_p_value(),
    p_exact = est[outcome,"Pr(>|z|)"]
  )
  
  #add on the model formula to the table to give easier access to this
  #might also help with version control
  test_coefs %<>%
    bind_cols(tibble(
      m_formula = as.character(m$call)[[2]]
    ))
  
  return(test_coefs)
}


#-----------------------
#get data
df <- read.csv('dummy_data.csv')%>%
  as_tibble()%>%
  select(!X)

df$gender <- as.factor(df$gender)
df$visual_imp <- as.factor(df$visual_imp)
df$fav_fruit <- as.factor(df$fav_fruit)

#correct impossible data
df$depression[40]#max score is 15
df$depression[40]<-NA
df$testA[20]#max score is 20
df$testA[20]<-NA


#now a regression
#the convention is to have the disease as 1 (present) and 0 (absent) rather than as a character/factor
df %<>%
  mutate(diagnosis = case_when(
    diagnosis == 'yes' ~ 1,
    diagnosis == 'no' ~ 0
  ))


#check basic model
m_basic <- glm(diagnosis ~ 
                 depression + 
                 age + 
                 gender +
                 education + 
                 visual_imp + 
                 fav_fruit, 
               data = df,
               family = 'binomial')
summary(m_basic)

#just as we expected, gender, education, visual impairment and fav fruite are useless
#lets run again
m_basic <- glm(diagnosis ~ 
                 depression + 
                 age,
               data = df,
               family = 'binomial')
summary(m_basic)


#odds ratio = exp(log odds)
exp(coef(m_basic))

#check model for tests
m_testA <- glm(diagnosis ~ 
                 testA +
                 depression + 
                 age, 
               data = df,
               family = 'binomial')
summary(m_testA)

#check testA with education due to known impact
m_testA_with_education <- glm(diagnosis ~ 
                 testA +
                 depression + 
                 age +
                 education, 
               data = df,
               family = 'binomial')
summary(m_testA_with_education)

m_testB <- glm(diagnosis ~ 
                 testB +
                 depression + 
                 age, 
               data = df,
               family = 'binomial')
summary(m_testB)
length(m_testB$residuals)


m_testC <- glm(diagnosis ~ 
                 testC +
                 depression + 
                 age,
               data = df,
               family = 'binomial')
summary(m_testC)
length(m_testC$residuals)

m_testC_with_visual_imp <- glm(diagnosis ~ 
                                 testC +
                                 depression + 
                                 age +
                                 visual_imp,
                               data = df,
                               family = 'binomial')
summary(m_testC_with_visual_imp)
length(m_testC_with_visual_imp$residuals)
summary(m_testC_with_visual_imp)


model_outputs_all <- bind_rows(
  get_coef(m_testA, 'testA'),
  get_coef(m_testA_with_education, 'testA'),#with education added
  get_coef(m_testB, 'testB'),
  get_coef(m_testC, 'testC'),
  get_coef(m_testC_with_visual_imp, 'testC')#with visual impairment added
)
model_outputs_all

#------------------------------




