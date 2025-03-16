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


#-----------------------
#get data
df <- read.csv('dummy_data.csv')%>%
  as_tibble()%>%
  select(!X) #removing the ID column. You might want to keep this though

#setting categorical data to factors
df$gender <- as.factor(df$gender)
df$visual_imp <- as.factor(df$visual_imp)
df$fav_fruit <- as.factor(df$fav_fruit)

#correct impossible data
df$depression[40]#max score is 15
df$depression[40]<-NA
df$testA[20]#max score is 20
df$testA[20]<-NA


#---------------------
#get demographics
#basically looking at summary statistics for people who have and don't have the diagnosis, then the sample as a whole
#numeric variables
demographics_yes <- df %>%
  filter(diagnosis=='yes')%>%
  summarise(across(where(is.numeric), .f = list(mean=mean,min=min,max=max, sd=sd), na.rm=TRUE))%>%
  t()

demographics_no <- df %>%
  filter(diagnosis=='no')%>%
  summarise(across(where(is.numeric), .f = list(mean=mean,min=min,max=max, sd=sd), na.rm=TRUE))%>%
  t()

demographics_whole_sample <- df %>%
  summarise(across(where(is.numeric), .f = list(mean=mean,min=min,max=max, sd=sd), na.rm=TRUE))%>%
  t()

demographics_all <- tibble(demographics_yes, demographics_no, demographics_whole_sample)%>%
  bind_cols(rownames(demographics_yes))
demographics_all

#a different way to look at the same thing
df %>% filter(diagnosis=='yes')%>% summary()
df %>% filter(diagnosis=='no')%>% summary()
df %>% summary() #for whole sample


#categorical variables
cat_vars <- colnames(df)[sapply(df,is.factor)]#this gets a vector of all the categorical variables - set to factors above
#produces a frequency table for each of these categorical variables
lapply(cat_vars, function(x){
  table(df[x])
})


#------------------------
#start some hypothesis testing
#first question - is there an association between the proposed index test (testA,testB,testC) and the diagnosis?

#I already know these are normally distributed, but lets check anyway
lapply(c('testA', 'testB', 'testC'), function(test){
  df[[test]]%>%hist(main=test)#remember to flick through to see all the tests
  mtext(paste('Shapiro-Wilk normality test',shapiro.test(df[[test]])$statistic))
})

#now lets look at comparing the tests with diagnosis
#testA
stat.testA <- df %>%
  t_test(testA ~ diagnosis) %>%
  add_significance()
stat.testA

ggplot(df, aes(x=diagnosis, y=testA)) + 
  geom_boxplot()+
  labs(caption=paste('P=',stat.testA$p, stat.testA$p.signif))

#testB
stat.testB <- df %>%
  t_test(testB ~ diagnosis) %>%
  add_significance()
stat.testB

ggplot(df, aes(x=diagnosis, y=testB)) + 
  geom_boxplot()+
  labs(caption=paste('P=',stat.testB$p, stat.testB$p.signif))

#testC
stat.testC <- df %>%
  t_test(testC ~ diagnosis) %>%
  add_significance()
stat.testC


ggplot(df, aes(x=diagnosis, y=testC)) + 
  geom_boxplot()+
  labs(caption=paste('P=',stat.testC$p, stat.testC$p.signif))

stat.test_all_tests <- bind_rows(
  stat.testA, stat.testB, stat.testC
)
#now lets look at all the potential confounders

#numeric
confounders_numeric <- bind_rows(
  t_test(df, depression ~ diagnosis),
  t_test(df, age ~ diagnosis),
  t_test(df, education ~ diagnosis)
)
confounders_numeric

chisq.test(df$gender, df$diagnosis)


confounders_catagorical <- bind_rows(
  chisq_test(df$gender, df$diagnosis),
  chisq_test(df$visual_imp, df$diagnosis),
  chisq_test(df$fav_fruit, df$diagnosis)
)%>%bind_cols(tibble(test=c('gender','visual impairment','favourite fruit')))
  
confounders_catagorical

#we can see that only depression and age are significant
#although we'll come back to visual impairment