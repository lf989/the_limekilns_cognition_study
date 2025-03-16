##The Limekilns Cognition Study- an R tutorial 
#generate the data
#© copyright of Lachlan Fotheringham
#This file is part of The Limekilns Cognition Study- an R tutorial
#The Limekilns Cognition Study- an R tutorial is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#The Limekilns Cognition Study- an R tutorial is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#You should have received a copy of the GNU General Public License along with this file. If not, see <https://www.gnu.org/licenses/>.

#initialise workspace
rm(list = ls());
cat("\14");
setwd("/Users/lfoth/Documents/R/tutorial")

#load fav libraries
library(dplyr)
library(ggplot2)
library(magrittr)
library(sn)


#-----------------------
#generate data
set.seed(100)

#generate diagnoses - the first 100 are +ve for the condition, and the second 100 are -ve
diagnosis <- c(rep('yes',100),rep('no', 100))%>%
  factor(ordered=FALSE)

#generate confounders
#depression
#this is a GDS-15 score - the people who have the condition score a bit highter on the test
depression <- c(round(rnorm(100, mean=3, sd=3)),round(rnorm(100, mean=1, sd=1)))
depression <- ifelse(depression > 15, 15, depression)#pin max
depression <- ifelse(depression < 0, 0, depression)#pin min
hist(depression)#inspect vector on it's own

#age
#this is typically right skewed in older populations, so making use of the 'sn' package
#in sn, there is slightly different syntax - xi is the mean, omega is the SD, alpha is skewness
#the people with the condition are slightly older
age <- c(round(rsn(100, xi = 65, omega = 7, alpha = 7)),round(rsn(100, xi = 70, omega = 7, alpha = 7)))
#no need for a max age - this can remain random
age <- ifelse(age < 60, 60, age)#pin min
hist(age)#inspect vector on it's own

#gender
#have decided not to try and replicate the known gender trends for this data
gender <- sample(c('m','f'), size = 200, replace = TRUE)%>%
  factor(ordered=FALSE)

#education
#number of years of education - people with the condition are slightly less educated to incorporate cog reserve
#ie. more skew and lower mean for those with the condition
#have skewed right
#in sn, there is slightly different syntax - xi is the mean, omega is the SD, alpha is skewness
education <- c(round(rsn(100, xi = 2, omega = 3, alpha = 7)),round(rsn(100, xi = 3, omega = 3, alpha = 5)))
education <- ifelse(education < 0, 0, education)#pin min
hist(education)

#visual impairment
#more visual impairment in people that are older
#this assigns a binary value in proportion to the age
#the 500 is a modifier to raise or lower the overall probability 
#- so someone with an age of 60/500 has a 0.12 probability of having a visual impairment
visual_imp <- sapply(age/400, rbinom, n = 1, size = 1)
visual_imp <- ifelse(visual_imp == 1, 'y', 'n')%>%
  factor(ordered=FALSE)
table(visual_imp)

#favourite fruit
#this is entirely random
fav_fruit <- sample(c('apple','pear'), size = 200, replace = TRUE)%>%
  factor(ordered=FALSE)


#generate index test data
#index test A is highly correlated with the reference test
#the first 100 did  poorly in the test, and the second 100 did fine
#there is a normal distrubution, and a min/max of 0/20 respectively
#it's educationally biased - people with less than four years of education did worse in the test, regardless of their diagnosis
testA <- c(round(rnorm(100, mean=7, sd=5)),round(rnorm(100, mean=13, sd=5)))#set up data
testA[which(education<4)] <- testA[which(education<4)]-8 #introduce penalty for those with <4 years education
testA <- ifelse(testA > 20, 20, testA)#pin max
testA <- ifelse(testA < 0, 0, testA)#pin min
hist(testA)#inspect vector on it's own

#index test B is weakly correlated with the reference test
#the first 100 did slighlty less well in the test, and the second 100 did slighly better
#there is a normal distrubution, and a min/max of 0/20 respectively
testB <- c(round(rnorm(100, mean=9, sd=5)),round(rnorm(100, mean=11, sd=5)))#set up data
testB <- ifelse(testB > 20, 20, testB)#pin max
testB <- ifelse(testB < 0, 0, testB)#pin min
hist(testB)#inspect vector on it's own

#index test C has a problem. Lots of people refused to do it
#turns out it was the people with visual impairment, and other than that, it's the same as testB
testC <- c(round(rnorm(100, mean=9, sd=5)),round(rnorm(100, mean=11, sd=5)))#set up data
testC <- ifelse(testC > 20, 20, testC)#pin max
testC <- ifelse(testC < 0, 0, testC)#pin mix
testC[which(visual_imp=='y')]<-0#sets the test score to 0 where visual impairment is present
hist(testC)#inspect vector on it's own

#insert some impossible data
depression[40]<-20#max score is 15
testA[20]<-50


#------------------------------
#combine into a dataframe - or tibble, which is similar
df <- tibble(
  diagnosis = diagnosis,
  testA = testA,
  testB = testB,
  testC = testC,
  depression = depression,
  age = age,
  gender = gender,
  education = education,
  visual_imp,
  fav_fruit = fav_fruit)

#-----------------------------
#inspect variables by condition
#testA by diagnosis
g_testA_by_diagnosis <- ggplot(df, aes(x = testA, colour = diagnosis)) + 
  geom_density()
g_testA_by_diagnosis

#testB by diagnosis
g_testB_by_diagnosis <- ggplot(df, aes(x = testB, colour = diagnosis)) + 
  geom_density()
g_testB_by_diagnosis

#testB by diagnosis
g_testC_by_diagnosis <- ggplot(df, aes(x = testC, colour = diagnosis)) + 
  geom_density()
g_testC_by_diagnosis

#depression by diagnosis
g_depression_by_diagnosis <- ggplot(df, aes(x = depression, colour = diagnosis)) + 
  geom_density()
g_depression_by_diagnosis

#-----------------------------
#output data
write.csv(df, "dummy_data.csv")
