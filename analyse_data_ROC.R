##The Limekilns Cognition Study- an R tutorial 
#analyse the data - regression
#© copyright of Lachlan Fotheringham
#This file is part of The Limekilns Cognition Study- an R tutorial
#The Limekilns Cognition Study- an R tutorial is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 3
#The Limekilns Cognition Study- an R tutorial is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#You should have received a copy of the GNU General Public License along with this file. If not, see <https://www.gnu.org/licenses/>.

#initialise workspace
rm(list = ls());
cat("\14");
setwd("/Users/lfoth/Documents/R/tutorial")#set to whatever your working directory is - probably easiest to have it wherever you have put the .R file

#load fav libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(plotROC)
library(cutpointr)

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

#ROC curves prefer 0 and 1 in general
df %<>%
  mutate(diagnosis = case_when(
    diagnosis == 'yes' ~ 1,
    diagnosis == 'no' ~ 0
  ))

#custom functions
get_ROC_output <- function(df, test_names = c("testA", "testB", "testC")){
  
  get_stats_for_single_test <- function(var){
    #states for testing
    #var = "testA"
    
    cat("getting stats for ...", var)
    
    tmp <- tibble(
      diagnosis = df$diagnosis,
      test_score = df[[var]]
    )%>%
      drop_na()
    
    get_p_value_by_MWU <- function(tmp){
      p_value <- wilcox.test(tmp$test_score ~ tmp$diagnosis)$p.value
    }
    
    
    opt_cut <- cutpointr(tmp, test_score, diagnosis,
                         direction = "<=",
                         pos_class = 1,
                         neg_class = 0, method = maximize_metric, metric = youden,
                         boot_runs = 100)
    
    CIs <- quantile(opt_cut$boot[[1]]$AUC_b, probs = c(0.05, 0.95))
    
    p_value <-get_p_value_by_MWU(tmp) 
    
    opt_cut%<>%
      select(optimal_cutpoint, sensitivity, specificity, youden, AUC)%>%
      bind_cols(CIs[1], CIs[2], p_value)
    
    names(opt_cut) <- c("optimal_cutpoint", "sensitivity", "specificity", "youden", "AUC", "AUC_lower_CI", "AUC_upper_CI", "p_value")
    return(opt_cut)
  }
  
  #this function for all tests, then output as a tibble
  test_names%>%
    as_tibble()%>%
    bind_cols(do.call(bind_rows, lapply(test_names, get_stats_for_single_test)))%>%
    return()
}

#-----------------
#plot some ROC curves

#need to make df long to combine roc curves in the one plot
ROC_data <- df %>%
  select(diagnosis, testA, testB, testC)%>%
  pivot_longer(!diagnosis, names_to = "test", values_to = "score")

testA_ROC <- ggplot(ROC_data, aes(d = diagnosis, m = score, color = test)) + 
  geom_roc()+
  scale_y_reverse()+ #need to flip the test data as it's a low score that's bad rather than a high one
  scale_x_reverse()+
  labs(title = "ROC curve - tests A,B & C to predict <diagnosis>")+
  xlab("False positive rate (1-specificity)")+
  ylab("True positive rate (sensitivity)")
testA_ROC

#lets have some data to support the ROC curves
#this is a function I wrote a while ago to get all the stats you need
ROC_output <- get_ROC_output(df)
ROC_output
