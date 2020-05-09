

##########
# LOAD LIBRARIES and read in data
##########
library(broom)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(doParallel)
library(caret)
library(glmnet)
registerDoParallel()
# source functions script 
source('functions.R')

data_dir <- '../Data/cleaned_data'

if('by_game.RData' %in% dir(data_dir)) {
  load('../Data/cleaned_data/by_game.RData')
} else {
  source('read_in.R')
}


temp_dat <- dat_game 
model_feats <- c('real_total_home', 'opening_total_home', 'date_home')
model_feats <- c('real_total_home', 'opening_total_home', 'teams_home', 
                 'teams_away', 'opening_spread_home', 'year', 'date_home',
                 'moneyline_home', 'month', 'home_team_favored')
model_name <- 'rf'
number_preds <- 173

get_classifier <- function(temp_dat, 
                           initial_window,
                           model_name,
                           model_feats, 
                           number_preds) {
  
  
  temp_dat <- temp_dat[, model_feats]
  initial_window <- test_start 
  
  # get complete data 
  temp_dat <- temp_dat[complete.cases(temp_dat),]
  
  
  # get time slice object
  time_slices <- createTimeSlices(1:nrow(temp_dat), 
                                  initialWindow = initial_window, 
                                  horizon = number_preds, 
                                  fixedWindow = FALSE)
  
  train_slices <- time_slices[[1]]
  test_slices <- time_slices[[2]]
  
  results_list <- list()
  
  #remove date
  temp_dat$date_home <- NULL
  
  
  training_dat <- temp_dat[unlist(train_slices),]
  test_dat <- temp_dat[unlist(test_slices),]
  
  if(model_name == 'rf') {
    
    training_dat$train_y <- as.factor(ifelse(training_dat$real_total_home >= training_dat$opening_total_home,
                                             'yes', 'no'))
    
    test_dat$test_y <- as.factor(ifelse(test_dat$real_total_home >= test_dat$opening_total_home,
                                        'yes', 'no'))
    
    training_dat$real_total_home <- test_dat$real_total_home <- NULL
    # determines how you train the model.
    
    fitControl <- trainControl(method = "timeslice",
                               initialWindow = 1000, 
                               horizon = 100,
                               classProbs = TRUE,
                               allowParallel = TRUE,
                               summaryFunction = twoClassSummary,
                               fixedWindow = FALSE)
    
    mtry <- sqrt(ncol(training_dat[,colnames(training_dat)]))
    tunegrid <- expand.grid(.mtry=mtry)
    
    train_time <- train(train_y ~.,
                        data = training_dat,
                        metric = 'ROC',
                        method = model_name,
                        trControl = fitControl,
                        tuneGrid = tunegrid,
                        importance = T,
                        verbose = TRUE)
    
    temp <- varImp(train_time)[[1]]
    importance <- as.data.frame(cbind(rownames(temp), temp$yes))
    
    true <- test_dat$test_y
    test_dat$test_y <- NULL
    
    pred <- as.numeric(as.character(predict(train_time, test_dat, type = 'prob')$yes))
    pred_fac <- ifelse(pred >= 0.5, 'yes', 'no')
    mkt <- test_dat$opening_total_home
    
    results_list <- as.data.frame(cbind(pred = pred,pred_fac= pred_fac, mkt = mkt, true = true))
    
  }
  return(list(results_list, importance))
}
