
# load functions
library(randomForest)
library(caret)
library(tidyverse)
library(preprocessCore)
library(lubridate)
library(MASS)

doParallel::registerDoParallel(3)

source('../global.R')

# keep only the variables necessary for modeling (lagged, mov_avg, cum_sum)
# and the outcomes for modeling (W/L, f, O/U, real_total)

mod_dat <- dat_game[, c(1, 2, 3,52, 13, 53, 54, 57,58, 55, 4,
                        60, c(70:146))]
rm(dat_game)

mod_dat <- mod_dat[order(mod_dat$date, decreasing = FALSE),]
# remove NAs first observations of each season
# mod_dat <- mod_dat[complete.cases(mod_dat),]

# remove first game of each season
mod_dat <- mod_dat[mod_dat$team_game_num != 1,]

# need to subset data for only the tot_game_num that has two observations
dup_nums <- mod_dat$tot_game_num[duplicated(mod_dat$tot_game_num)]
mod_dat <- mod_dat[mod_dat$tot_game_num %in% dup_nums,]


get_folds <- function(temp_dat){
  unique_dataset <- unique(temp_dat$dataset)
  data_list <- list()
  for (i in 1:length(unique_dataset)){
    this_dataset <- unique_dataset[i]
    sub_dat <- temp_dat[temp_dat$dataset == this_dataset,]
    if(this_dataset == '2014_2015_season'){
      sub_dat$fold <- ifelse(sub_dat$month == 10, 1, 
                             ifelse(sub_dat$month == 11, 2, 
                                    ifelse(sub_dat$month == 12, 3, 
                                           ifelse(sub_dat$month == 1, 4, 
                                                  ifelse(sub_dat$month == 2, 5, 
                                                         ifelse(sub_dat$month == 3, 5, 6))))))
    } else if(this_dataset == '2015_2016_season') {
      sub_dat$fold <- ifelse(sub_dat$month == 10, 7, 
                             ifelse(sub_dat$month == 11, 8, 
                                    ifelse(sub_dat$month == 12, 9, 
                                           ifelse(sub_dat$month == 1, 10, 
                                                  ifelse(sub_dat$month == 2, 11, 
                                                         ifelse(sub_dat$month == 3, 12, 13))))))
    }  else if(this_dataset == '2016_2017_season') {
      sub_dat$fold <- ifelse(sub_dat$month == 10, 14, 
                             ifelse(sub_dat$month == 11, 15, 
                                    ifelse(sub_dat$month == 12, 16, 
                                           ifelse(sub_dat$month == 1, 17, 
                                                  ifelse(sub_dat$month == 2, 18, 
                                                         ifelse(sub_dat$month == 3, 19, 20))))))
    }  else if(this_dataset == '2017_2018_season') {
      sub_dat$fold <- ifelse(sub_dat$month == 10, 21, 
                             ifelse(sub_dat$month == 11, 22, 
                                    ifelse(sub_dat$month == 12, 23, 
                                           ifelse(sub_dat$month == 1, 24, 
                                                  ifelse(sub_dat$month == 2, 25, 
                                                         ifelse(sub_dat$month == 3, 26, 27))))))
    } else {
      sub_dat$fold <- ifelse(sub_dat$month == 10, 28, 
                             ifelse(sub_dat$month == 11, 29, 
                                    ifelse(sub_dat$month == 12, 30, 31)))
    }
  
    data_list[[i]] <- sub_dat
  }
  
  final_dat <- do.call('rbind', data_list)
  
  return(final_dat)
}  

mod_dat$month <- month(as.POSIXlt(mod_dat$date, format == '%Y-%m-%d'))

mod_dat <- get_folds(mod_dat)

# # extract test set 
test_dat <- mod_dat[mod_dat$fold == 31,]
# mod_dat <- mod_dat[mod_dat$fold != 29,]
# 
# saveRDS(levels(as.factor(mod_dat$teams)), '../data/team_levels.rda')
# # get folds for each week month 
saveRDS(test_dat,'../data/test_dat.rda')
saveRDS(mod_dat,'../data/model_dat.rda')

# create classification outcome. 

# spread 
mod_dat$closing_spread <- mod_dat$closing_spread*(-1)
mod_dat$real_spread_fac <- ifelse(mod_dat$real_spread >= mod_dat$closing_spread, 'positive', 'negative')
mod_dat$real_spread_fac <- factor(mod_dat$real_spread_fac, levels = c('positive', 'negative'))

# total 
mod_dat$real_total_fac <- ifelse(mod_dat$real_total >= mod_dat$closing_total, 'positive', 'negative')
mod_dat$real_total_fac <- factor(mod_dat$real_total_fac, levels = c('positive', 'negative'))

# write.csv(mod_dat, '~/Desktop/temp_tail.csv')

model_matrix = mod_dat
train_window = c(1:28)
test_window =c(30:31)
response_variable = 'real_spread_fac'
include_ou = TRUE
include_spread = FALSE
include_team = TRUE
include_opp = FALSE
model_type = 'elastic_net'
importance_fac = FALSE
fixed_window = TRUE
initial_window = 5000
horizon_window = 100
lm_aic = FALSE

# make sure characters are factors
pred_team_regression <- function(model_matrix,
                                 train_window,
                                 test_window,
                                 response_variable,
                                 include_ou,
                                 include_spread,
                                 include_team,
                                 include_opp,
                                 model_type,
                                 importance_fac,
                                 fixed_window,
                                 initial_window,
                                 horizon_window,
                                 lm_aic) {
  
  # get traning data
  
  # get complete cases
  # model_matrix <- model_matrix[complete.cases(model_matrix),]
    # get training and test data
  train_x <- model_matrix %>% filter(fold %in% train_window)
  test_x <- model_matrix %>% filter(fold %in% test_window)
  
  if(include_team){
    # Assure they have the same levels for team and opponent
    # get intersecting teams and opponents so train and test data have same variables
    shared_teams <- intersect(train_x$teams, test_x$teams)
    
    # subset by shared teams
    train_x <- train_x %>% filter(teams %in% shared_teams)
    test_x <- test_x %>% filter(teams %in% shared_teams)
    
    # get opponenets 
    game_number_train <- unique(train_x$tot_game_num)
    game_number_test <- unique(test_x$tot_game_num)
    
    # use get by game function to get opponents
    train_x <- get_by_game(train_x)
    test_x <- get_by_game(test_x)
    
    # replace .x with home team and .y with away
    names(train_x) <- gsub('_home', '_one', names(train_x))
    names(train_x) <- gsub('_away', '_two', names(train_x))
    names(test_x) <- gsub('_home', '_one', names(test_x))
    names(test_x) <- gsub('_away', '_two', names(test_x))
    
    
    # # subset train_x by duplcated tot game numbers
    # dup_nums <- train_x$tot_game_num_one[duplicated(train_x$tot_game_num_one)]
    # train_x <- train_x[train_x$tot_game_num %in% dup_nums,]
    # 
    
    # condition to stop model if not met
    stopifnot(all(sort(unique(train_x$teams_one)) == sort(unique(test_x$one))))
    
    
  }
  if(!include_opp){
    
   train_x$teams_two <- NULL
   test_x$teams_two <- NULL
   
    
  } else {
    test_x$teams_two <- as.factor(test_x$teams_two)
    train_x$teams_two <- as.factor(train_x$teams_two)
    
  }
  
  
  # get training and test outcome
  train_y <- train_x[, paste0(response_variable, '_one')]
  train_x[, paste0(response_variable, '_one')] <- NULL
  train_x[, paste0(response_variable, '_two')] <- NULL
  
  train_x$real_spread_one <- train_x$real_spread_two <- NULL
  train_x$real_spread_fac_one <- train_x$real_spread_fac_two <- NULL
  
  train_x$real_total_one <- train_x$real_total_two <- NULL
  train_x$real_total_fac_one <- train_x$real_total_fac_two <- NULL
  
  
  train_x$win_ind_team_one <- train_x$win_ind_team_two <- train_x$win_loss_one <-
    train_x$win_loss_two <- train_x$fold_one <- train_x$fold_two <- train_x$dataset_one <-
    train_x$dataset_two <- train_x$date_one <- train_x$date_two <- train_x$f_one <-
    train_x$f_two <- train_x$tot_game_num_one <- train_x$tot_game_num_two <-
    train_x$venue_two <- train_x$month_two<- NULL
  
  if(!include_ou){
    train_x$closing_total_one <- train_x$closing_total_two <- NULL
  }
  
  if(!include_spread){
    train_x$closing_spread_one <- train_x$closing_spread_two <- NULL
  }
  
  
  # get closing spread and over under as well as real spread and over under
  test_spread <- test_x$closing_spread_one
  test_ou <- test_x$closing_total_one
  test_real_spread <- test_x$real_spread_one
  test_real_ou <- test_x$real_total_one
  test_teams_one <- test_x$teams_one
  test_teams_two <- test_x$teams_two

  # get training and test outcome
  test_y <- test_x[, paste0(response_variable, '_one')]
  test_x[, paste0(response_variable, '_one')] <- NULL
  test_x[, paste0(response_variable, '_two')] <- NULL
  
  test_x$real_spread_one <- test_x$real_spread_two <- NULL
  test_x$real_spread_fac_one <- test_x$real_spread_fac_two <- NULL
  
  test_x$real_total_one <- test_x$real_total_two <- NULL
  test_x$real_total_fac_one <- test_x$real_total_fac_two <- NULL
  
  
  test_x$win_ind_team_one <- test_x$win_ind_team_two <- test_x$win_loss_one <-
    test_x$win_loss_two <- test_x$fold_one <- test_x$fold_two <- test_x$dataset_one <-
    test_x$dataset_two <- test_x$date_one <- test_x$date_two <- test_x$f_one <-
    test_x$f_two <- test_x$tot_game_num_one <- test_x$tot_game_num_two <-
    test_x$venue_two <- test_x$month_two<- NULL
  
  if(!include_ou){
    test_x$closing_total_one <- test_x$closing_total_two <- NULL
  }
  
  if(!include_spread){
    test_x$closing_spread_one <- test_x$closing_spread_two <- NULL
  }
  
 
  # convert characters to factors
  train_x$venue_one <- as.factor(train_x$venue_one)
  train_x$teams_one <- as.factor(train_x$teams_one)

  # test x
  test_x$venue_one <- as.factor(test_x$venue_one)
  test_x$teams_one <- as.factor(test_x$teams_one)
 
  
  # models
  if(model_type == 'random_forest') {
    # determines how you train the model.
    fitControl <- trainControl(
      method = 'timeslice',  
      initialWindow = initial_window,
      fixedWindow = fixed_window,
      horizon = horizon_window,
      allowParallel = TRUE
    )
    
    
    # mtry: Number of variables randomly sampled as candidates at each split.
    # ntree: Number of trees to grow.
    mtry <- sqrt(ncol(train_x[,colnames(train_x)]))
    tunegrid <- expand.grid(.mtry=mtry)
    
    model <- train(x = train_x
                   , y = train_y
                   , metric = 'roc'
                   , method = "rf"
                   , trControl = fitControl
                   , tuneGrid = tunegrid
                   , importance = T
                   , verbose = FALSE)
    
    if(importance_fac){
      
      temp <- varImp(model)[[1]]
      importance <- cbind(variable = rownames(temp), W = temp$W, L = temp$L)
      importance <- as.data.frame(importance)
      importance$W <- round(as.numeric(as.character(importance$W)), 2)
      importance$L <- round(as.numeric(as.character(importance$W)), 2)
      
      importance <- importance %>% arrange(-W)
    } else {
      temp <- varImp(model)[[1]]
      importance <- cbind(variable = rownames(temp), score = temp$Overall)
      importance <- as.data.frame(importance)
      importance$score <- round(as.numeric(as.character(importance$score)), 2)
      importance <- importance %>% arrange(-score)
      
    }
    
    # Predictions on test data
    
    # This returns 100 prediction with 1-100 lambdas
    test.predictions <- predict(model,
                                newdata = test_x)
    
    
    
  
    # combine predictions and real labels
    temp_dat <- as.data.frame(cbind(predicted = as.character(test.predictions), 
                                    real_y = as.character(test_y),
                                    real_spread = test_spread,
                                    real_ou = test_ou,
                                    actual_diff = test_real_spread,
                                    actual_total = test_real_ou,
                                    teams_one = test_teams_one,
                                    teams_two = test_teams_two))
    temp_dat$predicted <- round(as.numeric(as.character(temp_dat$predicted)), 2)
    temp_dat$real_y <- round(as.numeric(as.character(temp_dat$real_y)), 2)
    temp_dat$real_spread <- round(as.numeric(as.character(temp_dat$real_spread)), 2)
    temp_dat$real_ou <- round(as.numeric(as.character(temp_dat$real_ou)), 2)
    temp_dat$actual_diff <- round(as.numeric(as.character(temp_dat$actual_diff)), 2)
    temp_dat$actual_total <- round(as.numeric(as.character(temp_dat$actual_total)), 2)
    
    
    return(list(temp_dat, importance,  model))
  }
  
  if(model_type == 'lm') {
    # recombine train_x and train_y
    lm_data <- as.data.frame(cbind(train_y, train_x))
    
    # run model
    lm_mod <- lm(train_y~., data = lm_data)
    
    if(lm_aic){
      lm_mod <- stepAIC(lm_mod, 
                        direction = "both",
                        trace = FALSE)
    }
    
    lm_preds <- predict(lm_mod, newdata = test_x)
    
    temp_dat <- as.data.frame(cbind(predicted = lm_preds, 
                                    real_y = test_y ,
                                    real_spread = test_spread,
                                    real_ou = test_ou,
                                    actual_diff = test_real_spread,
                                    actual_total = test_real_ou,
                                    teams_one = test_teams_one,
                                    teams_two = test_teams_two))
    temp_dat$predicted <- round(as.numeric(as.character(temp_dat$predicted)), 2)
    temp_dat$real_y <- round(as.numeric(as.character(temp_dat$real_y)), 2)
    temp_dat$real_spread <- round(as.numeric(as.character(temp_dat$real_spread)), 2)
    temp_dat$real_ou <- round(as.numeric(as.character(temp_dat$real_ou)), 2)
    temp_dat$actuall_diff <- round(as.numeric(as.character(temp_dat$actual_diff)), 2)
    temp_dat$actual_total <- round(as.numeric(as.character(temp_dat$actual_total)), 2)
    
    return(list(temp_dat, lm_mod))
    
    
  }
  
  if(model_type == 'elastic_net'){
    train_x$venue_one <- NULL
    test_x$venue_one <- NULL
    lambda.grid <- seq(0, 100)
    alpha.grid <- seq(0, 0.9, length = 10)
    
    trnCtrl = trainControl(
      method = 'timeslice',  
      initialWindow = initial_window,
      fixedWindow = fixed_window,
      horizon = horizon_window,
      allowParallel = TRUE)
    
    srchGrd = expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)
    
    trained_glmnet <- train(x = model.matrix(~., train_x),
                            y = train_y,
                            method = "glmnet",
                            tuneGrid = srchGrd,
                            trControl = trnCtrl,
                            standardize = FALSE,
                            maxit = 1000000)
    
    
    final_model <- trained_glmnet$finalModel
    lambda_min_index <- which(final_model$lambda == min(final_model$lambda))
    
    # This returns 100 prediction with 1-100 lambdas
    temp_test.predictions <- predict(final_model,
                                     model.matrix(~.,test_x),
                                     type = 'response')
    
    
    # get predictions with corresponding lambda.
    test.predictions <- temp_test.predictions[, lambda_min_index]
    
    temp_dat <- as.data.frame(cbind(predicted = test.predictions, 
                                   real_y = test_y ,
                                   real_spread = test_spread,
                                   real_ou = test_ou,
                                   actual_diff = test_real_spread,
                                   actual_total = test_real_ou,
                                   teams_one = test_teams_one,
                                   teams_two = test_teams_two))
    temp_dat$predicted <- round(as.numeric(as.character(temp_dat$predicted)), 2)
    temp_dat$real_y <- round(as.numeric(as.character(temp_dat$real_y)), 2)
    temp_dat$real_spread <- round(as.numeric(as.character(temp_dat$real_spread)), 2)
    temp_dat$real_ou <- round(as.numeric(as.character(temp_dat$real_ou)), 2)
    temp_dat$actuall_diff <- round(as.numeric(as.character(temp_dat$actual_diff)), 2)
    temp_dat$actual_total <- round(as.numeric(as.character(temp_dat$actual_total)), 2)
    
    return(list(temp_dat, trained_glmnet))
    
    
  }
  
}

temp_final = pred_team_regression(model_matrix = mod_dat,
                                  train_window = c(1:28),
                                  test_window =c(30:31),
                                  response_variable = 'real_spread',
                                  include_ou = TRUE,
                                  include_spread = TRUE,
                                  include_team = TRUE,
                                  include_opp = FALSE,
                                  model_type = 'random_forest',
                                  importance_fac = FALSE,
                                  fixed_window = TRUE,
                                  initial_window = 5000,
                                  horizon_window = 100,
                                  lm_aic = FALSE)

temp <- temp_final[[1]]
enet_model <- temp_final[[2]]


response_variable = 'real_spread'
include_ou = 'with_ou'
include_spread = 'no_spread'
include_team = 'with_team'
include_opp = 'with_opp'
model_type = 'elastic_net'
fixed_window = 'window_fixed'
lm_aic = 'no_aic'

# get saving var
saveRDS(temp, paste0('../results/regression/', model_type, '_',response_variable, '_',
                     include_team, '_', include_opp, '_', include_ou, '_', include_spread, '_',
                     fixed_window, '_', lm_aic, 'jan2.rda'))

# get saving var
saveRDS(enet_model, paste0('../trained_models/regression/trained_', model_type, '_',response_variable, '_',
                     include_team, '_', include_opp, '_', include_ou, '_', include_spread, '_',
                     fixed_window, '_', lm_aic, 'jan2.rda'))


# temp_tot <- readRDS('~/Desktop/total_results.rds')
# temp_spread <- readRDS('~/Desktop/spread_results.rds')

temp <- temp[!is.na(temp$real_y),]

temp$real_spread <- temp$real_spread*(-1)
temp$pred_class <- as.factor(ifelse(temp$predicted <= temp$real_spread, 'team_one_lose', 'team_one_win'))
temp$real_class <- as.factor(ifelse(temp$real_y <= temp$real_spread, 'team_one_lose', 'team_one_win'))
temp$pred_class <- factor(temp$pred_class, levels = c('team_one_win', 'team_one_lose'))
temp$real_class <- factor(temp$real_class, levels = c('team_one_win', 'team_one_lose'))

caret::confusionMatrix(table(temp$pred_class, temp$real_class))
