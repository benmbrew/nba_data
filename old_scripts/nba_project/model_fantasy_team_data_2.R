# this script will read in fantasy team data and run models for individual players
library(tidyverse)
library(caret)
library(glmnet)

# read in data
dat <- readRDS( '../data/cleaned_data/model_fan_team_data.rda')

# remove variables that aren't known until after a game (unless an outcome variable)
dat$f_minutes <- dat$f_position <- dat$t_to_to_player_team <- dat$t_to_to_opp_team <-
  dat$t_bl_player_team <- dat$t_bl_opp_team <- dat$t_pts_player_team <- dat$t_pts_opp_team <-
  dat$t_win_loss_player_team <- dat$t_win_loss_opp_team <- dat$t_real_spread_opp_team <- 
  dat$t_real_spread_player_team <- dat$counts <- dat$t_fg_per_opp_team <- 
  dat$t_fg_per_player_team <- dat$home_ind <- dat$t_ft_per_opp_team <- dat$t_ft_per_player_team <-
  dat$t_three_per_opp_team <- dat$t_three_per_player_team <- dat$starter_ind <-
  dat$t_closing_spread_opp_team <- dat$t_closing_total_opp_team <- 
  dat$t_moneyline_opp_team <- NULL

# convert to factor
dat$player_starting <- as.factor(dat$player_starting)
dat$t_rest_days_player_team <- as.factor(dat$t_rest_days_player_team)
dat$t_rest_days_opp_team <- as.factor(dat$t_rest_days_opp_team)
dat$f_player <- as.factor(dat$f_player)
dat$f_team <- as.factor(dat$f_team)
dat$f_opp_team <- as.factor(dat$f_opp_team)
dat$f_venue <- as.factor(dat$f_venue)
dat$year <- as.factor(dat$year)
dat$month <- as.factor(dat$month)

# remove NAs
dat <- dat[complete.cases(dat),]

# create a function that runs models - make arugments for what type of features to include
temp_dat <- dat
y_variable <- 'fan_points'
split <- 0.7
include_refs <- F
include_teammates <- F
include_opp_teammates <- F
include_team_name <- F
include_opp_name <- F
only_mov_avg <- T
i = 2

run_models <- function(temp_dat, 
                       model_type,
                       y_variable,
                       split,
                       include_refs, 
                       include_teammates, 
                       include_opp_teammates,
                       include_team_name, 
                       include_opp_name,
                       only_mov_avg){
  
  if(!include_refs){
    temp_dat$main_ref <- temp_dat$t_crew_1 <- temp_dat$t_crew_2 <- NULL
  }
  if(!include_teammates){
    temp_dat$t_starter_1_player_team <- temp_dat$t_starter_2_player_team <-
      temp_dat$t_starter_3_player_team <-  temp_dat$t_starter_4_player_team <- 
      temp_dat$t_starter_5_player_team <- NULL

  }
  if(!include_opp_teammates){
    temp_dat$t_starter_1_opp_team <- temp_dat$t_starter_2_opp_team <-
      temp_dat$t_starter_3_opp_team <-  temp_dat$t_starter_4_opp_team <- 
      temp_dat$t_starter_5_opp_team <- NULL
  }
  if(!include_team_name){
    temp_dat$f_team <- NULL
  }
  if(!include_opp_name){
    temp_dat$f_opp_team<- NULL
  }
  
  if(only_mov_avg){
    temp_dat <- temp_dat[,!grepl('sum_', names(temp_dat))]
  }
  
  # loop through each player and assign a fold per game
  unique_players <- unique(temp_dat$f_player)
  model_results <- list()
  importance_results <- list()
  
  for(i in 1:length(unique_players)){
    this_player <- unique_players[i]
    sub_player <- temp_dat[temp_dat$f_player == this_player,]
    #create folds 
    sub_player <- sub_player[order(sub_player$f_date),]
    sub_player$folds <- seq(1, nrow(sub_player), 1)
    # get train and test window
    train_window <- 1:(nrow(sub_player)*split)
    test_window <- (length(train_window) + 1):nrow(sub_player)
    # get training and test data
    train_x <- sub_player %>% filter(folds %in% train_window)
    test_x <- sub_player %>% filter(folds %in% test_window)
    
    # store train_y and test_y and remove fold.
    if(y_variable == 'usage_rate'){
      train_y <- train_x$f_usage_rate
      train_x$f_usage_rate <- NULL
      train_x$f_fan_points <- NULL
      
      test_y <- test_x$f_usage_rate
      test_x$f_usage_rate <- NULL
      test_x$f_fan_points <- NULL
    }
    if(y_variable == 'fan_points'){
      train_y <- train_x$f_fan_points
      train_x$f_usage_rate <- NULL
      train_x$f_fan_points <- NULL
      
      test_y <- test_x$f_fan_points
      test_x$f_usage_rate <- NULL
      test_x$f_fan_points <- NULL
    }
    
    # create time slices
    # remove folds
    train_x$folds <- NULL
    test_x$folds <- NULL
    
    # remove dates
    train_x$f_date <- test_x$f_date <- NULL
    
    # store player name and remove
    train_x$f_player <- test_x$f_player <- NULL 
    
    # set initial window based on train and test
    horizon_window <- floor(nrow(train_x)/5)
    fixed_window <- FALSE
    initial_window <- nrow(train_x) - horizon_window
    
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
                     , metric = 'RMSE'
                     , method = "rf"
                     , trControl = fitControl
                     , tuneGrid = tunegrid
                     , importance = T
                     , verbose = FALSE)
      
      
        temp <- varImp(model)[[1]]
        importance <- cbind(variable = rownames(temp), score = temp$Overall)
        importance <- as.data.frame(importance)
        importance$score <- round(as.numeric(as.character(importance$score)), 2)
        importance <- importance %>% arrange(-score)
        
      
      
      # Predictions on test data
      
      # This returns 100 prediction with 1-100 lambdas
      test.predictions <- predict(model,
                                  newdata = test_x)
      
      
      # combine predictions and real labels
      temp_results <- as.data.frame(cbind(predicted = test.predictions, 
                                      real = test_y))
      temp_results$player <- this_player
      
     
    }
    
    # store in list
    model_results[[i]] <- temp_results
    importance_results[[i]] <- importance
    message('finished ', this_player)
  }
  final_results <- do.call('rbind', model_results)
  final_importance <- do.call('rbind', importance_results)
  return(list(final_results, final_importance))
}

model_type = 'random_forest'
y_variable = 'fan_points'
split = 0.7
include_refs = FALSE
include_teammates = FALSE 
include_opp_teammates = FALSE 
include_team_name = FALSE 
include_opp_name = FALSE 
only_mov_avg = TRUE

temp <- run_models(temp_dat = dat, 
                   model_type = model_type,
                   y_variable = y_variable,
                   split = split,
                   include_refs = include_refs, 
                   include_teammates = include_teammates, 
                   include_opp_teammates = include_opp_teammates, 
                   include_team_name = include_team_name, 
                   include_opp_name = include_opp_name, 
                   only_mov_avg = only_mov_avg)

importance <- temp[[2]]
temp_result <- temp[[1]]
temp_importance <- importance %>%
  group_by(variable) %>%
  summarise(mean_score = mean(score))

# # loop through each player and 
# t <- temp %>% group_by(player) %>%
#   summarise(correlation = cor(predicted, real),
#             counts = n())

# save model
saveRDS(temp, paste0('../data/', model_type, '_', y_variable, '_', split, '_',
                     include_refs, '_', include_teammates, '_', include_opp_teammates,
                     '_', include_team_name, '_', include_opp_name, '_', only_mov_avg))

library(ggthemes)
ggplot(temp_result, aes(predicted, real)) + 
  geom_point(size = 2, alpha = 0.1, color = 'black') +
  geom_smooth(method = 'lm') +
  labs(x = 'Predicted Fantasy Points', 
       y = 'Real Fantasy Points') +
  xlim(c(0,100)) + ylim(c(0,100)) +
  theme_classic()


