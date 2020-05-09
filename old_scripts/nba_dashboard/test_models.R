# load functions
library(randomForest)
library(caret)
library(tidyverse)
library(preprocessCore)
library(lubridate)
library(MASS)
library(ggthemes)

source('../functions.R')
doParallel::registerDoParallel(3)

# get data parameters for reading in models
response_variable = 'real_spread'
include_ou = 'with_ou'
include_spread = 'with_spread'
include_team = 'with_team'
include_opp = 'with_opp'
model_type = 'enet'
fixed_window = 'window_fixed'
lm_aic = 'no_aic'
date = 'jan1'

# get saving var
temp1 <- readRDS(paste0('../results/', model_type, '_',response_variable, '_',
                        include_team, '_', include_opp, '_', include_ou, '_', include_spread, '_',
                        fixed_window, '_', lm_aic, '_',date,'.rda'))

response_var = 'real_total'

temp1_tot <- readRDS(paste0('../results/', model_type, '_',response_var, '_',
                            include_team, '_', include_opp, '_', include_ou, '_', include_spread, '_',
                            fixed_window, '_', lm_aic, '_',date,'.rda'))

date = 'jan2'
include_spread = 'no_spread'

temp2 <- readRDS(paste0('../results/', model_type, '_',response_variable, '_',
                        include_team, '_', include_opp, '_', include_ou, '_', include_spread, '_',
                        fixed_window, '_', lm_aic, '_',date,'.rda'))

# examine results of both models 
temp1 <- temp1[complete.cases(temp1),]
temp1_tot <- temp1_tot[complete.cases(temp1_tot),]
temp2 <- temp2[complete.cases(temp2),]

temp2$actual_diff <- as.numeric(as.character(temp2$actual_diff))

# MULTIPLY real_spread by negative 1.
temp1$real_spread <- (temp1$real_spread)*(-1)
temp1_tot$real_spread <- (temp1_tot$real_spread)*(-1)
temp2$real_spread <- (temp2$real_spread)*(-1)

# create correlation plot
temp_dat <- temp1
x_var = 'predictions'
x_var = 'real_y'
cor_plot <- function(temp_dat, 
                     x_var, 
                     y_var){

  temp_dat <- temp_dat[, c(x_var, y_var)]
  names(temp_dat)[1:2] <- c('V1', 'V2')
  cor_num <- cor(temp_dat$V1, temp_dat$V2)
  col_vec<- c('blue', 'red')
  p <-ggplot(temp_dat, aes(V1, V2)) +
    geom_point(size = 1.5,alpha = 0.6) +
    geom_smooth(method = 'lm',
                color = c('black', 'red')) +
    labs(x = x_var,, 
         y = y_var,
         title = paste0('Correlation = ', cor_num)) 
  return(p)
}


str(temp1)
cor_plot(temp1, x_var = 'predicted', y_var = 'real_y')
cor_plot(temp1, x_var = 'predicted', y_var = 'real_spread')
cor_plot(temp1, x_var = 'real_spread', y_var = 'real_y')

cor_plot(temp2, x_var = 'predicted', y_var = 'real_y')
cor_plot(temp2, x_var = 'predicted', y_var = 'real_spread')
cor_plot(temp2, x_var = 'real_spread', y_var = 'real_y')


# determine money i would make if going with model, assuming 40$ bets
# use temp1st



#######################################################################
# get saving var
enet_model2 <- readRDS( paste0('../trained_models/trained_', model_type, '_','real_spread', '_',
                               include_team, '_', include_opp, '_', include_ou, '_', include_spread, '_',
                               fixed_window, '_', lm_aic, '_',date,'.rda'))

date = 'jan1'
# get saving var
enet_model1 <- readRDS( paste0('../trained_models/trained_', 'enet', '_','real_spread', '_',
                               include_team, '_', include_opp, '_', include_ou, '_', 'with_spread', '_',
                               fixed_window, '_', lm_aic, '_',date,'.rda'))

# get saving var
enet_model_tot <- readRDS( paste0('../trained_models/trained_', 'enet', '_','real_total', '_',
                                  include_team, '_', include_opp, '_', include_ou, '_', 'with_spread', '_',
                                  fixed_window, '_', lm_aic, '_',date,'.rda'))


test_dat <- readRDS('../data/model_dat.rda')
test_dat <- get_by_game(test_dat)


# replace .x with home team and .y with away
names(test_dat) <- gsub('_home', '_one', names(test_dat))
names(test_dat) <- gsub('_away', '_two', names(test_dat))

# real_total <- test_dat$real_total_one
# real_spread <- test_dat$real_spread_one

test_dat$real_total_one <-test_dat$real_total_two <- 
  test_dat$real_spread_one <- test_dat$real_spread_two <- NULL

if(include_team != 'with_team'){
  test_dat$teams_one <- NULL
}
if(include_opp != 'with_opp'){
  test_dat$teams_two <- NULL
}
if(include_ou != 'with_ou'){
  test_dat$closing_total_one <- test_dat$closing_total_two <- NULL
  
}

if(include_spread != 'with_spread'){
  test_dat_1 <- test_dat
  test_dat$closing_spread_one <- test_dat$closing_spread_two <- NULL
}

# delete other columns
test_dat$win_ind_team_one <- test_dat$win_ind_team_two <- test_dat$win_loss_one <-
  test_dat$win_loss_two <- test_dat$fold_one <- test_dat$fold_two <- test_dat$dataset_one <-
  test_dat$dataset_two  <- test_dat$date_two <- test_dat$f_one <-
  test_dat$f_two <- test_dat$tot_game_num_one <- test_dat$tot_game_num_two <-
  test_dat$venue_two <- test_dat$month_two<- NULL

# delete other columns
test_dat_1$win_ind_team_one <- test_dat_1$win_ind_team_two <- test_dat_1$win_loss_one <-
  test_dat_1$win_loss_two <- test_dat_1$fold_one <- test_dat_1$fold_two <- test_dat_1$dataset_one <-
  test_dat_1$dataset_two  <- test_dat_1$date_two <- test_dat_1$f_one <-
  test_dat_1$f_two <- test_dat_1$tot_game_num_one <- test_dat_1$tot_game_num_two <-
  test_dat_1$venue_two <- test_dat_1$month_two<- NULL

# convert characters to factors
test_dat$venue_one <- as.factor(test_dat$venue_one)
test_dat$teams_one <- as.factor(test_dat$teams_one)
test_dat$teams_two <- as.factor(test_dat$teams_two)

test_temp <- test_dat[test_dat$date_one == '2019-01-02',]
test_temp$date_one <- NULL

# convert characters to factors
test_dat_1$venue_one <- as.factor(test_dat_1$venue_one)
test_dat_1$teams_one <- as.factor(test_dat_1$teams_one)
test_dat_1$teams_two <- as.factor(test_dat_1$teams_two)

test_temp_1 <- test_dat_1[test_dat_1$date_one == '2019-01-02',]
test_temp_1$date_one <- NULL
# predict on test dat (need to remove outcome and any variable not used in training)

# predict real diff
preds1 <- predict(enet_model1,
                  model.matrix(~.,test_temp_1))
preds2 <- predict(enet_model2,
                  model.matrix(~.,test_temp))
preds_tot <- predict(enet_model_tot,
                     model.matrix(~.,test_temp_1))


# real_spread <- real_spread[(length(real_spread) - 8):length(real_spread)]
# real_total <- real_total[(length(real_total) - 8):length(real_total)]


final_preds <- as.data.frame(cbind(team_1 = as.character(test_temp$teams_one), 
                                   team_2 = as.character(test_temp$teams_two),
                                   closing_total = as.character(test_temp_1$closing_total_one),
                                   preds_tot = preds_tot,
                                   closing_spread = as.character(test_temp_1$closing_spread_one),
                                   preds1 = preds1,
                                   preds2 = preds2))
final_preds$preds1 <- round(as.numeric(as.character(final_preds$preds1)),2)
final_preds$preds2 <- round(as.numeric(as.character(final_preds$preds2)),2)
final_preds$preds_tot <- round(as.numeric(as.character(final_preds$preds_tot)),2)
final_preds$closing_total <- round(as.numeric(as.character(final_preds$closing_total)),2)
final_preds$closing_spread <- round(as.numeric(as.character(final_preds$closing_spread)),2)

