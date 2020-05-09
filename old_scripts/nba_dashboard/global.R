##########
# LOAD LIBRARIES and read in data
##########
library(broom)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(caret)
library(glmnet)
library(pracma)

# source functions script 
source('functions.R')



# read in data
dat_2014 <- read_csv('data/team_stat_2014_15.csv')
dat_2015 <- read_csv('data/team_stat_2015_16.csv')
dat_2016 <- read_csv('data/team_stat_2016_17.csv')
dat_2017 <- read_csv('data/team_stat_2017_18.csv')
dat_current <- read_csv('data/nba-season-team-feed_june_2.csv')

##########
# recode current data to match all other dat
##########

# remove URL from names
names(dat_current) <- trimws(gsub('URL', '', names(dat_current)), which = 'both')
names(dat_current) <- trimws(gsub('\n', ' ', names(dat_current)), which = 'both')

# remove OT5 
dat_current$OT5 <- NULL

# recode team
names(dat_current)[3] <- 'TEAMS'

# remove 'TEAM' from rest days
names(dat_current) <- gsub('TEAM REST DAYS', 'REST DAYS', names(dat_current))

# recode X41 to x47 to match other dataset
names(dat_current)[names(dat_current) == 'X41'] <- 'X37'

# remove Movements from each data set that is not current
dat_2014$MOVEMENTS <- dat_2015$MOVEMENTS <- dat_2016$MOVEMENTS <-
  dat_2017$MOVEMENTS <- NULL

# make lower case 
names(dat_2014) <- tolower(names(dat_2014))
names(dat_2015) <- tolower(names(dat_2015))
names(dat_2016) <- tolower(names(dat_2016))
names(dat_2017) <- tolower(names(dat_2017))
names(dat_current) <- tolower(names(dat_current))

# replace spaces with underscore
names(dat_2014) <- gsub(' ', '_', names(dat_2014))
names(dat_2015) <- gsub(' ', '_', names(dat_2015))
names(dat_2016) <- gsub(' ', '_', names(dat_2016))
names(dat_2017) <- gsub(' ', '_', names(dat_2017))
names(dat_current) <- gsub(' ', '_', names(dat_current))


# recode 2014 data from main_referee to main_ref and crew_referees to crew
names(dat_2014) <- gsub('main_referee', 'main_ref', names(dat_2014))
names(dat_2014) <- gsub('crew_referees', 'crew', names(dat_2014))

# get season indicator
dat_2014$dataset <- '2014_2015_season'
dat_2015$dataset <- '2015_2016_season'
dat_2016$dataset <- '2016_2017_season'
dat_2017$dataset <- '2017_2018_season'
dat_current$dataset <- '2018_2019_season'

# THIS IS WHERE TEST SET IS CREATED AND ADDED TO REACENT DATA. NO NEED UNLESS MODELING.
# # create test set
# temp_test <- read_csv('../data/temp_test.csv')
# temp_test$X1 <- NULL
# 
# # combine with current
# dat_current <- rbind(dat_current,
#                      temp_test)
# 
# get game number for each dataset
dat_2014$game_number <- rep(1:(nrow(dat_2014)/2), each=2)
dat_2015$game_number <- rep(1:(nrow(dat_2015)/2), each=2)
dat_2016$game_number <- rep(1:(nrow(dat_2016)/2), each=2)
dat_2017$game_number <- rep(1:(nrow(dat_2017)/2), each=2)
dat_current$game_number <- rep(1:(nrow(dat_current)/2), each=2)

# get win loss varible
dat_2014 <- get_win_loss(dat_2014)
dat_2015 <- get_win_loss(dat_2015)
dat_2016 <- get_win_loss(dat_2016)
dat_2017 <- get_win_loss(dat_2017)
dat_current <- get_win_loss(dat_current)


# recode closing odds to creat closing spread and total in all datasets but current
dat_2014 <- get_closing_spread_ou(dat_2014)
dat_2015 <- get_closing_spread_ou(dat_2015)
dat_2016 <- get_closing_spread_ou(dat_2016)
dat_2017 <- get_closing_spread_ou(dat_2017)
dat_current$closing_spread <- dat_current$opening_spread
dat_current$closing_total <- dat_current$opening_total


# get underdog win, underdog loss, favorite win, favorite loss
dat_2014 <- get_underdog_stats(dat_2014)  
dat_2015 <- get_underdog_stats(dat_2015)  
dat_2016 <- get_underdog_stats(dat_2016)  
dat_2017 <- get_underdog_stats(dat_2017)  
dat_current <- get_underdog_stats(dat_current)  

dat_current$x54 <- dat_current$x55 <- dat_current$x56 <- NULL

# get total game number 
temp_all <- get_total_games(temp_2014 = dat_2014, 
                            temp_2015 = dat_2015, 
                            temp_2016 = dat_2016, 
                            temp_2017 = dat_2017, 
                            temp_current = dat_current)
dat_2014 <- temp_all[[1]]
dat_2015 <- temp_all[[2]]
dat_2016 <- temp_all[[3]]
dat_2017 <- temp_all[[4]]
dat_current <- temp_all[[5]]
rm(temp_all)

# get over under total
dat_2014 <- get_over_under_total(dat_2014)
dat_2015 <- get_over_under_total(dat_2015)
dat_2016 <- get_over_under_total(dat_2016)
dat_2017 <- get_over_under_total(dat_2017)
dat_current <- get_over_under_total(dat_current)

# get over under stats
dat_2014 <- get_over_under_stats(dat_2014) 
dat_2015 <- get_over_under_stats(dat_2015)  
dat_2016 <- get_over_under_stats(dat_2016)  
dat_2017 <- get_over_under_stats(dat_2017)  
dat_current <- get_over_under_stats(dat_current)  


# loop through data sets and assign team game number
dat_2014 <- get_team_game_number(dat_2014)
dat_2015 <- get_team_game_number(dat_2015)
dat_2016 <- get_team_game_number(dat_2016)
dat_2017 <- get_team_game_number(dat_2017)
dat_current <- get_team_game_number(dat_current)

# if team game num is 1 then rest days is last season
dat_2014$rest_days <- ifelse(dat_2014$team_game_num == 1, 
                             'last_season', 
                             dat_2014$rest_days)
dat_2015$rest_days <- ifelse(dat_2015$team_game_num == 1, 
                             'last_season', 
                             dat_2015$rest_days)
dat_2016$rest_days <- ifelse(dat_2016$team_game_num == 1, 
                             'last_season', 
                             dat_2016$rest_days)
dat_2017$rest_days <- ifelse(dat_2017$team_game_num == 1, 
                             'last_season', 
                             dat_2017$rest_days)
dat_current$rest_days <- ifelse(dat_current$team_game_num == 1, 
                                'last_season', 
                                dat_current$rest_days)

# joine data
full_dat <- rbind(dat_2014,
                  dat_2015,
                  dat_2016,
                  dat_2017,
                  dat_current)

rm(dat_2014, dat_2015, dat_2016, dat_2017, dat_current)


# get year
full_dat$year <- as.factor(format(full_dat$date, format = '%Y'))

# create a month variable 
full_dat$month <- month(as.POSIXlt(full_dat$date))

# recode number columns
names(full_dat) <- gsub('3p', 'three_p', names(full_dat))
names(full_dat) <- gsub('3pa', 'three_pa', names(full_dat))
names(full_dat) <- gsub('1q', 'first_q', names(full_dat))
names(full_dat) <- gsub('2q', 'second_q', names(full_dat))
names(full_dat) <- gsub('3q', 'third_q', names(full_dat))
names(full_dat) <- gsub('4q', 'fourth_q', names(full_dat))

# get percentages for fg fga, 3p 3pa, ft fta, 1q f, 2q f, 3q f, 4q f
full_dat$fg_per <- round(full_dat$fg/full_dat$fga, 2)
full_dat$ft_per <- round(full_dat$ft/full_dat$fta, 2)
full_dat$three_per <- round(full_dat$three_p/full_dat$three_pa, 2)
full_dat$per_first_q <- round(full_dat$first_q/full_dat$f, 2)
full_dat$per_second_q <- round(full_dat$second_q/full_dat$f, 2)
full_dat$per_third_q <- round(full_dat$third_q/full_dat$f, 2)
full_dat$per_fourth_q <- round(full_dat$fourth_q/full_dat$f, 2)

# recode player names
names(full_dat)[names(full_dat) == 'starting_lineups'] <- 'player_1'
names(full_dat)[names(full_dat) == 'x37'] <- 'player_2'
names(full_dat)[names(full_dat) == 'x38'] <- 'player_3'
names(full_dat)[names(full_dat) == 'x39'] <- 'player_4'
names(full_dat)[names(full_dat) == 'x40'] <- 'player_5'

# makue sure levels are homogenized across all season
# recode rest days 
full_dat$rest_days <- ifelse(grepl('4IN5', full_dat$rest_days), 
                             '4IN5', 
                             ifelse(grepl('B2B2B', full_dat$rest_days),
                                    'B2B', full_dat$rest_days))
# recode venue
full_dat$venue <- ifelse(full_dat$venue == 'R', 'Road',
                         ifelse(full_dat$venue == 'H', 'Home', full_dat$venue))

dat_game <- full_dat

rm(full_dat)

# convert numerics
dat_game <- featurize_team_data(dat_game)
dat_game <- get_team_ranks(dat_game)

saveRDS(dat_game, '../data/cleaned_data/cleaned_data.rda')




# temp <- dat_game[!duplicated(dat_game[,c(1,3)]),]
# temp <- temp[, c('dataset', 'teams', 'player_1', 'player_2', 'player_3', 'player_4', 'player_5')]
# write.csv(temp, '~/Desktop/temp_players.csv')

team_stats <- names(dat_game)[1:ncol(dat_game)]
remove_these <- 'dataset|^ot|poss|^odds|opening'
team_stats <- team_stats[!grepl(remove_these, team_stats)]
# plot_names <- c("mean_total_points", "mean_fg_per", "mean_three_per", "mean_closing_spread", "mean_real_spread",    
#                 "mean_closing_total", "mean_real_total", "mean_assists" , "mean_turn_over", "mean_off_eff" , 
#                 "mean_def_eff", "mean_pace" , "mean_or")



# temp_team <- temp_team[, c('date', 'teams', 'venue', 'win_loss', 'f',"cum_sum_ud_loss_lag_per_team" , "mov_avg_ud_loss_team"  ,        "ud_win_team_ind" ,   "cum_sum_ud_win_lag_team"  ,    
#                             "cum_sum_ud_win_lag_per_team"  , "mov_avg_ud_win_team"    ,       "fav_win_team_ind"  ,            "cum_sum_fav_win_lag_team"  ,   
#                            "cum_sum_fav_win_lag_per_team" , "mov_avg_fav_win_team"   ,       "fav_loss_team_ind"  ,           "cum_sum_fav_loss_lag_team" ,   
#                            "cum_sum_fav_loss_lag_per_team" ,"mov_avg_fav_loss_team"    ,     "ou_over_team_ind"   ,           "cum_sum_ou_over_lag_team"  ,   
#                            "cum_sum_ou_over_lag_per_team",  "mov_avg_ou_over_team" ,         "ou_under_team_ind"    ,         "cum_sum_ou_under_lag_team" ,   
#                            "cum_sum_ou_under_lag_per_team" ,"mov_avg_ou_under_team"   ,      "win_ind_team" ,                 "cum_wins_lag_team"   ,         
#                             "cum_wins_per_lag_team",         "cum_points_team"  ,             "mov_avg_points_team"    ,       "mov_avg_first_q"    ,          
#                             "mov_avg_second_q" ,             "mov_avg_third_q"  ,             "mov_avg_fourth_q"  ,            "momentum"  ,                   
#                            "momentum_team",                 "win_streak_team" ,              "lose_streak_team"   ,           "mov_avg_pace_team"  ,          
#                            "mov_avg_oeff_team"  ,           "mov_avg_deff_team"  ,           "mov_avg_real_spread_team" ,     "mov_avg_closing_spread_team",  
#                            "mov_avg_real_total_team",   "mov_avg_closing_total_team" ,   "mov_avg_or_team"  ,             "mov_avg_dr_team"   ,           
#                             "mov_avg_tot_team") ]        
