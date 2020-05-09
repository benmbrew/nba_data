# temp <- data.frame(year_2014 = c(names(dat_2014), NA, NA), year_2015 = c(names(dat_2015), NA, NA),
#                    year_2016 = c(names(dat_2016), NA, NA), year_2017 = c(names(dat_2017), NA, NA),
#                    year_current = names(dat_current))
# write.csv(temp, '~/Desktop/temp_dict.csv')
# keep this columns - the opening odds and closing odds are actually the over unders


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

# register cpu
registerDoParallel(3)
# source functions script 
source('functions.R')


# read in data
dat_2014 <- read_csv('../data/team_stat_2014_15.csv')
dat_2015 <- read_csv('../data/team_stat_2015_16.csv')
dat_2016 <- read_csv('../data/team_stat_2016_17.csv')
dat_2017 <- read_csv('../data/team_stat_2017_18.csv')
dat_current <- read_csv('../data/season_team_feed.csv')

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

# # add closing spread and closing total to each data set that is not current 
# # and fill with NA for time being
# dat_2014$closing_spread <- dat_2014$closing_total <- 
#   dat_2015$closing_spread <- dat_2015$closing_total <- 
#   dat_2016$closing_spread <- dat_2016$closing_total <- 
#   dat_2017$closing_spread <- dat_2017$closing_total <- NA
dat_current$closing_spread <- 
  dat_current$closing_total <- NULL

# recode 2014 data from main_referee to main_ref and crew_referees to crew
names(dat_2014) <- gsub('main_referee', 'main_ref', names(dat_2014))
names(dat_2014) <- gsub('crew_referees', 'crew', names(dat_2014))

# get season indicator
dat_2014$dataset <- '2014_2015_season'
dat_2015$dataset <- '2015_2016_season'
dat_2016$dataset <- '2016_2017_season'
dat_2017$dataset <- '2017_2018_season'
dat_current$dataset <- '2018_2019_season'

# get game number for each dataset
dat_2014$game_number <- rep(1:(nrow(dat_2014)/2), each=2)
dat_2015$game_number <- rep(1:(nrow(dat_2015)/2), each=2)
dat_2016$game_number <- rep(1:(nrow(dat_2016)/2), each=2)
dat_2017$game_number <- rep(1:(nrow(dat_2017)/2), each=2)
dat_current$game_number <- rep(1:(nrow(dat_current)/2), each=2)

# joine data
full_dat <- rbind(dat_2014,
                  dat_2015,
                  dat_2016,
                  dat_2017,
                  dat_current)

rm(dat_2014, dat_2015, dat_2016, dat_2017, dat_current)

# now get total game number 
full_dat$tot_game_num <- rep(1:(nrow(full_dat)/2), each=2)

# convert date
full_dat$date <- as.Date(full_dat$date, format = '%m/%d/%Y')

# get year
full_dat$year <- as.factor(format(full_dat$date, format = '%Y'))

# create a month variable 
full_dat$month <- month(as.POSIXlt(full_dat$date))

# get percentages for fg, 3p and ft
full_dat$fg_per <- round(full_dat$fg/full_dat$fga, 2)

# loop through and add up score for each game to get real total
for(i in seq(1, nrow(full_dat), 2)) {
  full_dat$real_total[i:(i+1)] <- sum(full_dat$f[i], full_dat$f[i+1])
  print(i)
}

# remove uneeded columns
full_dat$x37 <- full_dat$x38 <- full_dat$x39 <- full_dat$x40 <-
  full_dat$box_score <- full_dat$crew <- full_dat$ot1 <- full_dat$ot2 <-
  full_dat$ot3 <- full_dat$ot4 <- full_dat$min <- full_dat$odds <-
  full_dat$halftime <- full_dat$opening_odds <- NULL


# featurize data
temp_dat <- full_dat


# this is the main function that featurizes team data .
featurize_team_data <- function(temp_dat){
  # get a vector of team names to loop through
  unique_teams <- unique(temp_dat$team)
  # loop through unique teams and grab sub team and opponenet data to featurize
  data_list <- list()
  for(i in 1:length(unique_teams)){
    # get team name and subset
    this_team <- unique_teams[i]
    sub_team <- temp_dat[temp_dat$team == this_team, ]
    sub_team_name <- unique(sub_team$team)
    
    message('Creating features for ', sub_team_name)
    
    # check that the data is arranged by data
    sub_team <- sub_team[order(sub_team$date),]
    
    # begin generating features
    # to start: days since last game
    sub_team <- sub_team %>% mutate(last_game=round(c(250,diff(date)), 1))
    
    # create a numeric win column and use the lag function 
    sub_team$win_ind_team <- ifelse(sub_team$win_loss == 'W', 1, 0 )
    sub_team$win_ind_team <- get_lag_data(sub_team$win_ind_team)
    
    # create a numeric venue column and use the lag function 
    sub_team$venue_ind_team <- ifelse(sub_team$venue == 'Home', 1, 0 )
    sub_team$venue_ind_team <- get_lag_data(sub_team$venue_ind_team)
    
    # get cumulative sum of lagged wins and winning percentage 
    sub_team$cum_wins_lag_team <- cumsum(sub_team$win_ind_team)
    sub_team$cum_wins_per_lag_team <- cumsum(sub_team$win_ind_team)/get_lag_data(sub_team$game_num)
    sub_team$cum_wins_per_lag_team <- ifelse(sub_team$cum_wins_per_lag_team == 'NaN', 0, sub_team$cum_wins_per_lag_team)
    
    # get cumulative sum of points scored and points allowed 
    sub_team$cum_points_team <- cumsum(sub_team$final)
    sub_team$cum_points_team <- get_lag_data(sub_team$cum_points_team)
    
    sub_team$mov_avg_points_team <- movavg(sub_team$final, n = 3, type = 's')
    sub_team$mov_avg_points_team <- get_lag_data(sub_team$mov_avg_points_team)
    
    sub_team$cum_points_allowed_by_def_team <- cumsum(sub_team$points_allowed_by_def)
    sub_team$cum_points_allowed_by_def_team <- get_lag_data(sub_team$cum_points_allowed_by_def_team)
    
    sub_team$mov_avg_points_allowed_by_def_team <- movavg(sub_team$points_allowed_by_def, n = 3, type = 's')
    sub_team$mov_avg_points_allowed_by_def_team <- get_lag_data(sub_team$mov_avg_points_allowed_by_def_team)
    
    # get cumulative sum of yds 
    sub_team$cum_total_yds_team <- cumsum(sub_team$total_yds)
    sub_team$cum_total_yds_team <- get_lag_data(sub_team$cum_total_yds_team)
    
    sub_team$mov_avg_total_yds_team <- movavg(sub_team$total_yds, n = 3, type = 's')
    sub_team$mov_avg_total_yds_team <- get_lag_data(sub_team$mov_avg_total_yds_team)
    
    # create a momentum variable off of lagged cumulative wins
    sub_team$momentum_team <- diff(c(0,sub_team$cum_wins_per_lag_team))
    
    # take the inverse
    sub_team$momentum_team <- ifelse(sub_team$momentum_team == 0, 0, 1/sub_team$momentum_team)
    
    # get win streak using "streak" function from functions.R
    sub_team$win_streak_team <- streak(sub_team$win_loss, value = 'W')
    
    # get losing streak
    sub_team$lose_streak_team <- streak(sub_team$win_loss, value = 'L')
    
    # moving average for first downs,rush_yds, rush_tds, pass_comp, pass_yds, pass_tds, qb_interceptions, qb_sacked, 
    # fumbles, turnovers, penalties, 3rd (third_downs_made, third_down_att) and 4th,
    # def_sack, def_interceptions
    
    # first_downs
    sub_team$mov_avg_first_downs_team <- movavg(sub_team$first_downs, n = 5, type= 's')
    sub_team$mov_avg_first_downs_team <- get_lag_data(sub_team$mov_avg_first_downs_team)
    
    # cumsum
    sub_team$cum_sum_first_downs_team <- cumsum(sub_team$first_downs)
    sub_team$cum_sum_first_downs_team <- get_lag_data(sub_team$cum_sum_first_downs_team)
    
    # rush_yds
    sub_team$mov_avg_rush_yds_team <- movavg(sub_team$rush_yds, n = 5, type = 's')
    sub_team$mov_avg_rush_yds_team <- get_lag_data(sub_team$mov_avg_rush_yds_team)
    
    # cumsum
    sub_team$cum_sum_rush_yds_team <- cumsum(sub_team$rush_yds)
    sub_team$cum_sum_rush_yds_team <- get_lag_data(sub_team$cum_sum_rush_yds_team)
    
    # rush_tds
    sub_team$mov_avg_rush_tds_team <- movavg(sub_team$rush_tds, n = 5, type = 's')
    sub_team$mov_avg_rush_tds_team <- get_lag_data(sub_team$mov_avg_rush_tds_team)
    
    sub_team$cum_sum_rush_tds_team <- cumsum(sub_team$rush_tds)
    sub_team$cum_sum_rush_tds_team <- get_lag_data(sub_team$cum_sum_rush_tds_team)
    
    # pass_comp
    sub_team$mov_avg_pass_comp_team <- movavg(sub_team$pass_comp, n = 5, type = 's')
    sub_team$mov_avg_pass_comp_team <- get_lag_data(sub_team$mov_avg_pass_comp_team)
    
    # cumsum
    sub_team$cum_sum_pass_comp_team <- cumsum(sub_team$pass_comp)
    sub_team$cum_sum_pass_comp_team <- get_lag_data(sub_team$cum_sum_pass_comp_team)
    
    # pass_yds
    sub_team$mov_avg_pass_yds_team <- movavg(sub_team$pass_yds, n = 5, type = 's')
    sub_team$mov_avg_pass_yds_team <- get_lag_data(sub_team$mov_avg_pass_yds_team)
    
    # cumsum
    sub_team$cum_sum_pass_yds_team <- cumsum(sub_team$pass_yds)
    sub_team$cum_sum_pass_yds_team <- get_lag_data(sub_team$cum_sum_pass_yds_team)
    
    # pass_tds
    sub_team$mov_avg_pass_tds_team <- movavg(sub_team$pass_tds, n = 5, type = 's')
    sub_team$mov_avg_pass_tds_team <- get_lag_data(sub_team$mov_avg_pass_tds_team)
    
    # cumsum
    sub_team$cum_sum_pass_tds_team <- cumsum(sub_team$pass_tds)
    sub_team$cum_sum_pass_tds_team <- get_lag_data(sub_team$cum_sum_pass_tds_team)
    
    # qb_interceptions
    sub_team$mov_avg_qb_interceptions_team <- movavg(sub_team$qb_interceptions, n = 5, type = 's')
    sub_team$mov_avg_qb_interceptions_team <- get_lag_data(sub_team$mov_avg_qb_interceptions_team)
    
    # cumsum
    sub_team$cum_sum_qb_interceptions_team <- cumsum(sub_team$qb_interceptions)
    sub_team$cum_sum_qb_interceptions_team <- get_lag_data(sub_team$cum_sum_qb_interceptions_team)
    
    # qb_sacked
    sub_team$mov_avg_qb_sacked_team <- movavg(sub_team$qb_sacked, n = 5, type = 's')
    sub_team$mov_avg_qb_sacked_team <- get_lag_data(sub_team$mov_avg_qb_sacked_team)
    
    # cumsum
    sub_team$cum_sum_qb_sacked_team <- cumsum(sub_team$qb_sacked)
    sub_team$cum_sum_qb_sacked_team <- get_lag_data(sub_team$cum_sum_qb_sacked_team)
    
    # fumbles
    sub_team$mov_avg_fumbles_team <- movavg(sub_team$fumbles, n = 5, type = 's')
    sub_team$mov_avg_fumbles_team <- get_lag_data(sub_team$mov_avg_fumbles_team)
    
    #cumsum
    sub_team$cum_sum_fumbles_team <- cumsum(sub_team$fumbles)
    sub_team$cum_sum_fumbles_team <- get_lag_data(sub_team$cum_sum_fumbles_team)
    
    # turnovers
    sub_team$mov_avg_turnovers_team <- movavg(sub_team$turnovers, n = 5, type = 's')
    sub_team$mov_avg_turnovers_team <- get_lag_data(sub_team$mov_avg_turnovers_team)
    
    # cumsum
    sub_team$cum_sum_turnovers_team <- cumsum(sub_team$turnovers)
    sub_team$cum_sum_turnovers_team <- get_lag_data(sub_team$cum_sum_turnovers_team)
    
    # penalties
    sub_team$mov_avg_penalties_team <- movavg(sub_team$penalties , n = 5, type = 's')
    sub_team$mov_avg_penalties_team <- get_lag_data(sub_team$mov_avg_penalties_team)
    
    # cumsum
    sub_team$cum_sum_penalties_team <- cumsum(sub_team$penalties)
    sub_team$cum_sum_penalties_team <- get_lag_data(sub_team$cum_sum_penalties_team)
    
    # def_interception
    sub_team$mov_avg_def_interception_team <- movavg(sub_team$def_interception , n = 5, type = 's')
    sub_team$mov_avg_def_interception_team <- get_lag_data(sub_team$mov_avg_def_interception_team)
    
    # cumsum
    sub_team$cum_sum_def_interception_team <- cumsum(sub_team$def_interception)
    sub_team$cum_sum_def_interception_team <- get_lag_data(sub_team$cum_sum_def_interception_team)
    
    # def_sack
    sub_team$mov_avg_def_sack_team <- movavg(sub_team$def_sack , n = 5, type = 's')
    sub_team$mov_avg_def_sack_team <- get_lag_data(sub_team$mov_avg_def_sack_team)
    
    # first
    sub_team$mov_avg_first_team <- movavg(sub_team$first , n = 5, type = 's')
    sub_team$mov_avg_first_team <- get_lag_data(sub_team$mov_avg_first_team)
    
    #cumsum
    sub_team$cum_sum_first_team <- cumsum(sub_team$first)
    sub_team$cum_sum_first_team <- get_lag_data(sub_team$cum_sum_first_team)
    
    # second
    sub_team$mov_avg_second_team <- movavg(sub_team$second , n = 5, type = 's')
    sub_team$mov_avg_second_team <- get_lag_data(sub_team$mov_avg_second_team)
    
    # cumsum
    sub_team$cum_sum_second_team <- cumsum(sub_team$second)
    sub_team$cum_sum_second_team <- get_lag_data(sub_team$cum_sum_second_team)
    
    # third
    sub_team$mov_avg_third_team <- movavg(sub_team$third , n = 5, type = 's')
    sub_team$mov_avg_third_team <- get_lag_data(sub_team$mov_avg_third_team)
    
    # cumsum
    sub_team$cum_sum_third_team <- cumsum(sub_team$third)
    sub_team$cum_sum_third_team <- get_lag_data(sub_team$cum_sum_third_team)
    
    # fourth
    sub_team$mov_avg_fourth_team <- movavg(sub_team$fourth , n = 5, type = 's')
    sub_team$mov_avg_fourth_team <- get_lag_data(sub_team$mov_avg_fourth_team)
    
    # cumsum
    sub_team$cum_sum_fourth_team <- cumsum(sub_team$fourth)
    sub_team$cum_sum_fourth_team <- get_lag_data(sub_team$cum_sum_fourth_team)
    
    # final
    sub_team$mov_avg_final_team <- movavg(sub_team$final , n = 5, type = 's')
    sub_team$mov_avg_final_team <- get_lag_data(sub_team$mov_avg_fourth_team)
    
    #cumsum
    sub_team$cum_sum_final_team <- cumsum(sub_team$final)
    sub_team$cum_sum_final_team <- get_lag_data(sub_team$cum_sum_fourth_team)
    
    # points_allowed_by_def
    sub_team$mov_avg_points_allowed_by_def_team <- movavg(sub_team$points_allowed_by_def , n = 5, type = 's')
    sub_team$mov_avg_points_allowed_by_def_team <- get_lag_data(sub_team$mov_avg_points_allowed_by_def_team)
    
    # cumsum
    sub_team$cum_sum_points_allowed_by_def_team <- cumsum(sub_team$points_allowed_by_def)
    sub_team$cum_sum_points_allowed_by_def_team <- get_lag_data(sub_team$cum_sum_points_allowed_by_def_team)
    
    # third_downs_made, att, and efficiency
    sub_team$mov_avg_third_down_made_team <- movavg(sub_team$third_down_made , n = 5, type = 's')
    sub_team$mov_avg_third_down_made_team <- get_lag_data(sub_team$mov_avg_third_down_made_team)
    
    sub_team$mov_avg_third_down_att_team <- movavg(sub_team$third_down_att , n = 5, type = 's')
    sub_team$mov_avg_third_down_att_team <- get_lag_data(sub_team$mov_avg_third_down_att_team)
    
    sub_team$third_down_per_team <- round((sub_team$third_down_made/sub_team$third_down_att)*100,2)
    sub_team$mov_avg_third_down_per_team <- movavg(sub_team$third_down_per_team , n = 5, type = 's')
    sub_team$mov_avg_third_down_per_team <- get_lag_data(sub_team$mov_avg_third_down_per_team)
    
    # cumsum
    sub_team$cum_sum_third_down_made_team <- cumsum(sub_team$third_down_made)
    sub_team$cum_sum_third_down_made_team <- get_lag_data(sub_team$cum_sum_third_down_made_team)
    
    sub_team$cum_sum_third_down_att_team <- cumsum(sub_team$third_down_att)
    sub_team$cum_sum_third_down_att_team <- get_lag_data(sub_team$cum_sum_third_down_att_team)
    
    sub_team$third_down_per_team <- round((sub_team$third_down_made/sub_team$third_down_att)*100,2)
    sub_team$cum_sum_third_down_per_team <- cumsum(sub_team$third_down_per_team)
    sub_team$cum_sum_third_down_per_team <- get_lag_data(sub_team$cum_sum_third_down_per_team)
    
    
    # fourth_downs_made, att, and efficiency
    sub_team$mov_avg_fourth_down_made_team <- movavg(sub_team$fourth_down_made , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_made_team <- get_lag_data(sub_team$mov_avg_fourth_down_made_team)
    
    sub_team$mov_avg_fourth_down_att_team <- movavg(sub_team$fourth_down_att , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_att_team <- get_lag_data(sub_team$mov_avg_fourth_down_att_team)
    
    sub_team$fourth_down_per_team <- round((sub_team$fourth_down_made/sub_team$fourth_down_att)*100,2)
    sub_team$mov_avg_fourth_down_per_team <- movavg(sub_team$fourth_down_per_team , n = 5, type = 's')
    sub_team$mov_avg_fourth_down_per_team <- get_lag_data(sub_team$mov_avg_fourth_down_per_team)
    
    # cumsum
    sub_team$cum_sum_fourth_down_made_team <- cumsum(sub_team$fourth_down_made)
    sub_team$cum_sum_fourth_down_made_team <- get_lag_data(sub_team$cum_sum_fourth_down_made_team)
    
    sub_team$cum_sum_fourth_down_att_team <- cumsum(sub_team$fourth_down_att)
    sub_team$cum_sum_fourth_down_att_team <- get_lag_data(sub_team$cum_sum_fourth_down_att_team)
    
    sub_team$fourth_down_per_team <- round((sub_team$fourth_down_made/sub_team$fourth_down_att)*100,2)
    sub_team$cum_sum_fourth_down_per_team <- cumsum(sub_team$fourth_down_per_team)
    sub_team$cum_sum_fourth_down_per_team <- get_lag_data(sub_team$cum_sum_fourth_down_per_team)
    
    sub_team$fourth_down_per_team <- sub_team$third_down_per_team <- NULL
    
    # time_pss, openings_spread, opening_total
    sub_team$mov_avg_time_of_poss_team <- movavg(sub_team$time_of_poss , n = 5, type = 's')
    sub_team$mov_avg_time_of_poss_team <- get_lag_data(sub_team$mov_avg_time_of_poss_team)
    
    sub_team$cum_sum_time_of_poss_team <- cumsum(sub_team$time_of_poss)
    sub_team$cum_sum_time_of_poss_team <- get_lag_data(sub_team$cum_sum_time_of_poss_team)
    
    sub_team$mov_avg_opening_spread_team <- movavg(sub_team$openings_spread , n = 5, type = 's')
    sub_team$mov_avg_opening_spread_team <- get_lag_data(sub_team$mov_avg_opening_spread_team)
    
    sub_team$cum_sum_opening_spread_team <- cumsum(sub_team$openings_spread)
    sub_team$cum_sum_opening_spread_team <- get_lag_data(sub_team$cum_sum_opening_spread_team)
    
    sub_team$mov_avg_opening_total_team <- movavg(sub_team$opening_total, n = 5, type = 's')
    sub_team$mov_avg_opening_total_team <- get_lag_data(sub_team$mov_avg_opening_total_team)
    
    sub_team$cum_sum_opening_total_team <- cumsum(sub_team$opening_total)
    sub_team$cum_sum_opening_total_team <- get_lag_data(sub_team$cum_sum_opening_total_team)
    
    
    # only keep the variables that are created with the correct format = each row is previous weeks data
    # either in the form of cumulative sums or moving avgerages
    column_string <- c('mov_avg|closing_spread|cum_sum|win_streak_team|game_id|lose_streak_team|win_loss|game_num|last_game|momentum_team|date|week|team|^venue$|final')
    sub_team <- sub_team[, grepl(column_string, names(sub_team))]
    
    # store data in data_list
    data_list[[i]] <- sub_team
    
  }
  
  final_data <- do.call('rbind', data_list)
  return(final_data)
}


# make a dataset that has one observation per game, with corresponding features
dat_game <- get_by_game(full_dat)

# convert date
dat_game$date <- as.Date(dat_game$date_home, format = '%m/%d/%Y')

# get year
dat_game$year <- as.factor(format(dat_game$date, format = '%Y'))

# create a month variable 
dat_game$month <- month(as.POSIXlt(dat_game$date))

# add a favorite variable
dat_game$home_team_favored <- ifelse(dat_game$opening_spread_home < 0, TRUE, FALSE)

rm(dat_full)
save.image('../Data/cleaned_data/by_game.RData')





