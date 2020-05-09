
# This scritp will take cleaned player, team, and fantasy level data and combine them into
# two datasets: (1) player + fantasy and (2) player + fantasy + team.

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

# load in cleaned data files
dat_player <- readRDS('../data/cleaned_data/cleaned_player_data.rda')
dat_fan <- readRDS('../data/cleaned_data/cleaned_fantasy_data.rda')
dat_team <- readRDS('../data/cleaned_data/cleaned_team_data.rda')


# convert variable types 
dat_fan$f_salary <- as.numeric(dat_fan$f_salary)

# implement function to combine fantasy and team data and 
# return a dataframe where each row is an individuals game.
fan_team <- combine_fantasy_team_data(dat_fan)

# fix referee columns - all info is there, its just split between opponent and own team
fan_team$t_main_ref_player_team <- ifelse(is.na(fan_team$t_main_ref_player_team), 
                                          fan_team$t_main_ref_opp_team, 
                                          fan_team$t_main_ref_player_team)

fan_team$t_main_ref_opp_team <- ifelse(is.na(fan_team$t_main_ref_opp_team), 
                                       fan_team$t_main_ref_player_team, 
                                       fan_team$t_main_ref_opp_team)

# rename ref columns and remove main_ref for opp
fan_team$t_main_ref_opp_team <- NULL
names(fan_team)[names(fan_team) == 't_crew_opp_team'] <- 't_crew_2'
names(fan_team)[names(fan_team) == 't_crew_player_team'] <- 't_crew_1'
names(fan_team)[names(fan_team) == 't_main_ref_player_team'] <- 'main_ref'

# extract month and year from date
fan_team$f_date <- as.Date(fan_team$f_date, format = '%m/%d/%Y')
fan_team$month <- format(as.Date(fan_team$f_date), "%m")
fan_team$year <- format(as.Date(fan_team$f_date), "%Y")


# get percentages for player's team fg, three pointers, ft
# get percentages for opponent fg, three pointers, ft
fan_team$t_fg_per_player_team <- round((fan_team$t_fg_player_team/fan_team$t_fga_player_team)*100,2) 
fan_team$t_fg_per_opp_team <- round((fan_team$t_fg_opp_team/fan_team$t_fga_opp_team)*100,2) 

fan_team$t_three_per_player_team <- round((fan_team$t_3p_player_team/fan_team$t_3pa_player_team)*100,2) 
fan_team$t_three_per_opp_team <- round((fan_team$t_3p_opp_team/fan_team$t_3pa_opp_team)*100,2) 

fan_team$t_ft_per_player_team <- round((fan_team$t_ft_player_team/fan_team$t_fta_player_team)*100,2) 
fan_team$t_ft_per_opp_team <- round((fan_team$t_ft_opp_team/fan_team$t_fta_opp_team)*100,2) 

# remove variables that won't be included in the model
fan_team$t_fg_player_team <- fan_team$t_fg_opp_team <- fan_team$t_fga_player_team <- 
  fan_team$t_fga_opp_team <- fan_team$t_3p_player_team <- fan_team$t_3p_opp_team <- 
  fan_team$t_3pa_opp_team <- fan_team$t_ft_player_team <- fan_team$t_ft_opp_team <-
  fan_team$t_fta_player_team <- fan_team$t_fta_opp_team <- fan_team$t_3pa_player_team <- 
  fan_team$f_dataset <- fan_team$t_venue_player_team <- fan_team$t_venue_opp_team <- 
  fan_team$t_1q_player_team <- fan_team$t_1q_opp_team <- fan_team$t_2q_player_team <- 
  fan_team$t_2q_opp_team <- fan_team$t_3q_player_team <- fan_team$t_3q_opp_team <- 
  fan_team$t_4q_player_team <- fan_team$t_4q_opp_team <- fan_team$t_ot1_player_team <-
  fan_team$t_ot1_opp_team <- fan_team$t_ot2_player_team <- fan_team$t_ot2_opp_team <-
  fan_team$t_ot3_player_team <- fan_team$t_ot3_opp_team <- fan_team$t_ot4_player_team <- 
  fan_team$t_ot4_opp_team <- fan_team$t_min_player_team <- fan_team$t_min_opp_team <- 
  fan_team$t_opening_spread_player_team <- fan_team$t_opening_spread_opp_team <- 
  fan_team$t_opening_total_player_team <- fan_team$t_opening_total_opp_team <- 
  NULL

  
# create a variable to indicate if the player in question is starting that game, along with all other 
# starters. 
fan_team$player_starting <- ifelse(fan_team$f_player %in% c(fan_team$t_starter_1_player_team, fan_team$t_starter_2_player_team,
                                                            fan_team$t_starter_3_player_team, fan_team$t_starter_4_player_team,
                                                            fan_team$t_starter_5_player_team), 'Y', 'N')
# # get names of all main starters to create indicators for if they are starting 
# player_counts <- fan_team %>%
#   group_by(f_player, f_team) %>%
#   summarise(counts = n())

# group by player and get counts 
fan_team <- fan_team %>% 
  group_by(f_player, year) %>%
  mutate(counts = n())

# remove players with less than 10 appearances
fan_team <- fan_team[fan_team$counts > 30,]


# create function to featurize fantasy and team data, by creating lagged variables
# and moving avgs

i = 1
j = 2
temp_dat <- fan_team
featurize_fantasy_team_data <- function(temp_dat){
  
  year_list <- list()
  unique_years <- unique(temp_dat$year)

  for(i in 1:length(unique_years)){
    player_list <- list()
    this_year <- unique_years[i]
    sub_year <- temp_dat[temp_dat$year == this_year,]
    unique_names <- unique(sub_year$f_player)
    for(j in 1:length(unique_names)) {
      this_player <- unique_names[j]
      sub_player <- sub_year[sub_year$f_player == this_player,]
      
      
      # order by date and get days since last game 
      sub_player <- sub_player[order(sub_player$f_date),]
      sub_player <- sub_player %>% mutate(last_game=round(c(100,diff(f_date)), 1))
      
      # get a starter indicator and get streak and mov avg, not cumsum HERE
      sub_player$starter_ind <- ifelse(sub_player$player_starting == 'Y', 1, 0 )
      sub_player$mov_avg_starter <- movavg(sub_player$starter_ind, n = 3, type = 's')
      sub_player$mov_avg_starter <- get_lag_data(sub_player$mov_avg_starter)
      
      end_index <- nrow(sub_player) - 1
      starter_streak <- streak(sub_player$player_starting[1:end_index], value = 'Y')
      if(sub_player$player_starting[end_index] == 'Y'){
        end_value <- 1 + starter_streak[end_index]
      } else {
        end_value <- 0
      }
      starter_streak <- c(starter_streak, end_value)
      sub_player$starter_streak <- starter_streak
      
      # get a venue indicator and get streak and mov avg, not cumsum HERE
      sub_player$home_ind <- ifelse(sub_player$f_venue == 'H', 1, 0 )
      sub_player$mov_avg_home <- movavg(sub_player$home_ind, n = 3, type = 's')
      sub_player$mov_avg_home <- get_lag_data(sub_player$mov_avg_home)
      
      end_index <- nrow(sub_player) - 1
      home_streak <- streak(sub_player$f_venue[1:end_index], value = 'H')
      if(sub_player$player_starting[end_index] == 'H'){
        end_value <- 1 + home_streak[end_index]
      } else {
        end_value <- 0
      }
      home_streak <- c(home_streak, end_value)
      sub_player$home_streak <- home_streak
      
      # getmov avg variables for fg per, 3p per, and ft per
      sub_player$mov_avg_fg_per_team <- movavg(sub_player$t_fg_per_player_team, n = 3, type = 's')
      sub_player$mov_avg_fg_per_team <- get_lag_data(sub_player$mov_avg_fg_per_team)
      
      sub_player$mov_avg_fg_per_opp <- movavg(sub_player$t_fg_per_opp_team, n = 3, type = 's')
      sub_player$mov_avg_fg_per_opp <- get_lag_data(sub_player$mov_avg_fg_per_opp)
      
      sub_player$mov_avg_3_per_team <- movavg(sub_player$t_three_per_player_team, n = 3, type = 's')
      sub_player$mov_avg_3_per_team <- get_lag_data(sub_player$mov_avg_3_per_team)
      
      sub_player$mov_avg_3_per_opp <- movavg(sub_player$t_three_per_opp_team, n = 3, type = 's')
      sub_player$mov_avg_3_per_opp <- get_lag_data(sub_player$mov_avg_3_per_opp)
      
      sub_player$mov_avg_ft_per_team <- movavg(sub_player$t_ft_per_player_team, n = 3, type = 's')
      sub_player$mov_avg_ft_per_team <- get_lag_data(sub_player$mov_avg_ft_per_team)
      
      sub_player$mov_avg_ft_per_opp <- movavg(sub_player$t_ft_per_opp_team, n = 3, type = 's')
      sub_player$mov_avg_ft_per_opp <- get_lag_data(sub_player$mov_avg_ft_per_opp)
      
      # get lagged and moving avg for player team, f (points), off reb, def reb, tot reb, assists,
      # steals, to, personal fouls, poss, pace, oeff, deff, rest days
      
      # points
      sub_player$sum_points_team <- cumsum(sub_player$t_f_player_team)
      sub_player$sum_points_team<- get_lag_data(sub_player$sum_points_team)
      sub_player$mov_avg_points_team <- movavg(sub_player$t_f_player_team, n = 3, type = 's')
      sub_player$mov_avg_points_team  <- get_lag_data(sub_player$mov_avg_points_team )
      
      sub_player$sum_points_opp <- cumsum(sub_player$t_f_opp_team)
      sub_player$sum_points_opp <- get_lag_data(sub_player$sum_points_opp)
      sub_player$mov_avg_points_opp <- movavg(sub_player$t_f_opp_team, n = 3, type = 's')
      sub_player$mov_avg_points_opp  <- get_lag_data(sub_player$mov_avg_points_opp )
      
      # remove old variable 
      sub_player$t_f_player_team <- sub_player$t_f_opp_team <- NULL
      
      # or
      sub_player$sum_or_team <- cumsum(sub_player$t_or_player_team)
      sub_player$sum_or_team<- get_lag_data(sub_player$sum_or_team)
      sub_player$mov_avg_or_team <- movavg(sub_player$t_or_player_team, n = 3, type = 's')
      sub_player$mov_avg_or_team  <- get_lag_data(sub_player$mov_avg_or_team )
      
      sub_player$sum_or_opp <- cumsum(sub_player$t_or_opp_team)
      sub_player$sum_or_opp <- get_lag_data(sub_player$sum_or_opp)
      sub_player$mov_avg_or_opp <- movavg(sub_player$t_or_opp_team, n = 3, type = 's')
      sub_player$mov_avg_or_opp  <- get_lag_data(sub_player$mov_avg_or_opp )
      
      sub_player$t_or_player_team <- sub_player$t_or_opp_team <- NULL
      
      
      # dr
      sub_player$sum_dr_team <- cumsum(sub_player$t_dr_player_team)
      sub_player$sum_dr_team<- get_lag_data(sub_player$sum_dr_team)
      sub_player$mov_avg_dr_team <- movavg(sub_player$t_dr_player_team, n = 3, type = 's')
      sub_player$mov_avg_dr_team  <- get_lag_data(sub_player$mov_avg_dr_team )
      
      sub_player$sum_dr_opp <- cumsum(sub_player$t_dr_opp_team)
      sub_player$sum_dr_opp <- get_lag_data(sub_player$sum_dr_opp)
      sub_player$mov_avg_dr_opp <- movavg(sub_player$t_dr_opp_team, n = 3, type = 's')
      sub_player$mov_avg_dr_opp  <- get_lag_data(sub_player$mov_avg_dr_opp )
      
      sub_player$t_dr_player_team <- sub_player$t_dr_opp_team <- NULL
      
     # tot
      sub_player$sum_tot_team <- cumsum(sub_player$t_tot_player_team)
      sub_player$sum_tot_team<- get_lag_data(sub_player$sum_tot_team)
      sub_player$mov_avg_tot_team <- movavg(sub_player$t_tot_player_team, n = 3, type = 's')
      sub_player$mov_avg_tot_team  <- get_lag_data(sub_player$mov_avg_tot_team )
      
      sub_player$sum_tot_opp <- cumsum(sub_player$t_tot_opp_team)
      sub_player$sum_tot_opp <- get_lag_data(sub_player$sum_tot_opp)
      sub_player$mov_avg_tot_opp <- movavg(sub_player$t_tot_opp_team, n = 3, type = 's')
      sub_player$mov_avg_tot_opp  <- get_lag_data(sub_player$mov_avg_tot_opp )
      
      sub_player$t_tot_player_team <- sub_player$t_tot_opp_team <- NULL
      
      # a
      sub_player$sum_a_team <- cumsum(sub_player$t_a_player_team)
      sub_player$sum_a_team<- get_lag_data(sub_player$sum_a_team)
      sub_player$mov_avg_a_team <- movavg(sub_player$t_a_player_team, n = 3, type = 's')
      sub_player$mov_avg_a_team  <- get_lag_data(sub_player$mov_avg_a_team )
      
      sub_player$sum_a_opp <- cumsum(sub_player$t_a_opp_team)
      sub_player$sum_a_opp <- get_lag_data(sub_player$sum_a_opp)
      sub_player$mov_avg_a_opp <- movavg(sub_player$t_a_opp_team, n = 3, type = 's')
      sub_player$mov_avg_a_opp  <- get_lag_data(sub_player$mov_avg_a_opp )
      
      sub_player$t_a_player_team <- sub_player$t_a_opp_team <- NULL
      
      
      # st
      sub_player$sum_st_team <- cumsum(sub_player$t_st_player_team)
      sub_player$sum_st_team<- get_lag_data(sub_player$sum_st_team)
      sub_player$mov_avg_st_team <- movavg(sub_player$t_st_player_team, n = 3, type = 's')
      sub_player$mov_avg_st_team  <- get_lag_data(sub_player$mov_avg_st_team )
      
      sub_player$sum_st_opp <- cumsum(sub_player$t_st_opp_team)
      sub_player$sum_st_opp <- get_lag_data(sub_player$sum_st_opp)
      sub_player$mov_avg_st_opp <- movavg(sub_player$t_st_opp_team, n = 3, type = 's')
      sub_player$mov_avg_st_opp  <- get_lag_data(sub_player$mov_avg_st_opp )
      
      sub_player$t_st_player_team <- sub_player$t_st_opp_team <- NULL
      
      
      # to
      sub_player$sum_to_team <- cumsum(sub_player$t_to_player_team)
      sub_player$sum_to_team<- get_lag_data(sub_player$sum_to_team)
      sub_player$mov_avg_to_team <- movavg(sub_player$t_to_player_team, n = 3, type = 's')
      sub_player$mov_avg_to_team  <- get_lag_data(sub_player$mov_avg_to_team )
      
      sub_player$sum_to_opp <- cumsum(sub_player$t_to_opp_team)
      sub_player$sum_to_opp <- get_lag_data(sub_player$sum_to_opp)
      sub_player$mov_avg_to_opp <- movavg(sub_player$t_to_opp_team, n = 3, type = 's')
      sub_player$mov_avg_to_opp  <- get_lag_data(sub_player$mov_avg_to_opp )
      
      sub_player$t_to_player_team <- sub_player$t_to_opp_team <- NULL
      
      # personal fouls, poss, pace, oeff, deff, rest days
      
      # pf
      sub_player$sum_pf_team <- cumsum(sub_player$t_pf_player_team)
      sub_player$sum_pf_team<- get_lag_data(sub_player$sum_pf_team)
      sub_player$mov_avg_pf_team <- movavg(sub_player$t_pf_player_team, n = 3, type = 's')
      sub_player$mov_avg_pf_team  <- get_lag_data(sub_player$mov_avg_pf_team )
      
      sub_player$sum_pf_opp <- cumsum(sub_player$t_pf_opp_team)
      sub_player$sum_pf_opp <- get_lag_data(sub_player$sum_pf_opp)
      sub_player$mov_avg_pf_opp <- movavg(sub_player$t_pf_opp_team, n = 3, type = 's')
      sub_player$mov_avg_pf_opp  <- get_lag_data(sub_player$mov_avg_pf_opp )
      
      sub_player$t_pf_player_team <- sub_player$t_pf_opp_team <- NULL
      
      # poss
      sub_player$sum_poss_team <- cumsum(sub_player$t_poss_player_team)
      sub_player$sum_poss_team<- get_lag_data(sub_player$sum_poss_team)
      sub_player$mov_avg_poss_team <- movavg(sub_player$t_poss_player_team, n = 3, type = 's')
      sub_player$mov_avg_poss_team  <- get_lag_data(sub_player$mov_avg_poss_team )
      
      sub_player$sum_poss_opp <- cumsum(sub_player$t_poss_opp_team)
      sub_player$sum_poss_opp <- get_lag_data(sub_player$sum_poss_opp)
      sub_player$mov_avg_poss_opp <- movavg(sub_player$t_poss_opp_team, n = 3, type = 's')
      sub_player$mov_avg_poss_opp  <- get_lag_data(sub_player$mov_avg_poss_opp )
      
      sub_player$t_poss_player_team <- sub_player$t_poss_opp_team <- NULL
      
      # pace
      sub_player$sum_pace_team <- cumsum(sub_player$t_pace_player_team)
      sub_player$sum_pace_team<- get_lag_data(sub_player$sum_pace_team)
      sub_player$mov_avg_pace_team <- movavg(sub_player$t_pace_player_team, n = 3, type = 's')
      sub_player$mov_avg_pace_team  <- get_lag_data(sub_player$mov_avg_pace_team )
      
      sub_player$sum_pace_opp <- cumsum(sub_player$t_pace_opp_team)
      sub_player$sum_pace_opp <- get_lag_data(sub_player$sum_pace_opp)
      sub_player$mov_avg_pace_opp <- movavg(sub_player$t_pace_opp_team, n = 3, type = 's')
      sub_player$mov_avg_pace_opp  <- get_lag_data(sub_player$mov_avg_pace_opp )
      
      sub_player$t_pace_player_team <- sub_player$t_pace_opp_team <- NULL
      
      # oeff
      sub_player$sum_oeff_team <- cumsum(sub_player$t_oeff_player_team)
      sub_player$sum_oeff_team<- get_lag_data(sub_player$sum_oeff_team)
      sub_player$mov_avg_oeff_team <- movavg(sub_player$t_oeff_player_team, n = 3, type = 's')
      sub_player$mov_avg_oeff_team  <- get_lag_data(sub_player$mov_avg_oeff_team )
      
      sub_player$sum_oeff_opp <- cumsum(sub_player$t_oeff_opp_team)
      sub_player$sum_oeff_opp <- get_lag_data(sub_player$sum_oeff_opp)
      sub_player$mov_avg_oeff_opp <- movavg(sub_player$t_oeff_opp_team, n = 3, type = 's')
      sub_player$mov_avg_oeff_opp  <- get_lag_data(sub_player$mov_avg_oeff_opp )
      
      sub_player$t_oeff_player_team <- sub_player$t_oeff_opp_team <- NULL
      
      # deff
      sub_player$sum_deff_team <- cumsum(sub_player$t_deff_player_team)
      sub_player$sum_deff_team<- get_lag_data(sub_player$sum_deff_team)
      sub_player$mov_avg_deff_team <- movavg(sub_player$t_deff_player_team, n = 3, type = 's')
      sub_player$mov_avg_deff_team  <- get_lag_data(sub_player$mov_avg_deff_team )
      
      sub_player$sum_deff_opp <- cumsum(sub_player$t_deff_opp_team)
      sub_player$sum_deff_opp <- get_lag_data(sub_player$sum_deff_opp)
      sub_player$mov_avg_deff_opp <- movavg(sub_player$t_deff_opp_team, n = 3, type = 's')
      sub_player$mov_avg_deff_opp  <- get_lag_data(sub_player$mov_avg_deff_opp )
      
      sub_player$t_deff_player_team <- sub_player$t_deff_opp_team <- NULL
      
      # # rest_days - here
      # sub_player$sum_rest_days_team <- cumsum(sub_player$t_rest_days_player_team)
      # sub_player$sum_rest_days_team<- get_lag_data(sub_player$sum_rest_days_team)
      # sub_player$mov_avg_rest_days_team <- movavg(sub_player$t_rest_days_player_team, n = 3, type = 's')
      # sub_player$mov_avg_rest_days_team  <- get_lag_data(sub_player$mov_avg_rest_days_team )
      # 
      # sub_player$sum_rest_days_opp <- cumsum(sub_player$t_rest_days_opp_team)
      # sub_player$sum_rest_days_opp <- get_lag_data(sub_player$sum_rest_days_opp)
      # sub_player$mov_avg_rest_days_opp <- movavg(sub_player$t_rest_days_opp_team, n = 3, type = 's')
      # sub_player$mov_avg_rest_days_opp  <- get_lag_data(sub_player$mov_avg_rest_days_opp )
      # 
      # sub_player$t_rest_days_player_team <- sub_player$t_rest_days_opp_team <- NULL
      # 
      # # get lagged and mov avg features for team spread, total, moneyline, win streak, real spread
      # 
      # # rest_days
      # sub_player$sum_rest_days_team <- cumsum(sub_player$t_rest_days_player_team)
      # sub_player$sum_rest_days_team <- get_lag_data(sub_player$sum_rest_days_team)
      # sub_player$mov_avg_rest_days_team <- movavg(sub_player$t_rest_days_player_team, n = 3, type = 's')
      # sub_player$mov_avg_rest_days_team  <- get_lag_data(sub_player$mov_avg_rest_days_team )
      # 
      # sub_player$sum_rest_days_opp <- cumsum(sub_player$t_rest_days_opp_team)
      # sub_player$sum_rest_days_opp <- get_lag_data(sub_player$sum_rest_days_opp)
      # sub_player$mov_avg_rest_days_opp <- movavg(sub_player$t_rest_days_opp_team, n = 3, type = 's')
      # sub_player$mov_avg_rest_days_opp  <- get_lag_data(sub_player$mov_avg_rest_days_opp )
      # 
      # sub_player$t_rest_days_player_team <- sub_player$t_rest_days_opp_team <- NULL
      # 
      
      # get lagged and moving avg features for f_minutes, f_usage_rate, f_position, f_salary,
      # f_fan_points, f_venue
      # minutes
      sub_player$sum_minutes_player <- cumsum(sub_player$f_minutes)
      sub_player$sum_minutes_player<- get_lag_data(sub_player$sum_minutes_player)
      sub_player$mov_avg_minutes_player <- movavg(sub_player$f_minutes, n = 3, type = 's')
      sub_player$mov_avg_minutes_player  <- get_lag_data(sub_player$mov_avg_minutes_player )
      
      # usage_rate
      sub_player$sum_usage_rate_player <- cumsum(sub_player$f_usage_rate)
      sub_player$sum_usage_rate_player<- get_lag_data(sub_player$sum_usage_rate_player)
      sub_player$mov_avg_usage_rate_player <- movavg(sub_player$f_usage_rate, n = 3, type = 's')
      sub_player$mov_avg_usage_rate_player  <- get_lag_data(sub_player$mov_avg_usage_rate_player )
      
      # salary
      sub_player$sum_salary_player <- cumsum(sub_player$f_salary)
      sub_player$sum_salary_player<- get_lag_data(sub_player$sum_salary_player)
      sub_player$mov_avg_salary_player <- movavg(sub_player$f_salary, n = 3, type = 's')
      sub_player$mov_avg_salary_player  <- get_lag_data(sub_player$mov_avg_salary_player )
      
      
      # fan_points
      sub_player$f_fan_points <- as.numeric(sub_player$f_fan_points)
      sub_player$sum_fan_points_player <- cumsum(sub_player$f_fan_points)
      sub_player$sum_fan_points_player<- get_lag_data(sub_player$sum_fan_points_player)
      sub_player$mov_avg_fan_points_player <- movavg(sub_player$f_fan_points, n = 3, type = 's')
      sub_player$mov_avg_fan_points_player  <- get_lag_data(sub_player$mov_avg_fan_points_player )
    
      # store in list
      player_list[[j]] <- sub_player
    }
    
   year_list[[i]] <- do.call('rbind', player_list)
   print(i)
    
  }
  final <- do.call('rbind', year_list)
  return(final)
  
}

# run function
fan_team <- featurize_fantasy_team_data(fan_team)

#
saveRDS(fan_team, '../data/cleaned_data/model_fan_team_data.rda')
