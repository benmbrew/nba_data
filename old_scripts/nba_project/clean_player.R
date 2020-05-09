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

# read in player data
dat_1 <- read.csv('../data/player_data/player_stat_2015_16.csv')
dat_2 <- read.csv('../data/player_data/player_stat_2016_17.csv')
dat_3 <- read.csv('../data/player_data/player_stat_2017_2018.csv')
dat_4 <- read.csv('../data/player_data/nba-season-player-feed_april.csv')

# remove columns that are in 2019 that dont exist in other
dat_4$game_id <- dat_4$player_id <- dat_4$starter <- dat_4$usage_rate <- dat_4$days_rest <-
  NULL
# make all lower casse
names(dat_1) <- tolower(names(dat_1))
names(dat_2) <- tolower(names(dat_2))
names(dat_3) <- tolower(names(dat_3))
names(dat_4) <- tolower(names(dat_4))


# create new names 
names(dat_1) <- names(dat_2) <- names(dat_3) <- names(dat_4) <-
  c('p_dataset', 'p_date', 'p_player_name', 'p_position', 'p_player_team', 'p_opp_team', 'p_venue',
    'p_minutes', 'p_fg', 'p_fga', 'p_three_p', 'p_three_a', 'p_ft', 'p_fta', 'p_or', 'p_dr', 
    'p_tot_r', 'p_assists', 'p_personal_fouls', 'p_steals', 'p_to', 'p_bl', 'p_pts')

# combine data into one dataset
dat <- rbind(dat_1,
             dat_2,
             dat_3, 
             dat_4)


# save clean player data
saveRDS(dat, '../data/cleaned_data/cleaned_player_data.rda')
