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
library(ggplot2)
library(ggthemes)

# source functions script 
source('../functions.R')
source('../global.R')

# change name of team data set
dat_team <- dat_game
rm(dat_game)


# read in player data
dat_2016 <- read.csv('../data/player_stat_2015_16.csv')
dat_2017 <- read.csv('../data/player_stat_2016_17.csv')
dat_2018 <- read.csv('../data/player_stat_2018_reg.csv')
dat_player <- read.csv('../data/player.csv')

# for now remove playoff data
dat_2016 <- dat_2016[!grepl('Playoffs', dat_2016$DATA.SET),]
dat_2017 <- dat_2017[!grepl('Playoffs', dat_2017$DATA.SET),]
dat_2018 <- dat_2018[!grepl('Playoffs', dat_2018$DATA.SET),]

# make all lower casse
names(dat_2016) <- tolower(names(dat_2016))
names(dat_2017) <- tolower(names(dat_2017))
names(dat_2018) <- tolower(names(dat_2018))
names(dat_player) <- tolower(names(dat_player))

# add starter, usage_rate, rest_days to all datasets except current
dat_2016$starter <- dat_2016$usage_rate <- dat_2016$rest_days <- NA
dat_2017$starter <- dat_2017$usage_rate <- dat_2017$rest_days <- NA
dat_2018$starter <- dat_2018$usage_rate <- dat_2018$rest_days <- NA

# create new names 
names(dat_2016) <- c('dataset', 'date', 'player_name', 'position', 'player_team', 'opp_team', 'venue',
                       'minutes', 'fg', 'fga', 'three_p', 'three_a', 'ft', 'fta', 'or', 'dr', 
                       'tot_r', 'assists', 'personal_fouls', 'steals', 'to', 'bl', 'pts', 
                       'days_rest', 'usage_rate', 'starter')

names(dat_2017) <- c('dataset', 'date', 'player_name', 'position', 'player_team', 'opp_team', 'venue',
                     'minutes', 'fg', 'fga', 'three_p', 'three_a', 'ft', 'fta', 'or', 'dr', 
                     'tot_r', 'assists', 'personal_fouls', 'steals', 'to', 'bl', 'pts', 
                     'days_rest', 'usage_rate', 'starter')

names(dat_2018) <- c('dataset', 'date', 'player_name', 'position', 'player_team', 'opp_team', 'venue',
                     'minutes', 'fg', 'fga', 'three_p', 'three_a', 'ft', 'fta', 'or', 'dr', 
                     'tot_r', 'assists', 'personal_fouls', 'steals', 'to', 'bl', 'pts', 
                     'days_rest', 'usage_rate', 'starter')

names(dat_player) <- c('dataset', 'date', 'player_name', 'position', 'player_team', 'opp_team', 'venue', 'starter',
                     'minutes', 'fg', 'fga', 'three_p', 'three_a', 'ft', 'fta', 'or', 'dr', 
                     'tot_r', 'assists', 'personal_fouls', 'steals', 'to', 'bl', 'pts', 'usage_rate',
                     'days_rest')

# combine data into one dataset
dat_player <- rbind(dat_2016,
                    dat_2017,
                    dat_2018, 
                    dat_player)
rm(dat_2016,
   dat_2017,
   dat_2018)

rm(data_folder, 
   remove_these,
   team_stats)
