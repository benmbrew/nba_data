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
dat_1 <- read.csv('../data/fantasy_data/dfs_2016_2017.csv', stringsAsFactors =  FALSE)
dat_2 <- read.csv('../data/fantasy_data/dfs_2017_2018.csv', stringsAsFactors =  FALSE)
dat_3 <- read.csv('../data/fantasy_data/dfs_in_season_april.csv', stringsAsFactors =  FALSE)

# remove rows and colunms to homogenize column names
dat_1 <- dat_1[-1,]
dat_1$X <- dat_1$X.1 <- dat_1$X.2 <- dat_1$X.3 <- dat_1$X.4 <- dat_1$X.5 <- NULL
dat_2$position_fd <- dat_2$position_yahoo <- dat_2$fantasy_points_fd <- dat_2$fantasy_points_yahoo <-
  dat_2$salary_yahoo <- dat_2$salary_fd <- NULL
dat_3$GAME.ID <- dat_3$PLAYER.ID <- dat_3$STARTER..Y.N. <- dat_3$YAHOO.2 <- dat_3$FANDUEL.2 <- 
  dat_3$YAHOO.1 <- dat_3$FANDUEL.1 <- dat_3$FANDUEL <- dat_3$YAHOO <- dat_3$DAYS.REST <-  NULL

# assign new column names
names(dat_1) <- 
  names(dat_2) <- 
  names(dat_3) <- c('f_dataset', 'f_date','f_player', 'f_team', 'f_opp_team', 'f_venue', 'f_minutes',
                  'f_usage_rate', 'f_position', 'f_salary', 'f_fan_points')


# combine data
dat <- rbind(dat_1,
             dat_2,
             dat_3)

rm(dat_1, dat_2, dat_3)

saveRDS(dat, '../data/cleaned_data/cleaned_fantasy_data.rda')
