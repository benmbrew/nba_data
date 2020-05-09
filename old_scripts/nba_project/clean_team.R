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
dat_2014 <- read_csv('../data/team_data/team_stat_2014_15.csv')
dat_2015 <- read_csv('../data/team_data/team_stat_2015_16.csv')
dat_2016 <- read_csv('../data/team_data/team_stat_2016_17.csv')
dat_2017 <- read_csv('../data/team_data/team_stat_2017_18.csv')
dat_current <- read_csv('../data/team_data/nba-season-team-feed_april.csv')

##########
# recode current data to match all other dat
##########

# remove URL from names
names(dat_current) <- trimws(gsub('URL', '', names(dat_current)), which = 'both')
names(dat_current) <- trimws(gsub('\n', ' ', names(dat_current)), which = 'both')

# remove OT5 
dat_current$OT5 <- NULL

# recode team
names(dat_current)[4] <- 'TEAMS'

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
# dat_current$closing_spread <- dat_current$opening_spread
# dat_current$closing_total <- dat_current$opening_total

# change names of 'x player' columns to match other data
names(dat_current)[38:41] <- c('x37', 'x38', 'x39', 'x40')

# remove game id and line movement columns
dat_current$`game-id` <- 
  dat_current$`line__movement_#1` <- 
  dat_current$`line__movement_#2` <- 
  dat_current$`line__movement_#3` <- NULL

# combine data
dat <- rbind(dat_2014,
             dat_2015,
             dat_2016,
             dat_2017,
             dat_current)
rm(dat_2014,
      dat_2015,
      dat_2016,
      dat_2017,
      dat_current)

# add a t in front of every column name
names(dat) <- paste0('t_', names(dat))

# recode venue
dat$t_venue <- ifelse(dat$t_venue == 'Road', 'R',
                         ifelse(dat$t_venue == 'Home', 'H', dat$t_venue))

# remove other columns
dat$t_odds <- dat$t_box_score <- dat$t_halftime <- dat$t_closing_odds <-
  dat$t_opening_odds <- NULL

# remove 'x' from player lineup
names(dat) <- gsub('t_starting_lineups', 't_starter_1', names(dat))
names(dat) <- gsub('t_x37', 't_starter_2', names(dat))
names(dat) <- gsub('t_x38', 't_starter_3', names(dat))
names(dat) <- gsub('t_x39', 't_starter_4', names(dat))
names(dat) <- gsub('t_x40', 't_starter_5', names(dat))

# # fix referee data
# temp_dat <- dat
# i = 1
# get_ref_data <- function(temp_dat){
#   for(i in 1:nrow(temp_dat)){
#     sub_row <- temp_dat[i,]
#     if(is.na(sub_row$t_main_ref)){
#       
#       
#     }
#   }
#   
# }

# save data
saveRDS(dat, '../data/cleaned_data/cleaned_team_data.rda')
