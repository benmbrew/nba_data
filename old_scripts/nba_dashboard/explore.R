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
source('../functions.R')

# get data folder
data_folder <- '../data/cleaned_data'

dat_game <- read.csv('data/nba-season-player-feed-feb.csv')
names(dat_game) <- c('dataset', 'date', 'player_name', 'position', 'player_team', 'opp_team', 'venue',
                     'starter', 'minutes', 'fg', 'fga', 'three_p', 'three_a', 'ft', 'fta', 'or', 'dr', 'tot_r',
                     'assists', 'personal_fouls', 'steals', 'to', 'bl', 'pts', 'usage_rate', 'days_rest')
# nba all stars


# West Starters
# Giannia Antetokounmpo
# Lebron James
# Stephen Curry
# James Harden
# Kevin Durant

# get minnesota data
minn <- dat_game[dat_game$player_team == 'Minnesota',]

# get lakers data
lakers <- dat_game[dat_game$player_team == 'LA Lakers',]

# get warriors data
gs <- dat_game[dat_game$player_team == 'Golden State',]

# get houston data
houston <- dat_game[dat_game$player_team == 'Houston',]

# get warriors data


# West Bench
# Russel Westbrook
# Victor Oladipo (injured)
# # Paul George
# Damian Lillard
# Klay Thomspon
# LaMarcus Aldridge
# Nicola Jokic

# bench
# get thunder
okc  <- dat_game[dat_game$player_team == 'Oklahoma City',]

# get trailblazers data
portland <- dat_game[dat_game$player_team == 'Portland',]

# warriors data 


# spurs data
spurs <- dat_game[dat_game$player_team == 'San Antonio',]

# nuggets data
nuggets <- dat_game[dat_game$player_team == 'Denver',]

# minnesota


# East Starters
# Kemba Walker
# Kyrie Irving 
# Kawhi Leonard
# Joe Embiid
# Dirk Nowitzki
# Ben Simmons


#indiana
pacers <- dat_game[dat_game$player_team == 'Indiana',]

# hornets 
hornets <- dat_game[dat_game$player_team == 'Charlotte',]

# celtics
boston <- dat_game[dat_game$player_team == 'Boston',]

# raptors 
raps <- dat_game[dat_game$player_team == 'Toronto',]

# mavs 
dallas <- dat_game[dat_game$player_team == 'Dallas']


# 76ers data
philly <- dat_game[dat_game$player_team == 'Philadelphia',]


# East Bench 
# Kyle Lowry
# Krish Middelton
# Bradley Beal
# Anthony Davis
# Blake Griffin
# Nikola Vucevic
# Dwayne Wade

# raptors 

# bucks 
bucks <- dat_game[dat_game$player_team == 'Milwaukee',]

# washington
wash <- dat_game[dat_game$player_team == 'Washington',]

# pelicans 
pelicans <- dat_game[dat_game$player_team == 'New Orleans',]

# pistons
pistons <- dat_game[dat_game$player_name == 'Detroit',]

# orlando 
orlando <- dat_game[dat_game$player_name == 'Orlando',]

# miami 
miami <- dat_game[dat_game$player_name == 'Miami',]




