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
registerDoParallel()
# source functions script 
source('functions.R')

# read in data
dat_2015 <- read_csv('../../Data/player_stat_2015_16.csv')
dat_2016 <- read_csv('../../Data/player_stat_2016_17.csv')
dat_current <- read_csv('../../Data/season_player_feed.csv')

# combine data
dat_full <- bind_rows(dat_2015, 
                      dat_2016,
                      dat_current)

rm(dat_2015, dat_2016, dat_current)

# clean column names 
colnames(dat_full) <- tolower(colnames(dat_full))

colnames(dat_full) <- gsub(' ', '_', colnames(dat_full))
colnames(dat_full) <- gsub('_(r/h)', '', colnames(dat_full), fixed = TRUE)

# convert date
dat_full$date <- as.Date(dat_full$date, format = '%m/%d/%Y')

# get year
dat_full$year <- as.factor(format(dat_full$date, format = '%Y'))

# create a month variable 
dat_full$month <- month(as.POSIXlt(dat_full$date))

# get percentages for fg, 3p and ft
dat_full$fg_per <- round(dat_full$fg/dat_full$fga, 2)

# calculate per minute statistics 
dat_full$fga_per <- round(dat_full$fga/dat_full$min, 2)
dat_full$pts_per <- round(dat_full$pts/dat_full$min, 2)
dat_full$to_per <- round(dat_full$to/dat_full$min, 2)


# game score GmsC - a simple version of the PER
dat_full$game_score <- get_game_score(dat_full)

# fill na and inf with zero
dat_full <- full_inf_with_na(dat_full)

# subset to only raptors in own_team
dat <- dat_full %>% dplyr::filter(own_team == 'Toronto')

# subset to this season 
dat <- dat %>% dplyr::filter(date > '2017/06/11')

# get avg per game and ass_to, bh, and ft_rate aggregated (total)
temp_game_score <- dat %>% group_by(player_full_name) %>%
  summarise(mean_fg_per = mean(fg_per, na.rm = T),
            mean_fga_per = mean(fga_per, na.rm = T), 
            mean_pts_per = mean(pts_per, na.rm = T),
            mean_to_per = mean(to_per, na.rm = T),
            mean_game_score = mean(game_score, na.rm = T))
         



