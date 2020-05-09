# this script will analyze play by play data
# load libraries
library(tidyverse)
library(ggplot2)
library(lattice)
library(pracma)
library(reshape2)
library(ggthemes)


# read in pbp data
dat <- read.csv('data/pbp_data/combined/may_25th_combined.csv', na.strings = '', stringsAsFactors = FALSE)

# subset data by removing rows where result is NA (only keep shots)
dat <- dat[!is.na(dat$result), ]

# subset by danny green
tor <- dat[dat$team == 'TOR',]

# create a win column 
# group

# look at shot distance for each player and examine wins, made shots
# group by player, 

# group by assist
# summarise mean shot_distance and made shots
temp_1 <- tor %>%
  filter(event_type != 'free throw') %>%
  group_by(player, assist) %>%
  summarise(mean_shot_distance = mean(shot_distance),
            sum_made_shot = sum(result == 'made', na.rm = TRUE))

temp_1$assist <- ifelse(is.na(temp_1$assist), 'no_assist', temp_1$assist)
temp_1$per_made <- round(temp_1$sum_made_shot/sum(temp_1$sum_made_shot), 3)

# # group by player and assist and summarise mean shot distance and sum of made shots
temp_2 <- dat %>%
  filter(event_type != 'free throw') %>%
  filter(!is.na(player)) %>%
  group_by(player) %>%
  summarise(mean_shot_distance = mean(shot_distance),
            sum_made_shot = sum(result == 'made', na.rm = TRUE),
            sum_missed_shot = sum(result == 'missed', na.rm = TRUE),
            tot_shots = sum_made_shot + sum_missed_shot,
            per_made = round(sum_made_shot/tot_shots, 3))

summary(lm(per_made ~ mean_shot_distance, data = temp_2))

# how predictive is the slop of shot distance vs percent made in predicting team wins??