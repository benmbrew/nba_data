# this script will look at which teams underperformed the odds and over performed, as well as under and over 
# counts

# run global player script
source('../global_player.R')

library(reshape2)
# remove unneeded columns
dat_team$ot1 <- dat_team$ot2 <- dat_team$ot3 <- dat_team$ot4<- dat_team$ot5 <- NULL
dat_team$opening_odds <- dat_team$opening_spread <- dat_team$opening_total <- dat_team$moneyline <- NULL
dat_team$halftime <- dat_team$box_score <- dat_team$odds <- dat_team$closing_odds <-  NULL
# dat_17 <- read.csv('data/player_stat_2016_17.csv')
# dat_16 <- read.csv('data/player_stat_2015_16.csv')

# get complete cases
dat_team <- dat_team[!is.na(dat_team$first_q),]

names(dat_team) <- c('dataset', 'date', 'teams', 'venue', 'first', 'second', 'third', 'fourth','final',
                     'minutes_team', 'fg_team', 'fga_team', 'three_p_team', 'three_a_team', 
                     'ft_team', 'fta_team', 'or_team', 'dr_team', 'tot_r_team', 'assists_team', 'person_foul_team',
                     'steals_team', 'to_team', 'to_to_team', 'bl_team','points', 'poss', 'pace','off_ef', 'def_ef',
                     'days_rest_team', 'player_1', 'player_2',  'player_3', 'player_4', 'player_5', 'main_ref', 
                     'crew', 'game_num', 'win_loss','real_spread', 'closing_spread', 'closing_total', 'underdog_team', 
                     'tot_game_num', 'real_total', 'over_under_team',  'team_game_num','year', 'month', 'fg_per', 'ft_per', 'three_per',
                     'per_first', 'per_second', 'per_third', 'per_fourth')

# # get two columns for crew. 
# dat_team <- get_by_game(dat_team)

# multiply spread by -1
dat_team$closing_spread <- (dat_team$closing_spread)*(-1)

# make a cover or not cover for spread
dat_team$cover_spread <- ifelse(dat_team$real_spread >= dat_team$closing_spread, 'covered', 'did_not_cover')

# change name for refs
names(dat_team)[names(dat_team) == 'main_ref_away'] <- 'main_ref'
names(dat_team)[names(dat_team) == 'crew_home'] <- 'crew_1'
names(dat_team)[names(dat_team) == 'crew_away'] <- 'crew_2'

# group by team and get cover and not cover counts for spread and over under and..
# fg percentage, three percentage, ft percentage, mean wins, mean points by quarter, 
# mean total points, mean off eff, mean deff eff, mean total rebounds, mean assists,
# mean poss, sum of rest days, sum of underdog stats, sum of over under stats, 
dat_team$teams_home <- as.factor(dat_team$teams_home)
dat_team$teams_away <- as.factor(dat_team$teams_away)

# # make data long so that teams is one columns
# dat_long <- gather(dat_team, key = 'home_away', value = 'team', 2:53)
# dat_long <- melt(dat_team, id.vars)

# group by home team and away team
temp_home <- dat_team %>%
  group_by(teams_home) %>%
  summarise(counts = n(),
            home_sum_covered = sum(cover_spread_home == 'covered', na.rm = TRUE),
            home_sum_not_covered = sum(cover_spread_home == 'did_not_cover', na.rm = TRUE),
            home_sum_under = sum(over_under_team_home == 'under', na.rm = TRUE),
            home_sum_over = sum(over_under_team_home == 'over', na.rm = TRUE),
            home_sum_even = sum(over_under_team_home == 'even', na.rm = TRUE),
            away_sum_under = sum(over_under_team_away == 'under', na.rm = TRUE),
            away_sum_over = sum(over_under_team_away == 'over', na.rm = TRUE),
            away_sum_even = sum(over_under_team_away == 'even', na.rm = TRUE),
            home_sum_win = sum(win_loss_home == 'W', na.rm = TRUE),
            away_sum_win = sum(win_loss_away == 'L',na.rm = TRUE),
            home_sum_rest_last_season = sum(days_rest_team_home == 'last_season', na.rm = TRUE),
            away_sum_rest_last_season = sum(days_rest_team_away == 'last_season', na.rm = TRUE),
            home_sum_rest_b2b = sum(days_rest_team_home == 'B2B', na.rm = TRUE),
            away_sum_rest_b2b = sum(days_rest_team_away == 'B2B', na.rm = TRUE),
            home_sum_rest_1 = sum(days_rest_team_home == '1', na.rm = TRUE),
            away_sum_rest_1 = sum(days_rest_team_away == '1', na.rm = TRUE),
            home_sum_rest_2 = sum(days_rest_team_home == '2', na.rm = TRUE),
            away_sum_rest_2 = sum(days_rest_team_away == '2', na.rm = TRUE),
            home_sum_rest_3_in_4_b2b = sum(days_rest_team_home == '3IN4-B2B', na.rm = TRUE),
            away_sum_rest_3_in_4_b2b = sum(days_rest_team_away == '3IN4-B2B', na.rm = TRUE),
            home_sum_rest_4_in_5 = sum(days_rest_team_home == '4IN5', na.rm = TRUE),
            away_sum_rest_4_in_5 = sum(days_rest_team_away == '4IN5', na.rm = TRUE),
            home_sum_rest_3_in_4 = sum(days_rest_team_home == '3IN4', na.rm = TRUE),
            away_sum_rest_3_in_4 = sum(days_rest_team_away == '3IN4', na.rm = TRUE),
            home_sum_rest_3_plus = sum(days_rest_team_home == '3+', na.rm = TRUE),
            away_sum_rest_3_plus = sum(days_rest_team_away == '3+', na.rm = TRUE),
            home_sum_fav_win = sum(underdog_team_home == 'favorite_win', na.rm = TRUE),
            home_sum_underdog_loss = sum(underdog_team_home == 'underdog_loss', na.rm = TRUE),
            home_sum_straight_win = sum(underdog_team_home == 'straight_win', na.rm = TRUE),
            home_sum_fav_loss = sum(underdog_team_home == 'favorite_loss', na.rm = TRUE),
            home_sum_underdog_win = sum(underdog_team_home == 'underdog_win', na.rm = TRUE),
            home_sum_straight_loss = sum(underdog_team_home == 'straight_loss', na.rm = TRUE),
            home_mean_fg_per = mean(fg_per_home, na.rm = TRUE),
            away_mean_fg_per = mean(fg_per_away, na.rm = TRUE),
            home_mean_three_per = mean(three_per_home, na.rm = TRUE),
            away_mean_three_per = mean(three_per_away, na.rm = TRUE),
            home_mean_ft_per = mean(ft_per_home, na.rm = TRUE),
            away_mean_ft_per = mean(ft_per_away, na.rm = TRUE),
            home_mean_first_quarter = mean(first_home, na.rm = TRUE),
            away_mean_first_quarter = mean(first_away, na.rm = TRUE),
            home_mean_second_quarter = mean(second_home, na.rm = TRUE),
            away_mean_second_quarter = mean(second_away, na.rm = TRUE),
            home_mean_third_quarter = mean(third_home, na.rm = TRUE),
            away_mean_third_quarter = mean(third_away, na.rm = TRUE),
            home_mean_fourth_quarter = mean(fourth_home, na.rm = TRUE),
            away_mean_fourth_quarter = mean(fourth_away, na.rm = TRUE),
            home_mean_total_points = mean(final_home, na.rm = TRUE),
            away_mean_total_points = mean(final_away, na.rm = TRUE),
            home_mean_off_eff = mean(off_ef_home, na.rm = TRUE),
            away_mean_off_eff = mean(off_ef_away, na.rm = TRUE),
            home_mean_def_eff = mean(def_ef_home, na.rm = TRUE),
            away_mean_def_eff = mean(def_ef_away, na.rm = TRUE),
            home_mean_total_rebounds = mean(tot_r_team_home, na.rm = TRUE),
            away_mean_total_rebounds = mean(tot_r_team_away, na.rm = TRUE),
            home_mean_assists = mean(assists_team_home, na.rm = TRUE),
            away_mean_assists = mean(assists_team_away, na.rm = TRUE),
            home_mean_poss = mean(poss_home, na.rm = TRUE),
            away_mean_poss = mean(poss_away, na.rm = TRUE),
            home_mean_pace = mean(pace_home, na.rm = TRUE),
            away_mean_pace = mean(pace_away, na.rm = TRUE))
  

temp_away <- dat_team %>%
  group_by(teams_away) %>%
  summarise(counts = n(),
            home_sum_covered = sum(cover_spread_home == 'covered', na.rm = TRUE),
            home_sum_not_covered = sum(cover_spread_home == 'did_not_cover', na.rm = TRUE),
            home_sum_under = sum(over_under_team_home == 'under', na.rm = TRUE),
            home_sum_over = sum(over_under_team_home == 'over', na.rm = TRUE),
            home_sum_even = sum(over_under_team_home == 'even', na.rm = TRUE),
            away_sum_under = sum(over_under_team_away == 'under', na.rm = TRUE),
            away_sum_over = sum(over_under_team_away == 'over', na.rm = TRUE),
            away_sum_even = sum(over_under_team_away == 'even', na.rm = TRUE),
            home_sum_win = sum(win_loss_home == 'W', na.rm = TRUE),
            away_sum_win = sum(win_loss_away == 'L',na.rm = TRUE),
            home_sum_rest_last_season = sum(days_rest_team_home == 'last_season', na.rm = TRUE),
            away_sum_rest_last_season = sum(days_rest_team_away == 'last_season', na.rm = TRUE),
            home_sum_rest_b2b = sum(days_rest_team_home == 'B2B', na.rm = TRUE),
            away_sum_rest_b2b = sum(days_rest_team_away == 'B2B', na.rm = TRUE),
            home_sum_rest_1 = sum(days_rest_team_home == '1', na.rm = TRUE),
            away_sum_rest_1 = sum(days_rest_team_away == '1', na.rm = TRUE),
            home_sum_rest_2 = sum(days_rest_team_home == '2', na.rm = TRUE),
            away_sum_rest_2 = sum(days_rest_team_away == '2', na.rm = TRUE),
            home_sum_rest_3_in_4_b2b = sum(days_rest_team_home == '3IN4-B2B', na.rm = TRUE),
            away_sum_rest_3_in_4_b2b = sum(days_rest_team_away == '3IN4-B2B', na.rm = TRUE),
            home_sum_rest_4_in_5 = sum(days_rest_team_home == '4IN5', na.rm = TRUE),
            away_sum_rest_4_in_5 = sum(days_rest_team_away == '4IN5', na.rm = TRUE),
            home_sum_rest_3_in_4 = sum(days_rest_team_home == '3IN4', na.rm = TRUE),
            away_sum_rest_3_in_4 = sum(days_rest_team_away == '3IN4', na.rm = TRUE),
            home_sum_rest_3_plus = sum(days_rest_team_home == '3+', na.rm = TRUE),
            away_sum_rest_3_plus = sum(days_rest_team_away == '3+', na.rm = TRUE),
            home_sum_fav_win = sum(underdog_team_home == 'favorite_win', na.rm = TRUE),
            home_sum_underdog_loss = sum(underdog_team_home == 'underdog_loss', na.rm = TRUE),
            home_sum_straight_win = sum(underdog_team_home == 'straight_win', na.rm = TRUE),
            home_sum_fav_loss = sum(underdog_team_home == 'favorite_loss', na.rm = TRUE),
            home_sum_underdog_win = sum(underdog_team_home == 'underdog_win', na.rm = TRUE),
            home_sum_straight_loss = sum(underdog_team_home == 'straight_loss', na.rm = TRUE),
            home_mean_fg_per = mean(fg_per_home, na.rm = TRUE),
            away_mean_fg_per = mean(fg_per_away, na.rm = TRUE),
            home_mean_three_per = mean(three_per_home, na.rm = TRUE),
            away_mean_three_per = mean(three_per_away, na.rm = TRUE),
            home_mean_ft_per = mean(ft_per_home, na.rm = TRUE),
            away_mean_ft_per = mean(ft_per_away, na.rm = TRUE),
            home_mean_first_quarter = mean(first_home, na.rm = TRUE),
            away_mean_first_quarter = mean(first_away, na.rm = TRUE),
            home_mean_second_quarter = mean(second_home, na.rm = TRUE),
            away_mean_second_quarter = mean(second_away, na.rm = TRUE),
            home_mean_third_quarter = mean(third_home, na.rm = TRUE),
            away_mean_third_quarter = mean(third_away, na.rm = TRUE),
            home_mean_fourth_quarter = mean(fourth_home, na.rm = TRUE),
            away_mean_fourth_quarter = mean(fourth_away, na.rm = TRUE),
            home_mean_total_points = mean(final_home, na.rm = TRUE),
            away_mean_total_points = mean(final_away, na.rm = TRUE),
            home_mean_off_eff = mean(off_ef_home, na.rm = TRUE),
            away_mean_off_eff = mean(off_ef_away, na.rm = TRUE),
            home_mean_def_eff = mean(def_ef_home, na.rm = TRUE),
            away_mean_def_eff = mean(def_ef_away, na.rm = TRUE),
            home_mean_total_rebounds = mean(tot_r_team_home, na.rm = TRUE),
            away_mean_total_rebounds = mean(tot_r_team_away, na.rm = TRUE),
            home_mean_assists = mean(assists_team_home, na.rm = TRUE),
            away_mean_assists = mean(assists_team_away, na.rm = TRUE),
            home_mean_poss = mean(poss_home, na.rm = TRUE),
            away_mean_poss = mean(poss_away, na.rm = TRUE),
            home_mean_pace = mean(pace_home, na.rm = TRUE),
            away_mean_pace = mean(pace_away, na.rm = TRUE))

# add an indicator to both data sets and then combine
temp_away$home_away <- 'away'
temp_home$home_away <- 'home'

# renames teams_home and teams_away
names(temp_home)[1] <- 'team'
names(temp_away)[1] <- 'team'

# combine 
temp_all <- rbind(temp_home,
                  temp_away)
