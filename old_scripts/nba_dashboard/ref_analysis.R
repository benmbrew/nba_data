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

# get two columns for crew. 
dat_team <- get_by_game(dat_team)

# remove unneded columns again
dat_team$main_ref_home <- dat_team$closing_spread_away <- dat_team$closing_total_away <- 
  dat_team$real_spread_away <- dat_team$real_total_away <- dat_team$date_away <-
  dat_team$month_away <- dat_team$year_away <- NULL

# multiply home spread by -1
dat_team$closing_spread_home <- (dat_team$closing_spread_home)*-1

# make a cover or not cover for spread
dat_team$cover_spread_home <- ifelse(dat_team$real_spread_home >= dat_team$closing_spread_home, 'covered', 'did_not_cover')

# change name for refs
names(dat_team)[names(dat_team) == 'main_ref_away'] <- 'main_ref'
names(dat_team)[names(dat_team) == 'crew_home'] <- 'crew_1'
names(dat_team)[names(dat_team) == 'crew_away'] <- 'crew_2'

############
# group by ref
############

# use dplyr to get different datasets characterized by the group by variable
by_main <- dat_team %>%
  group_by(main_ref) %>%
  summarise(home_win = sum(win_loss_home == 'W'),
            home_lose = sum(win_loss_home == 'L'),
            home_fouls = sum(person_foul_team_home),
            away_fouls = sum(person_foul_team_away),
            home_mean_closing_spread = round(mean(closing_spread_home, na.rm = TRUE), 3),
            home_mean_real_spread = round(mean(real_spread_home, na.rm = TRUE), 3),
            home_mean_closing_total = round(mean(closing_total_home, na.rm = TRUE), 3),
            home_mean_real_total = round(mean(real_total_home, na.rm = TRUE), 3),
            ou_over = sum(over_under_team_home == 'over'),
            ou_under = sum(over_under_team_home == 'under'),
            ou_even = sum(over_under_team_home == 'even'),
            home_fav_win = sum(underdog_team_home == 'favorite_win'),
            home_underdog_loss = sum(underdog_team_home == 'underdog_loss'),
            home_straight_win = sum(underdog_team_home == 'straight_win'),
            home_fav_loss = sum(underdog_team_home == 'favorite_loss'),
            home_underdog_win = sum(underdog_team_home == 'underdog_win'),
            home_straight_loss = sum(underdog_team_home == 'straight_loss'),
            home_covered = sum(cover_spread_home == 'covered'),
            home_not_cover = sum(cover_spread_home == 'did_not_cover'),
            counts = n()) %>%
  filter(counts >= 50)

# difference between home and away: (1) win and lose, (2) fouls, 

# get percent (1) home covered and home not covered, (2) percent home_fav_win,, (3) percent home_underdog_loss,
# (4) percent home_straight_win, (5) percent home_fav_loss, (6) percent home_underdog_win, (7) home_straight_loss

# get total fouls 
by_main$tot_fouls <- by_main$home_fouls + by_main$away_fouls

# get percentage of home fouls
by_main$per_home_fouls <- round((by_main$home_fouls/by_main$tot_fouls)*100, 2)

# (1) 
by_main$per_home_covered <- round(((by_main$home_covered/by_main$counts)*100), 2)
by_main$per_away_covered <- round(100 - by_main$per_home_covered, 2)

# (2)
by_main$per_home_fav_win <- round((by_main$home_fav_win/by_main$counts)*100, 2)

# (3)
by_main$per_home_ud_loss <- round(((by_main$home_underdog_loss/by_main$counts)*100), 2)

# (4)
by_main$per_home_straight_win <- round(((by_main$home_straight_win/by_main$counts)*100), 2)

# (5) 
by_main$per_home_fav_loss <- round(((by_main$home_fav_loss/by_main$counts)*100), 2)

# (6) 
by_main$per_home_ud_win <- round(((by_main$home_underdog_win/by_main$counts)*100), 2)

# (7) 
by_main$per_home_straight_win <- round(((by_main$home_straight_loss/by_main$counts)*100), 2)


# get home win diff
by_main$home_win_diff <- by_main$home_win - by_main$home_lose

# get percent of home games won
by_main$per_home_wins <- round((by_main$home_win/by_main$counts), 2)

# group by each game and get same stats as above for comparison
by_main_home <- dat_team %>%
  group_by(teams_home) %>%
  summarise(home_win = sum(win_loss_home == 'W'),
            home_lose = sum(win_loss_home == 'L'),
            home_fouls = sum(person_foul_team_home),
            away_fouls = sum(person_foul_team_away),
            home_mean_closing_spread = round(mean(closing_spread_home, na.rm = TRUE), 3),
            home_mean_real_spread = round(mean(real_spread_home, na.rm = TRUE), 3),
            home_mean_closing_total = round(mean(closing_total_home, na.rm = TRUE), 3),
            home_mean_real_total = round(mean(real_total_home, na.rm = TRUE), 3),
            ou_over = sum(over_under_team_home == 'over'),
            ou_under = sum(over_under_team_home == 'under'),
            ou_even = sum(over_under_team_home == 'even'),
            home_fav_win = sum(underdog_team_home == 'favorite_win'),
            home_underdog_loss = sum(underdog_team_home == 'underdog_loss'),
            home_straight_win = sum(underdog_team_home == 'straight_win'),
            home_fav_loss = sum(underdog_team_home == 'favorite_loss'),
            home_underdog_win = sum(underdog_team_home == 'underdog_win'),
            home_straight_loss = sum(underdog_team_home == 'straight_loss'),
            home_covered = sum(cover_spread_home == 'covered'),
            home_not_cover = sum(cover_spread_home == 'did_not_cover'),
            counts = n()) %>%
  filter(counts >= 50)



# get total fouls 
by_main_home$tot_fouls <- by_main_home$home_fouls + by_main_home$away_fouls

by_main_home$per_home_fouls <- round((by_main_home$home_fouls/by_main_home$tot_fouls)*100, 2)

# new columns
# (1) 
by_main_home$per_home_covered <- round(((by_main_home$home_covered/by_main_home$counts)*100), 2)
by_main_home$per_away_covered <- round(100 - by_main_home$per_home_covered, 2)

# (2)
by_main_home$per_home_fav_win <- round((by_main_home$home_fav_win/by_main_home$counts)*100, 2)

# (3)
by_main_home$per_home_ud_loss <- round(((by_main_home$home_underdog_loss/by_main_home$counts)*100), 2)

# (4)
by_main_home$per_home_straight_win <- round(((by_main_home$home_straight_win/by_main_home$counts)*100), 2)

# (5) 
by_main_home$per_home_fav_loss <- round(((by_main_home$home_fav_loss/by_main_home$counts)*100), 2)

# (6) 
by_main_home$per_home_ud_win <- round(((by_main_home$home_underdog_win/by_main_home$counts)*100), 2)

# (7) 
by_main_home$per_home_straight_win <- round(((by_main_home$home_straight_loss/by_main_home$counts)*100), 2)


# get home win diff
by_main_home$home_win_diff <- by_main_home$home_win - by_main_home$home_lose

# get percent of home games won
by_main_home$per_home_wins <- round((by_main_home$home_win/by_main_home$counts), 2)

# for both datasets get difference or percentage of variable in respect to counts
# (ie percent home_covered, percent_fouls)


#########
#
#########
# get total counts for each ref
plot_bar(by_main, 
         x_var = 'main_ref', 
         y_var = 'counts',
         x_lab = 'Referee',
         y_lab = 'Total home Wins')


# get percent home team wins
plot_bar(by_main, 
         x_var = 'main_ref', 
         y_var = 'per_home_wins',
         x_lab = 'Referee',
         y_lab = 'Total home Wins')

# get percent home team wins
plot_bar(by_main, 
         x_var = 'main_ref', 
         y_var = 'per_home_wins',
         x_lab = 'Referee',
         y_lab = '% home Wins')
# get percent home fouls
plot_bar(by_main, 
         x_var = 'main_ref', 
         y_var = 'per_home_fouls',
         x_lab = 'Referee',
         y_lab = '% home fouls')

# get percent home covered 
plot_bar(by_main, 
         x_var = 'main_ref', 
         y_var = 'per_home_covered',
         x_lab = 'Referee',
         y_lab = '% home covered')




# use dplyr to get different datasets characterized by the group by variable
by_refs <- dat_team %>%
  group_by(main_ref,crew_1, crew_2, year_home, month_home) %>%
  summarise(home_win = sum(win_loss_home == 'W'),
            home_lose = sum(win_loss_home == 'L'),
            home_fouls = sum(person_foul_team_home),
            away_fouls = sum(person_foul_team_away),
            home_mean_closing_spread = round(mean(closing_spread_home, na.rm = TRUE), 3),
            home_mean_closing_total = round(mean(closing_total_home, na.rm = TRUE), 3),
            ou_over = sum(over_under_team_home == 'over'),
            ou_under = sum(over_under_team_home == 'under'),
            ou_even = sum(over_under_team_home == 'even'),
            home_fav_win = sum(underdog_team_home == 'favorite_win'),
            home_underdog_loss = sum(underdog_team_home == 'underdog_loss'),
            home_straight_win = sum(underdog_team_home == 'straight_win'),
            home_fav_loss = sum(underdog_team_home == 'favorite_loss'),
            home_underdog_win = sum(underdog_team_home == 'underdog_win'),
            home_straight_loss = sum(underdog_team_home == 'straight_loss'),
            home_covered = sum(cover_spread_home == 'covered'),
            home_not_cover = sum(cover_spread_home == 'did_not_cover'),
            counts = n())



# group by main_ref and crew and summarise data
by_all <- dat_team %>%
  group_by(main_ref, crew_1, crew_2, teams_home, teams_away, year_home, month_home) %>%
  summarise(home_win = sum(win_loss_home == 'W'),
            home_lose = sum(win_loss_home == 'L'),
            home_fouls = sum(person_foul_team_home),
            away_fouls = sum(person_foul_team_away),
            home_mean_closing_spread = mean(closing_spread_home, na.rm = TRUE),
            home_mean_closing_total = mean(closing_total_home, na.rm = TRUE),
            ou_over = sum(over_under_team_home == 'over'),
            ou_under = sum(over_under_team_home == 'under'),
            ou_even = sum(over_under_team_home == 'even'),
            home_fav_win = sum(underdog_team_home == 'favorite_win'),
            home_underdog_loss = sum(underdog_team_home == 'underdog_loss'),
            home_straight_win = sum(underdog_team_home == 'straight_win'),
            home_fav_loss = sum(underdog_team_home == 'favorite_loss'),
            home_underdog_win = sum(underdog_team_home == 'underdog_win'),
            home_straight_loss = sum(underdog_team_home == 'straight_loss'),
            home_covered = sum(cover_spread_home == 'covered'),
            home_not_cover = sum(cover_spread_home == 'did_not_cover'),
            counts = n())




############
# Question 2: are refs super star biased - ie is there a strong relationship between pts scored (super stars)
# and fouls called.
############

