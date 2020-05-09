# this script will analyze raptors vs warriors finals game 

# load libraries
library(tidyverse)
library(ggplot2)
library(lattice)
library(pracma)
library(reshape2)
library(ggthemes)

# run functions.R scripts
source('functions.R')


# read in team datsa
dat_team <- read.csv('data/team_data/nba-season-team-feed_final.csv')
dat_player <- read.csv('data/player_data/nba-season-player-feed_finals.csv')

# recode three point column
names(dat_team)[names(dat_team) == 'X3P'] <- 'three_p'
names(dat_team)[names(dat_team) == 'X3PA'] <- 'three_pa'


# remove and recode columns for team data
dat_team$LINE..MOVEMENT..1 <- dat_team$LINE..MOVEMENT..2 <- dat_team$LINE..MOVEMENT..3 <- dat_team$CLOSING.ODDS <- 
  dat_team$OPENING.ODDS <- dat_team$BOX.SCORE.URL <- dat_team$ODDS.URL <-  NULL
names(dat_team) <- trimws(gsub('URL', '', names(dat_team)), which = 'both')
names(dat_team) <- trimws(gsub('\n', ' ', names(dat_team)), which = 'both')
names(dat_team) <- gsub('X1', 'first_', names(dat_team))
names(dat_team) <- gsub('X2', 'second_', names(dat_team))
names(dat_team) <- gsub('X3', 'third_', names(dat_team))
names(dat_team) <- gsub('X4', 'fourth_', names(dat_team))
names(dat_team) <- gsub('STARTING.LINEUPS', 'player_1', names(dat_team))
names(dat_team) <- gsub('X.1', 'player_2', names(dat_team))
names(dat_team) <- gsub('X.2', 'player_3', names(dat_team))
names(dat_team) <- gsub('X.3', 'player_4', names(dat_team))
names(dat_team) <- gsub('X', 'player_5', names(dat_team))
names(dat_team) <- gsub('.', '_', names(dat_team), fixed = TRUE)
names(dat_team) <- tolower(names(dat_team))

# remove and recode columns for player data
names(dat_player) <- tolower(names(dat_player))
names(dat_player) <- gsub('.', '_', names(dat_player), fixed = TRUE)
names(dat_player)[names(dat_player) == 'venue__r_h_'] <- 'venue'

# convert date
dat_player$date <- as.Date(dat_player$date, format = '%m/%d/%Y')

# team match up - who plays better at home and away, who plays better with less rest, records vs east and west, who should start for the raptors
# which player gives team most of a boost - when player x plays well, how does team y do, and when player y plays bad, how does team x do.
# player match up - compare by position, plot rolling avg points throughout season (who is on uptrend and downtrend).

#########
# raptors vs opponents
#########

# convert date
dat_team$date <- as.Date(dat_team$date, format = '%m/%d/%Y')

# make each row a game (spread to make data wider)
full_dat <- combine_game_rows(dat_team)

#########
# histogram overlayed of points and points against and point diff
#########

ggplot(full_dat %>% 
         filter(grepl('Toronto|Golden', team_team)) , aes(x = f_team, 
                     fill = team_team,
                     color = team_team)) + 
  geom_histogram(alpha = 0.8, 
                 binwidth = 2,
                 bins = 20) +
  labs(x = 'Total points per game',
       y = 'Counts',
       title = 'Distribution of points scored') +
  scale_fill_manual(name = '',
                    values = c('#006bb6', '#CD1141')) +
  scale_color_manual(name = '',
                     values = c('#FDB927', 'black'))

# points allowed
ggplot(full_dat %>% 
         filter(grepl('Toronto|Golden', team_team)),
       aes(x = f_opp, 
                     fill = team_team,
                     color = team_team)) + 
  geom_histogram(alpha = 0.8, 
                 binwidth = 2,
                 bins = 20) +
  labs(x = 'Total points allowed per game',
       y = 'Counts',
       title = 'Distribution of points allowed') +
  scale_fill_manual(name = '',
                    values = c('#006bb6', '#CD1141')) +
  scale_color_manual(name = '',
                     values = c('#FDB927', 'black'))

# points diff
ggplot(full_dat %>% 
         filter(grepl('Toronto|Golden', team_team)),
       aes(x = point_diff, 
                     fill = team_team,
                     color = team_team)) + 
  geom_histogram(alpha = 0.8, 
                 binwidth = 2,
                 bins = 20) +
  labs(x = 'Difference between points scored and allowed',
       y = 'Counts',
       title = 'Distribution of point differentials') +
  scale_fill_manual(name = '',
                    values = c('#006bb6', '#CD1141')) +
  scale_color_manual(name = '',
                     values = c('#FDB927', 'black')) +
  theme_classic()


##########
# moving avg of points and points againsgt and point diff
#########

# get mov avg
full_dat$mov_avg_points_team <- movavg(full_dat$f_team, n = 10, type = 's')
full_dat$mov_avg_points_opp <- movavg(full_dat$f_opp, n = 10, type = 's')
full_dat$mov_avg_points_diff <- movavg(full_dat$point_diff, n = 10, type = 's')

# plot mov avg
ggplot(full_dat %>% 
         filter(grepl('Toronto|Golden', team_team)), 
       aes(date_team, 
                     mov_avg_points_team, 
                     group = team_team, 
                     color = team_team)) +
  geom_point(size = 1.5) +
  geom_smooth(method = 'loess') +
  scale_color_manual(name = '',
                     values = c('#006bb6', '#CD1141')) +
  ylim(c(90, 140)) +
  labs(x = 'Date',
       y = 'Moving avg of points scored') +
  scale_x_date(date_breaks = "2 month", 
               date_minor_breaks = "1 week", 
               date_labels = "%B") +
  theme_pander()

# plot mov avg
ggplot(full_dat %>% 
         filter(grepl('Toronto|Golden', team_team)), 
       aes(date_team, 
                     mov_avg_points_opp, 
                     group = team_team, 
                     color = team_team)) +
  geom_point(size = 1.5) +
  geom_smooth(method = 'loess') +
  scale_color_manual(name = '',
                     values = c('#006bb6', '#CD1141')) +
  ylim(c(90, 140)) +
  labs(x = 'Date',
       y = 'Moving avg of points allowed') +
  scale_x_date(date_breaks = "2 month", 
               date_minor_breaks = "1 week", 
               date_labels = "%B") +
  theme_pander()

# plot mov avg
ggplot(full_dat %>% 
         filter(grepl('Toronto|Golden', team_team)), 
       aes(date_team, 
                     mov_avg_points_diff, 
                     group = team_team, 
                     color = team_team)) +
  geom_point(size = 1.5) +
  geom_smooth(method = 'loess') +
  scale_color_manual(name = '',
                     values = c('#006bb6', '#CD1141')) +
  labs(x = 'Date',
       y = 'Moving avg of final points differential') +
  scale_x_date(date_breaks = "2 month", 
               date_minor_breaks = "1 week", 
               date_labels = "%B") +
  theme_pander()
  


##########
# get performace vs east and west
##########

# read in teams.csv
con_teams <- read.csv('data/team_data/teams.csv')

# join with full_dat
full_dat <- inner_join(full_dat, con_teams, by = 'team_opp')

# create a win column
full_dat$win_team <- ifelse(full_dat$f_team > full_dat$f_opp, 'W', 'L')

##########
# breakdown pt for, against, diff, and wins by conference
##########

# group by team, conference, filter out opponents that are raptors and warriors and summarizes results 
temp_1 <- full_dat %>% 
  filter(team_opp != 'Toronto') %>%
  filter(team_opp != 'Golden State') %>%
  group_by(team_team, conference) %>%
  summarise(counts = n(),
            sum_wins = sum(win_team == 'W'),
            sum_loss = sum(win_team == 'L'),
            per_win = round((sum_wins/counts)*100, 2))
            

# bar plot 
ggplot(temp_1  %>% 
         filter(grepl('Toronto|Golden', team_team)) , 
       aes(x = team_team,
                   y = per_win,
                   fill = conference)) + 
  geom_bar(stat = 'identity', 
           position = 'dodge',
           alpha = 0.8) +
  labs(x = '',
       y = 'Winning percentage',
       title = 'Winning % by conference') +
  scale_fill_manual(name = '',
                    labels = c('East', 'West'),
                    values = c('#006bb6', '#CD1141')) +
  theme_pander()


#########
# plot teams success when favored and underdog
#########



# create favored/underdog columns
full_dat$is_favored_team <- ifelse(full_dat$closing_spread_team < 0, 'yes', 'no')

# group by team_team and is_favored and get win percent
temp_2 <- full_dat %>% 
  filter(team_opp != 'Toronto') %>%
  filter(team_opp != 'Golden State') %>%
  group_by(team_team, is_favored_team) %>%
  summarise(counts = n(),
            sum_wins = sum(win_team == 'W'),
            sum_loss = sum(win_team == 'L'),
            per_win = round((sum_wins/counts)*100, 2))

# bar plot 
# bar plot 
ggplot(temp_2 %>% 
         filter(grepl('Toronto|Golden', team_team)),
       aes(x = team_team,
                   y = per_win,
                   fill = is_favored_team)) + 
  geom_bar(stat = 'identity', 
           position = 'dodge',
           alpha = 0.8) +
  labs(x = '',
       y = 'Winning percentage',
       title = 'Winning % by favorite status') +
  scale_fill_manual(name = '',
                    labels = c('Underdog', 'Favorite'),
                    values = c('#006bb6', '#CD1141')) +
  theme_pander()


#########
# plot ALL teams success when favored or underdog
########
temp_3 <- full_dat %>%
  group_by(team_team, is_favored_team) %>%
  summarise(counts = n(),
            sum_wins = sum(win_team == 'W'),
            sum_loss = sum(win_team == 'L'),
            per_win = round((sum_wins/counts)*100, 2)) 

# rank by favorite and underdog
fav <- temp_3[temp_3$is_favored_team == 'yes',]
ud <- temp_3[temp_3$is_favored_team == 'no',]

# rank them
fav$rank <- rank(-fav$per_win)
ud$rank <- rank(-ud$per_win)


#########
# examine rest days
#########

# create new variables for rest days more than 2
full_dat$rest_2_more_team <- ifelse(full_dat$team_rest_days_team == '2', 'yes',
                                    ifelse(full_dat$team_rest_days_team == '3+', 'yes', 'no'))

# group by team_team, filter out toronto and goldenstate in team_opp, and summarise 
# key variables
temp_1 <- full_dat %>% 
  group_by(team_team, rest_2_more_team) %>%
  summarise(counts = n(),
            wins_sum = sum(win_team == 'W'),
            loss_sum = sum(win_team == 'L'),
            wins_per = wins_sum/counts,
            ft_sum = sum(ft_team, na.rm = TRUE),
            fta_sum = sum(fta_team, na.rm = TRUE),
            ft_per = ft_sum/fta_sum,
            three_p_sum = sum(three_p_team),
            three_pa_sum = sum(three_pa_team),
            three_per = three_p_sum/three_pa_sum,
            or_sum = sum(or_team),
            dr_sum = sum(dr_team),
            totr_sum = sum(tot_team),
            or_mean = or_sum/counts,
            dr_mean = dr_sum/counts,
            a_sum = sum(a_team),
            a_mean = a_sum/counts,
            pf_sum = sum(pf_team),
            pf_mean = pf_sum/counts,
            st_sum = sum(st_team),
            st_mean = st_sum/counts,
            to_sum = sum(to_team),
            to_mean = to_sum/counts,
            bl_sum = sum(bl_team),
            bl_mean = bl_sum/counts,
            pts_mean = mean(pts_team),
            poss_mean = mean(poss_team),
            pace_mean = mean(pace_team),
            oeff_mean = mean(oeff_team),
            deff_mean = mean(deff_team))
# do rest days
