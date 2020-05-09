# objectives: (1) compare season avg and playoff avg with game 1 team stats
# (2) compare season avg and playoff avg with game 1 player stats 
# (3) logit of what predicts likihood of winning
# (4) find player who did well in game 1 and show that when he does well, raptors when x% of time

# load libraries 
library(tidyverse)
library(ggplot2)
library(reshape2)
library(pracma)
library(MASS)


# run functions.R scripts
source('functions.R')

##########
# load team data and clean
##########

# read in team data
dat_team <- read.csv('data/team_data/nba-season-team-feed_june_2.csv')

# load pbp data
# dat_pbp <- read.csv('data/pbp_data/combined/may_31st_combined.csv')

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

# convert date
dat_team$date <- as.Date(dat_team$date, format = '%m/%d/%Y')

# create a win loss variable
dat_team <- get_w_l(dat_team)
dat_team$win_loss <- as.factor(dat_team$win_loss)

# get percent vars for quarters
dat_team$per_first_q <- round((dat_team$first_q/dat_team$pts)*100, 2)
dat_team$per_second_q <- round((dat_team$second_q/dat_team$pts)*100, 2)
dat_team$per_third_q <- round((dat_team$third_q/dat_team$pts)*100, 2)
dat_team$per_fourth_q <- round((dat_team$fourth_q/dat_team$pts)*100, 2)

# get percent vars for shooting 
dat_team$fg_per <- round((dat_team$fg/(dat_team$fg + dat_team$fga))*100, 2) 
dat_team$three_per <- round((dat_team$three_p/(dat_team$three_p + dat_team$three_pa))*100, 2) 
dat_team$ft_per <- round((dat_team$ft/(dat_team$ft + dat_team$fta))*100, 2) 

# use function to create stat differentials between winner and loser 
dat_team <- team_stat_diff(dat_team)

##########
# estimate a glm with percent variables 
##########

# estimate a g linear model with variables given for toronto
mod_toronto_1  <- glm(formula = win_loss ~ venue + fg_per_diff + three_per_diff + 
                        ft_per_diff + or_diff + dr_diff + a_diff + pf_diff + st_diff + 
                        to_diff + bl_diff + poss_diff + pace_diff,  
                      data = dat_team %>% filter(team == 'Toronto'), family = 'binomial') %>% 
  stepAIC(direction = 'both', steps = 50)
summary(mod_toronto_1)
mod_toronto_1_odds <- exp(cbind(coef(mod_toronto_1), confint(mod_toronto_1)))
mod_toronto_1_odds

        
# estimate a g linear model with variables given for gs
mod_gs_1  <- glm(formula = win_loss ~ venue + fg_per + fga + three_per + 
                   three_pa + ft_per + fta +or + dr + a + pf + st + to + bl + poss + pace,  
                 data = dat_team %>% filter(team == 'Golden State'), 
                 family = 'binomial', 
                 control = list(maxit = 50)) %>% 
  stepAIC(direction = 'both')
summary(mod_gs_1)
mod_gs_1_odds <- exp(cbind(coef(mod_gs_1), confint(mod_gs_1)))
mod_gs_1_odds

# estimate model for toronto with percent variables 
# estimate a g linear model with variables given for toronto
mod_toronto_2  <- glm(formula = win_loss ~ venue + per_first_q + per_second_q + 
                        per_third_q + per_fourth_q + fg + fga + three_p + ft + 
                        fta +  or + dr + a + pf + st + to + bl + poss + pace, 
                      data = dat_team %>% 
                        filter(team == 'Toronto'), 
                      family = 'binomial')
summary(mod_toronto_2)
mod_toronto_2_odds <- exp(cbind(coef(mod_toronto_2), confint(mod_toronto_2)))
mod_toronto_2_odds

# estimate a g linear model with variables given for gs
mod_gs_2  <- glm(formula = win_loss ~ venue + fg_per + three_per + ft_per + or + 
                   dr +  a + pf + st + to + bl + poss + pace,  
                 data = mod_dat %>% filter(team == 'Golden State'), family = 'binomial')  %>% 
  stepAIC(trace = FALSE)
summary(mod_gs_2)
mod_gs_2_odds <- exp(cbind(coef(mod_gs_2), confint(mod_gs_2)))
mod_gs_2_odds

###########
# based on glm results, look at stats of interest and the differentials between reg, playoff and game1
###########

# get data set 
dat_team$dataset <- ifelse(grepl('Regular', dat_team$dataset), 'reg', 'playoffs')
dat_team$dataset <- ifelse(dat_team$date == '2019-05-30', 'finals_1', dat_team$dataset)

# from team_dat group by team, data_set, and get mean fg_per, three_per, or, dr, st, poss, pace
temp <- dat_team %>% 
  filter(grepl('Toronto|Golden State', team)) %>%
  group_by(team, dataset) %>%
  summarise(mean_fg_per = mean(fg_per, na.rm = TRUE),
            mean_three_per = mean(three_per, na.rm = TRUE),
            mean_or = mean(or, na.rm = TRUE),
            mean_dr = mean(dr, na.rm = TRUE),
            mean_st = mean(fg_per, na.rm = TRUE),
            mean_poss = mean(fg_per, na.rm = TRUE),
            mean_pace = mean(fg_per, na.rm = TRUE))



##########
# load player data and clean
##########

# load player data
dat_player <- read.csv('data/player_data/nba-season-player-feed_final_1.csv')


# remove and recode columns for player data
names(dat_player) <- tolower(names(dat_player))
names(dat_player) <- gsub('.', '_', names(dat_player), fixed = TRUE)
names(dat_player)[names(dat_player) == 'venue__r_h_'] <- 'venue'

# convert date
dat_player$date <- as.Date(dat_player$date, format = '%m/%d/%Y')

##########
# compare season and playoff avg with game 1 stats
##########

# subset data by season, playoff and game 1
reg <- dat_team[dat_team$dataset == 'NBA 2018-2019 Regular Season',]
po <- dat_team[dat_team$dataset == 'NBA 2019 Playoffs',]

# get game 1 data
game <- po[po$date == '2019-05-30',]

# remove game 1 from po
po <- po[po$date != '2019-05-30',]

# get raptos and warriors data from reg, po, and game
reg <- reg[grepl('Toronto|Golden', reg$team),]
po <- po[grepl('Toronto|Golden', po$team),]

# add indicator for data set and combine
reg$dat <- 'regular_season'
po$dat <- 'playoffs'
game$dat <- 'finals'

full_dat <- rbind(reg,
                  po,
                  game)

rm(reg,
   po,
   game)

# group by dat and summarise stats
temp <- full_dat %>%
  group_by(team, dat) %>%
  summarise(mean_first = mean(first_q),
            mean_second = mean(second_q),
            mean_third  = mean(third_q),
            mean_fourth = mean(fourth_q),
            mean_fg = mean(fg),
            mean_fga = mean(fga),
            mean_three = mean(three_p),
            mean_threea = mean(three_pa),
            mean_ft = mean(ft),
            mean_fta = mean(fta),
            mean_or = mean(or),
            mean_dr = mean(dr),
            mean_tot = mean(tot),
            mean_a = mean(a),
            mean_pf = mean(pf),
            mean_st = mean(st),
            mean_to = mean(to),
            mean_bl = mean(bl),
            mean_poss = mean(poss),
            mean_pace = mean(pace),
            mean_oeff = mean(oeff),
            mean_deff = mean(deff))

# subset by team
temp_raps <- temp[temp$team == 'Toronto',]
temp_wars <- temp[temp$team == 'Golden State',]
rm(temp)




