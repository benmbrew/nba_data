# create a function that uses lag to get previous weeks data 

get_lag_data <- function(column_variable){
  column_variable <- lag(as.numeric(column_variable))
  column_variable <- ifelse(is.na(column_variable), 0, column_variable)
  
}

# get stats for underdog and favorite
get_underdog_stats <- function(temp_dat){
  temp_dat$underdog_stats <- ifelse(temp_dat$win_loss == 'W' & temp_dat$closing_spread > 0, 'underdog_win',
                                    ifelse(temp_dat$win_loss == 'W' & temp_dat$closing_spread < 0, 'favorite_win',
                                           ifelse(temp_dat$win_loss == 'W' & temp_dat$closing_spread == 0, 'straight_win',
                                                  ifelse(temp_dat$win_loss == 'L' & temp_dat$closing_spread > 0, 'underdog_loss',
                                                         ifelse(temp_dat$win_loss == 'L' & temp_dat$closing_spread < 0, 'favorite_loss',
                                                                ifelse(temp_dat$win_loss == 'L' & temp_dat$closing_spread == 0, 'straight_loss', 'oops'))))))
  
  return(temp_dat)
}


#create a function to create a variable to indicate win or loss
get_win_loss <- function(temp_dat){
  
  # create list to store loop results
  data_list <- list()
  
  temp_dat$win_loss <- NA
  temp_dat$real_spread <- NA
  for(i in 1:length(unique(temp_dat$game_number))){
    # get individual game 
    sub_game <- temp_dat[temp_dat$game_number == i,]
    
    if(any(is.na(sub_game$f))){
      sub_game$win_loss <- NA
      sub_game$real_spread <- NA
      
    } else {
      # condition for winning
      if(sub_game$f[1] > sub_game$f[2]){
        sub_game$win_loss[1] <- 'W'
        sub_game$win_loss[2] <- 'L'
        sub_game$real_spread[1] <- sub_game$f[1] - sub_game$f[2]
        sub_game$real_spread[2] <- sub_game$f[2] - sub_game$f[1]
        
        
      } else if(sub_game$f[1] < sub_game$f[2]) {
        sub_game$win_loss[1] <- 'L'
        sub_game$win_loss[2] <- 'W'
        sub_game$real_spread[2] <- sub_game$f[2] - sub_game$f[1]
        sub_game$real_spread[1] <- sub_game$f[1] - sub_game$f[2]
        
        
      } else {
        sub_game$win_loss[1] <- 'D'
        sub_game$win_loss[2] <- 'D'
      }
    }
    
    data_list[[i]] <- sub_game
  }
  
  final_dat <- as.data.frame(do.call('rbind', data_list))
  return(final_dat)
}


# function to create closing spread and total from messy closing odds

get_closing_spread_ou <- function(temp_dat){
  
  
  
  temp_dat$closing_spread <- NA
  temp_dat$closing_total <- NA
  
  
  result_list <- list()
  for(i in 1:max(temp_dat$game_number)){
    
    # subset 
    sub_dat <- temp_dat[temp_dat$game_number == i,]
    
    # get max odds index
    max_value <- max(sub_dat$closing_odds)
    choose_index <- c(1,2)
    total_index <- which(sub_dat$closing_odds == max(sub_dat$closing_odds))
    spread_index <- choose_index[!choose_index %in% total_index] 
    
    if(spread_index == 2){
      # get closing spread
      sub_dat$closing_spread[1] <- (sub_dat$closing_odds[spread_index])*(-1)
      sub_dat$closing_spread[2] <- sub_dat$closing_odds[spread_index]
    } else {
      # get closing spread
      sub_dat$closing_spread[1] <- (sub_dat$closing_odds[spread_index])
      sub_dat$closing_spread[2] <- sub_dat$closing_odds[spread_index]*(-1)
    }
   
    
    # get closing total
    sub_dat$closing_total <- sub_dat$closing_odds[total_index]
    
    # store in list
    result_list[[i]] <- sub_dat
    
  }
  
  final_dat <- do.call('rbind', result_list)
  
}


# function that takes every other row and attaches to the dataframe 
get_by_game <- function(dat) {
  
  # make column names lower case 
  
  temp_new_game <- list()
  
  # loop through by 2 and combine 
  for(game in unique(dat$tot_game_num)){
    # subset data 
    temp_game <- dat[dat$tot_game_num == game,]
    
    temp_game <- temp_game[order(temp_game$venue),]
    # get first and second row
    temp_1st_row <- as.data.frame(temp_game[1,])
    temp_2nd_row <- as.data.frame(temp_game[2,])
    
    # add "away" to 1st row columns
    names(temp_1st_row) <- paste0(names(temp_1st_row), '_home')
    names(temp_2nd_row) <- paste0(names(temp_2nd_row), '_away')
    
    # bind them together 
    temp_new_game[[game]] <- cbind(temp_2nd_row, temp_1st_row)
    print(game)
  }
  final_game <- do.call(rbind, temp_new_game)
  return(final_game)
}


get_team_game_number <- function(temp_dat){
  result_list <- list()
  unique_teams <- unique(temp_dat$teams)
  temp_dat$date <- as.Date(temp_dat$date, format = '%m/%d/%Y')
  for(i in 1:length(unique_teams)){
    this_team <- unique_teams[i]
    sub_team <- temp_dat[temp_dat$teams == this_team,]
    sub_team <- sub_team[order(sub_team$date, decreasing = FALSE),]
    season_length <- nrow(sub_team)
    sub_team$team_game_num <- seq(1, season_length, 1)
    result_list[[i]] <- sub_team
  }
  final_dat <- do.call(rbind, result_list)
  return(final_dat)
}

# Define function for printing nice html tables
prettify_scroll <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE,
                             cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y",
                             round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE,
                             data_table = TRUE, nrows = 5, download_options = FALSE, no_scroll = TRUE,
                             scroll_x = TRUE){
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        if(!grepl('year', tolower(names(the_table)[j]))){
          the_column <- scales::comma(the_column)
        }
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          scrollX = scroll_x,
          dom = "Bfrtip", buttons = list("copy", 
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             # scrollY = '300px', paging = FALSE,
                                                             dom = "Bfrtip", buttons = list("copy",
                                                                                            list(extend = "collection", buttons = "csv",
                                                                                                 text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      
    }
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          scrollX = scroll_x,
          columnDefs = list(list(className = "dt-right",
                                 targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
  }
  return(the_table)
}


# loop through and add up score for each game to get real total
# temp_dat <- dat_current
get_over_under_total <- function(temp_dat){
  temp_dat$real_total <- NA

  for(i in seq(1, nrow(temp_dat), 2)) {
    if(is.na(temp_dat$f[i])){
      temp_dat$real_total[i:(i+1)] <- NA
    } else {
      temp_dat$real_total[i:(i+1)] <- sum(temp_dat$f[i], temp_dat$f[i+1])
      print(i)
    }
    
  }
  return(temp_dat)
}

# get stats for over under
get_over_under_stats <- function(temp_dat){
  temp_dat$over_under_stats <- ifelse(temp_dat$closing_total > temp_dat$real_total, 'under',
                                      ifelse(temp_dat$closing_total < temp_dat$real_total, 'over',
                                             ifelse(temp_dat$closing_total == temp_dat$real_total, 'even', NA)))
  return(temp_dat)
}


get_total_games <- function(temp_2014 = dat_2014, 
                            temp_2015 = dat_2015, 
                            temp_2016 = dat_2016, 
                            temp_2017 = dat_2017, 
                            temp_current= dat_current){
  temp_all <- rbind(temp_2014,
                    temp_2015,
                    temp_2016,
                    temp_2017,
                    temp_current)
  
  temp_all$tot_game_num <- rep(1:(nrow(temp_all)/2), each=2)
  temp_2014 <- temp_all[temp_all$dataset == '2014_2015_season',]
  temp_2015 <- temp_all[temp_all$dataset == '2015_2016_season',]
  temp_2016 <- temp_all[temp_all$dataset == '2016_2017_season',]
  temp_2017 <- temp_all[temp_all$dataset == '2017_2018_season',]
  temp_current <- temp_all[temp_all$dataset == '2018_2019_season',]
  
  return(list(temp_2014, temp_2015, temp_2016, temp_2017, temp_current))
  
}

get_lag_data <- function(column_variable){
  column_variable <- lag(as.numeric(column_variable))
  column_variable <- ifelse(is.na(column_variable), 0, column_variable)
  
  return(column_variable)
}

# create function to find winning streaks
streak <- function(x, value = 0)  {
  x <- x == value
  rl <- rle(x)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x
  # replace the 0 at the end of each zero-block in z by the
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  final_x <- x*cumsum(z)
  final_x <- final_x[-length(final_x)]
  final_x <- c(0, final_x)
  return(final_x)
}

# this is the main function that featurizes team data .
featurize_team_data <- function(temp_dat){
  # get a vector of team names to loop through
  unique_teams <- unique(temp_dat$teams)
  unique_years <- unique(temp_dat$dataset)
  # loop through unique teams and grab sub team and opponenet data to featurize
  data_year <- list()
  
  for(i in 1:length(unique_years)){
    data_team <- list()
    this_year <- unique_years[i]
    sub_year <- temp_dat[temp_dat$dataset == this_year,]
    message('Creating features for ', this_year)
    
    
    for(j in 1:length(unique_teams)){
      # get team name and subset
      this_team <- unique_teams[j]
      sub_team <- sub_year[sub_year$teams == this_team, ]
      sub_team_name <- unique(sub_team$teams)
      
      message('Creating features for ', sub_team_name)
      
      # check that the data is arranged by data
      sub_team <- sub_team[order(sub_team$date),]
      
      # begin generating features
      # to start: days since last game
      sub_team <- sub_team %>% mutate(last_game=round(c(196,diff(date)), 1))
      
      # create a numeric win column and use the lag function 
      sub_team$ud_loss_team_ind <- ifelse(sub_team$underdog_stats == 'underdog_loss', 1, 0 )
      sub_team$ud_loss_team_ind <- get_lag_data(sub_team$ud_loss_team_ind)
      # get cumulative sum of lagged wins and winning percentage 
      sub_team$cum_sum_ud_loss_lag_team <- cumsum(sub_team$ud_loss_team_ind)
      sub_team$cum_sum_ud_loss_lag_per_team  <- cumsum(sub_team$ud_loss_team_ind)/get_lag_data(sub_team$team_game_num)
      sub_team$cum_sum_ud_loss_lag_per_team  <- ifelse(sub_team$cum_sum_ud_loss_lag_per_team  == 'NaN', 0, sub_team$cum_sum_ud_loss_lag_per_team )
      sub_team$mov_avg_ud_loss_team <- movavg(sub_team$ud_loss_team_ind, n = 10, type = 's')

      
      # create a numeric win column and use the lag function 
      sub_team$ud_win_team_ind <- ifelse(sub_team$underdog_stats == 'underdog_win', 1, 0 )
      sub_team$ud_win_team_ind <- get_lag_data(sub_team$ud_win_team_ind)
      # get cumulative sum of lagged wins and winning percentage 
      sub_team$cum_sum_ud_win_lag_team <- cumsum(sub_team$ud_win_team_ind)
      sub_team$cum_sum_ud_win_lag_per_team  <- cumsum(sub_team$ud_win_team_ind)/get_lag_data(sub_team$team_game_num)
      sub_team$cum_sum_ud_win_lag_per_team  <- ifelse(sub_team$cum_sum_ud_win_lag_per_team  == 'NaN', 0, sub_team$cum_sum_ud_win_lag_per_team )
      sub_team$mov_avg_ud_win_team <- movavg(sub_team$ud_win_team_ind, n = 10, type = 's')

      
      # create a numeric win column and use the lag function 
      sub_team$fav_win_team_ind <- ifelse(sub_team$underdog_stats == 'favorite_win', 1, 0 )
      sub_team$fav_win_team_ind <- get_lag_data(sub_team$fav_win_team_ind)
      # get cumulative sum of lagged wins and winning percentage 
      sub_team$cum_sum_fav_win_lag_team <- cumsum(sub_team$fav_win_team_ind)
      sub_team$cum_sum_fav_win_lag_per_team  <- cumsum(sub_team$fav_win_team_ind)/get_lag_data(sub_team$team_game_num)
      sub_team$cum_sum_fav_win_lag_per_team  <- ifelse(sub_team$cum_sum_fav_win_lag_per_team  == 'NaN', 0, sub_team$cum_sum_fav_win_lag_per_team )
      sub_team$mov_avg_fav_win_team <- movavg(sub_team$fav_win_team_ind, n = 10, type = 's')

      
      # create a numeric loss column and use the lag function 
      sub_team$fav_loss_team_ind <- ifelse(sub_team$underdog_stats == 'favorite_loss', 1, 0 )
      sub_team$fav_loss_team_ind <- get_lag_data(sub_team$fav_loss_team_ind)
      # get cumulative sum of lagged losss and lossning percentage 
      sub_team$cum_sum_fav_loss_lag_team <- cumsum(sub_team$fav_loss_team_ind)
      sub_team$cum_sum_fav_loss_lag_per_team  <- cumsum(sub_team$fav_loss_team_ind)/get_lag_data(sub_team$team_game_num)
      sub_team$cum_sum_fav_loss_lag_per_team  <- ifelse(sub_team$cum_sum_fav_loss_lag_per_team  == 'NaN', 0, sub_team$cum_sum_fav_loss_lag_per_team )
      sub_team$mov_avg_fav_loss_team <- movavg(sub_team$fav_loss_team_ind, n = 10, type = 's')
      temp <- sub_team[ c('date', 'teams', 'win_loss', 'underdog_stats', 'fav_loss_team_ind','mov_avg_fav_loss_team','cum_sum_fav_loss_lag_team', 'cum_sum_fav_loss_lag_per_team')]
      
      # create a numeric loss column and use the lag function 
      sub_team$ou_over_team_ind <- ifelse(sub_team$over_under_stats == 'over', 1, 0 )
      sub_team$ou_over_team_ind <- get_lag_data(sub_team$ou_over_team_ind)
      # get cumulative sum of lagged losss and lossning percentage 
      sub_team$cum_sum_ou_over_lag_team <- cumsum(sub_team$ou_over_team_ind)
      sub_team$cum_sum_ou_over_lag_per_team  <- cumsum(sub_team$ou_over_team_ind)/get_lag_data(sub_team$team_game_num)
      sub_team$cum_sum_ou_over_lag_per_team  <- ifelse(sub_team$cum_sum_ou_over_lag_per_team  == 'NaN', 0, sub_team$cum_sum_ou_over_lag_per_team )
      sub_team$mov_avg_ou_over_team <- movavg(sub_team$ou_over_team_ind, n = 10, type = 's')

      
      # create a numeric loss column and use the lag function 
      sub_team$ou_under_team_ind <- ifelse(sub_team$over_under_stats == 'under', 1, 0 )
      sub_team$ou_under_team_ind <- get_lag_data(sub_team$ou_under_team_ind)
      # get cumulative sum of lagged losss and lossning percentage 
      sub_team$cum_sum_ou_under_lag_team <- cumsum(sub_team$ou_under_team_ind)
      sub_team$cum_sum_ou_under_lag_per_team  <- cumsum(sub_team$ou_under_team_ind)/get_lag_data(sub_team$team_game_num)
      sub_team$cum_sum_ou_under_lag_per_team  <- ifelse(sub_team$cum_sum_ou_under_lag_per_team  == 'NaN', 0, sub_team$cum_sum_ou_under_lag_per_team )
      sub_team$mov_avg_ou_under_team <- movavg(sub_team$ou_under_team_ind, n = 10, type = 's')

      # edits: player_1:player_5 - get stats of star players
      # get cumulative sum of points scored and points allowed 
      
      # create a numeric column and use the lag function 
      sub_team$win_ind_team <- ifelse(sub_team$win_loss == 'W', 1, 0 )
      sub_team$win_ind_team <- get_lag_data(sub_team$win_ind_team)
      
      
      # get cumulative sum of lagged wins and winning percentage 
      sub_team$cum_wins_lag_team <- cumsum(sub_team$win_ind_team)
      sub_team$cum_wins_per_lag_team <- cumsum(sub_team$win_ind_team)/get_lag_data(sub_team$team_game_num)
      sub_team$cum_wins_per_lag_team <- ifelse(sub_team$cum_wins_per_lag_team == 'NaN', 0, sub_team$cum_wins_per_lag_team)
      
      sub_team$cum_points_team <- cumsum(sub_team$f)
      sub_team$cum_points_team <- get_lag_data(sub_team$cum_points_team)
      
      sub_team$mov_avg_points_team <- movavg(sub_team$f, n = 5, type = 's')
      sub_team$mov_avg_points_team <- get_lag_data(sub_team$mov_avg_points_team)
      
      # first_q
      sub_team$mov_avg_first_q <- movavg(sub_team$first_q, n = 5, type = 's')
      sub_team$mov_avg_first_q <- get_lag_data(sub_team$mov_avg_first_q)
      
      # second_q
      sub_team$mov_avg_second_q <- movavg(sub_team$second_q, n = 5, type = 's')
      sub_team$mov_avg_second_q <- get_lag_data(sub_team$mov_avg_second_q)
      
      # third_q
      sub_team$mov_avg_third_q <- movavg(sub_team$third_q, n = 5, type = 's')
      sub_team$mov_avg_third_q <- get_lag_data(sub_team$mov_avg_third_q)
      
      # fourth_q
      sub_team$mov_avg_fourth_q <- movavg(sub_team$fourth_q, n = 5, type = 's')
      sub_team$mov_avg_fourth_q <- get_lag_data(sub_team$mov_avg_fourth_q)
      
      # create a momentum variable off of lagged cumulative wins
      sub_team$momentum <- diff(c(0,sub_team$cum_wins_per_lag_team))
      
      # take the inverse
      sub_team$momentum_team <- ifelse(sub_team$momentum == 0, 0, 1/sub_team$momentum)
      
      # get win streak using "streak" function from functions.R
      # get second to last index
      end_index <- nrow(sub_team) - 1
      win_streak <- streak(sub_team$win_loss[1:end_index], value = 'W')
      if(sub_team$win_loss[end_index] == 'W'){
        end_value <- 1 + win_streak[end_index]
      } else {
        end_value <- 0
      }
      win_streak <- c(win_streak, end_value)
      sub_team$win_streak_team <- win_streak
      
      # get losing streak
      lose_streak <- streak(sub_team$win_loss[1:end_index], value = 'L')
      if(sub_team$win_loss[end_index] == 'L'){
        end_value <- 1 + lose_streak[end_index]
      } else {
        end_value <- 0
      }
      lose_streak <- c(lose_streak, end_value)
      sub_team$lose_streak_team <- lose_streak
      
      # pace
      sub_team$mov_avg_pace_team <- movavg(sub_team$pace, n = 5, type= 's')
      sub_team$mov_avg_pace_team <- get_lag_data(sub_team$mov_avg_pace_team)
      # oeff
      sub_team$mov_avg_oeff_team <- movavg(sub_team$oeff, n = 5, type= 's')
      sub_team$mov_avg_oeff_team <- get_lag_data(sub_team$mov_avg_oeff_team)
      # deff
      sub_team$mov_avg_deff_team <- movavg(sub_team$deff, n = 5, type= 's')
      sub_team$mov_avg_deff_team <- get_lag_data(sub_team$mov_avg_deff_team)
      # real_spread
      sub_team$mov_avg_real_spread_team <- movavg(sub_team$real_spread, n = 5, type= 's')
      sub_team$mov_avg_real_spread_team <- get_lag_data(sub_team$mov_avg_real_spread_team)
      # closing_spread
      sub_team$mov_avg_closing_spread_team <- movavg(sub_team$closing_spread, n = 5, type= 's')
      sub_team$mov_avg_closing_spread_team <- get_lag_data(sub_team$mov_avg_closing_spread_team)
      # real_total
      sub_team$mov_avg_real_total_team <- movavg(sub_team$real_total, n = 5, type= 's')
      sub_team$mov_avg_real_total_team <- get_lag_data(sub_team$mov_avg_real_total_team)
      # closing_total
      sub_team$mov_avg_closing_total_team <- movavg(sub_team$closing_total, n = 5, type= 's')
      sub_team$mov_avg_closing_total_team <- get_lag_data(sub_team$mov_avg_closing_total_team)
      
      
      # or
      sub_team$mov_avg_or_team <- movavg(sub_team$or, n = 5, type= 's')
      sub_team$mov_avg_or_team <- get_lag_data(sub_team$mov_avg_or_team)
      # dr
      sub_team$mov_avg_dr_team <- movavg(sub_team$dr, n = 5, type= 's')
      sub_team$mov_avg_dr_team <- get_lag_data(sub_team$mov_avg_dr_team)
      # tot
      sub_team$mov_avg_tot_team <- movavg(sub_team$tot, n = 5, type= 's')
      sub_team$mov_avg_tot_team <- get_lag_data(sub_team$mov_avg_tot_team)
      # a
      sub_team$mov_avg_a_team <- movavg(sub_team$a, n = 5, type= 's')
      sub_team$mov_avg_a_team <- get_lag_data(sub_team$mov_avg_a_team)
      # pf
      sub_team$mov_avg_pf_team <- movavg(sub_team$pf, n = 5, type= 's')
      sub_team$mov_avg_pf_team <- get_lag_data(sub_team$mov_avg_pf_team)
      # st
      sub_team$mov_avg_st_team <- movavg(sub_team$st, n = 5, type= 's')
      sub_team$mov_avg_st_team <- get_lag_data(sub_team$mov_avg_st_team)
      # to
      sub_team$mov_avg_to_team <- movavg(sub_team$to, n = 5, type= 's')
      sub_team$mov_avg_to_team <- get_lag_data(sub_team$mov_avg_to_team)
      # bl
      sub_team$mov_avg_bl_team <- movavg(sub_team$bl, n = 5, type= 's')
      sub_team$mov_avg_bl_team <- get_lag_data(sub_team$mov_avg_bl_team)
      
      
      # field goals
      sub_team$mov_avg_fg_team <- movavg(sub_team$fg, n = 5, type= 's')
      sub_team$mov_avg_fg_team <- get_lag_data(sub_team$mov_avg_fg_team)
      
      sub_team$mov_avg_fga_team <- movavg(sub_team$fga, n = 5, type= 's')
      sub_team$mov_avg_fga_team <- get_lag_data(sub_team$mov_avg_fga_team)
      
      sub_team$mov_avg_fg_per_team <- movavg(sub_team$fg_per, n = 5, type= 's')
      sub_team$mov_avg_fg_per_team <- get_lag_data(sub_team$mov_avg_fg_per_team)
      
      # free throws
      sub_team$mov_avg_ft_team <- movavg(sub_team$ft, n = 5, type= 's')
      sub_team$mov_avg_ft_team <- get_lag_data(sub_team$mov_avg_ft_team)
      
      sub_team$mov_avg_fta_team <- movavg(sub_team$fta, n = 5, type= 's')
      sub_team$mov_avg_fta_team <- get_lag_data(sub_team$mov_avg_fta_team)
      
      sub_team$mov_avg_ft_per_team <- movavg(sub_team$ft_per, n = 5, type= 's')
      sub_team$mov_avg_ft_per_team <- get_lag_data(sub_team$mov_avg_ft_per_team)

      # three points
      sub_team$mov_avg_three_p_team <- movavg(sub_team$three_p, n = 5, type= 's')
      sub_team$mov_avg_three_p_team <- get_lag_data(sub_team$mov_avg_three_p_team)
      
      sub_team$mov_avg_three_pa_team <- movavg(sub_team$three_pa, n = 5, type= 's')
      sub_team$mov_avg_three_pa_team <- get_lag_data(sub_team$mov_avg_three_pa_team)
      
      sub_team$mov_avg_three_per_team <- movavg(sub_team$three_per, n = 5, type= 's')
      sub_team$mov_avg_three_per_team <- get_lag_data(sub_team$mov_avg_three_per_team)
      
      # store data in data_list
      data_team[[j]] <- sub_team
      
    }
    all_teams <- do.call('rbind', data_team)
    data_year[[i]] <- all_teams
  }
  
  
  final_data <- do.call('rbind', data_year)
  return(final_data)
}



# function to remove players that have less than 4 games in a season
remove_low_frequency_players <- function(temp_dat){
  temp_dat <- temp_dat %>% 
    group_by(player) %>%
    mutate(counts = n()) %>%
    filter(counts >= 4)
  
  return(temp_dat)
  
}

get_team_ranks <- function(temp_dat){
  
  year_list <- list()
  
  unique_years <- as.character(unique(temp_dat$dataset))
  
  for(i in 1:length(unique_years)){
    
    num_list <- list()
    
    # get year object
    this_year <- unique_years[i]
    
    # subset by year
    sub_year <- temp_dat[temp_dat$dataset == this_year,]
    
    # get unique weeks for next loop
    unique_game_num <- unique(sub_year$team_game_num)
    
    for(j in 1:length(unique_game_num)) {
      this_num <- unique_game_num[j]
      sub_num <- sub_year %>% filter(team_game_num == this_num)
      
      if(this_num ==  1){
        
        
        # mov avg only
        sub_num$rank_mov_avg_pace_team <- NA
        sub_num$rank_mov_avg_oeff_team <- NA
        sub_num$rank_mov_avg_deff_team <- NA
        sub_num$rank_mov_avg_tot_team <- NA
        sub_num$rank_mov_avg_a_team <- NA
        sub_num$rank_mov_avg_pf_team <- NA
        sub_num$rank_mov_avg_st_team <- NA
        sub_num$rank_mov_avg_fg_per_team <- NA
        sub_num$rank_mov_avg_ft_per_team <- NA
        sub_num$rank_mov_avg_three_per_team <- NA
        
        
        sub_num$rank_cum_wins_lag_team <- NA
        
        sub_num$rank_cum_points_team <- NA
        sub_num$rank_mov_avg_points_team <- NA
        sub_num$rank_momentum_team <- NA
        
        sub_num$rank_win_streak_team <- NA
        
        
        
      } else {
        # total yds and points

        # mov avg only
        sub_num$rank_mov_avg_pace_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_pace_team)))))
        sub_num$rank_mov_avg_oeff_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_oeff_team)))))
        sub_num$rank_mov_avg_deff_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_deff_team)))))
        sub_num$rank_mov_avg_tot_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_to_team)))))
        sub_num$rank_mov_avg_a_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_a_team)))))
        sub_num$rank_mov_avg_pf_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_pf_team)))))
        sub_num$rank_mov_avg_st_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_st_team)))))
        sub_num$rank_mov_avg_fg_per_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_fg_per_team)))))
        sub_num$rank_mov_avg_ft_per_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_ft_per_team)))))
        sub_num$rank_mov_avg_three_per_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_three_per_team)))))
      
        
        sub_num$rank_cum_wins_lag_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$cum_wins_lag_team)))))

        sub_num$rank_cum_points_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$cum_points_team)))))
        sub_num$rank_mov_avg_points_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$mov_avg_points_team)))))
        sub_num$rank_momentum_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$momentum_team)))))
        
        sub_num$rank_win_streak_team <- floor(as.numeric(as.character(rank(as.numeric(-sub_num$win_streak_team)))))
        
      }
      
     
      num_list[[j]] <- sub_num
    }
    
    # combine numly data
    num_data <- do.call('rbind', num_list)
    
    year_list[[i]] <- num_data
  }
  
  final_data <- do.call('rbind', year_list)
  
  return(final_data)
}


# # function for plotting per minute
# temp_dat = dat_stats
# y_axis = 'ft_mean'
# x_lab = ''
# y_lab = 'Avg free throws made per min played'
# add_top_margin = TRUE
# year = '2015-2016'
plot_per_min <- function(temp_dat, 
                         x_axis, 
                         y_axis, 
                         x_lab, 
                         y_lab,
                         add_top_margin,
                         year){
  
  
  # first subset 
  if(year == 'all') {
    temp_title <- '2015-2019'
    temp_title <- paste0(temp_title, ' Regular seasons')
  } else {
    temp_title <- year
    temp_title <- paste0(temp_title, ' Regular season')
    temp_dat <- temp_dat[temp_dat$dataset == year,]
  }
  
  # # rename
  # names(temp_dat)[names(temp_dat) == x_axis] <- 'V1'
  names(temp_dat)[names(temp_dat) == y_axis] <- 'V2'
  
  temp <- temp_dat %>% 
    filter(!is.infinite(V2)) %>%
    arrange(-V2) 
  
  temp <- temp[1:10,]
  
  if(add_top_margin){
    # get max y 
    max_y <- max(temp_dat$V2) + 0.05
  } else {
    max_y <- max(temp_dat$V2)
  }
 
  
  # plot
  g1 <-  ggplot(temp, aes(reorder(last_name, -V2), V2)) +
    geom_point(size=3) + 
    geom_segment(aes(x=last_name, 
                     xend=last_name, 
                     y=0, 
                     yend=V2)) +
    ylim(c(0, max_y)) +
    labs(x = x_lab, 
         y = y_lab,
         title = temp_title) +
    theme_hc() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  return(g1)
}


#########
# create plotting function that takes column name for barplot
#########
plot_bar <- function(temp_dat, 
                     x_var, 
                     y_var,
                     x_lab,
                     y_lab){
  names(temp_dat)[names(temp_dat) == x_var] <- 'V1'
  names(temp_dat)[names(temp_dat) == y_var] <- 'V2'
  
  g1 <- ggplot(temp_dat, aes(reorder(V1, -V2), V2)) + 
    geom_bar(stat = 'identity') +
    labs(x = x_lab, 
         y = y_lab) +
    theme_hc() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  return(g1)
}


