
#fill na with zero
fill_na_zero <- function(data_frame) {
  col_index <- apply(data_frame, 2, function(x) any(is.na(x)))
  fill_cols <- colnames(data_frame)[col_index]
  for(i in fill_cols){
    data_frame[, i][is.na(dat_full[, i])] <- 0
  }
  return(data_frame)
}

# create a function that uses lag to get previous weeks data 

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


# creat function to go through each game id and get data on opposing team
get_opposing_team_stats <- function(temp_dat){
  result_list <- list()
  
  for(i in 1:nrow(temp_dat)){
    sub_dat <- temp_dat[temp_dat$game_id == i, ]
    sub_dat$dup_ind <- duplicated(sub_dat$game_id)
    sub_dat_1 <- sub_dat[sub_dat$dup_ind == TRUE,]
    sub_dat_2 <- sub_dat[sub_dat$dup_ind == FALSE,]
    sub_dat_1$dup_ind <- sub_dat_2$dup_ind <- NULL
    
    # grab features for sub_dat_1
    sub_dat_1$team_opp <- sub_dat_2$team
    sub_dat_1$last_game_opp <- sub_dat_2$last_game
    sub_dat_1$final_opp <- sub_dat_2$final_team
    sub_dat_1$closing_spread_opp <- sub_dat_2$closing_spread_team
    sub_dat_1$cum_wins_lag_opp <- sub_dat_2$cum_wins_lag
    sub_dat_1$cum_wins_per_lag_opp <- sub_dat_2$cum_wins_per_lag_team
    sub_dat_1$cum_points_opp <- sub_dat_2$cum_points_team
    sub_dat_1$cum_points_allowed_by_def_opp <- sub_dat_2$cum_points_allowed_by_def_team
    sub_dat_1$cum_total_yds_opp <- sub_dat_2$cum_total_yds_team
    
    sub_dat_1$mov_avg_points_opp <- sub_dat_2$mov_avg_points_team
    sub_dat_1$mov_avg_points_allowed_by_def_opp <- sub_dat_2$mov_avg_points_allowed_by_def_team
    sub_dat_1$mov_avg_total_yds_opp <- sub_dat_2$mov_avg_total_yds_team
    
    sub_dat_1$win_streak_opp <- sub_dat_2$win_streak_team
    sub_dat_1$lose_streak_opp <- sub_dat_2$lose_streak_team
    sub_dat_1$mov_avg_first_downs_opp <- sub_dat_2$mov_avg_first_downs_team
    sub_dat_1$mov_avg_rush_yds_opp <- sub_dat_2$mov_avg_rush_yds_team
    sub_dat_1$mov_avg_rush_tds_opp <- sub_dat_2$mov_avg_rush_tds_team
    sub_dat_1$mov_avg_pass_comp_opp <- sub_dat_2$mov_avg_pass_comp_team
    sub_dat_1$mov_avg_pass_yds_opp <- sub_dat_2$mov_avg_pass_yds_team
    sub_dat_1$mov_avg_pass_tds_opp <- sub_dat_2$mov_avg_pass_tds_team
    sub_dat_1$mov_avg_qb_interceptions_opp <- sub_dat_2$mov_avg_qb_interceptions_team
    sub_dat_1$mov_avg_qb_sacked_opp <- sub_dat_2$mov_avg_qb_sacked_team
    sub_dat_1$mov_avg_fumbles_opp <- sub_dat_2$mov_avg_fumbles_team
    sub_dat_1$mov_avg_turnovers_opp <- sub_dat_2$mov_avg_turnovers_team
    sub_dat_1$mov_avg_penalties_opp <- sub_dat_2$mov_avg_penalties_team
    sub_dat_1$mov_avg_def_interception_opp <- sub_dat_2$mov_avg_def_interception_team
    sub_dat_1$mov_avg_def_sack_opp <- sub_dat_2$mov_avg_def_sack_team
    sub_dat_1$mov_avg_first_opp <- sub_dat_2$mov_avg_first_team
    sub_dat_1$mov_avg_second_opp <- sub_dat_2$mov_avg_second_team
    sub_dat_1$mov_avg_third_opp <- sub_dat_2$mov_avg_third_team
    sub_dat_1$mov_avg_fourth_opp <- sub_dat_2$mov_avg_fourth_team
    sub_dat_1$mov_avg_final_opp <- sub_dat_2$mov_avg_final_team
    sub_dat_1$mov_avg_points_allowed_by_def_opp <- sub_dat_2$mov_avg_points_allowed_by_def_team
    sub_dat_1$mov_avg_third_down_made_opp <- sub_dat_2$mov_avg_third_down_made_team
    sub_dat_1$mov_avg_third_down_att_opp <- sub_dat_2$mov_avg_third_down_att_team
    sub_dat_1$mov_avg_third_down_per_opp <- sub_dat_2$mov_avg_third_down_per_team
    sub_dat_1$mov_avg_fourth_down_made_opp <- sub_dat_2$mov_avg_fourth_down_made_team
    sub_dat_1$mov_avg_fourth_down_att_opp <- sub_dat_2$mov_avg_fourth_down_att_team
    sub_dat_1$mov_avg_fourth_down_per_opp <- sub_dat_2$mov_avg_fourth_down_per_team
    sub_dat_1$mov_avg_time_of_poss_opp <- sub_dat_2$mov_avg_time_of_poss_team
    sub_dat_1$mov_avg_opening_spread_opp <- sub_dat_2$mov_avg_opening_spread_team
    sub_dat_1$mov_avg_opening_total_opp <- sub_dat_2$mov_avg_opening_total_team
    
    
    sub_dat_1$cum_sum_first_downs_opp <- sub_dat_2$cum_sum_first_downs_team
    sub_dat_1$cum_sum_rush_yds_opp <- sub_dat_2$cum_sum_rush_yds_team
    sub_dat_1$cum_sum_rush_tds_opp <- sub_dat_2$cum_sum_rush_tds_team
    sub_dat_1$cum_sum_pass_comp_opp <- sub_dat_2$cum_sum_pass_comp_team
    sub_dat_1$cum_sum_pass_yds_opp <- sub_dat_2$cum_sum_pass_yds_team
    sub_dat_1$cum_sum_pass_tds_opp <- sub_dat_2$cum_sum_pass_tds_team
    sub_dat_1$cum_sum_qb_interceptions_opp <- sub_dat_2$cum_sum_qb_interceptions_team
    sub_dat_1$cum_sum_qb_sacked_opp <- sub_dat_2$cum_sum_qb_sacked_team
    sub_dat_1$cum_sum_fumbles_opp <- sub_dat_2$cum_sum_fumbles_team
    sub_dat_1$cum_sum_turnovers_opp <- sub_dat_2$cum_sum_turnovers_team
    sub_dat_1$cum_sum_penalties_opp <- sub_dat_2$cum_sum_penalties_team
    sub_dat_1$cum_sum_def_interception_opp <- sub_dat_2$cum_sum_def_interception_team
    sub_dat_1$cum_sum_def_sack_opp <- sub_dat_2$cum_sum_def_sack_team
    sub_dat_1$cum_sum_first_opp <- sub_dat_2$cum_sum_first_team
    sub_dat_1$cum_sum_second_opp <- sub_dat_2$cum_sum_second_team
    sub_dat_1$cum_sum_third_opp <- sub_dat_2$cum_sum_third_team
    sub_dat_1$cum_sum_fourth_opp <- sub_dat_2$cum_sum_fourth_team
    sub_dat_1$cum_sum_final_opp <- sub_dat_2$cum_sum_final_team
    sub_dat_1$cum_sum_points_allowed_by_def_opp <- sub_dat_2$cum_sum_points_allowed_by_def_team
    sub_dat_1$cum_sum_third_down_made_opp <- sub_dat_2$cum_sum_third_down_made_team
    sub_dat_1$cum_sum_third_down_att_opp <- sub_dat_2$cum_sum_third_down_att_team
    sub_dat_1$cum_sum_third_down_per_opp <- sub_dat_2$cum_sum_third_down_per_team
    sub_dat_1$cum_sum_fourth_down_made_opp <- sub_dat_2$cum_sum_fourth_down_made_team
    sub_dat_1$cum_sum_fourth_down_att_opp <- sub_dat_2$cum_sum_fourth_down_att_team
    sub_dat_1$cum_sum_fourth_down_per_opp <- sub_dat_2$cum_sum_fourth_down_per_team
    sub_dat_1$cum_sum_time_of_poss_opp <- sub_dat_2$cum_sum_time_of_poss_team
    sub_dat_1$cum_sum_opening_spread_opp <- sub_dat_2$cum_sum_opening_spread_team
    sub_dat_1$cum_sum_opening_total_opp <- sub_dat_2$cum_sum_opening_total_team
    
    # grab features for sub_dat_2
    sub_dat_2$team_opp <- sub_dat_1$team
    sub_dat_2$last_game_opp <- sub_dat_1$last_game
    sub_dat_2$final_opp <- sub_dat_1$final_team
    sub_dat_2$closing_spread_opp <- sub_dat_1$closing_spread_team
    sub_dat_2$cum_wins_lag_opp <- sub_dat_1$cum_wins_lag_team
    sub_dat_2$cum_wins_per_lag_opp <- sub_dat_1$cum_wins_per_lag_team
    sub_dat_2$cum_points_opp <- sub_dat_1$cum_points_team
    sub_dat_2$cum_points_allowed_by_def_opp <- sub_dat_1$cum_points_allowed_by_def_team
    sub_dat_2$cum_total_yds_opp <- sub_dat_1$cum_total_yds_team
    
    sub_dat_2$mov_avg_points_opp <- sub_dat_1$mov_avg_points_team
    sub_dat_2$mov_avg_points_allowed_by_def_opp <- sub_dat_1$mov_avg_points_allowed_by_def_team
    sub_dat_2$mov_avg_total_yds_opp <- sub_dat_1$mov_avg_total_yds_team
    
    sub_dat_2$win_streak_opp <- sub_dat_1$win_streak_team
    sub_dat_2$lose_streak_opp <- sub_dat_1$lose_streak_team
    sub_dat_2$mov_avg_first_downs_opp <- sub_dat_1$mov_avg_first_downs_team
    sub_dat_2$mov_avg_rush_yds_opp <- sub_dat_1$mov_avg_rush_yds_team
    sub_dat_2$mov_avg_rush_tds_opp <- sub_dat_1$mov_avg_rush_tds_team
    sub_dat_2$mov_avg_pass_comp_opp <- sub_dat_1$mov_avg_pass_comp_team
    sub_dat_2$mov_avg_pass_yds_opp <- sub_dat_1$mov_avg_pass_yds_team
    sub_dat_2$mov_avg_pass_tds_opp <- sub_dat_1$mov_avg_pass_tds_team 
    sub_dat_2$mov_avg_qb_interceptions_opp <- sub_dat_1$mov_avg_qb_interceptions_team
    sub_dat_2$mov_avg_qb_sacked_opp <- sub_dat_1$mov_avg_qb_sacked_team
    sub_dat_2$mov_avg_fumbles_opp <- sub_dat_1$mov_avg_fumbles_team
    sub_dat_2$mov_avg_turnovers_opp <- sub_dat_1$mov_avg_turnovers_team
    sub_dat_2$mov_avg_penalties_opp <- sub_dat_1$mov_avg_penalties_team
    sub_dat_2$mov_avg_def_interception_opp <- sub_dat_1$mov_avg_def_interception_team
    sub_dat_2$mov_avg_def_sack_opp <- sub_dat_1$mov_avg_def_sack_team
    sub_dat_2$mov_avg_first_opp <- sub_dat_1$mov_avg_first_team
    sub_dat_2$mov_avg_second_opp <- sub_dat_1$mov_avg_second_team
    sub_dat_2$mov_avg_third_opp <- sub_dat_1$mov_avg_third_team
    sub_dat_2$mov_avg_fourth_opp <- sub_dat_1$mov_avg_fourth_team
    sub_dat_2$mov_avg_final_opp <- sub_dat_1$mov_avg_final_team
    sub_dat_2$mov_avg_points_allowed_by_def_opp <- sub_dat_1$mov_avg_points_allowed_by_def_team
    sub_dat_2$mov_avg_third_down_made_opp <- sub_dat_1$mov_avg_third_down_made_team
    sub_dat_2$mov_avg_third_down_att_opp <- sub_dat_1$mov_avg_third_down_att_team
    sub_dat_2$mov_avg_third_down_per_opp <- sub_dat_1$mov_avg_third_down_per_team
    sub_dat_2$mov_avg_fourth_down_made_opp <- sub_dat_1$mov_avg_fourth_down_made_team
    sub_dat_2$mov_avg_fourth_down_att_opp <- sub_dat_1$mov_avg_fourth_down_att_team
    sub_dat_2$mov_avg_fourth_down_per_opp <- sub_dat_1$mov_avg_fourth_down_per_team
    sub_dat_2$mov_avg_time_of_poss_opp <- sub_dat_1$mov_avg_time_of_poss_team
    sub_dat_2$mov_avg_opening_spread_opp <- sub_dat_1$mov_avg_opening_spread_team
    sub_dat_2$mov_avg_opening_total_opp <- sub_dat_1$mov_avg_opening_total_team
    
    sub_dat_2$cum_sum_first_downs_opp <- sub_dat_1$cum_sum_first_downs_team
    sub_dat_2$cum_sum_rush_yds_opp <- sub_dat_1$cum_sum_rush_yds_team
    sub_dat_2$cum_sum_rush_tds_opp <- sub_dat_1$cum_sum_rush_tds_team
    sub_dat_2$cum_sum_pass_comp_opp <- sub_dat_1$cum_sum_pass_comp_team
    sub_dat_2$cum_sum_pass_yds_opp <- sub_dat_1$cum_sum_pass_yds_team
    sub_dat_2$cum_sum_pass_tds_opp <- sub_dat_1$cum_sum_pass_tds_team 
    sub_dat_2$cum_sum_qb_interceptions_opp <- sub_dat_1$cum_sum_qb_interceptions_team
    sub_dat_2$cum_sum_qb_sacked_opp <- sub_dat_1$cum_sum_qb_sacked_team
    sub_dat_2$cum_sum_fumbles_opp <- sub_dat_1$cum_sum_fumbles_team
    sub_dat_2$cum_sum_turnovers_opp <- sub_dat_1$cum_sum_turnovers_team
    sub_dat_2$cum_sum_penalties_opp <- sub_dat_1$cum_sum_penalties_team
    sub_dat_2$cum_sum_def_interception_opp <- sub_dat_1$cum_sum_def_interception_team
    sub_dat_2$cum_sum_def_sack_opp <- sub_dat_1$cum_sum_def_sack_team
    sub_dat_2$cum_sum_first_opp <- sub_dat_1$cum_sum_first_team
    sub_dat_2$cum_sum_second_opp <- sub_dat_1$cum_sum_second_team
    sub_dat_2$cum_sum_third_opp <- sub_dat_1$cum_sum_third_team
    sub_dat_2$cum_sum_fourth_opp <- sub_dat_1$cum_sum_fourth_team
    sub_dat_2$cum_sum_final_opp <- sub_dat_1$cum_sum_final_team
    sub_dat_2$cum_sum_points_allowed_by_def_opp <- sub_dat_1$cum_sum_points_allowed_by_def_team
    sub_dat_2$cum_sum_third_down_made_opp <- sub_dat_1$cum_sum_third_down_made_team
    sub_dat_2$cum_sum_third_down_att_opp <- sub_dat_1$cum_sum_third_down_att_team
    sub_dat_2$cum_sum_third_down_per_opp <- sub_dat_1$cum_sum_third_down_per_team
    sub_dat_2$cum_sum_fourth_down_made_opp <- sub_dat_1$cum_sum_fourth_down_made_team
    sub_dat_2$cum_sum_fourth_down_att_opp <- sub_dat_1$cum_sum_fourth_down_att_team
    sub_dat_2$cum_sum_fourth_down_per_opp <- sub_dat_1$cum_sum_fourth_down_per_team
    sub_dat_2$cum_sum_time_of_poss_opp <- sub_dat_1$cum_sum_time_of_poss_team
    sub_dat_2$cum_sum_opening_spread_opp <- sub_dat_1$cum_sum_opening_spread_team
    sub_dat_2$cum_sum_opening_total_opp <- sub_dat_1$cum_sum_opening_total_team
    
    # combine date frames and store in list result_list
    sub_dat_both <- rbind(sub_dat_1,
                          sub_dat_2)
    
    result_list[[i]] <- sub_dat_both
    
    message('finished with game_id = ', i)
    
  }
  
  final_data <- do.call('rbind', result_list)
  return(final_data)
}




# function that takes every other row and attaches to the dataframe 
get_by_game <- function(dat) {
  
  # make column names lower case 
  colnames(dat) <- tolower(colnames(dat))

  temp_new_game <- list()
  
  # loop through by 2 and combine 
  for(game in unique(dat$game_number)){
    # subset data 
    temp_game <- dat[dat$game_number == game,]
    
    # get first and second row
    temp_1st_row <- as.data.frame(temp_game[1,])
    temp_2nd_row <- as.data.frame(temp_game[2,])
    
    # add "away" to 1st row columns
    colnames(temp_1st_row) <- paste0(colnames(temp_1st_row), '_away')
    colnames(temp_2nd_row) <- paste0(colnames(temp_2nd_row), '_home')
    
    # bind them together 
    temp_new_game[[game]] <- cbind(temp_2nd_row, temp_1st_row)
    
  }
  final_game <- do.call(rbind, temp_new_game)
  return(final_game)
}

