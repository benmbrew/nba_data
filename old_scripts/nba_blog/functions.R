# create function that combines by game 
combine_game_rows <- function(temp_dat){
  data_list <- list()
  team_names <- unique(temp_dat$team)
  
  for(i in 1:length(team_names)){
    this_team <- team_names[i]
    sub_team <- temp_dat[temp_dat$team == this_team,]
    sub_opp <- temp_dat[temp_dat$team != this_team,]
    
    #combine data
    sub_all <- inner_join(sub_team, sub_opp, by = 'game_id')
    
    # fix names
    names(sub_all) <- gsub('.x', '_team', names(sub_all), fixed = TRUE)
    names(sub_all) <- gsub('.y', '_opp', names(sub_all), fixed = TRUE)
    
    # store in list
    data_list[[i]] <- sub_all
    
  }
  
  # collapse list 
  final_dat <- do.call('rbind', data_list)
  
  # remove duplicate game_ids
  final_dat <- final_dat[!duplicated(final_dat$game_id),]
  
  # get diff 
  final_dat$point_diff <- final_dat$f_team - final_dat$f_opp
  
  return(final_dat)
  
}

#fill na with zero
fill_na_zero <- function(data_frame) {
  col_index <- apply(data_frame, 2, function(x) any(is.na(x)))
  fill_cols <- colnames(data_frame)[col_index]
  for(i in fill_cols){
    data_frame[, i][is.na(dat_full[, i])] <- 0
  }
  return(data_frame)
}

# Get chi squared statistic for each team
# loop through each team and do chi square on venue and final result
get_chi_squared <- function(dat_test) {
  result_list <- list()
  team_vector <- unique(dat_test$teams)
  for(i in 1:length(unique(dat_test$teams))) {
    temp_team <- dat_test[dat_test$teams == team_vector[i],]
    team_name <- unique(temp_team$teams)
    team_table <- table(temp_team$venue, temp_team$final_result)
    team_home_w <- nrow(temp_team[temp_team$venue == 'Home' & temp_team$final_result == 'W',])
    team_home_l <- nrow(temp_team[temp_team$venue == 'Home' & temp_team$final_result == 'L',])
    team_away_w <- nrow(temp_team[temp_team$venue == 'Road' & temp_team$final_result == 'W',])
    team_away_l <- nrow(temp_team[temp_team$venue == 'Road' & temp_team$final_result == 'L',])
    
    chi_sqr_test <- chisq.test(team_table, correct = FALSE)
    temp_tab <- as.data.frame(cbind(team_name, round(chi_sqr_test$statistic,4), round(chi_sqr_test$p.value,4), team_home_w, team_home_l, team_away_w, team_away_l))
    colnames(temp_tab) <- c('team_name', 'chi_square_statistic', 'p_value', 'home_wins', 'home_losses', 'away_wins', 'away_losses')
    temp_tab$chi_square_statistic <- as.numeric(as.character(temp_tab$chi_square_statistic))
    temp_tab$p_value<- as.numeric(as.character(temp_tab$p_value))
    temp_tab$home_losses <- as.numeric(as.character(temp_tab$home_losses))
    temp_tab$home_wins <- as.numeric(as.character(temp_tab$home_wins))
    temp_tab$away_losses <- as.numeric(as.character(temp_tab$away_losses))
    temp_tab$away_wins <- as.numeric(as.character(temp_tab$away_wins))
    
    result_list[[i]] <- temp_tab
  }
  
  # combine list 
  temp_results <- do.call('rbind', result_list)
  
  # order by test stat
  temp_results <- temp_results[order(temp_results$chi_square_statistic, decreasing = TRUE),]
  
  return(temp_results)
  
}

team_stat_diff <- function(temp_dat){
  data_list <- list()
  game_ids <- unique(temp_dat$game_id)
  
  for(i in 1:length(game_ids)){
    this_game <- game_ids[i]
    sub_game <- temp_dat %>% filter(game_id == this_game)
    
    # get stat differentials
    win_index <- which(sub_game$win_loss == 'W')
    loss_index <- which(sub_game$win_loss == 'L')
    
    # differentials - fg, fga, three_p, three_pa, ft, fta, or, dr, tot, a, pf, st, to, bl pts, poss, 
    # pave, oeff per_first_q, per_second_q, per_third_q, per_fourth_q, fg_per, three_per, ft_per
    sub_game$fg_diff[win_index] <- sub_game$fg[win_index] - sub_game$fg[loss_index]
    sub_game$fg_diff[loss_index] <- sub_game$fg[loss_index] - sub_game$fg[win_index]
    sub_game$fga_diff[win_index] <- sub_game$fga[win_index] - sub_game$fga[loss_index]
    sub_game$fga_diff[loss_index] <- sub_game$fga[loss_index] - sub_game$fga[win_index]
    sub_game$three_p_diff[win_index] <- sub_game$three_p[win_index] - sub_game$three_p[loss_index]
    sub_game$three_p_diff[loss_index] <- sub_game$three_p[loss_index] - sub_game$three_p[win_index]
    sub_game$three_pa_diff[win_index] <- sub_game$three_pa[win_index] - sub_game$three_pa[loss_index]
    sub_game$three_pa_diff[loss_index] <- sub_game$three_pa[loss_index] - sub_game$three_pa[win_index]
    sub_game$ft_diff[win_index] <- sub_game$ft[win_index] - sub_game$ft[loss_index]
    sub_game$ft_diff[loss_index] <- sub_game$ft[loss_index] - sub_game$ft[win_index]
    sub_game$fta_diff[win_index] <- sub_game$fta[win_index] - sub_game$fta[loss_index]
    sub_game$fta_diff[loss_index] <- sub_game$fta[loss_index] - sub_game$fta[win_index]
    sub_game$or_diff[win_index] <- sub_game$or[win_index] - sub_game$or[loss_index]
    sub_game$or_diff[loss_index] <- sub_game$or[loss_index] - sub_game$or[win_index]
    sub_game$dr_diff[win_index] <- sub_game$dr[win_index] - sub_game$dr[loss_index]
    sub_game$dr_diff[loss_index] <- sub_game$dr[loss_index] - sub_game$dr[win_index]
    sub_game$tot_diff[win_index] <- sub_game$tot[win_index] - sub_game$tot[loss_index]
    sub_game$tot_diff[loss_index] <- sub_game$tot[loss_index] - sub_game$tot[win_index]
    sub_game$a_diff[win_index] <- sub_game$a[win_index] - sub_game$a[loss_index]
    sub_game$a_diff[loss_index] <- sub_game$a[loss_index] - sub_game$a[win_index]
    sub_game$pf_diff[win_index] <- sub_game$pf[win_index] - sub_game$pf[loss_index]
    sub_game$pf_diff[loss_index] <- sub_game$pf[loss_index] - sub_game$pf[win_index]
    sub_game$st_diff[win_index] <- sub_game$st[win_index] - sub_game$st[loss_index]
    sub_game$st_diff[loss_index] <- sub_game$st[loss_index] - sub_game$st[win_index]
    sub_game$to_diff[win_index] <- sub_game$to[win_index] - sub_game$to[loss_index]
    sub_game$to_diff[loss_index] <- sub_game$to[loss_index] - sub_game$to[win_index]
    sub_game$bl_diff[win_index] <- sub_game$bl[win_index] - sub_game$bl[loss_index]
    sub_game$bl_diff[loss_index] <- sub_game$bl[loss_index] - sub_game$bl[win_index]
    sub_game$pts_diff[win_index] <- sub_game$pts[win_index] - sub_game$pts[loss_index]
    sub_game$pts_diff[loss_index] <- sub_game$pts[loss_index] - sub_game$pts[win_index]
    sub_game$poss_diff[win_index] <- sub_game$poss[win_index] - sub_game$poss[loss_index]
    sub_game$poss_diff[loss_index] <- sub_game$poss[loss_index] - sub_game$poss[win_index]
    sub_game$pace_diff[win_index] <- sub_game$pace[win_index] - sub_game$pace[loss_index]
    sub_game$pace_diff[loss_index] <- sub_game$pace[loss_index] - sub_game$pace[win_index]
    sub_game$oeff_diff[win_index] <- sub_game$oeff[win_index] - sub_game$oeff[loss_index]
    sub_game$oeff_diff[loss_index] <- sub_game$oeff[loss_index] - sub_game$oeff[win_index]
    sub_game$deff_diff[win_index] <- sub_game$deff[win_index] - sub_game$deff[loss_index]
    sub_game$deff_diff[loss_index] <- sub_game$deff[loss_index] - sub_game$deff[win_index]
    sub_game$first_q_per_diff[win_index] <- sub_game$per_first_q[win_index] - sub_game$per_first_q[loss_index]
    sub_game$first_q_per_diff[loss_index] <- sub_game$per_first_q[loss_index] - sub_game$per_first_q[win_index]
    sub_game$second_q_per_diff[win_index] <- sub_game$per_second_q[win_index] - sub_game$per_second_q[loss_index]
    sub_game$second_q_per_diff[loss_index] <- sub_game$per_second_q[loss_index] - sub_game$per_second_q[win_index]
    sub_game$third_q_per_diff[win_index] <- sub_game$per_third_q[win_index] - sub_game$per_third_q[loss_index]
    sub_game$third_q_per_diff[loss_index] <- sub_game$per_third_q[loss_index] - sub_game$per_third_q[win_index]
    sub_game$fourth_q_per_diff[win_index] <- sub_game$per_fourth_q[win_index] - sub_game$per_fourth_q[loss_index]
    sub_game$fourth_q_per_diff[loss_index] <- sub_game$per_fourth_q[loss_index] - sub_game$per_fourth_q[win_index]
    sub_game$three_per_diff[win_index] <-  sub_game$three_per[win_index] -sub_game$three_per[loss_index]
    sub_game$three_per_diff[loss_index] <-  sub_game$three_per[loss_index] -sub_game$three_per[win_index]
    sub_game$fg_per_diff[win_index] <-  sub_game$fg_per[win_index] -sub_game$fg_per[loss_index]
    sub_game$fg_per_diff[loss_index] <-  sub_game$fg_per[loss_index] -sub_game$fg_per[win_index]
    sub_game$ft_per_diff[win_index] <-  sub_game$ft_per[win_index] -sub_game$ft_per[loss_index]
    sub_game$ft_per_diff[loss_index] <-  sub_game$ft_per[loss_index] -sub_game$ft_per[win_index]
    
    # store in list
    data_list[[i]] <- sub_game
  }
  
  final_dat <- do.call('rbind', data_list)
  return(final_dat)
}


# function that takes every other row and attaches to the dataframe 
get_by_game <- function(dat) {
  
  # make column names lower case 
  colnames(dat) <- tolower(colnames(dat))
  
  
  temp_new_game <- list()
  
  # loop through by 2 and combine 
  for(game in unique(dat$game_id)){
    # subset data 
    temp_game <- dat[dat$game_id == game,]
    
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

# get game number
get_game_num <- function(temp_dat) {
  
  result_list <- list()
  unique_teams <- unique(temp_dat$TEAMS)
  
  for(i in 1:length(unique(temp_dat$TEAMS))){
    sub_dat <- temp_dat[temp_dat$TEAMS == unique_teams[i],]
    sub_dat$game_num <- seq(1, nrow(sub_dat), 1)
    result_list[[i]] <- sub_dat
  }
  
  result_dat <- do.call('rbind', result_list)
  return(result_dat)
}

# clean the column names and keep only relevant columns - at this point, just the odds and game outcome. 
remove_cols <- function(dat, keep_cols) {
  names(dat) <- gsub(' ', '_', names(dat))
  dat <- dat[, names(dat) %in% keep_cols]
  return(dat)
}

# pllot a distribution of the errors for each season 
hist_plot <- function(temp_dat, plot_title){
  temp_dat <- temp_dat %>% filter(variable %in% 'Total points scored')
  n_row <- nrow(temp_dat)
  
  g1 <- ggplot(temp_dat, aes(x=diff)) + 
    geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                   binwidth=5,
                   colour="black", fill="#A7A7A7") +
    geom_density(aes(y = ..density.. *(n_row*5)),
                 alpha=.2, fill= "dodgerblue") +
    geom_vline(xintercept = 0) +
    labs(title = plot_title, x = 'Difference  between Real total and betting mkt total', y = 'Counts') +
    theme_minimal(base_size = 12, base_family = 'Ubuntu')
  
  return(g1)
}

# pllot a distribution of the errors for each season 
hist_plot_double <- function(temp_dat, plot_title){
  n_row <- nrow(temp_dat)/2
  cols <- c("#A7A7A7",
            "dodgerblue")
  g1 <- ggplot(temp_dat, aes(x=value, fill = variable)) + 
    geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                   binwidth=10,
                   colour="grey", position = 'dodge',
                   alpha = 0.6) +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(title = plot_title, x = 'Difference  between Real total and betting mkt total', y = 'Counts') +
    theme_minimal(base_size = 12, base_family = 'Ubuntu')
  
  return(g1)
}


# function that plots the over under over time 
points_plot <- function(temp_dat, column_index,smooth_line, plot_title, x_time) {
  
  # revlevel variable if needed
  cols <- c("#A7A7A7",
            "dodgerblue")
   g <- ggplot(data = temp_dat,
              aes(x = date,
                  y = value,
                  group = variable,
                  colour = variable,
                  text = paste0('<br>', home_team, ": ", pts_home,
                                '<br>', away_team, ": ", pts_away))) 
    
  g1 <- g +  geom_point(size = 1.5, alpha = 0.2)
  
  if(smooth_line){
    g1 <- g1 + geom_smooth()
  }
  
  g2 <- g1 + labs(x = '', 
         y = 'Total points',
         title = plot_title) +
    scale_color_manual(name = '', 
                       values = cols) + 
    theme_bw(base_size = 12, 
             base_family = 'Ubuntu') + scale_x_date(breaks = date_breaks(x_time), labels = date_format("%b-%y"))
  
  p1 <- plotly::ggplotly(g2, tooltip = 'text') %>% 
    config(displayModeBar = F) %>% 
    layout( 
      legend = list(
        orientation = "l",
        x = 0,
        y = -0.6))
  
  return(p1)
  
}


get_w_l <- function(temp_dat){
  
  # define new variables 
  temp_dat$win_loss <- NA
  
  # define list 
  data_list <- list()
  all_games <- unique(temp_dat$game_id)
  
  for(i in 1:length(all_games)) {
    
    # get game number 
    this_game <- all_games[i]
    
    # subset to individual game level
    sub_game <- temp_dat[temp_dat$game_id == this_game, ]
    
    # get max of pts index to indicate winner
    max_pts <- max(sub_game$pts)
    max_pts_index <- which(sub_game$pts == max_pts)
    
    # add win indicator
    if(max_pts_index == 1){
      sub_game$win_loss[1] <- 'W'
      sub_game$win_loss[2] <- 'L'
      
    } else {
      sub_game$win_loss[1] <- 'L'
      sub_game$win_loss[2] <- 'W'
    }
    
    # store in list
    data_list[[i]] <- sub_game
    
  }
  
  # collapse list 
  final_dat <- do.call('rbind', data_list)
  
  return(final_dat)
  
}







