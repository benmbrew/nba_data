


# Game Score = Points Scored + (0.4 x Field Goals) – (0.7 x Field Goal Attempts) – (0.4 x (Free Throw Attempts – Free Throws)) + (0.7 x Offensive Rebounds) + (0.3 x Defensive Rebounds) + Steals + (0.7 x Assists) + (0.7 x Blocks) – (0.4 x Personal Fouls) – Turnovers
get_game_score <- function(temp_dat) {
  game_score <- temp_dat$pts + (0.4* temp_dat$fg) - (0.7*temp_dat$fga) - (0.4*(temp_dat$fta - temp_dat$ft)) +
    (0.7*temp_dat$or) + (0.3*temp_dat$dr) + temp_dat$st + (0.7* temp_dat$a) + (0.7*temp_dat$bl) - 
    (0.4*temp_dat$pf) - temp_dat$to
  return(game_score)
}

# fill NA and Inf with 0
full_inf_with_na <- function(temp_dat) {
  temp_dat[is.na(temp_dat)] <- 0
  temp_dat$fga_per[is.infinite(temp_dat$fga_per)] <- NA
  temp_dat$pts_per[is.infinite(temp_dat$pts_per)] <- NA
  temp_dat$to_per[is.infinite(temp_dat$to_per)] <- NA
  
  return(temp_dat)
}
