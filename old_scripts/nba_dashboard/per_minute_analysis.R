
# run global player script
source('../global_player.R')

##########
# get player minutes stats
##########

# first get avg minutes played with game counts
dat_stats <- dat_player %>%
  group_by(player_name) %>%
  mutate(mean_min = mean(minutes, na.rm = TRUE),
         game_counts = n()) %>%
  filter(game_counts >= 30)
rm(dat_player)

# get stats per minute
dat_stats$fg_min <- round((dat_stats$fg/dat_stats$minutes), 2)
dat_stats$fga_min <- round((dat_stats$fga/dat_stats$minutes), 2)
dat_stats$three_p_min<- round((dat_stats$three_p/dat_stats$minutes), 2)
dat_stats$three_a_min<- round((dat_stats$three_a/dat_stats$minutes), 2)
dat_stats$ft_min <- round((dat_stats$ft/dat_stats$minutes), 2)
dat_stats$fta_min <- round((dat_stats$fta/dat_stats$minutes), 2)
dat_stats$or_min <- round((dat_stats$or/dat_stats$minutes), 2)
dat_stats$dr_min <- round((dat_stats$dr/dat_stats$minutes), 2)
dat_stats$tot_r_min <- round((dat_stats$tot_r/dat_stats$minutes), 2)
dat_stats$assists_min <- round((dat_stats$assists/dat_stats$minutes), 2)
dat_stats$personal_fouls_min <- round((dat_stats$personal_fouls/dat_stats$minutes), 2)
dat_stats$steals_min <- round((dat_stats$steals/dat_stats$minutes), 2)

# delete other columns
dat_stats$fg <- dat_stats$fga <- dat_stats$three_p <- dat_stats$three_a <- dat_stats$ft <-
  dat_stats$fta <- dat_stats$or <- dat_stats$dr <- dat_stats$tot_r <- dat_stats$assists <-
  dat_stats$personal_fouls <- dat_stats$steals <- NULL

# get last name 
dat_stats$last_name <- unlist(lapply(strsplit(as.character(dat_stats$player_name), split = ' '), 
                                     function(x) x[length(x)]))


# group by player name and get per minute means
dat_stats <- dat_stats %>%
  group_by(last_name, dataset) %>%
  summarise(fg_mean = round(mean(fg_min, na.rm = TRUE), 3),
            fga_mean = round(mean(fga_min, na.rm = TRUE), 3),
            three_mean= round(mean(three_p_min, na.rm = TRUE), 3),
            threea_mean= round(mean(three_a_min, na.rm = TRUE), 3),
            ft_mean = round(mean(ft_min, na.rm = TRUE), 3),
            fta_mean = round(mean(fta_min, na.rm = TRUE), 3),
            or_mean = round(mean(or_min, na.rm = TRUE), 3),
            dr_mean = round(mean(dr_min, na.rm = TRUE), 3),
            tot_r_mean = round(mean(tot_r_min, na.rm = TRUE), 3),
            assists_mean = round(mean(assists_min, na.rm = TRUE), 3),
            pf_mean = round(mean(personal_fouls_min, na.rm = TRUE), 3),
            steals_mean = round(mean(steals_min, na.rm = TRUE), 3))

# remove NBA from dataset
dat_stats$dataset <- gsub('NBA ', '', dat_stats$dataset)
dat_stats$dataset <- gsub(' Regular Season', '', dat_stats$dataset)


# get top mean fg per minute

pdf('../blog_posts/visuals/fg_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fg_mean', 
             x_lab = '', 
             y_lab = 'Avg FGs per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fg_mean', 
             x_lab = '', 
             y_lab = 'Avg FGs per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fg_mean', 
             x_lab = '', 
             y_lab = 'Avg FGs per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fg_mean', 
             x_lab = '', 
             y_lab = 'Avg FGs per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fg_mean', 
             x_lab = '', 
             y_lab = 'Avg FGs per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()


# get top mean fg per minute

pdf('../blog_posts/visuals/fga_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fga_mean', 
             x_lab = '', 
             y_lab = 'Avg FG att per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fga_mean', 
             x_lab = '', 
             y_lab = 'Avg FG att per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fga_mean', 
             x_lab = '', 
             y_lab = 'Avg FG att per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fga_mean', 
             x_lab = '', 
             y_lab = 'Avg FG att per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fga_mean', 
             x_lab = '', 
             y_lab = 'Avg FG att per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()




# threes made
pdf('../blog_posts/visuals/three_mean')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'three_mean', 
             x_lab = '', 
             y_lab = 'Avg three pointers per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'three_mean', 
             x_lab = '', 
             y_lab = 'Avg three pointers per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'three_mean', 
             x_lab = '', 
             y_lab = 'Avg three pointers per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'three_mean', 
             x_lab = '', 
             y_lab = 'Avg three pointers per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'three_mean', 
             x_lab = '', 
             y_lab = 'Avg three pointers per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()


# threes made
pdf('../blog_posts/visuals/threea_mean')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'threea_mean', 
             x_lab = '', 
             y_lab = 'Avg three point att per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'threea_mean', 
             x_lab = '', 
             y_lab = 'Avg three point att per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'threea_mean', 
             x_lab = '', 
             y_lab = 'Avg three point att per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'threea_mean', 
             x_lab = '', 
             y_lab = 'Avg three point att per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'threea_mean', 
             x_lab = '', 
             y_lab = 'Avg three point att per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()


# get top mean fg per minute

pdf('../blog_posts/visuals/ft_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'ft_mean', 
             x_lab = '', 
             y_lab = 'Avg FT per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'ft_mean', 
             x_lab = '', 
             y_lab = 'Avg FT per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'ft_mean', 
             x_lab = '', 
             y_lab = 'Avg FT per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'ft_mean', 
             x_lab = '', 
             y_lab = 'Avg FT per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'ft_mean', 
             x_lab = '', 
             y_lab = 'Avg FT per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()



# get top mean fg per minute

pdf('../blog_posts/visuals/fta_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fta_mean', 
             x_lab = '', 
             y_lab = 'Avg FTA per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fta_mean', 
             x_lab = '', 
             y_lab = 'Avg FTA per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fta_mean', 
             x_lab = '', 
             y_lab = 'Avg FTA per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fta_mean', 
             x_lab = '', 
             y_lab = 'Avg FTA per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'fta_mean', 
             x_lab = '', 
             y_lab = 'Avg FTA per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()




# get top mean fg per minute

pdf('../blog_posts/visuals/or_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'or_mean', 
             x_lab = '', 
             y_lab = 'Avg off reb per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'or_mean', 
             x_lab = '', 
             y_lab = 'Avg off reb per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'or_mean', 
             x_lab = '', 
             y_lab = 'Avg off reb per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'or_mean', 
             x_lab = '', 
             y_lab = 'Avg off reb per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'or_mean', 
             x_lab = '', 
             y_lab = 'Avg off reb per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()


# get top mean fg per minute

pdf('../blog_posts/visuals/dr_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'dr_mean', 
             x_lab = '', 
             y_lab = 'Avg def reb per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'dr_mean', 
             x_lab = '', 
             y_lab = 'Avg def reb per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'dr_mean', 
             x_lab = '', 
             y_lab = 'Avg def reb per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'dr_mean', 
             x_lab = '', 
             y_lab = 'Avg def reb per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'dr_mean', 
             x_lab = '', 
             y_lab = 'Avg def reb per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()


# get top mean fg per minute

pdf('../blog_posts/visuals/tot_r_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'tot_r_mean', 
             x_lab = '', 
             y_lab = 'Avg total reb per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'tot_r_mean', 
             x_lab = '', 
             y_lab = 'Avg total reb per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'tot_r_mean', 
             x_lab = '', 
             y_lab = 'Avg total reb per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'tot_r_mean', 
             x_lab = '', 
             y_lab = 'Avg total reb per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'tot_r_mean', 
             x_lab = '', 
             y_lab = 'Avg total reb per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()



# get top mean fg per minute

pdf('../blog_posts/visuals/assists_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'assists_mean', 
             x_lab = '', 
             y_lab = 'Avg assists per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'assists_mean', 
             x_lab = '', 
             y_lab = 'Avg assists per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'assists_mean', 
             x_lab = '', 
             y_lab = 'Avg assists per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'assists_mean', 
             x_lab = '', 
             y_lab = 'Avg assists per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'assists_mean', 
             x_lab = '', 
             y_lab = 'Avg assists per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()


# get top mean fg per minute

pdf('../blog_posts/visuals/pf_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'pf_mean', 
             x_lab = '', 
             y_lab = 'Avg personal fouls per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'pf_mean', 
             x_lab = '', 
             y_lab = 'Avg personal fouls per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'pf_mean', 
             x_lab = '', 
             y_lab = 'Avg personal fouls per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'pf_mean', 
             x_lab = '', 
             y_lab = 'Avg personal fouls per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'pf_mean', 
             x_lab = '', 
             y_lab = 'Avg personal fouls per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()

# get top mean fg per minute

pdf('../blog_posts/visuals/steals_mean')
plot_per_min(temp_dat = dat_stats, 
             y_axis = 'steals_mean', 
             x_lab = '', 
             y_lab = 'Avg steals per min played',
             add_top_margin = TRUE,
             year = '2015-2016')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'steals_mean', 
             x_lab = '', 
             y_lab = 'Avg steals per min played',
             add_top_margin = TRUE,
             year = '2016-2017')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'steals_mean', 
             x_lab = '', 
             y_lab = 'Avg steals per min played',
             add_top_margin = TRUE,
             year = '2017-2018')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'steals_mean', 
             x_lab = '', 
             y_lab = 'Avg steals per min played',
             add_top_margin = TRUE,
             year = '2018-2019')

plot_per_min(temp_dat = dat_stats, 
             y_axis = 'steals_mean', 
             x_lab = '', 
             y_lab = 'Avg steals per min played',
             add_top_margin = TRUE,
             year = 'all')

dev.off()

