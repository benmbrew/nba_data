# overall help here: https://ericrayanderson.github.io/shinymaterial/
# color universe here: http://materializecss.com/color.html
# icon universe here :http://materializecss.com/icons.html

library(shiny)
library(shinymaterial)
library(shinydashboard)
library(stringr)
library(readr)
library(tidyverse)
library(memisc)
library(shiny)
library(DT)
library(RColorBrewer)
options(gvis.plot.tag = 'chart')
options(scipen = 999)
library(ggplot2)
library(ggthemes)
library(shinymaterial)
library(reshape2)
library(Hmisc)

source('../global.R')


ui <- dashboardPage(skin = 'black',
                    
                    
                    dashboardHeader(
                      title = 'NBA stats',
                      titleWidth = 200
                    ),
                    
                    dashboardSidebar(width = 200,
                                     
                                     sidebarMenu(
                                       menuItem('Teamn level',
                                                icon = icon('table'),
                                                tabName = 'team_level'))),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                        tabItem(tabName = "team_level",
                                fluidRow(column(6,
                                                selectInput('pick_team',
                                                            "Choose team",
                                                            choices = unique(dat_game$teams),
                                                            selected = 'Toronto')),
                                         column(6,
                                                selectInput('pick_opp',
                                                            'Choose opponent',
                                                            choices = c('All', unique(dat_game$teams)),
                                                            selected = 'All'))),
                                fluidRow(column(3,
                                                dateRangeInput('date_picker',
                                                               'Choose Dates',
                                                               start = '2018/10/01',
                                                               end = max(dat_game$date),
                                                               format = 'yyyy/mm/dd',
                                                               separator = '--',
                                                               startview = "Year")),
                                         column(3,
                                                selectInput('choose_stats',
                                                            'Choose statistics to view',
                                                            choices = team_stats,
                                                            selected = c('date','teams', 'win_loss', 
                                                                         'closing_spread', 'closing_total'),
                                                            multiple = TRUE)),
                                         column(3,
                                                sliderInput("ou_range", 
                                                            "Choose over under range",
                                                            min = 180, 
                                                            max = 250,
                                                            value = c(185,235))),
                                         column(3,
                                                sliderInput("spread_range", 
                                                            "Choose spread range",
                                                            min = -22, 
                                                            max = 22,
                                                            value = c(-10,10)))),
                                fluidRow(column(12,
                                                DT::dataTableOutput('table_1'))),
                                br(), 
                                h2('Visualize last x number of games', align = 'center'),
                                fluidRow(column(6,
                                                radioButtons('choose_time',
                                                             'Choose time period', 
                                                             choices = c(5, 10, 20, 50, 100),
                                                             selected = 10,
                                                             inline = TRUE)),
                                         column(6,
                                                checkboxInput('view_opp',
                                                              'View opponent stats',
                                                              value = FALSE))),
                                fluidRow(column(6,
                                                h4('Winning % and total points'),
                                                plotOutput('win_points')),
                                         column(6,
                                                h4('Shot %'),
                                                plotOutput('shot_per'))),
                                fluidRow(column(6,
                                                h4('The spread vs real difference'),
                                                plotOutput('spread')),
                                         column(6,
                                                h4('The over under vs real total'),
                                                plotOutput('over_under')))
                        ))))



# Define server 
server <- function(input, output) {
  
  
  # get data for dates speccified
  get_all_stats <- reactive({
    
    
    team <- 'Toronto'
    opp <- 'All'
    x <- dat_game
    # for dates
    date_picker <- c("2015-10-17", "2018-10-07")
    
    
    date_picker <- input$date_picker
    
    date_beginning <- date_picker[1]
    date_end <- date_picker[2]
    slider_input_spread <- c(-15, 15)
    slider_input_ou <- c(150, 250)
    
    # get slider ranges
    slider_input_spread <- input$spread_range
    slider_input_ou <- input$ou_range
    
    # betting constraint
    low_number_spread <- slider_input_spread[1]
    high_number_spread <- slider_input_spread[2]
    low_number_ou <- slider_input_ou[1]
    high_number_ou <- slider_input_ou[2]
    
    team <- input$pick_team
    opp <- input$pick_opp
    
    x <- x[x$date >= date_beginning & x$date <= date_end,]
    x <- x[x$closing_spread >= low_number_spread & x$closing_spread <= high_number_spread,]
    x <- x[x$closing_total >= low_number_ou & x$closing_total <= high_number_ou,]
    
    
    
    if(opp == 'All'){
      x_team <- x[x$teams == team,]
      total_game_numbers <- unique(x_team$tot_game_num)
      opps <- x[x$tot_game_num %in% total_game_numbers,]
      opps <- opps[opps$teams!= team,]
      x$tot_game_num <- as.character(x$tot_game_num)
      
      x <- inner_join(x_team, opps, by = 'tot_game_num')
      names(x) <- gsub('.x', '_team', names(x), fixed = TRUE)
      names(x) <- gsub('.y', '_opp', names(x), fixed = TRUE)
      x$tot_game_num <- NULL
      x <- x[order(x$date_team, decreasing = TRUE),]
      
      
    } else {
      x_team <- x[x$teams == team,]
      
      x_opp <- x[x$teams == opp,]
      # get intersecting game_numbers
      x_team_game_nums <- as.character(unique(x_team$tot_game_num))
      x_opp_game_nums <- as.character(unique(x_opp$tot_game_num))
      
      intersect_game_nums <- as.character(intersect(x_team_game_nums, x_opp_game_nums))
      
      x_team <- x_team[x_team$tot_game_num %in% intersect_game_nums,]
      x_opp <- x_opp[x_opp$tot_game_num %in% intersect_game_nums,]
      
      x <- rbind(x_team,
                 x_opp)
      x <- x[order(x$date, decreasing = TRUE),]
      
      
    }
    
    return(x)
    
    
  })
  
  output$table_1 <- renderDataTable({
    
    # get date teams, points, wins, spread, over under, etc
    x <- get_all_stats()
    choose_stats <- c('teams', 'venue', 'win_loss')
    opp <- 'All'
    choose_stats <- input$choose_stats
    opp <- input$pick_opp
    
    if(opp == 'All') {
      choose_stats_team <- paste0(choose_stats, '_team')
      choose_stats_opp <- paste0(choose_stats, '_opp')
      new_stats <- c(choose_stats_team, choose_stats_opp)
      
      x <- x[, new_stats]
      prettify_scroll(x)
      
    } else {
      x <- x[, choose_stats]
      x <- x[order(x$date, decreasing = TRUE),]
      prettify_scroll(x)
    }
    
    
  })
  
  
  # get data for dates speccified
  get_plot_data_team <- reactive({
    # choices = c('5', '10', '20', '50', 'All season'),
    
    choose_time <- 5
    team <- 'All'
    x <- dat_game
    pick_opp = 'All'
    view_opp = TRUE
    view_opp <- input$view_opp
    # get time period
    choose_time <- input$choose_time
    
    # get teams 
    if(view_opp){
      team <- input$pick_opp
      x_team <- x
      
    } else {
      team <- input$pick_team
      x_team <- x[x$teams == team,]
    }

    
    x_team <- x_team[order(x_team$date, decreasing = TRUE),]
    x_team <- x_team[1:choose_time,]
    
    these_vars <- c('date', 'venue','f', 'win_loss','fg_per', 'three_per',
                    'closing_spread', 'real_spread','closing_total','real_total',
                    'a','to','oeff','deff','pace','or', 'dr')
    x_team <- x_team[, these_vars]
    # multiply closing_spread by negative 1 
    x_team$closing_spread <- x_team$closing_spread*(-1)
    x_team <- x_team %>% group_by(venue) %>%
      summarise(mean_total_points = round(mean(f, na.rm = TRUE), 2),
                total_wins = sum(win_loss == 'W'),
                total_losses = sum(win_loss == 'L'),
                mean_fg_per = round(mean(fg_per, na.rm = TRUE)*100,2),
                mean_three_per = round(mean(three_per, na.rm = TRUE)*100,2),
                mean_closing_spread = round(mean(closing_spread, na.rm = TRUE),2),
                mean_real_spread = round(mean(real_spread, na.rm = TRUE),2),
                mean_closing_total = round(mean(closing_total, na.rm = TRUE),2),
                mean_real_total = round(mean(real_total, na.rm = TRUE),2))
    
    x_team$win_per <- NA
    x_team$total_wins <- as.numeric(x_team$total_wins)
    choose_time <- as.numeric(choose_time)
    x_team$win_per <- round((x_team$total_wins/choose_time)*100, 2)
    
    x_team$total_wins <- x_team$total_losses <- NULL
    
    names(x_team)[names(x_team) == 'mean_closing_spread'] <- 'mean_favored_by'
    names(x_team)[names(x_team) == 'mean_real_spread '] <- 'mean_win_by'
    x_team <- melt(x_team, id.vars = 'venue')
    
    x_team$variable <- gsub('_', ' ', x_team$variable)
    x_team$variable <- gsub('mean ', '', x_team$variable)
    
    x_team$variable <- Hmisc::capitalize(x_team$variable)
    
    # create variable for faceting
    x_team$to_facet <- ifelse(grepl('^Total|Win per', x_team$variable), 'Total points and win %',
                         ifelse(grepl('Fg|Three', x_team$variable), 'Shot %',
                                ifelse(grepl('Favored|spread', x_team$variable), 'The spread', 'The over under')))
    
    
    return(x_team)
    
  })
  
  output$win_points <- renderPlot({
    # x <- x_team
    
    # subset by 
    x <- get_plot_data_team()
   
    x <- x %>% filter(to_facet == 'Total points and win %')
    ggplot(x, aes(reorder(variable, -value), 
                  value,
                  fill = venue)) +
      geom_bar(stat = 'identity', 
               position = 'dodge',
               alpha = 0.7) +
      geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -0.1) +
      scale_fill_manual(name = '', 
                        values = c('black', 'grey')) +
      labs(y = paste0('Average over ', 5, ' days'),
           x = '') +
      theme(legend.position = 'bottom', 
            axis.text.x = element_text(size = 12, angle = 0),
            axis.text.y = element_text(size = 12, angle = 0)) 
    })
  
  output$shot_per <- renderPlot({

    # subset by 
    x <- get_plot_data_team()
    
    x <- x %>% filter(to_facet == 'Shot %')
    ggplot(x, aes(reorder(variable, -value), 
                  value,
                  fill = venue)) +
      geom_bar(stat = 'identity', 
               position = 'dodge',
               alpha = 0.7) +
      geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -0.1) +
      scale_fill_manual(name = '', 
                        values = c('black', 'grey')) +
      labs(y = paste0('Average over ', 5, ' days'),
           x = '') +
      theme(legend.position = 'bottom', 
            axis.text.x = element_text(size = 12, angle = 0),
            axis.text.y = element_text(size = 12, angle = 0)) 
  })
  
  
  output$spread <- renderPlot({

    # subset by 
    x <- get_plot_data_team()
    
    x <- x %>% filter(to_facet == 'The spread')
    ggplot(x, aes(reorder(variable, -value), 
                  value,
                  fill = venue)) +
      geom_bar(stat = 'identity', 
               position = 'dodge',
               alpha = 0.7) +
      geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -0.1) +
      scale_fill_manual(name = '', 
                        values = c('black', 'grey')) +
      labs(y = paste0('Average over ', 5, ' days'),
           x = '') +
      theme(legend.position = 'bottom', 
            axis.text.x = element_text(size = 12, angle = 0),
            axis.text.y = element_text(size = 12, angle = 0)) 
  })
  
  
  output$over_under <- renderPlot({
    # x <- x_team
    
    # subset by 
    x <- get_plot_data_team()
    
    x <- x %>% filter(to_facet == 'The over under')
    ggplot(x, aes(reorder(variable, -value), 
                  value,
                  fill = venue)) +
      geom_bar(stat = 'identity', 
               position = 'dodge',
               alpha = 0.7) +
      geom_text(aes(label = value), position = position_dodge(width = 1), vjust = -0.1) +
      scale_fill_manual(name = '', 
                        values = c('black', 'grey')) +
      labs(y = paste0('Average over ', 5, ' days'),
           x = '') +
      theme(legend.position = 'bottom', 
            axis.text.x = element_text(size = 12, angle = 0),
            axis.text.y = element_text(size = 12, angle = 0)) 
  })
  
  
  
 
  
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

