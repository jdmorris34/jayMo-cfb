### LOAD LIBRARIES USED ###

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(tidyr)
library(tidyverse)
library(lubridate)
library(DBI)
library(RMySQL)
library(pool)
library(ggplot2)


### LOAD MAIN DATA FRAME ###

## CONNECT TO MYSQL SERVER (IN-SEASON) ##
# conn <- dbConnect(
  # drv = RMySQL::MySQL(),
  # dbname = "jayMo-CFB",
  # host = "jaymo-cfb.cfk0ycmc8men.us-east-2.rds.amazonaws.com",
  # username = "guest",
  # password = "hLm^5sDfw071}nG-76")

# jay_pred_query <- "SELECT * FROM jay_predictions;"
# jay_predictions <- dbGetQuery(conn, jay_pred_query)

# dbDisconnect(conn)

## POWER OFF CSV FILES (OFFSEASON) ##
jay_predictions <- read.csv('jay_predictions.csv')
jay_h2h <- read.csv('jmpi_head_to_head.csv')
jay_ranks <- read.csv('jmpi_final_rank.csv')

jay_predictions <-
  jay_predictions %>%
  arrange(start_date)

### DEFINE FUNCTIONS FOR TRANSFORMATIONS ###

create_small_image <- function(x) {
  paste0("<img src='",x,"' height='25'></img>")
}

create_image <- function(x) {
  paste0("<img src='",x,"' height='50'></img>")
}

create_large_image <- function(x) {
  paste0("<img src='",x,"' height='250',width='150'></img>")
}

date_format_full <- function(x){
  date_string <- as_datetime(x)  
  eastern_string <- with_tz(date_string, tz="America/New_York")
  sf <- stamp("Sunday, May 1, 2000 3:10 PM EST")
  sf(eastern_string)
}

date_format_half <- function(x){
  date_string <- as_datetime(x)  
  eastern_string <- with_tz(date_string, tz="America/New_York")
  sf <- stamp("Sunday, May 1, 2000")
  sf(eastern_string)
}

check_placeholder_kick <- function(x){
  date_string <- as_datetime(x)  
  lubridate::hour(date_string)
}

bold_winner <- function(x) {
  paste0("<strong>",x,"</strong>&#9666")
}

### TRANSFORMATIONS ###

## CREATE LOGO IMAGES FOR TABLE ##

jay_predictions$logo_home <- apply(jay_predictions[, "logo_home", drop = FALSE], MARGIN = 1, FUN = create_image)
jay_predictions$logo_away <- apply(jay_predictions[, "logo_away", drop = FALSE], MARGIN = 1, FUN = create_image)

## FORMAT PREDICTIONS/ACTUAL SCORES FOR TABLE ##

jay_predictions$formatted_home_points <- ifelse(jay_predictions$home_points>jay_predictions$away_points,
                                               apply(jay_predictions[, "home_points", drop = FALSE], MARGIN = 1, FUN = bold_winner),
                                               jay_predictions$home_points)

jay_predictions$formatted_away_points <- ifelse(jay_predictions$home_points<jay_predictions$away_points,
                                               apply(jay_predictions[, "away_points", drop = FALSE], MARGIN = 1, FUN = bold_winner),
                                               jay_predictions$away_points)

jay_predictions$formatted_jay_home_points <- ifelse(jay_predictions$rounded_jay_home>jay_predictions$rounded_jay_away,
                                                   apply(jay_predictions[, "rounded_jay_home", drop = FALSE], MARGIN = 1, FUN = bold_winner),
                                                   jay_predictions$rounded_jay_home)

jay_predictions$formatted_jay_away_points <- ifelse(jay_predictions$rounded_jay_home<jay_predictions$rounded_jay_away,
                                               apply(jay_predictions[, "rounded_jay_away", drop = FALSE], MARGIN = 1, FUN = bold_winner),
                                               jay_predictions$rounded_jay_away)

## FORMAT DATES ##

jay_predictions$true_start_tbd <- check_placeholder_kick(jay_predictions$start_date)


jay_predictions$formatted_start_date <- ifelse(jay_predictions$true_start_tbd==5,
                                               paste0(date_format_half(jay_predictions$start_date),", Start Time TBD"),
                                               date_format_full(jay_predictions$start_date))


## ADD IN GAME STATUS FIELD ##

jay_predictions$game_status <- ifelse(is.na(jay_predictions$home_points),"Incomplete","Complete")

## CREATE ICONS BASED ON CORRECT/INCORRECT (OR NA) PICKS ## 

jay_predictions$moneyline_icon <- ifelse((jay_predictions$correct_pick_jay==1) | (jay_predictions$correct_pick_jay==2), paste0("&#9;&#x2705;"),
                                         ifelse(jay_predictions$correct_pick_jay==0, paste0("&#9;&#10060;"),
                                                ""))

jay_predictions$moneyline_icon <- ifelse(is.na(jay_predictions$correct_pick_jay),"",
                                         jay_predictions$moneyline_icon)

jay_predictions$ats_icon <- ifelse(jay_predictions$correct_pick_jay_ats==1, paste0("&#9;&#x2705;"),
                                         ifelse(jay_predictions$correct_pick_jay_ats==0, paste0("&#9;&#10060;"),
                                                ""))

jay_predictions$ats_icon <- ifelse(is.na(jay_predictions$correct_pick_jay_ats) & !is.na(jay_predictions$home_points),
                                          "&#10134;",
                                   ifelse(is.na(jay_predictions$correct_pick_jay_ats) & is.na(jay_predictions$home_points),
                                   "",
                                   jay_predictions$ats_icon))

jay_predictions$ou_icon <- ifelse(jay_predictions$correct_jay_over==1, paste0("&#9;&#x2705;"),
                                   ifelse(jay_predictions$correct_jay_over==0, paste0("&#9;&#10060;"),
                                          ""))

jay_predictions$ou_icon <- ifelse(is.na(jay_predictions$correct_jay_over) & !is.na(jay_predictions$home_points),
                                   "&#10134;",
                                   ifelse(is.na(jay_predictions$correct_jay_over) & is.na(jay_predictions$home_points),
                                          "",
                                          jay_predictions$ou_icon))
                                                
## FORMAT SPREADS/PICKS ##


jay_predictions$vegas_ML_pick <- paste0("M/L: ",ifelse(jay_predictions$spread < 0,
                                                       jay_predictions$abbreviation_home,
                                                       jay_predictions$abbreviation_away))

jay_predictions$vegas_spread_formatted <- paste0("Spread: ", ifelse(jay_predictions$spread<0,paste0(jay_predictions$abbreviation_home," ",jay_predictions$spread),
                                                                    paste0(jay_predictions$abbreviation_home," +",jay_predictions$spread)))

jay_predictions$vegas_ou_formatted <- paste0("Total: ", jay_predictions$over_under)

jay_predictions$jayMo_ML_pick <- ifelse(jay_predictions$jay_spread < 0,
                                        paste0("M/L: ",jay_predictions$abbreviation_home,jay_predictions$jay_ML_pick_icon),
                                        paste0("M/L: ",jay_predictions$abbreviation_away,jay_predictions$jay_ML_pick_icon))

jay_predictions$jayMo_spread_formatted <- ifelse(jay_predictions$jay_cover==1 & jay_predictions$spread > 0,
                                                 paste0("ATS: ", jay_predictions$abbreviation_home," +", jay_predictions$spread),
                                                 ifelse(jay_predictions$jay_cover==1 & jay_predictions$spread < 0,
                                                        paste0("ATS: ", jay_predictions$abbreviation_home," ", jay_predictions$spread),
                                                        ifelse(jay_predictions$jay_cover==0 & jay_predictions$spread > 0,
                                                               paste0("ATS: ", jay_predictions$abbreviation_away," ", (jay_predictions$spread*-1)),
                                                               ifelse(jay_predictions$jay_cover==0 & jay_predictions$spread < 0,
                                                                      paste0("ATS: ", jay_predictions$abbreviation_away," +", (jay_predictions$spread*-1)),
                                                                      NA))))

jay_predictions$jayMo_spread_formatted <- ifelse(is.na(jay_predictions$jayMo_spread_formatted),
                                                 paste0("ATS: Push"),
                                                 jay_predictions$jayMo_spread_formatted)

jay_predictions$jayMo_spread_formatted <- ifelse(is.na(jay_predictions$spread),
                                                 paste0("ATS: NA"),
                                                 jay_predictions$jayMo_spread_formatted)
                                                 
                                                 
                                                 

jay_predictions$jayMo_ou_formatted <- paste0("O/U: ",ifelse(jay_predictions$rounded_jay_total > jay_predictions$over_under,
                                                            paste0("Over"),
                                                            ifelse(jay_predictions$rounded_jay_total==jay_predictions$over_under,
                                                                   paste0("Push"),
                                                                   paste0("Under"))))

jay_predictions$jayMo_ou_formatted <- ifelse(is.na(jay_predictions$over_under),
                                             paste0("O/U: NA"),
                                             jay_predictions$jayMo_ou_formatted)
## ADD IN RECORDS ##

jay_predictions$home_record <-
  paste0("(",jay_predictions$win_total_home,"-",jay_predictions$loss_total_home,")")

jay_predictions$away_record <-
  paste0("(",jay_predictions$win_total_away,"-",jay_predictions$loss_total_away,")")
  


## ADD IN RANKINGS ##

jay_predictions$formatted_home <- ifelse(is.na(jay_predictions$rank_home),
                                         paste0(jay_predictions$home_team," ",jay_predictions$home_record),
                                         paste0(jay_predictions$home_team," <sub>",jay_predictions$rank_home,"</sub> ", jay_predictions$home_record))

jay_predictions$formatted_away <- ifelse(is.na(jay_predictions$rank_away),
                                         paste0(jay_predictions$away_team," ",jay_predictions$away_record),
                                         paste0(jay_predictions$away_team," <sub>",jay_predictions$rank_away,"</sub> ",jay_predictions$away_record))


## CREATE STRINGS FOR FORMATTING WITHIN TABLE ##

logos <- c("logo_away","logo_home")
names <- c("formatted_away","formatted_home")
actual_scores <- c("formatted_away_points","formatted_home_points")
jay_scores <- c("formatted_jay_away_points","formatted_jay_home_points")
vegas_lines <- c("vegas_ML_pick","vegas_spread_formatted","vegas_ou_formatted")
jayMo_lines <- c("jayMo_ML_pick","jayMo_spread_formatted","jayMo_ou_formatted")
jayMo_pick_icons <- c("moneyline_icon","ats_icon","ou_icon")

jay_predictions$logos <- apply(jay_predictions[, logos], 1, paste, collapse = "<br>")
jay_predictions$names <- apply(jay_predictions[, names], 1, paste, collapse = "<br> <br>")

jay_predictions$formatted_score <- apply(jay_predictions[, actual_scores], 1, paste, collapse = "<br> <br>")
jay_predictions$formatted_jay_score <- apply(jay_predictions[, jay_scores], 1, paste, collapse = "<br> <br>")

jay_predictions$vegas_lines_formatted <- apply(jay_predictions[, vegas_lines], 1, paste, collapse = "<br>")
jay_predictions$jayMo_lines_formatted <- apply(jay_predictions[, jayMo_lines], 1, paste, collapse = "<br>")
jay_predictions$formatted_picks_icons <- apply(jay_predictions[, jayMo_pick_icons], 1, paste, collapse = "<br>")

### UNDERDOG DATA ###

jay_underdogs <- jay_predictions %>%
  filter(jay_underdog==1)

jay_underdogs$underdog_logo <- ifelse(jay_underdogs$spread<0,jay_underdogs$logo_away,jay_underdogs$logo_home)
jay_underdogs$favorite_logo <- ifelse(jay_underdogs$spread<0,jay_underdogs$logo_home,jay_underdogs$logo_away)

jay_underdogs$formatted_string <- paste0(ifelse(jay_underdogs$spread<0,jay_underdogs$abbreviation_away,jay_underdogs$abbreviation_home),
                                         " over ",
                                         ifelse(jay_underdogs$spread<0,jay_underdogs$abbreviation_home,jay_underdogs$abbreviation_away))

### BETTING FAVORITE DATA ###

betting_favorites_a <-
  jay_predictions %>%
  drop_na(home_points) %>%
  group_by(season,home_team) %>%
  summarise(ml_wins = sum(correct_pick_jay),
            ml_losses = sum(!is.na(home_points))-ml_wins,
            ml_wp = ml_wins/sum(!is.na(home_points)),
            ats_wins = sum(correct_pick_jay_ats, na.rm=TRUE),
            ats_losses = sum(!is.na(correct_pick_jay_ats))-ats_wins,
            ats_pushes = sum(is.na(correct_pick_jay_ats)),
            ats_wp = ats_wins/sum(!is.na(correct_pick_jay_ats)),
            ou_wins = sum(correct_jay_over, na.rm = TRUE),
            ou_losses = sum(!is.na(correct_jay_over))-ou_wins,
            ou_pushes = sum(is.na(correct_jay_over)),
            ou_wp = ou_wins/sum(!is.na(correct_jay_over)),
            logo = max(logo_home),
            color = max(color_home)) %>%
  rename(team = home_team)

betting_favorites_b <-
  jay_predictions %>%
  drop_na(home_points) %>%
  group_by(season,away_team) %>%
  summarise(ml_wins = sum(correct_pick_jay),
            ml_losses = sum(!is.na(home_points))-ml_wins,
            ml_wp = ml_wins/sum(!is.na(home_points)),
            ats_wins = sum(correct_pick_jay_ats, na.rm=TRUE),
            ats_losses = sum(!is.na(correct_pick_jay_ats))-ats_wins,
            ats_pushes = sum(is.na(correct_pick_jay_ats)),
            ats_wp = ats_wins/sum(!is.na(correct_pick_jay_ats)),
            ou_wins = sum(correct_jay_over, na.rm = TRUE),
            ou_losses = sum(!is.na(correct_jay_over))-ou_wins,
            ou_pushes = sum(is.na(correct_jay_over)),
            ou_wp = ou_wins/sum(!is.na(correct_jay_over)),
            logo = max(logo_away),
            color = max(color_away)) %>%
  rename(team = away_team)

betting_records_by_team <- rbind(betting_favorites_a, betting_favorites_b)


## Game Simulator Data Transformations ##

jay_h2h$logo_team <- apply(jay_h2h[, "logo_team", drop = FALSE], MARGIN = 1, FUN = create_large_image)
jay_h2h$logo_opp <- apply(jay_h2h[, "logo_opp", drop = FALSE], MARGIN = 1, FUN = create_large_image)

jay_h2h$dash_column <- paste("-")

## Rankings Data Transformations ##

jay_ranks$logo <- apply(jay_ranks[, "logo", drop = FALSE], MARGIN = 1, FUN = create_small_image)
jay_ranks$jmpi_index <- jay_ranks$jmpi_index * 100
jay_ranks$record <- paste0(jay_ranks$total_wins, "-",jay_ranks$total_losses)

jay_ranks <-
  jay_ranks %>%
  select(jmpi_rank,logo,team,record,jmpi_index)

### APP UI ###

ui <- page_navbar(tags$head(tags$style(
  ".main-header { background-color: #f0f0f0; }",
  HTML('body, label, input, button, select { 
          font-family: "Roboto", sans-serif;
        }')
)),
title = span("jayMo's CFB Predictions",
                                    style = "color: black; font-size: 28px; font-family: 'Roboto', sans-serif;"),
                  nav_panel(title = "Predictions",
                            layout_sidebar(sidebar= sidebar(
                              selectizeInput(
                                "game_status",
                                "Select Game Status:",
                                choices = c("All","Incomplete","Complete"),
                                multiple = FALSE,
                                selected = "All"),
                              selectizeInput(
                                "season",
                                "Select Season:",
                                choices = unique(c(jay_predictions$season)),
                                selected = max(unique(c(jay_predictions$season))),
                                multiple = FALSE),
                              fluidRow(span(textOutput("season_records_header"),
                                         style='font-size: 16px'),
                              span(textOutput("season_records_ml"),
                                   style='font-size: 14px'),
                              span(textOutput("season_records_ats"),
                                   style='font-size: 14px'),
                              span(textOutput("season_records_ou"),
                                   style='font-size: 14px')),
                              selectizeInput(
                                "week",
                                "Select Week:",
                                choices = unique(c(jay_predictions$week)),
                                selected = max(unique(c(jay_predictions$week))),
                                multiple = FALSE),
                              fluidRow(span(textOutput("week_records_header"),
                                            style='font-size: 16px'),
                                       span(textOutput("week_records_ml"),
                                            style='font-size: 14px'),
                                       span(textOutput("week_records_ats"),
                                            style='font-size: 14px'),
                                       span(textOutput("week_records_ou"),
                                            style='font-size: 14px')),
                              selectizeInput(
                                "conference",
                                "Select Conference/Top 25:",
                                choices = c("All","AP Top 25",sort(jay_predictions$home_conference)),
                                multiple = FALSE,
                                selected = "All"),
                              fluidRow(span(textOutput("conference_records_header"),
                                            style='font-size: 16px'),
                                       span(textOutput("conference_records_ml"),
                                            style='font-size: 14px'),
                                       span(textOutput("conference_records_ats"),
                                            style='font-size: 14px'),
                                       span(textOutput("conference_records_ou"),
                                            style='font-size: 14px')),
                              selectizeInput(
                                "team",
                                "Select Team:",
                                choices = c("All",sort(jay_predictions$home_team)),
                                multiple = FALSE,
                                selected = "All"),
                              fluidRow(span(textOutput("team_records_header"),
                                            style='font-size: 16px'),
                                       span(textOutput("team_records_ml"),
                                            style='font-size: 14px'),
                                       span(textOutput("team_records_ats"),
                                            style='font-size: 14px'),
                                       span(textOutput("team_records_ou"),
                                            style='font-size: 14px')),
                            ),
                            layout_columns(
                              card(card_header(textOutput("predictions_table_title")),
                                   DT::dataTableOutput("jay_predictions_table")),
                              card(fluidRow(card(max_height = 350,
                                                 card_header(textOutput("underdogs_title")),
                                                 DT::dataTableOutput("jay_underdogs_table")),
                                            card(max_height=450,
                                                 card_header(textOutput("favorites_title")),
                                                 DT::dataTableOutput("jay_betting_faves_table")))),
                              col_widths = c(8, 4))
                            )),
                  nav_panel(title="Matchup Simulator/Rankings",
                            layout_columns(
                              fluidRow(layout_columns(
                                selectizeInput(
                                  "team_1",
                                  "Select Team 1:",
                                  choices = unique(c(sort(jay_h2h$team))),
                                  multiple = FALSE,
                                  selected = "Oregon"),
                                selectizeInput(
                                  "team_2",
                                  "Select Team 2:",
                                  choices = unique(c(sort(jay_h2h$team))),
                                  multiple = FALSE,
                                  selected = "Notre Dame"),
                                col_widths = c(6, 6)),
                                card(DT::dataTableOutput("jay_h2h_table")),
                                card(uiOutput("game_sim_tab",
                                              style='font-size: 24px'))
                              ),
                            card(card_header("jayMo Rankings"),
                              DT::dataTableOutput("jay_ranks_table")),
                            col_widths = c(8, 4))),
                  nav_panel(title = "About",
                            uiOutput("about_tab"),
                            style='font-size: 14px')
                  
                  )

server <- function(input, output, session) {
  
  ## RENDER ABOUT TEXT ##
  
  output$about_tab <- renderUI({
    HTML(paste(paste0("<p><strong>","GENERAL INFO","</strong>"),
    "",
    "The purpose of this project is to predict the scores for both teams playing in an FBS Division I-A college football contest.
    I used a linear regression model that uses a mix of advanced and traditional play-by-play statistics.",
    "I call the calculated values jayMo scores, which stands for ad<span><b>J</b></span>usted <span><b>A</b></span>nticipated <span><b>Y</b></span>ield <span><b>MO</b></span>del.",
    "",
    "The predictive modeling element of the app was done through Python, while the web application was constructed in R.",
    "All data used is public through the CFBD API (https://www.collegefootballdata.com).",
    "",
    "2024 betting lines are from Bovada.",
    "Listed team records are updated as games are completed, listed rankings are reflective of pre-contest standings.",
    "Kickoff times will sometimes be listed as TBD, please be patient as these will be updated closer to the game date.",
    "",
    "Please reach out to me at justindmorris34@gmail.com for any inquiries or to learn more.",
    "",
    paste(paste0("<strong>","GLOSSARY","</strong>"),
          "",
          "M/L: Moneyline - The outright winner for a contest.",
          "Spread - The betting line at which you add/subtract points to/from the listed team in order to decide which team is the winner ATS.",
          "ATS: Against the Spread - The picks from jayMo using the sourced spread.",
          "Total: The betting line of the expected total number of points (sum of both teams) in a contest.",
          "O/U: Over/Under - The picks from jayMo using the sourced total.",
          "Push - A no contest, where the winner for a bet cannot be awarded due to the score ATS or the point total being equal to the predicted or actual value.",
          "",
          sep = "<br/>"),
    paste(paste0("<strong>","COLOR CODING","</strong>"),
          "",
          "Scores - Predicted/Actual scores have different background shading for completed games.",
          "Gray background - The actual result for a contest.",
          "Gold background - A perfect score prediction.",
          "Green background - The predicted winner was correct.",
          "Red background - The predicted winner was incorrect.",
          "",
          "Betting - Betting lines/picks will have different shading for completed games.",
          "Gray background - The betting lines for a contest (2024 - Bovada).",
          "Green background - All jayMo picks hit for this game (disregarding predicted or actual pushes/no contests).",
          "Yellow background - 1-2 jayMo picks hit for this game (disregarding predicted or actual pushes/no contests).",
          "Red background - Zero jayMo picks hit for this game (disregarding predicted or actual pushes/no contests).</p>",
          sep = "<br/>"),
    sep = "<br/>"))
  })
  
  output$game_sim_tab <- renderUI({
    h2(HTML(paste("<p>",
               "The above score is a jayMo prediction based on the selected teams playing a game at a neutral site.",
               "<br>",
               "Use this tool to simulate potential bowl game matchups or see where your team stands against any opponent!",
               "<p>")),
       style = "text-align:center")
  })
  
  ## UPDATE TEAM SELECTION OPTIONS BY CONFERENCE ##
  observe({
    updateSelectInput(session, "team", choices = if(input$conference=="All"){c("All",sort(jay_predictions$home_team))}
                      else{
                        c("All",sort(as.character(jay_predictions[jay_predictions$home_conference == input$conference,"home_team"])),
                               sort(as.character(jay_predictions[jay_predictions$away_conference == input$conference,"away_team"])))}
    )})
  
  
  observe({
    if(input$team != "All"){
    updateSelectInput(session,
                      "week",
                      choices = c("All",sort(jay_predictions$week)),
                      selected = "All")}
    else{updateSelectInput(session,
                           "week",
                           choices = unique(c(jay_predictions$week)),
                           selected = max(unique(c(jay_predictions$week))))}
                      })
  
  ## CREATE HEADERS FOR ALL CARDS ##
  
  output$favorites_title <- renderText({
      paste0("jayMo's ",input$season, " Favorites")
  })
  
  output$underdogs_title <- renderText({
    if(input$week != "All"){
    paste0("jayMo's ",input$season," Week ",input$week," Underdogs")}
    else{paste0("Select one week for underdogs")}
  })
  
  output$predictions_table_title <- renderText({
    if(input$conference == "AP Top 25" && nrow(jay_predictions_status_filter())==0){paste0("AP Rankings have not yet been released for this week. Please choose another group.")}
    else if(input$week != "All" && input$conference=="All"){paste0("jayMo's ",input$season," Week ",input$week," Predictions")}
    else if(input$week != "All" && input$conference!="All"){paste0("jayMo's ",input$season," ",input$conference," Week ",input$week," Predictions")}
    else if(input$week == "All"){paste0("jayMo's ",input$season," ",input$team," Predictions")}
    })
  
  ## SIDEBAR RECORDS BY SEASON ##
  records_by_season <- reactive({
    jay_predictions %>%
      drop_na(home_points) %>%
      group_by(season) %>%
      summarise(ml_wins = sum(correct_pick_jay),
                ml_losses = sum(!is.na(home_points))-ml_wins,
                ml_wp = ml_wins/sum(!is.na(home_points)),
                ats_wins = sum(correct_pick_jay_ats, na.rm=TRUE),
                ats_losses = sum(!is.na(correct_pick_jay_ats))-ats_wins,
                ats_pushes = sum(is.na(correct_pick_jay_ats)),
                ats_wp = ats_wins/sum(!is.na(correct_pick_jay_ats)),
                ou_wins = sum(correct_jay_over, na.rm = TRUE),
                ou_losses = sum(!is.na(correct_jay_over))-ou_wins,
                ou_pushes = sum(is.na(correct_jay_over)),
                ou_wp = ou_wins/sum(!is.na(correct_jay_over))) %>%
      filter(season == input$season)
  })
  
  output$season_records_header <- renderText({
    paste0(input$season," Prediction Records")
  })
  
  output$season_records_ml <- renderText({
    paste0("M/L: ",records_by_season()$ml_wins,"-",records_by_season()$ml_losses," (",
           ifelse(records_by_season()$ml_wp<1,
                  paste0(round(records_by_season()$ml_wp, digits=3),")"),
                  paste0("1.000)")))
  })
  output$season_records_ats <- renderText({
    paste0("ATS: ",records_by_season()$ats_wins,"-",records_by_season()$ats_losses,"-",records_by_season()$ats_pushes,
                 " (",ifelse(records_by_season()$ats_wp<1,
                             paste0(round(records_by_season()$ats_wp, digits=3),")"),
                             paste0("1.000)")))
  })
  output$season_records_ou <- renderText({
    paste0("O/U: ",records_by_season()$ou_wins,"-",records_by_season()$ou_losses,"-",records_by_season()$ou_pushes,
                 " (",ifelse(records_by_season()$ou_wp<1,
                             paste0(round(records_by_season()$ou_wp, digits=3),")"),
                             paste0("1.000)")))
  })
  
  ## SIDEBAR RECORDS BY WEEK ##
  records_by_week <- reactive({
    jay_predictions %>%
      drop_na(home_points) %>%
      group_by(season, week) %>%
      summarise(ml_wins = sum(correct_pick_jay),
                ml_losses = sum(!is.na(home_points))-ml_wins,
                ml_wp = ml_wins/sum(!is.na(home_points)),
                ats_wins = sum(correct_pick_jay_ats, na.rm=TRUE),
                ats_losses = sum(!is.na(correct_pick_jay_ats))-ats_wins,
                ats_pushes = sum(is.na(correct_pick_jay_ats)),
                ats_wp = ats_wins/sum(!is.na(correct_pick_jay_ats)),
                ou_wins = sum(correct_jay_over, na.rm = TRUE),
                ou_losses = sum(!is.na(correct_jay_over))-ou_wins,
                ou_pushes = sum(is.na(correct_jay_over)),
                ou_wp = ou_wins/sum(!is.na(correct_jay_over))) %>%
      filter(season == input$season) %>%
      filter(week == input$week)
  })
  
  output$week_records_header <- renderText({
    if(length(records_by_week()$ml_wins)==0){NULL}
    else{paste0("Week ",input$week," Predictions")}
  })
  output$week_records_ml <- renderText({
    if(length(records_by_week()$ml_wins)==0){NULL}
    else{paste0("M/L: ",records_by_week()$ml_wins,"-",records_by_week()$ml_losses," (",
                ifelse(records_by_week()$ml_wp<1,
                       paste0(round(records_by_week()$ml_wp, digits=3),")"),
                       paste0("1.000)")))}
  })
  output$week_records_ats <- renderText({
    if(length(records_by_week()$ml_wins)==0){NULL}
    else{paste0("ATS: ",records_by_week()$ats_wins,"-",records_by_week()$ats_losses,"-",records_by_week()$ats_pushes,
           " (",ifelse(records_by_week()$ats_wp<1,
                       paste0(round(records_by_week()$ats_wp, digits=3),")"),
                       paste0("1.000)")))}
  })
  output$week_records_ou <- renderText({
    if(length(records_by_week()$ml_wins)==0){NULL}
    else{paste0("O/U: ",records_by_week()$ou_wins,"-",records_by_week()$ou_losses,"-",records_by_week()$ou_pushes,
           " (",ifelse(records_by_week()$ou_wp<1,
                       paste0(round(records_by_week()$ou_wp, digits=3),")"),
                       paste0("1.000)")))}
  })
  
  ## SIDEBAR RECORDS BY CONFERENCE ##
  records_by_conference <- reactive({
    if(input$conference != "AP Top 25"){
    jay_predictions %>%
      drop_na(home_points) %>%
      filter(home_conference == input$conference | away_conference == input$conference) %>%
      group_by(season) %>%
      summarise(ml_wins = sum(correct_pick_jay),
                ml_losses = sum(!is.na(home_points))-ml_wins,
                ml_wp = ml_wins/sum(!is.na(home_points)),
                ats_wins = sum(correct_pick_jay_ats, na.rm=TRUE),
                ats_losses = sum(!is.na(correct_pick_jay_ats))-ats_wins,
                ats_pushes = sum(is.na(correct_pick_jay_ats)),
                ats_wp = ats_wins/sum(!is.na(correct_pick_jay_ats)),
                ou_wins = sum(correct_jay_over, na.rm = TRUE),
                ou_losses = sum(!is.na(correct_jay_over))-ou_wins,
                ou_pushes = sum(is.na(correct_jay_over)),
                ou_wp = ou_wins/sum(!is.na(correct_jay_over))) %>%
      filter(season == input$season)}
    else{
        jay_predictions %>%
          drop_na(home_points) %>%
          filter(!is.na(rank_home) | !is.na(rank_away)) %>%
          group_by(season) %>%
          summarise(ml_wins = sum(correct_pick_jay),
                    ml_losses = sum(!is.na(home_points))-ml_wins,
                    ml_wp = ml_wins/sum(!is.na(home_points)),
                    ats_wins = sum(correct_pick_jay_ats, na.rm=TRUE),
                    ats_losses = sum(!is.na(correct_pick_jay_ats))-ats_wins,
                    ats_pushes = sum(is.na(correct_pick_jay_ats)),
                    ats_wp = ats_wins/sum(!is.na(correct_pick_jay_ats)),
                    ou_wins = sum(correct_jay_over, na.rm = TRUE),
                    ou_losses = sum(!is.na(correct_jay_over))-ou_wins,
                    ou_pushes = sum(is.na(correct_jay_over)),
                    ou_wp = ou_wins/sum(!is.na(correct_jay_over))) %>%
          filter(season == input$season)
    }
  })
  
  output$conference_records_header <- renderText({
    if(input$conference=="All"){NULL}
    else{paste0(input$season," ",input$conference," Predictions")}
  })
  output$conference_records_ml <- renderText({
    if(input$conference=="All"){NULL}
    else{paste0("M/L: ",records_by_conference()$ml_wins,"-",records_by_conference()$ml_losses," (",
                ifelse(records_by_conference()$ml_wp<1,
                       paste0(round(records_by_conference()$ml_wp, digits=3),")"),
                       paste0("1.000)")))}
  })
  output$conference_records_ats <- renderText({
    if(input$conference=="All"){NULL}
    else{paste0("ATS: ",records_by_conference()$ats_wins,"-",records_by_conference()$ats_losses,"-",records_by_conference()$ats_pushes,
           " (",ifelse(records_by_conference()$ats_wp<1,
                       paste0(round(records_by_conference()$ats_wp, digits=3),")"),
                       paste0("1.000)")))}
  })
  output$conference_records_ou <- renderText({
    if(input$conference=="All"){NULL}
    else{paste0("O/U: ",records_by_conference()$ou_wins,"-",records_by_conference()$ou_losses,"-",records_by_conference()$ou_pushes,
           " (",ifelse(records_by_conference()$ou_wp<1,
                       paste0(round(records_by_conference()$ou_wp, digits=3),")"),
                       paste0("1.000)")))}
  })
  
  ## SIDEBAR RECORDS BY TEAM ##
  records_by_team <- reactive({
    jay_predictions %>%
      drop_na(home_points) %>%
      filter(home_team == input$team | away_team == input$team) %>%
      group_by(season) %>%
      summarise(ml_wins = sum(correct_pick_jay),
                ml_losses = sum(!is.na(home_points))-ml_wins,
                ml_wp = ml_wins/sum(!is.na(home_points)),
                ats_wins = sum(correct_pick_jay_ats, na.rm=TRUE),
                ats_losses = sum(!is.na(correct_pick_jay_ats))-ats_wins,
                ats_pushes = sum(is.na(correct_pick_jay_ats)),
                ats_wp = ats_wins/sum(!is.na(correct_pick_jay_ats)),
                ou_wins = sum(correct_jay_over, na.rm = TRUE),
                ou_losses = sum(!is.na(correct_jay_over))-ou_wins,
                ou_pushes = sum(is.na(correct_jay_over)),
                ou_wp = ou_wins/sum(!is.na(correct_jay_over))) %>%
      filter(season == input$season)
  })
  
  output$team_records_header <- renderText({
    if(input$team=="All"){NULL}
    else{paste0(input$season," ",input$team," Predictions")}
  })
  output$team_records_ml <- renderText({
    if(input$team=="All"){NULL}
    else{paste0("M/L: ",records_by_team()$ml_wins,"-",records_by_team()$ml_losses," (",
                ifelse(records_by_team()$ml_wp<1,
                       paste0(round(records_by_team()$ml_wp, digits=3),")"),
                       paste0("1.000)")))}
  })
  output$team_records_ats <- renderText({
    if(input$team=="All"){NULL}
    else{paste0("ATS: ",records_by_team()$ats_wins,"-",records_by_team()$ats_losses,"-",records_by_team()$ats_pushes,
                " (",ifelse(records_by_team()$ats_wp<1,
                            paste0(round(records_by_team()$ats_wp, digits=3),")"),
                            paste0("1.000)")))}
  })
  output$team_records_ou <- renderText({
    if(input$team=="All"){NULL}
    else{paste0("O/U: ",records_by_team()$ou_wins,"-",records_by_team()$ou_losses,"-",records_by_team()$ou_pushes,
                " (",ifelse(records_by_team()$ou_wp<1,
                            paste0(round(records_by_team()$ou_wp, digits=3),")"),
                            paste0("1.000)")))}
  })
  
  ## FILTERS AND SELECTING FIELDS FOR MAIN JAY PREDICTIONS TABLE ##
  
  jay_predictions_teams_filter <- reactive({
    if(input$week != "All"){
      if(input$conference=="All" && input$team=="All"){
        jay_predictions %>%
          filter(season == input$season) %>%
          filter(week == input$week)}
      else if(input$conference=="All" && input$team != "All"){
        jay_predictions %>%
          filter(season == input$season) %>%
          filter(week == input$week) %>%
          filter(home_team %in% input$team | away_team %in% input$team)}
      else if(input$conference %in% c("ACC","Big Ten","Big 12","American Athletic","SEC","Mid-American",
                                      "FBS Independents","Sun Belt","Mountain West","Conference USA","Pac-12") && input$team=="All"){
        jay_predictions %>%
          filter(season == input$season) %>%
          filter(week == input$week) %>%
          filter(home_conference == input$conference | away_conference == input$conference)}
      else if(input$conference=="AP Top 25" && input$team == "All"){
        jay_predictions %>%
          filter(season == input$season) %>%
          filter(week == input$week) %>%
          filter(!is.na(rank_home) | !is.na(rank_away))}
      else if(input$conference=="AP Top 25" && input$team != "All"){
        jay_predictions %>%
          filter(season == input$season) %>%
          filter(week == input$week) %>%
          filter(!is.na(rank_home) | !is.na(rank_away)) %>%
          filter(home_team %in% input$team | away_team %in% input$team)}
      else{
        jay_predictions %>%
          filter(season == input$season) %>%
          filter(week == input$week) %>%
          filter(home_conference == input$conference | away_conference == input$conference) %>%
          filter(home_team == input$team | away_team == input$team)}
    }
    else if(input$week == "All"){
      jay_predictions %>%
        filter(season == input$season) %>%
        filter(home_team %in% input$team | away_team %in% input$team)}
    })
  
  jay_predictions_status_filter <- reactive({
    if(input$game_status == "All"){
      jay_predictions_teams_filter() %>%
        select(logos,names,formatted_score,formatted_jay_score,vegas_lines_formatted,jayMo_lines_formatted,formatted_picks_icons,
               formatted_start_date, score_proximity,hit_proximity,game_status,correct_pick_jay)
        }
    else{
      jay_predictions_teams_filter() %>%
        filter(game_status == input$game_status) %>%
        select(logos,names,formatted_score,formatted_jay_score,vegas_lines_formatted,jayMo_lines_formatted,formatted_picks_icons,
               formatted_start_date, score_proximity,hit_proximity,game_status,correct_pick_jay)}
  })
  
  ## CREATE JAY PREDICTIONS TABLE ##
  
  output$jay_predictions_table <- DT::renderDataTable({
    DT::datatable(jay_predictions_status_filter(),
                  class = list(stripe = FALSE),
                  escape=FALSE,
                  colnames = c("","", "Final Score","jayMo Score","Betting Lines","jayMo Picks"),
                  rownames = FALSE,
                  extensions = 'RowGroup',
                  options=list(pageLength = 5,
                               rowGroup = list(dataSrc=c(7)),
                               columnDefs = list(list(visible=FALSE, targets = c("formatted_start_date",
                                                                                 "score_proximity",
                                                                                 "hit_proximity",
                                                                                 "game_status",
                                                                                 "correct_pick_jay")),
                                                 list(className = 'dt-center', targets = c("logos")),
                                                 list(className = 'dt-left', targets = c("names","formatted_score","formatted_jay_score",
                                                                                         "vegas_lines_formatted","jayMo_lines_formatted",
                                                                                         "formatted_picks_icons")),
                                                 list(targets = 0, width = '100px'),
                                                 list(targets = 1, width = '250px'),
                                                 list(targets = 2, width = '100px'),
                                                 list(targets = 3, width = '100px'),
                                                 list(targets = 4, width = '200px'),
                                                 list(targets = 5, width = '200px'),
                                                 list(targets = 6, width = '50px'),
                                                 list(orderable = F,
                                                      targets = "_all")),
                               dom = 'tpr')) %>%
      formatStyle('formatted_jay_score', 'score_proximity', backgroundColor = styleEqual(c(NA,0,1,2), c('','pink','lightgreen','gold'))) %>%
      formatStyle('formatted_score','game_status', backgroundColor = styleEqual(c("Incomplete","Complete"), c('','lightgray'))) %>%
      formatStyle(c('jayMo_lines_formatted','formatted_picks_icons'), 'hit_proximity', backgroundColor = styleEqual(c(0,1,2,3), c('','pink','lightyellow', 'lightgreen'))) %>%
      formatStyle('vegas_lines_formatted','game_status', backgroundColor = styleEqual(c("Incomplete","Complete"), c('','lightgray'))) %>%
      formatStyle(columns = c("logos","names","formatted_score","formatted_jay_score"), fontSize = '14pt', 'vertical-align'='middle') %>%
      formatStyle(columns = c("vegas_lines_formatted","jayMo_lines_formatted","formatted_picks_icons"), fontSize = '12pt', 'vertical-align'='middle') %>%
      formatStyle(columns = c("logos","names","formatted_score","formatted_jay_score",
                  "vegas_lines_formatted","jayMo_lines_formatted","formatted_picks_icons"),
                  borderBottomColor = "black", borderBottomStyle = "solid", borderBottomWidth = "2px")
  })
  
  ## JAY UNDERDOGS FORMATTING AND FILTERING ##
  
  jay_underdogs_filtered <- reactive({
    jay_underdogs %>%
      filter(season == input$season) %>%
      filter(week == input$week) %>%
      select(underdog_logo,formatted_string, favorite_logo, underdog_win_probability, correct_pick_jay) %>%
      slice_max(order_by = underdog_win_probability, n=3)
  })

  ## CREATE JAY UNDERDOGS TABLE ##
  
  output$jay_underdogs_table <- DT::renderDataTable({
    DT::datatable(jay_underdogs_filtered(),
                  class = list(stripe = FALSE),
                  escape=FALSE,
                  rownames = TRUE,
                  colnames = c("","jayMo Prediction","","jayMo Win Probability",""),
                  options=list(columnDefs = list(list(visible=FALSE, targets = c(5)),
                                                 list(className = 'dt-center', targets = c(0,1,2,3,4)),
                                                 list(orderable = F,
                                                      targets = "_all")),
                               dom = '')) %>%
      formatPercentage(4,digits=1,interval=3,mark=".") %>%
      formatStyle(columns = c(0,1,2,3,4), fontSize = '12pt', 'vertical-align'='middle') %>%
      formatStyle(c(0,1,2,3,4),'correct_pick_jay',backgroundColor = styleEqual(c(0, 1, NA), c('pink', 'lightgreen',''))
      )
    
  })
  
  ## JAY BETTING FAVES FORMATTING AND FILTERING ##
  
  betting_favorites <- reactive({
    betting_records_by_team %>%
      filter(season==input$season) %>%
      group_by(team) %>%
      summarise(total_hits = sum(ml_wins,ats_wins,ou_wins),
                total_misses = sum(ml_losses,ats_losses,ou_losses),
                total_pushes = sum(ats_pushes,ou_pushes),
                total_hp = total_hits/(total_hits+total_misses),
                logo = max(logo),
                color = max(color))
  })
  
  betting_favorites_table <- reactive({
    betting_favorites() %>%
      mutate(record_formatted = paste0(betting_favorites()$total_hits,"-",betting_favorites()$total_misses,"-",betting_favorites()$total_pushes)) %>%
      select(logo,team,color,record_formatted,total_hp) %>%
      arrange(desc(total_hp)) %>%
      top_n(5)
  })
    
  ## CREATE JAY BETTING FAVORITES TABLE ##
  
  
  output$jay_betting_faves_table <- DT::renderDataTable({
    DT::datatable(betting_favorites_table(),
                  class = list(stripe = FALSE),
                  escape=FALSE,
                  colnames = c("","","","Picks Record","Hit Percentage"),
                  options=list(columnDefs = list(list(visible=FALSE, targets = c(3)),
                                                 list(className = 'dt-center', targets = c(0,1,2,4,5)),
                                                 list(orderable = F,
                                                      targets = "_all")),
                               dom = '')) %>%
      formatPercentage(5,digits=1,interval=3,mark=".") %>%
      formatStyle(columns = c(0,1,2,4,5), fontSize = '12pt', 'vertical-align'='middle')
      
  })
  
#  output$jay_betting_faves_chart <- renderPlot({
#    ggplot(betting_favorites_table(), aes(x = logo, y = total_hp)) +
#      geom_bar(stat = "identity") +
#      ggdraw(insert_yaxis_grob(logo, position = "left"))
#  })
  
  ## JAYMO HEAD-TO-HEAD PREDICTIONS ##
  
  jay_game_sim <- reactive({
    jay_h2h %>%
      filter(team == input$team_1) %>%
      filter(opponent == input$team_2) %>%
      select(logo_team,rounded_team_jay,dash_column,rounded_jay_opp,logo_opp)
    })
  
  output$jay_h2h_table <- DT::renderDataTable({
    DT::datatable(jay_game_sim(),
                  class = list(stripe = FALSE),
                  escape=FALSE,
                  colnames = c("Team 1", "","jayMo Prediction","","Team 2"),
                  rownames = FALSE,
                  options=list(columnDefs = list(list(className = 'dt-center', targets = c(0,1,2,3,4)),
                                                 list(orderable = F,
                                                      targets = "_all")),
                               dom = 'tr')) %>%
      formatStyle(columns = c(0,1,2,3,4), fontSize = '42pt', 'vertical-align'='middle') %>%
      formatStyle(columns = c(0,1,2,3,4),
                  borderBottomColor = "black", borderBottomStyle = "solid", borderBottomWidth = "2px",
                  borderTopColor = "black", borderTopStyle = "solid", borderTopWidth = "2px") %>%
      formatStyle(columns = c(5),
                  borderRightColor = "black", borderRightStyle = "solid", borderRightWidth = "2px") %>%
      formatStyle(columns = c(1),
                  borderLeftColor = "black", borderLeftStyle = "solid", borderLeftWidth = "2px")
  })
  
  output$jay_ranks_table <- DT::renderDataTable({
    DT::datatable(jay_ranks,
                  class = list(stripe = FALSE),
                  escape=FALSE,
                  colnames = c("Rank","","Team","Record","jayMo Rating"),
                  rownames = FALSE,
                  options=list(pageLength = 134,
                               columnDefs = list(list(className = 'dt-center', targets = c(0,1,2,3,4)),
                                                 list(orderable = F,
                                                      targets = "_all")),
                               dom = 'tr')) %>%
      formatRound(c(5),digits=1,interval=3,mark=".")
    
  })

}

shinyApp(ui, server)
