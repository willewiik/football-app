
library(shiny)
library(DT)
library(dplyr)
library(worldfootballR)
library(shinythemes)
library(tidyr)
library(htmltools)
library(htmlwidgets)
library(tidyverse)
library(gt)
library(shinyalert)
library(RMySQL)
library(DBI)
library(RSQLite)
library(dbplyr)
library(shinyWidgets)
library(RJSONIO)
library(httr)
library(stringr)
library(lme4)
library(plotly)
library(RColorBrewer)




ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("Football betting EV+ app, created by @wiikwilliam"),
                
               includeCSS("style.css"),
                navbarPage("Home",
                           tabPanel("Premier League",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Matchup",
                                                 selectInput("pl_select", "Select Match", choices = NULL),
                                                 
                                                 fluidRow(
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "pl_checkbox_season_home",  label = "Season home team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "pl_radio_location_home",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   ),
                
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "pl_checkbox_season_away",  label = "Season Away team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "pl_radio_location_away",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          gt_output("pl_teams_stats")
                                                   ),
                                                   column(6,
                                                          includeHTML("html/pl_match.html")
                                                   ),
                                                 ),
                                
                                                 prettyRadioButtons(
                                                   inputId = "pl_radio_home_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("pl_home_team_matches"),
                                                 prettyRadioButtons(
                                                   inputId = "pl_radio_away_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("pl_away_team_matches"),
                                                 materialSwitch(
                                                   inputId = "pl_odds_switch_model",
                                                   label = "Use willewiiks model to predict odds", 
                                                   value = TRUE,
                                                   status = "primary"
                                                 ),
                                                 actionBttn(
                                                   inputId = "pl_show_kambi",
                                                   label = "Show kambi odds",
                                                   style = "pill", 
                                                   color = "danger"
                                                 ),
                                                 gt_output("pl_teams_odds"),
                                                 
                                                ),
                                               
                                        
                                        tabPanel("League Table",
                                                 DTOutput("pl_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("pl_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "pl_home_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "pl_home_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("pl_home_player"),
                                                 DTOutput("pl_home_player_per_match"),
                                                 gt_output("pl_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("pl_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "pl_away_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "pl_away_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("pl_away_player"),
                                                 DTOutput("pl_away_player_per_match"),
                                                 gt_output("pl_away_player_odds")),
                                      )
                                    )
                           ),
                           tabPanel("Serie A",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Matchup",
                                                 selectInput("serie_a_select", "Select Match", choices = NULL),
                                                 
                                                 fluidRow(
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "serie_a_checkbox_season_home",  label = "Season home team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "serie_a_radio_location_home",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "serie_a_checkbox_season_away",  label = "Season Away team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "serie_a_radio_location_away",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          gt_output("serie_a_teams_stats")
                                                   ),
                                                   column(6,
                                                          includeHTML("html/serie_a_match.html")
                                                   ),
                                                 ),
                                                 
                                                 prettyRadioButtons(
                                                   inputId = "serie_a_radio_home_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("serie_a_home_team_matches"),
                                                 prettyRadioButtons(
                                                   inputId = "serie_a_radio_away_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("serie_a_away_team_matches"),
                                                 materialSwitch(
                                                   inputId = "serie_a_odds_switch_model",
                                                   label = "Use willewiiks model to predict odds", 
                                                   value = TRUE,
                                                   status = "primary"
                                                 ),
                                                 actionBttn(
                                                   inputId = "serie_a_show_kambi",
                                                   label = "Show kambi odds",
                                                   style = "pill", 
                                                   color = "danger"
                                                 ),
                                                 gt_output("serie_a_teams_odds"),
                                        ),
                                        
                                        
                                        tabPanel("League Table",
                                                 DTOutput("serie_a_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("serie_a_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "serie_a_home_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "serie_a_home_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("serie_a_home_player"),
                                                 DTOutput("serie_a_home_player_per_match"),
                                                 gt_output("serie_a_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("serie_a_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "serie_a_away_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "serie_a_away_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("serie_a_away_player"),
                                                 DTOutput("serie_a_away_player_per_match"),
                                                 gt_output("serie_a_away_player_odds")),
                                      )
                                    )
                           ),
                           tabPanel("La liga",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Matchup",
                                                 selectInput("la_liga_select", "Select Match", choices = NULL),
                                                 
                                                 fluidRow(
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "la_liga_checkbox_season_home",  label = "Season home team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "la_liga_radio_location_home",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "la_liga_checkbox_season_away",  label = "Season Away team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "la_liga_radio_location_away",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          gt_output("la_liga_teams_stats")
                                                   ),
                                                   column(6,
                                                          includeHTML("html/la_liga_match.html")
                                                   ),
                                                 ),
                                                 
                                                 prettyRadioButtons(
                                                   inputId = "la_liga_radio_home_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("la_liga_home_team_matches"),
                                                 prettyRadioButtons(
                                                   inputId = "la_liga_radio_away_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("la_liga_away_team_matches"),
                                                 materialSwitch(
                                                   inputId = "la_liga_odds_switch_model",
                                                   label = "Use willewiiks model to predict odds", 
                                                   value = TRUE,
                                                   status = "primary"
                                                 ),
                                                 actionBttn(
                                                   inputId = "la_liga_show_kambi",
                                                   label = "Show kambi odds",
                                                   style = "pill", 
                                                   color = "danger"
                                                 ),
                                                 gt_output("la_liga_teams_odds"),
                                        ),
                                        
                                        
                                        tabPanel("League Table",
                                                 DTOutput("la_liga_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("la_liga_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "la_liga_home_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "la_liga_home_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("la_liga_home_player"),
                                                 DTOutput("la_liga_home_player_per_match"),
                                                 gt_output("la_liga_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("la_liga_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "la_liga_away_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "la_liga_away_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("la_liga_away_player"),
                                                 DTOutput("la_liga_away_player_per_match"),
                                                 gt_output("la_liga_away_player_odds")),
                                      )
                                    )
                           ),
                           tabPanel("Bundesliga",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Matchup",
                                                 selectInput("bundesliga_select", "Select Match", choices = NULL),
                                                 
                                                 fluidRow(
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "bundesliga_checkbox_season_home",  label = "Season home team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "bundesliga_radio_location_home",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "bundesliga_checkbox_season_away",  label = "Season Away team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "bundesliga_radio_location_away",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          gt_output("bundesliga_teams_stats")
                                                   ),
                                                   column(6,
                                                          includeHTML("html/bundesliga_match.html")
                                                   ),
                                                 ),
                                                 
                                                 prettyRadioButtons(
                                                   inputId = "bundesliga_radio_home_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("bundesliga_home_team_matches"),
                                                 prettyRadioButtons(
                                                   inputId = "bundesliga_radio_away_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("bundesliga_away_team_matches"),
                                                 materialSwitch(
                                                   inputId = "bundesliga_odds_switch_model",
                                                   label = "Use willewiiks model to predict odds", 
                                                   value = TRUE,
                                                   status = "primary"
                                                 ),
                                                 actionBttn(
                                                   inputId = "bundesliga_show_kambi",
                                                   label = "Show kambi odds",
                                                   style = "pill", 
                                                   color = "danger"
                                                 ),
                                                 gt_output("bundesliga_teams_odds"),
                                        ),
                                        
                                        
                                        tabPanel("League Table",
                                                 DTOutput("bundesliga_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("bundesliga_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "bundesliga_home_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "bundesliga_home_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("bundesliga_home_player"),
                                                 DTOutput("bundesliga_home_player_per_match"),
                                                 gt_output("bundesliga_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("bundesliga_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "bundesliga_away_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "bundesliga_away_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("bundesliga_away_player"),
                                                 DTOutput("bundesliga_away_player_per_match"),
                                                 gt_output("bundesliga_away_player_odds")),
                                      )
                                    )
                           ),
                           tabPanel("Ligue 1",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Matchup",
                                                 selectInput("ligue_1_select", "Select Match", choices = NULL),
                                                 
                                                 fluidRow(
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "ligue_1_checkbox_season_home",  label = "Season home team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "ligue_1_radio_location_home",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "ligue_1_checkbox_season_away",  label = "Season Away team",
                                                            choices = c("2023/2024", "2022/2023","All"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                          )
                                                   ),
                                                   
                                                   column(3,
                                                          prettyRadioButtons( 
                                                            inputId = "ligue_1_radio_location_away",  label = "Home/Away",
                                                            choices = c("Home", "Away", "Both"), icon = icon("check"), animation = "smooth",
                                                            status = "primary", bigger = TRUE,thick = TRUE, selected = "Both"
                                                          )
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          gt_output("ligue_1_teams_stats")
                                                   ),
                                                   column(6,
                                                          includeHTML("html/ligue_1_match.html")
                                                   ),
                                                 ),
                                                 
                                                 prettyRadioButtons(
                                                   inputId = "ligue_1_radio_home_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("ligue_1_home_team_matches"),
                                                 prettyRadioButtons(
                                                   inputId = "ligue_1_radio_away_team", label = "",
                                                   choices = c("For", "Against", "Total"), shape = "round",
                                                   status = "danger", fill = TRUE, inline = TRUE
                                                 ),
                                                 DTOutput("ligue_1_away_team_matches"),
                                                 materialSwitch(
                                                   inputId = "ligue_1_odds_switch_model",
                                                   label = "Use willewiiks model to predict odds", 
                                                   value = TRUE,
                                                   status = "primary"
                                                 ),
                                                 actionBttn(
                                                   inputId = "ligue_1_show_kambi",
                                                   label = "Show kambi odds",
                                                   style = "pill", 
                                                   color = "danger"
                                                 ),
                                                 gt_output("ligue_1_teams_odds"),
                                        ),
                                        
                                        
                                        tabPanel("League Table",
                                                 DTOutput("ligue_1_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("ligue_1_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "ligue_1_home_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "ligue_1_home_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("ligue_1_home_player"),
                                                 DTOutput("ligue_1_home_player_per_match"),
                                                 gt_output("ligue_1_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("ligue_1_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 prettyRadioButtons( 
                                                   inputId = "ligue_1_away_player_season",  label = "Season",
                                                   choices = c("2023/2024","2022/2023", "All"), icon = icon("check"), animation = "smooth",
                                                   status = "primary", bigger = TRUE,thick = TRUE, selected = "2023/2024"
                                                 ),
                                                 materialSwitch(
                                                   inputId = "ligue_1_away_player_pos_ALL",
                                                   label = "Ungroup on position", 
                                                   value = FALSE,
                                                   status = "primary"
                                                 ),
                                                 DTOutput("ligue_1_away_player"),
                                                 DTOutput("ligue_1_away_player_per_match"),
                                                 gt_output("ligue_1_away_player_odds")),
                                      )
                                    )
                           ),
                           tabPanel("Referees",
                                    mainPanel(
                                    #  selectInput("referee_selector", "Select Referee:", choices = NULL),
                                      multiInput(
                                        inputId = "referee_selector",
                                        label = "Select Referee:", 
                                        choices = c("","")
                                       
                                      ),
                                      plotlyOutput("referee_plot")
                                    )
                           ),
                           tabPanel("Show EV+ Bets",
                                    mainPanel(
                                      fluidRow(
                                        column(3,
                                               sliderTextInput(
                                                 inputId = "pick_EV",
                                                 label = "Choose minimum EV", 
                                                 choices = c("90% +","100% +", "105% +", "110% +", "115% +", "120% +",
                                                             "125% +", "130% +"),
                                                 selected = "100% +"
                                               )
                                        ),
                                        column(3,
                                               awesomeCheckboxGroup(
                                                 inputId = "EV_leagues", label = "League",
                                                 choices = c("PL", "Serie A",
                                                             "La Liga", "Bundesliga",
                                                             "Ligue 1"),
                                                 status = "danger",
                                                 selected = c("PL", "Serie A",
                                                              "La Liga", "Bundesliga",
                                                              "Ligue 1")
                                               )
                                        ),
                                        column(3,
                                               awesomeCheckboxGroup(
                                                 inputId = "EV_markets", label = "Market",
                                                 choices = c("Sot", "Shots",
                                                             "Offside"),
                                                 status = "danger",
                                                 selected = c("Sot", "Shots",
                                                              "Offside")
                                               )
                                        ),
                                      ),
                                      
                                      fluidRow(
                                        column(3,
                                               actionBttn(
                                                 inputId = "pl_show_EV_bets",
                                                 label = "Load EV+ Bets, Premier League",
                                                 style = "simple", 
                                                 size = "sm",
                                                 color = "primary"
                                               )
                                        ),
                                        
                                        column(2,
                                               actionBttn(
                                                 inputId = "serie_a_show_EV_bets",
                                                 label = "Load EV+ Bets, Serie A",
                                                 size = "sm",
                                                 style = "simple", 
                                                 color = "success"
                                               )
                                        ),
                                        column(2,
                                               actionBttn(
                                                 inputId = "la_liga_show_EV_bets",
                                                 label = "Load EV+ Bets, La Liga",
                                                 style = "simple", 
                                                 size = "sm",
                                                 color = "danger"
                                               )
                                        ),
                                        column(3,
                                               actionBttn(
                                                 inputId = "bundesliga_show_EV_bets",
                                                 label = "Load EV+ Bets, Bundesliga",
                                                 style = "simple", 
                                                 size = "sm",
                                                 color = "royal"
                                               ),
                                               
                                        ),
                                        column(2,
                                               actionBttn(
                                                 inputId = "ligue_1_show_EV_bets",
                                                 label = "Load EV+ Bets, Ligue 1",
                                                 style = "simple", 
                                                 size = "sm",
                                                 color = "warning"
                                               ),
                                        )),
                                      progressBar(
                                        id = "progress_bar",
                                        value = 0,
                                        total = 100,
                                        title = "",
                                        display_pct = TRUE
                                      ),
                                      DTOutput("DT_EV"),
                                     
                                    )
                           )
                )
             
                
)


server <- function(input, output, session) {
  
  
  source("worldfootball.R", local = TRUE) # Functions
  source("gt_tabels.R", local = TRUE) # GT functions
  source("selenium_code.R", local = TRUE)
  
  res <- load_match_results(country = c("ENG","ITA","ESP","GER","FRA"),
                            gender ="M",
                            season_end_year = 2024,
                            tier = "1st")
  count <- 0
  
  
  kambi_odds_on_pl <- reactiveVal(NULL)
  kambi_odds_on_serie_a <- reactiveVal(NULL)
  kambi_odds_on_la_liga <- reactiveVal(NULL)
  kambi_odds_on_bundesliga <- reactiveVal(NULL)
  kambi_odds_on_ligue_1 <- reactiveVal(NULL)
  
  
  show_EV <- reactiveVal(NULL)
  serie_a_show_EV <- reactiveVal(NULL)
  la_liga_show_EV <- reactiveVal(NULL)
  bundesliga_show_EV <- reactiveVal(NULL)
  ligue_1_show_EV <- reactiveVal(NULL)
  
  
  observeEvent(input$pl_select, {kambi_odds_on_pl(NULL)})
  observeEvent(input$serie_a_select, {kambi_odds_on_serie_a(NULL)})
  observeEvent(input$la_liga_select, {kambi_odds_on_la_liga(NULL)})
  observeEvent(input$bundesliga_select, {kambi_odds_on_bundesliga(NULL)})
  observeEvent(input$ligue_1_select, {kambi_odds_on_ligue_1(NULL)})
  
  league_table <- get_league_table(res)
  
  # DATABASE
  dbHost <- "localhost"
  dbPort <- 3306  # Portnummer
  dbName <- "sql_workbench"
  dbUser <- "root"
  dbPassword <- Sys.getenv("Key1")
  

  con <- dbConnect(MySQL(), host = dbHost, port = dbPort, dbname = dbName, user = dbUser, password = dbPassword)
  
  id_teams_2023 <- readRDS("rds_files/id_teams_2023.rds")
  id_teams_2023$hteam <- rename_teams(id_teams_2023$hteam, from = "fbref_full", to = "fbref")
  
  
  # ============================================================================
  # ============================ SelectInput ===================================
  # ============================================================================
  
  # Get the current date
  current_date <- as.Date(Sys.Date())
  
  # Filter the data to include only future matches
  upc_all <- res %>%
    filter(Date >= current_date) %>%
    group_by(Competition_Name) %>%
    slice(1:10) %>%
    ungroup()
  
  
  # Get ref data 
  referee_stats <- get_referee_data(con)
  
  
  observe({
    upc <- upc_all[upc_all$Competition_Name == "Premier League",]
  updateSelectInput(session,"pl_select",
                  choices = paste(upc$Date, upc$Home, "vs", upc$Away))
  
  upc <- upc_all[upc_all$Competition_Name == "Serie A",]
  updateSelectInput(session,"serie_a_select",
                    choices = paste(upc$Date, upc$Home, "vs", upc$Away))
  
  upc <- upc_all[upc_all$Competition_Name == "La Liga",]
  updateSelectInput(session,"la_liga_select",
                    choices = paste(upc$Date, upc$Home, "vs", upc$Away))
  
  upc <- upc_all[upc_all$Competition_Name == "Fußball-Bundesliga",]
  updateSelectInput(session,"bundesliga_select",
                    choices = paste(upc$Date, upc$Home, "vs", upc$Away))
  
  upc <- upc_all[upc_all$Competition_Name == "Ligue 1",]
  updateSelectInput(session,"ligue_1_select",
                    choices = paste(upc$Date, upc$Home, "vs", upc$Away))
  
  #updateSelectInput(session, "referee_selector", choices = unique(referee_stats$referee))
  updateMultiInput(session, "referee_selector", choices = unique(referee_stats$referee))
    
  })
  

 
  # ============================================================================
  # ============================================================================
  

    # Filter the desired columns for Premier League
    # pl_league <- cbind(1:20,league_table[league_table$Competition_Name == "Premier League",-1 ])
    # 
    # output$pl_table <- renderDT({
    #   datatable(pl_league,
    #              options = list(
    #                 ordering = FALSE,
    #                 paging = FALSE,
    #                 autoWidth = FALSE,  # Allow automatic width
    #                 initComplete = JS('function(settings, json) {$(this.api().table().node()).css("width", "50%");}'),
    #                 searching = FALSE
    #                  ),
    #               selection = 'none',
    #               class = 'cell-border stripe',
    #               escape = FALSE,
    #               rownames = FALSE,
    #               filter = "none",
    #               width = 50
    #            )
    # })
    
    # ==========================================================================
    # ============================ 5 SEC WAIT =================================
    # ==========================================================================
    
  
  
     all_team_stats <- get_all_team_specific_stats(con)
     mod <-  models_shots_sot(con)
     
     models <-  mod[[1]]
     sot_ratio <-  mod[[2]]

                                    
    # ==========================================================================
    # ============================ 5 SEC WAIT =================================
    # ==========================================================================
                                  
    
    # Empty matrix for teams stats
    mat <- matrix(NA, 15, 6) %>% 
      as.data.frame() %>% 
      `colnames<-`(c("Stats","HomeTeam_For", "HomeTeam_Against",
                     "AwayTeam_For", "AwayTeam_Against", "Total"))
    
    mat$Stats <- c("Fouls","Corners","Tackles","Offsides","Goal kicks",
                   "Throw ins","Shots","Shots on target","Posession","Goals",
                   "xG","Yellow cards","Red cards","Odds 1x2","Odds Over 2.5")
    
    
    # ==========================================================================
    # ODDS 1x2  ================================================================
    get_odds <- function(league) {
      
      teams <- switch(league,
                      pl = team_stats_pl(),
                      serie_a = team_stats_serie_a(),
                      la_liga = team_stats_la_liga(),
                      bundesliga = team_stats_bundesliga(),
                      ligue_1 = team_stats_ligue_1()
      )
      
      if(is.null(teams)) return(NULL)
      
      url <- switch(league,
                    pl = "england/premier_league",
                    serie_a = "italy/serie_a",
                    la_liga = "spain/la_liga",
                    bundesliga = "germany/bundesliga",
                    ligue_1 =  "france/ligue_1"
      )
      odds <- get_odds_kambi(url, 12579, teams[[1]])
      
      return(odds)
    }
    
    odds_1x2_pl <- reactive({ get_odds("pl") })
    odds_1x2_serie_a <- reactive({ get_odds("serie_a") })
    odds_1x2_la_liga <- reactive({ get_odds("la_liga") })
    odds_1x2_bundesliga <- reactive({ get_odds("bundesliga") })
    odds_1x2_ligue_1 <- reactive({ get_odds("ligue_1") })
    
    
    # ODDS 1x2  ================================================================
    # ==========================================================================
    
    
    # ==========================================================================
    # TEAM NAME AND ID  ========================================================
    
    
    get_teams_id <- function(league) {
      count <- count + 1
      teams <- c()
      teams_ex <- strsplit(input[[paste0(league, "_select")]], " vs ")[[1]]
      if(identical(teams_ex, character(0))) return(NULL)
    
      # Extract home team and away team, removing the date part for the home team
      teams[1] <- gsub("^[0-9-]+\\s", "", teams_ex[1])
      teams[2] <- teams_ex[2]
      
      teams_id <- c()
      teams_id[1] <- id_teams_2023[id_teams_2023$hteam == teams[1],2]
      teams_id[2] <- id_teams_2023[id_teams_2023$hteam == teams[2],2]
      date <- str_sub(input[[paste0(league, "_select")]],1,10)
      print(paste("get_teams_id() has been used"))
      return(list(teams,teams_id,date))
    }
    
    team_stats_pl <- reactive({ get_teams_id("pl") })
    team_stats_serie_a <- reactive({ get_teams_id("serie_a") })
    team_stats_la_liga <- reactive({ get_teams_id("la_liga") })
    team_stats_bundesliga <- reactive({ get_teams_id("bundesliga") })
    team_stats_ligue_1 <- reactive({ get_teams_id("ligue_1") })
    
    
    # TEAM NAME AND ID  ========================================================
    # ==========================================================================
    
    
    # ==========================================================================
    # TEAMS STATS ==============================================================

    get_team_stats_output <- function(league) {
      
      this_teams_id <- switch(league,
                              pl = team_stats_pl(),
                              serie_a = team_stats_serie_a(),
                              la_liga = team_stats_la_liga(),
                              bundesliga = team_stats_bundesliga(),
                              ligue_1 = team_stats_ligue_1()
      )
      
      if(is.null(this_teams_id)) return(NULL)
      
      checkbox_home <- input[[paste0(league, "_checkbox_season_home")]]
      radio_home <- input[[paste0(league, "_radio_location_home")]]
      checkbox_away <- input[[paste0(league, "_checkbox_season_away")]]
      radio_away <- input[[paste0(league, "_radio_location_away")]]
      
      this_teams_stats <- get_team_stats(all_team_stats, mat, this_teams_id[[2]],
                                         checkbox_home, radio_home,
                                         checkbox_away, radio_away) 
      return(this_teams_stats)
    
    }
    
    team_stats_output_pl <- reactive({ get_team_stats_output("pl") })
    team_stats_output_serie_a <- reactive({ get_team_stats_output("serie_a") })
    team_stats_output_la_liga <- reactive({ get_team_stats_output("la_liga") })
    team_stats_output_bundesliga <- reactive({ get_team_stats_output("bundesliga") })
    team_stats_output_ligue_1 <- reactive({ get_team_stats_output("ligue_1") })
    
    
    # TEAMS STATS ==============================================================
    # ==========================================================================
    
    
    # ==========================================================================
    # GET TEAM MATCHES  ========================================================
    
    get_one_team_matches_output <- function(home_away = 1, league){
      
      this_teams_id <- switch(league,
                              pl = team_stats_pl(),
                              serie_a = team_stats_serie_a(),
                              la_liga = team_stats_la_liga(),
                              bundesliga = team_stats_bundesliga(),
                              ligue_1 = team_stats_ligue_1()
      )
      
      checkbox_home <- input[[paste0(league, "_checkbox_season_home")]]
      radio_home <- input[[paste0(league, "_radio_location_home")]]
      checkbox_away <- input[[paste0(league, "_checkbox_season_away")]]
      radio_away <- input[[paste0(league, "_radio_location_away")]]
      
      ifelse(home_away==1,{input1 <- checkbox_home},
                          {input1 <- checkbox_away})
      
      ifelse(home_away==1,{input2 <- radio_home},
                          {input2 <- radio_away})
      
      team_matches <- get_one_team_matches(con,this_teams_id[[2]][home_away],
                                           input1,input2) 
      
      return(team_matches)
    }
    
    
    home_team_matches_pl <- reactive({ get_one_team_matches_output(1,"pl") })
    home_team_matches_serie_a <- reactive({ get_one_team_matches_output(1,"serie_a") })
    home_team_matches_la_liga <- reactive({ get_one_team_matches_output(1,"la_liga") })
    home_team_matches_bundesliga <- reactive({ get_one_team_matches_output(1,"bundesliga") })
    home_team_matches_ligue_1 <- reactive({ get_one_team_matches_output(1,"ligue_1") })
    
    away_team_matches_pl <- reactive({ get_one_team_matches_output(2,"pl") })
    away_team_matches_serie_a <- reactive({ get_one_team_matches_output(2,"serie_a") })
    away_team_matches_la_liga <- reactive({ get_one_team_matches_output(2,"la_liga") })
    away_team_matches_bundesliga <- reactive({ get_one_team_matches_output(2,"bundesliga") })
    away_team_matches_ligue_1 <- reactive({ get_one_team_matches_output(2,"ligue_1") })
    
    # GET TEAM MATCHES  ========================================================
    # ==========================================================================
    
    # ==========================================================================
    # GET ODDS FOR TEAM ========================================================
    
    get_team_odds_output <- function(league) {
   
      teams <- switch(league,
                      pl = team_stats_pl(),
                      serie_a = team_stats_serie_a(),
                      la_liga = team_stats_la_liga(),
                      bundesliga = team_stats_bundesliga(),
                      ligue_1 = team_stats_ligue_1()
      )
      
      teams_stats <- switch(league,
                            pl = team_stats_output_pl(),
                            serie_a = team_stats_output_serie_a(),
                            la_liga = team_stats_output_la_liga(),
                            bundesliga = team_stats_output_bundesliga(),
                            ligue_1 = team_stats_output_ligue_1()
      )
      
      odds <- switch(league,
                    pl = odds_1x2_pl(),
                    serie_a = odds_1x2_serie_a(),
                    la_liga = odds_1x2_la_liga(),
                    bundesliga = odds_1x2_bundesliga(),
                    ligue_1 = odds_1x2_ligue_1()
      )
      
      switch_model <- input[[paste0(league, "_odds_switch_model")]]
      
      rownames(teams_stats) <- teams_stats[, 1]
      rownames(teams_stats)[c(8,12)] <- c("Sot","Cards")
      teams_stats[12,-1] <- teams_stats[12,-1] + (teams_stats[13,-1]*2) # card
      teams_stats <- teams_stats[-c(9,11,13,14,15), -1]
      
        df_team_odds <- get_team_odds(teams_stats, num_stats = 10, model = switch_model,
                                      teams[[1]], odds, models, sot_ratio)
        
        gt_obj <- get_gt_odds_team(df_team_odds, teams[[1]][1], teams[[1]][2])
        return(list(gt_obj,df_team_odds))

    }
    
    team_odds_output_pl <- reactive({ get_team_odds_output("pl") })
    team_odds_output_serie_a <- reactive({ get_team_odds_output("serie_a") })
    team_odds_output_la_liga <- reactive({ get_team_odds_output("la_liga") })
    team_odds_output_bundesliga <- reactive({ get_team_odds_output("bundesliga") })
    team_odds_output_ligue_1 <- reactive({ get_team_odds_output("ligue_1") })
    
    
    # GET ODDS FOR TEAM ========================================================
    # ==========================================================================
    
  
    
    # //////////////////////////////////////////////////////////////////////////
    # //////////////////////////////////////////////////////////////////////////
    # ============================= RENDER =====================================
    # //////////////////////////////////////////////////////////////////////////
    # //////////////////////////////////////////////////////////////////////////
    
    
    # ==========================================================================
    # ========================== Team Stats GT =================================
    
    output$pl_teams_stats <- render_gt({
      
      this_teams_id <- team_stats_pl()
      if(!is.null(this_teams_id)){
      mat_output <- team_stats_output_pl()
      
      tab_options(get_gt_teams(mat_output,this_teams_id[[1]][1],this_teams_id[[1]][2]),
                  table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
      }
    })
    
    output$serie_a_teams_stats <- render_gt({
      
      this_teams_id <- team_stats_serie_a()
      if(!is.null(this_teams_id)){
        mat_output <- team_stats_output_serie_a()
        
        tab_options(get_gt_teams(mat_output,this_teams_id[[1]][1],this_teams_id[[1]][2]),
                    table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
      }
    })
    
    output$la_liga_teams_stats <- render_gt({
      
      this_teams_id <- team_stats_la_liga()
      if(!is.null(this_teams_id)){
        mat_output <- team_stats_output_la_liga()
        
        tab_options(get_gt_teams(mat_output,this_teams_id[[1]][1],this_teams_id[[1]][2]),
                    table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
      }
    })
    
    
    output$bundesliga_teams_stats <- render_gt({
      
      this_teams_id <- team_stats_bundesliga()
      if(!is.null(this_teams_id)){
        mat_output <- team_stats_output_bundesliga()
        
        tab_options(get_gt_teams(mat_output,this_teams_id[[1]][1],this_teams_id[[1]][2]),
                    table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
      }
    })
    
    output$ligue_1_teams_stats <- render_gt({
      
      this_teams_id <- team_stats_ligue_1()
      if(!is.null(this_teams_id)){
        mat_output <- team_stats_output_ligue_1()
        
        tab_options(get_gt_teams(mat_output,this_teams_id[[1]][1],this_teams_id[[1]][2]),
                    table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
      }
    })
    
    
    # ========================== Team Stats GT =================================
    # ==========================================================================
    
    
    # ====================== HOME TEAM matches DT ==============================
    # ==========================================================================
    
    
    
    # ======================== FUNCTION FOR OUTPUT =============================
    renderDataTableForLeague <- function(league, radio_input, home = TRUE) {
      
      if(home) {
        team_matches_output <- switch(league,
                                           pl = home_team_matches_pl(),
                                           serie_a = home_team_matches_serie_a(),
                                           la_liga = home_team_matches_la_liga(),
                                           bundesliga = home_team_matches_bundesliga(),
                                           ligue_1 = home_team_matches_ligue_1()
        )
      } else {
        team_matches_output <- switch(league,
                                           pl = away_team_matches_pl(),
                                           serie_a = away_team_matches_serie_a(),
                                           la_liga = away_team_matches_la_liga(),
                                           bundesliga = away_team_matches_bundesliga(),
                                           ligue_1 = away_team_matches_ligue_1()
        )
      }
      
      yellow <- 21
      red <- 22
      
      if (radio_input == "For") {
        team_matches_output <- team_matches_output[, -c(24:36)]
      } else if (radio_input == "Against") {
        team_matches_output <- team_matches_output[, -c(11:23)]
      } else {
        team_matches_output <- cbind(team_matches_output[, c(1:10)],
                                          team_matches_output[, c(11:18, 20:22)] +
                                            team_matches_output[, c(24:31, 33:35)])
        yellow <- 20
        red <- 21
      }
      
      datatable(team_matches_output, escape = c(3, 4), rownames = FALSE) %>%
        formatStyle(c("Gh", "Ga"), fontWeight = "bolder", fontSize = "20px", textAlign = "center") %>%
        formatStyle("1", backgroundColor = "#83ca89", borderLeft = "0.5rem solid") %>%
        formatStyle(c("X", "2"), backgroundColor = "#83ca89") %>%
        formatStyle(c("o2.5", "u2.5"), backgroundColor = "#a6d9aa") %>%
        formatStyle(yellow, backgroundColor = "#fbffa1", borderLeft = "solid") %>%
        formatStyle(11, borderLeft = "solid") %>%
       # formatStyle(17, fontSize = "25px", color = "red") %>%
        formatStyle(red, backgroundColor = "#F47174", borderRight = "solid") %>%
        formatStyle("Date", backgroundColor = "#83ca89", borderRight = "0.5rem solid") %>%
        formatStyle(c(1, 6:23), fontWeight = "bold")
    }
    
    # ==========================================================================
    
    output$pl_home_team_matches <- renderDataTable({
      renderDataTableForLeague("pl", input$pl_radio_home_team)
    })
    
    output$serie_a_home_team_matches <- renderDataTable({
      renderDataTableForLeague("serie_a", input$serie_a_radio_home_team)
    })
    
    output$la_liga_home_team_matches <- renderDataTable({
      renderDataTableForLeague("la_liga", input$la_liga_radio_home_team)
    })
    
    output$bundesliga_home_team_matches <- renderDataTable({
      renderDataTableForLeague("bundesliga", input$bundesliga_radio_home_team)
    })
    
    output$ligue_1_home_team_matches <- renderDataTable({
      renderDataTableForLeague("ligue_1", input$ligue_1_radio_home_team)
    })
    
    # ==========================================================================
    # ====================== HOME TEAM matches DT ==============================
    
    
    # ==========================================================================
    # ====================== AWAY TEAM matches DT ==============================
    
    
    output$pl_away_team_matches <- renderDataTable({
      renderDataTableForLeague("pl", input$pl_radio_away_team,FALSE)
    })
    
    output$serie_a_away_team_matches <- renderDataTable({
      renderDataTableForLeague("serie_a", input$serie_a_radio_away_team,FALSE)
    })
    
    output$la_liga_away_team_matches <- renderDataTable({
      renderDataTableForLeague("la_liga", input$la_liga_radio_away_team,FALSE)
    })
    
    output$bundesliga_away_team_matches <- renderDataTable({
      renderDataTableForLeague("bundesliga", input$bundesliga_radio_away_team,FALSE)
    })
    
    output$ligue_1_away_team_matches <- renderDataTable({
      renderDataTableForLeague("ligue_1", input$ligue_1_radio_away_team,FALSE)
    })
    
  
    # ====================== AWAY TEAM matches DT ==============================
    # ==========================================================================
    
    
    # ==========================================================================
    # ======================= TEAM NAME, LOGOS, ODDS ===========================
    
    
    # ======================== FUNCTION FOR OUTPUT =============================
    get_team_logo <- function(league) {
      
      team <- switch(league,
                      pl = team_stats_pl(),
                      serie_a = team_stats_serie_a(),
                      la_liga = team_stats_la_liga(),
                      bundesliga = team_stats_bundesliga(),
                      ligue_1 = team_stats_ligue_1()
      )
      
      if(is.null(team)) return(NULL)
      
        url_home <- rename_teams(team[[1]][1], from = "fbref",to = "logo_local")
        url_away <- rename_teams(team[[1]][2], from = "fbref",to = "logo_local")
        
        list_home <- list(src = url_home,
                          alt = team[[1]][1])
        
        list_away <- list(src = url_away,
                          alt = team[[1]][2])
        
        
        return(list(list_home,list_away))
      
    }
    
    
    logo_pl <- reactive({ get_team_logo("pl") })
    logo_serie_a <- reactive({ get_team_logo("serie_a") })
    logo_la_liga <- reactive({ get_team_logo("la_liga") })
    logo_bundesliga <- reactive({ get_team_logo("bundesliga") })
    logo_ligue_1 <- reactive({ get_team_logo("ligue_1") })

    
    
    
    # ======================== FUNCTION FOR OUTPUT =============================
    
  
    # =========================== LOGOS ========================================
    output$pl_hometeam_logo <- renderImage({  logo_pl()[[1]] },deleteFile=FALSE)
    output$pl_awayteam_logo <- renderImage({  logo_pl()[[2]] },deleteFile=FALSE)
    
    output$serie_a_hometeam_logo <- renderImage({  logo_serie_a()[[1]] },deleteFile=FALSE)
    output$serie_a_awayteam_logo <- renderImage({  logo_serie_a()[[2]] },deleteFile=FALSE)
    
    output$la_liga_hometeam_logo <- renderImage({  logo_la_liga()[[1]] },deleteFile=FALSE)
    output$la_liga_awayteam_logo <- renderImage({  logo_la_liga()[[2]] },deleteFile=FALSE)
    
    output$bundesliga_hometeam_logo <- renderImage({  logo_bundesliga()[[1]] },deleteFile=FALSE)
    output$bundesliga_awayteam_logo <- renderImage({  logo_bundesliga()[[2]] },deleteFile=FALSE)
    
    output$ligue_1_hometeam_logo <- renderImage({  logo_ligue_1()[[1]] },deleteFile=FALSE)
    output$ligue_1_awayteam_logo <- renderImage({  logo_ligue_1()[[2]] },deleteFile=FALSE)
    # =========================== LOGOS ========================================
   
    # =========================== ODDS ========================================
    output$pl_odds1kambi <- renderText({  odds <- odds_1x2_pl(); paste(odds[1]) })
    output$pl_oddsXkambi <- renderText({  odds <- odds_1x2_pl(); paste(odds[2]) })
    output$pl_odds2kambi <- renderText({  odds <- odds_1x2_pl(); paste(odds[3]) })
    
    output$serie_a_odds1kambi <- renderText({  odds <- odds_1x2_serie_a(); paste(odds[1]) })
    output$serie_a_oddsXkambi <- renderText({  odds <- odds_1x2_serie_a(); paste(odds[2]) })
    output$serie_a_odds2kambi <- renderText({  odds <- odds_1x2_serie_a(); paste(odds[3]) })
    
    output$la_liga_odds1kambi <- renderText({  odds <- odds_1x2_la_liga(); paste(odds[1]) })
    output$la_liga_oddsXkambi <- renderText({  odds <- odds_1x2_la_liga(); paste(odds[2]) })
    output$la_liga_odds2kambi <- renderText({  odds <- odds_1x2_la_liga(); paste(odds[3]) })
    
    output$bundesliga_odds1kambi <- renderText({  odds <- odds_1x2_bundesliga(); paste(odds[1]) })
    output$bundesliga_oddsXkambi <- renderText({  odds <- odds_1x2_bundesliga(); paste(odds[2]) })
    output$bundesliga_odds2kambi <- renderText({  odds <- odds_1x2_bundesliga(); paste(odds[3]) })
    
    output$ligue_1_odds1kambi <- renderText({  odds <- odds_1x2_ligue_1(); paste(odds[1]) })
    output$ligue_1_oddsXkambi <- renderText({  odds <- odds_1x2_ligue_1(); paste(odds[2]) })
    output$ligue_1_odds2kambi <- renderText({  odds <- odds_1x2_ligue_1(); paste(odds[3]) })
    # =========================== ODDS ========================================
    
    
    
    # =========================== TEAM NAME AND DATE ===========================
    # PL =======================================================================
    
    output$pl_hometeam_name <- renderText({
      hometeam <- team_stats_pl()
      paste(hometeam[[1]][1])
    })
    
    output$pl_awayteam_name <- renderText({
      awayteam <- team_stats_pl()
      paste(awayteam[[1]][2])
    })
    
    output$pl_date <- renderText({
     this_date <- team_stats_pl()
      paste(this_date[[3]][1])
    })
    
    # SERIE A ==================================================================
    
    output$serie_a_hometeam_name <- renderText({
      hometeam <- team_stats_serie_a()
      paste(hometeam[[1]][1])
    })
    
    output$serie_a_awayteam_name <- renderText({
      awayteam <- team_stats_serie_a()
      paste(awayteam[[1]][2])
    })
    
    output$serie_a_date <- renderText({
      this_date <- team_stats_serie_a()
      paste(this_date[[3]][1])
    })
    
    # LA LIGA ==================================================================
    
    output$la_liga_hometeam_name <- renderText({
      hometeam <- team_stats_la_liga()
      paste(hometeam[[1]][1])
    })
    
    output$la_liga_awayteam_name <- renderText({
      awayteam <- team_stats_la_liga()
      paste(awayteam[[1]][2])
    })
    
    output$la_liga_date <- renderText({
      this_date <- team_stats_la_liga()
      paste(this_date[[3]][1])
    })
    
    
    # BUNDESLIGA ===============================================================
    output$bundesliga_hometeam_name <- renderText({
      hometeam <- team_stats_bundesliga()
      paste(hometeam[[1]][1])
    })
    
    output$bundesliga_awayteam_name <- renderText({
      awayteam <- team_stats_bundesliga()
      paste(awayteam[[1]][2])
    })
    
    output$bundesliga_date <- renderText({
      this_date <- team_stats_bundesliga()
      paste(this_date[[3]][1])
    })
    
    # LIGUE 1 ==================================================================
    output$ligue_1_hometeam_name <- renderText({
      hometeam <- team_stats_ligue_1()
      paste(hometeam[[1]][1])
    })
    
    output$ligue_1_awayteam_name <- renderText({
      awayteam <- team_stats_ligue_1()
      paste(awayteam[[1]][2])
    })
    
    output$ligue_1_date <- renderText({
      this_date <- team_stats_ligue_1()
      paste(this_date[[3]][1])
    })
    # =========================== TEAM NAME AND DATE ===========================
    
  
    
    # ======================= TEAM NAME, LOGOS, ODDS ===========================
    # ==========================================================================
    
    
  
    
    
    # ==========================================================================
    # ============================= ODDS GT ====================================
    
    output$pl_teams_odds <- render_gt({
      if (!is.null(kambi_odds_on_pl())) {
        tab_options(
          get_gt_odds_team_kambi(
            team_odds_output_pl()[[2]],
            team_stats_pl()[[1]][1],
            team_stats_pl()[[1]][2],
            kambi_odds_on_pl()
          ),
          table.width = "500px",
          table.font.size = 14, column_labels.font.weight = "bold"
        )
      } else {
        tab_options(team_odds_output_pl()[[1]], table.width = "500px",
                    table.font.size = 14, column_labels.font.weight = "bold")
      }
    })

    output$serie_a_teams_odds <- render_gt({
      if (!is.null(kambi_odds_on_serie_a())) {
        tab_options(
          get_gt_odds_team_kambi(
            team_odds_output_serie_a()[[2]],
            team_stats_serie_a()[[1]][1],
            team_stats_serie_a()[[1]][2],
            kambi_odds_on_serie_a()
          ),
          table.width = "500px",
          table.font.size = 14, column_labels.font.weight = "bold"
        )
      } else {
        tab_options(team_odds_output_serie_a()[[1]], table.width = "500px",
                    table.font.size = 14, column_labels.font.weight = "bold")
      }
    })

    output$la_liga_teams_odds <- render_gt({
      if (!is.null(kambi_odds_on_la_liga())) {
        tab_options(
          get_gt_odds_team_kambi(
            team_odds_output_la_liga()[[2]],
            team_stats_la_liga()[[1]][1],
            team_stats_la_liga()[[1]][2],
            kambi_odds_on_la_liga()
          ),
          table.width = "500px",
          table.font.size = 14, column_labels.font.weight = "bold"
        )
      } else {
        tab_options(team_odds_output_la_liga()[[1]], table.width = "500px",
                    table.font.size = 14, column_labels.font.weight = "bold")
      }
    })

    output$bundesliga_teams_odds <- render_gt({
      if (!is.null(kambi_odds_on_bundesliga())) {
        tab_options(
          get_gt_odds_team_kambi(
            team_odds_output_bundesliga()[[2]],
            team_stats_bundesliga()[[1]][1],
            team_stats_bundesliga()[[1]][2],
            kambi_odds_on_bundesliga()
          ),
          table.width = "500px",
          table.font.size = 14, column_labels.font.weight = "bold"
        )
      } else {
        tab_options(team_odds_output_bundesliga()[[1]], table.width = "500px",
                    table.font.size = 14, column_labels.font.weight = "bold")
      }
    })

    output$ligue_1_teams_odds <- render_gt({
      if (!is.null(kambi_odds_on_ligue_1())) {
        tab_options(
          get_gt_odds_team_kambi(
            team_odds_output_ligue_1()[[2]],
            team_stats_ligue_1()[[1]][1],
            team_stats_ligue_1()[[1]][2],
            kambi_odds_on_ligue_1()
          ),
          table.width = "500px",
          table.font.size = 14, column_labels.font.weight = "bold"
        )
      } else {
        tab_options(team_odds_output_ligue_1()[[1]], table.width = "500px",
                    table.font.size = 14, column_labels.font.weight = "bold")
      }
    })
    
    # ============================= ODDS GT ====================================
    # ==========================================================================
    
    get_player_stats <- function(home = TRUE, league) {
      
      this_teams_id <- switch(league,
                              pl = team_stats_pl(),
                              serie_a = team_stats_serie_a(),
                              la_liga = team_stats_la_liga(),
                              bundesliga = team_stats_bundesliga(),
                              ligue_1 = team_stats_ligue_1()
      )
      
      if(home){
        season <- input[[paste0(league,"_home_player_season")]]
        team_id <- this_teams_id[[2]][1]
      } else {
        season <- input[[paste0(league,"_away_player_season")]]
        team_id <- this_teams_id[[2]][2]
      }
      
      min_minutes <- ifelse(home, input[[paste0(league,"_home_player_minutes")]],
                            input[[paste0(league,"_away_player_minutes")]])
      
      player_pos_ALL <- ifelse(home, input[[paste0(league,"_home_player_pos_ALL")]],
                               input[[paste0(league,"_away_player_pos_ALL")]])
      
    
      player_stats_DT <- sql_querys_team(con, team_id, season, min_minutes, !player_pos_ALL)
      return(player_stats_DT)
    }
    
    # Define a function to retrieve specific player's statistics
    get_specific_player_stats <- function(home = TRUE, league) {
      
      row <- ifelse(home,input[[paste0(league,"_home_player_rows_selected")]],
                    input[[paste0(league,"_away_player_rows_selected")]])
      
      if(home){
        season <- input[[paste0(league,"_home_player_season")]]
      } else {
        season <- input[[paste0(league,"_away_player_season")]]
      }
      
      player_stats_DT <- get_player_stats(home, league)
      this_player <- player_stats_DT[row, 1]
      this_player_df <- sql_querys_player(con, this_player, season)

      return(this_player_df)
    }
    
    # Define Shiny reactive elements for home and away player stats
    player_stats_reactive_home_pl <- reactive({ get_player_stats(TRUE,"pl") })
    player_stats_reactive_away_pl <- reactive({ get_player_stats(FALSE,"pl") })
    player_stats_2_reactive_home_pl <- reactive({ get_specific_player_stats(TRUE,"pl") })
    player_stats_2_reactive_away_pl <- reactive({ get_specific_player_stats(FALSE,"pl") })
    
    player_stats_reactive_home_serie_a <- reactive({ get_player_stats(TRUE,"serie_a") })
    player_stats_reactive_away_serie_a <- reactive({ get_player_stats(FALSE,"serie_a") })
    player_stats_2_reactive_home_serie_a <- reactive({ get_specific_player_stats(TRUE,"serie_a") })
    player_stats_2_reactive_away_serie_a <- reactive({ get_specific_player_stats(FALSE,"serie_a") })
    
    player_stats_reactive_home_la_liga <- reactive({ get_player_stats(TRUE,"la_liga") })
    player_stats_reactive_away_la_liga <- reactive({ get_player_stats(FALSE,"la_liga") })
    player_stats_2_reactive_home_la_liga <- reactive({ get_specific_player_stats(TRUE,"la_liga") })
    player_stats_2_reactive_away_la_liga <- reactive({ get_specific_player_stats(FALSE,"la_liga") })
    
    player_stats_reactive_home_bundesliga <- reactive({ get_player_stats(TRUE,"bundesliga") })
    player_stats_reactive_away_bundesliga <- reactive({ get_player_stats(FALSE,"bundesliga") })
    player_stats_2_reactive_home_bundesliga <- reactive({ get_specific_player_stats(TRUE,"bundesliga") })
    player_stats_2_reactive_away_bundesliga <- reactive({ get_specific_player_stats(FALSE,"bundesliga") })
    
    player_stats_reactive_home_ligue_1 <- reactive({ get_player_stats(TRUE,"ligue_1") })
    player_stats_reactive_away_ligue_1 <- reactive({ get_player_stats(FALSE,"ligue_1") })
    player_stats_2_reactive_home_ligue_1 <- reactive({ get_specific_player_stats(TRUE,"ligue_1") })
    player_stats_2_reactive_away_ligue_1 <- reactive({ get_specific_player_stats(FALSE,"ligue_1") })
    
    # Define a function to render DataTable
    render_data_table <- function(player_stats_DT, round = FALSE) {
      if(round) player_stats_DT[,-c(1:3)] <- round(player_stats_DT[,-c(1:3)], 2)
      datatable(player_stats_DT, rownames = FALSE, filter = "none", selection = "single")
    }
    
    # Shiny render functions =====================================================
    
    
    
    # PL =========================================================================
    
    player_match_DT <- function(mat) {
      
     df <- datatable(mat, escape = c(1,4), rownames = FALSE) %>% 
            formatStyle(c("Gh","Ga"),fontWeight = "bolder", fontSize = "20px", textAlign = "center" ) %>% 
            formatStyle("A_team",borderRight = "0.5rem solid" ) %>% 
            formatStyle(14, backgroundColor = "#fbffa1",borderLeft = "solid") %>% 
            formatStyle(15, backgroundColor = "#F47174",borderRight = "solid") %>% 
            formatStyle("Date", backgroundColor = "#83ca89", borderRight = "0.5rem solid") %>% 
            formatStyle(c(1,5:15),fontWeight = "bold")
     
     return(df)
      
    }
    
    output$pl_home_player <- renderDataTable({
      render_data_table(player_stats_reactive_home_pl(),round = TRUE) 
    }, server = TRUE)
    
    output$pl_home_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_home_pl()
      player_match_DT(out)
    }, server = TRUE)
    
    output$pl_home_player_odds <- render_gt({
      mat <- player_stats_reactive_home_pl()
      mat <- mat[input$pl_home_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$pl_away_player <- renderDataTable({
      render_data_table(player_stats_reactive_away_pl(),round = TRUE)
    }, server = TRUE)
    
    output$pl_away_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_away_pl()
      player_match_DT(out)
    }, server = TRUE)
    
    output$pl_away_player_odds <- render_gt({
      mat <- player_stats_reactive_away_pl()
      mat <- mat[input$pl_away_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    # SERIE A ====================================================================
    output$serie_a_home_player <- renderDataTable({
      render_data_table(player_stats_reactive_home_serie_a(),round = TRUE) 
    }, server = TRUE)
    
    output$serie_a_home_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_home_serie_a()
      player_match_DT(out)
    }, server = TRUE)
    
    output$serie_a_home_player_odds <- render_gt({
      mat <- player_stats_reactive_home_serie_a()
      mat <- mat[input$serie_a_home_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$serie_a_away_player <- renderDataTable({
      render_data_table(player_stats_reactive_away_serie_a(),round = TRUE)
    }, server = TRUE)
    
    output$serie_a_away_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_away_serie_a()
      player_match_DT(out)
    }, server = TRUE)
    
    output$serie_a_away_player_odds <- render_gt({
      mat <- player_stats_reactive_away_serie_a()
      mat <- mat[input$serie_a_away_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    # La liga =========================================================================
    output$la_liga_home_player <- renderDataTable({
      render_data_table(player_stats_reactive_home_la_liga(),round = TRUE) 
    }, server = TRUE)
    
    output$la_liga_home_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_home_la_liga()
      player_match_DT(out)
    }, server = TRUE)
    
    output$la_liga_home_player_odds <- render_gt({
      mat <- player_stats_reactive_home_la_liga()
      mat <- mat[input$la_liga_home_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$la_liga_away_player <- renderDataTable({
      render_data_table(player_stats_reactive_away_la_liga(),round = TRUE)
    }, server = TRUE)
    
    output$la_liga_away_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_away_la_liga()
      player_match_DT(out)
    }, server = TRUE)
    
    output$la_liga_away_player_odds <- render_gt({
      mat <- player_stats_reactive_away_la_liga()
      mat <- mat[input$la_liga_away_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    
    # BUNDESLIGA =================================================================
    output$bundesliga_home_player <- renderDataTable({
      render_data_table(player_stats_reactive_home_bundesliga(),round = TRUE) 
    }, server = TRUE)
    
    output$bundesliga_home_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_home_bundesliga()
      player_match_DT(out)
    }, server = TRUE)
    
    output$bundesliga_home_player_odds <- render_gt({
      mat <- player_stats_reactive_home_bundesliga()
      mat <- mat[input$bundesliga_home_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$bundesliga_away_player <- renderDataTable({
      render_data_table(player_stats_reactive_away_bundesliga(),round = TRUE)
    }, server = TRUE)
    
    output$bundesliga_away_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_away_bundesliga()
      player_match_DT(out)
    }, server = TRUE)
    
    output$bundesliga_away_player_odds <- render_gt({
      mat <- player_stats_reactive_away_bundesliga()
      mat <- mat[input$bundesliga_away_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    # LIGUE 1 ====================================================================
    output$ligue_1_home_player <- renderDataTable({
      render_data_table(player_stats_reactive_home_ligue_1(),round = TRUE) 
    }, server = TRUE)
    
    output$ligue_1_home_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_home_ligue_1()
      player_match_DT(out)
    }, server = TRUE)
    
    output$ligue_1_home_player_odds <- render_gt({
      mat <- player_stats_reactive_home_ligue_1()
      mat <- mat[input$ligue_1_home_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$ligue_1_away_player <- renderDataTable({
      render_data_table(player_stats_reactive_away_ligue_1(),round = TRUE)
    }, server = TRUE)
    
    output$ligue_1_away_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_away_ligue_1()
      player_match_DT(out)
    }, server = TRUE)
    
    output$ligue_1_away_player_odds <- render_gt({
      mat <- player_stats_reactive_away_ligue_1()
      mat <- mat[input$ligue_1_away_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    
    # ==========================================================================
    # ============================= Referee PLOTLY =============================
    
    output$referee_plot <- renderPlotly({
      
      get_plotly_ref(referee_stats)
      
    })
    
    # ============================= Referee PLOTLY =============================
    # ==========================================================================
    
    
    
    # ==========================================================================
    # ============================= KAMBI ODDS =================================
    observeEvent(input$pl_show_kambi, {
     
      teams <- rename_teams(team_stats_pl()[[1]],from = "fbref", to = "kambi_2")
      kambi_odds_on_pl(run_selenium(teams[1], teams[2], "PL"))
      
    })
    
    observeEvent(input$serie_a_show_kambi, {
      
      teams <- rename_teams(team_stats_serie_a()[[1]],from = "fbref", to = "kambi_2")
      kambi_odds_on_serie_a(run_selenium(teams[1], teams[2], "Serie A"))
      
    })
    
    observeEvent(input$la_liga_show_kambi, {
      
      teams <- rename_teams(team_stats_la_liga()[[1]],from = "fbref", to = "kambi_2")
      kambi_odds_on_la_liga(run_selenium(teams[1], teams[2], "La Liga"))
      
    })
    
    observeEvent(input$bundesliga_show_kambi, {
      
      teams <- rename_teams(team_stats_bundesliga()[[1]],from = "fbref", to = "kambi_2")
      kambi_odds_on_bundesliga(run_selenium(teams[1], teams[2], "Bundesliga"))
      
    })
    
    observeEvent(input$ligue_1_show_kambi, {
      
      teams <- rename_teams(team_stats_ligue_1()[[1]],from = "fbref", to = "kambi_2")
      kambi_odds_on_ligue_1(run_selenium(teams[1], teams[2], "Ligue 1"))
      
    })
    
    # ============================= KAMBI ODDS =================================
    # ==========================================================================

    
    # ==========================================================================
    # ==========================================================================
    # ============================= EV SECTION =================================
    # ==========================================================================
    # ==========================================================================
    
    
    # ==========================================================================
    # ============================= OUTPUT EV BETS =============================

    
    output$DT_EV <-  renderDataTable({
      if (!is.null(show_EV())) {
        
        data <- show_EV()
        data <- data[data$League %in% input$EV_leagues &
                      data$Market %in% input$EV_markets,]
        
        
        data <- data[data$EV > as.numeric(str_sub(input$pick_EV,0,-4)),]
        
        render_data_table(
          data
          )
      } else {
        NULL
      }
    })
    

    # ============================= OUTPUT EV BETS =============================
    # ==========================================================================
    
    # ==========================================================================
    # ============================= BTN SHOW EV ================================
    
    observeEvent(input$pl_show_EV_bets, {
      
      teams <- rename_teams(team_stats_pl()[[1]],from = "fbref", to = "kambi_2")
      if(is.null(show_EV)){
        show_EV(run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "PL"))
      } else {
        show_EV(rbind(show_EV(),run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "PL")))
      }
    })

    observeEvent(input$serie_a_show_EV_bets, {

      teams <- rename_teams(team_stats_serie_a()[[1]],from = "fbref", to = "kambi_2")
      if(is.null(show_EV)){
        show_EV(run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "Serie A"))
      } else {
        show_EV(rbind(show_EV(),run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "Serie A")))
      }

    })
    
    
    

    observeEvent(input$la_liga_show_EV_bets, {

      teams <- rename_teams(team_stats_la_liga()[[1]],from = "fbref", to = "kambi_2")
      if(is.null(show_EV)){
        show_EV(run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "La Liga"))
      } else {
        show_EV(rbind(show_EV(),run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "La Liga")))
      }

    })

    observeEvent(input$bundesliga_show_EV_bets, {

      teams <- rename_teams(team_stats_bundesliga()[[1]],from = "fbref", to = "kambi_2")
      if(is.null(show_EV)){
        show_EV(run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "Bundesliga"))
      } else {
        show_EV(rbind(show_EV(),run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "Bundesliga")))
      }

    })

    observeEvent(input$ligue_1_show_EV_bets, {

      teams <- rename_teams(team_stats_ligue_1()[[1]],from = "fbref", to = "kambi_2")
      if(is.null(show_EV)){
        show_EV(run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "Ligue 1"))
      } else {
        show_EV(rbind(show_EV(),run_EV_bets(all_team_stats, models, sot_ratio, id_teams_2023, "Ligue 1")))
      }
      

    })
    

    # ============================= BTN SHOW EV ================================
    # ==========================================================================
    
    
    
    # DISCONNECT FROM THE CON DATABASE
    onStop(function() {
      dbDisconnect(con)
    })

    
    
}
# source("app.R")
# shinyApp(ui = ui, server = server)

