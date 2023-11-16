
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




html_content <- readLines("match.html", warn = FALSE)

ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("Football Leagues App signed by willewiik"),
                
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
                                                          includeHTML("match.html")
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
                                                 gt_output("pl_teams_odds"),
                                                ),
                                               
                                        
                                        tabPanel("League Table",
                                                 DTOutput("pl_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("pl_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("pl_home_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("pl_home_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
                                                 DTOutput("pl_home_player"),
                                                 DTOutput("pl_home_player_per_match"),
                                                 gt_output("pl_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("pl_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("pl_away_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("pl_away_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
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
                                                 DTOutput("serie_a_teams_stats"),
                                                 gt_output("serie_a_teams_odds")),
                                        
                                        tabPanel("League Table",
                                                 DTOutput("serie_a_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("serie_a_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("serie_a_home_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("serie_a_home_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
                                                 DTOutput("serie_a_home_player"),
                                                 DTOutput("serie_a_home_player_per_match"),
                                                 gt_output("serie_a_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("serie_a_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("serie_a_away_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("serie_a_away_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
                                                 DTOutput("serie_a_away_player"),
                                                 DTOutput("serie_a_away_player_per_match"),
                                                 gt_output("serie_a_away_player_odds")),
                                      )
                                    )
                           ),
                           tabPanel("La Liga",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Matchup",
                                                 selectInput("la_liga_select", "Select Match", choices = NULL),
                                                 DTOutput("la_liga_teams_stats"),
                                                 gt_output("la_liga_teams_odds")),
                                        
                                        tabPanel("League Table",
                                                 DTOutput("la_liga_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("la_liga_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("la_liga_home_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("la_liga_home_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
                                                 DTOutput("la_liga_home_player"),
                                                 DTOutput("la_liga_home_player_per_match"),
                                                 gt_output("la_liga_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("la_liga_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("la_liga_away_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("la_liga_away_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
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
                                                 DTOutput("bundesliga_teams_stats"),
                                                 gt_output("bundesliga_teams_odds")),
                                        
                                        tabPanel("League Table",
                                                 DTOutput("bundesliga_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("bundesliga_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("bundesliga_home_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("bundesliga_home_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
                                                 DTOutput("bundesliga_home_player"),
                                                 DTOutput("bundesliga_home_player_per_match"),
                                                 gt_output("bundesliga_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("bundesliga_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("bundesliga_away_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("bundesliga_away_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
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
                                                 DTOutput("ligue_1_teams_stats"),
                                                 gt_output("ligue_1_teams_odds")),
                                        
                                        tabPanel("League Table",
                                                 DTOutput("ligue_1_table")),
                                        
                                        tabPanel("Home team player stats",
                                                 numericInput("ligue_1_home_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("ligue_1_home_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("ligue_1_home_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
                                                 DTOutput("ligue_1_home_player"),
                                                 DTOutput("ligue_1_home_player_per_match"),
                                                 gt_output("ligue_1_home_player_odds")),
                                        
                                        tabPanel("Away team player stats",
                                                 numericInput("ligue_1_away_player_minutes",
                                                              label = "Minimal minutes played",
                                                              value = 200),
                                                 checkboxGroupInput("ligue_1_away_player_season",
                                                                    label = "Choose which season",
                                                                    choices = c("2023/2024","2022/2023"),
                                                                    selected = "2023/2024"
                                                 ),
                                                 checkboxInput("ligue_1_away_player_pos_ALL",
                                                               label = "Ungroup on position",
                                                               value = FALSE),
                                                 DTOutput("ligue_1_away_player"),
                                                 DTOutput("ligue_1_away_player_per_match"),
                                                 gt_output("ligue_1_away_player_odds")),
                                      )
                                    )
                           )
                )
                
)

server <- function(input, output, session) {
  
  
  source("worldfootball.R", local = TRUE) # Functions
  source("gt_tabels.R", local = TRUE) # GT functions
  
  res <- load_match_results(country = c("ENG","ITA","ESP","GER","FRA"),
                            gender ="M",
                            season_end_year = 2024,
                            tier = "1st")
  count <- 0
  
  
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
      print(paste("COUNTER get_teams_id:",count))
      return(list(teams,teams_id,date))
    }
    
    team_stats_pl <- reactive({ get_teams_id("pl") })
    team_stats_serie_a <- reactive({ get_teams_id("serie_a") })
    team_stats_la_liga <- reactive({ get_teams_id("la_liga") })
    team_stats_bundesliga <- reactive({ get_teams_id("bundesliga") })
    team_stats_ligue_1 <- reactive({ get_teams_id("ligue_1") })
    
    

    get_team_stats_output <- reactive({
      this_teams_id <- team_stats_pl()
      if(is.null(this_teams_id)) return(NULL)
      this_teams_stats <- get_team_stats(all_team_stats, mat, this_teams_id[[2]],
                                         input$pl_checkbox_season_home,
                                         input$pl_radio_location_home,
                                         input$pl_checkbox_season_away,
                                         input$pl_radio_location_away) 
      
      return(this_teams_stats)
    })
    
    get_one_team_matches_output <- function(home_away = 1, league){
      this_teams_id <- team_stats_pl()
      
      ifelse(home_away==1,{input1<-input$pl_checkbox_season_home},
             {input1<-input$pl_checkbox_season_away})
      
      ifelse(home_away==1,{input2<-input$pl_radio_location_home},
             {input2<-input$pl_radio_location_away})
      
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
    
    
    
    
    
    
    # generate_team_stats_output <- function(league, odds = FALSE) {
    #   data <- switch(league,
    #                  "pl" = team_stats_pl(),
    #                  "serie_a" = team_stats_serie_a(),
    #                  "la_liga" = team_stats_la_liga(),
    #                  "bundesliga" = team_stats_bundesliga(),
    #                  "ligue_1" = team_stats_ligue_1()
    #   )
    #   teams <- data$teams
    #   stats_team_mat <- data$stats
    #   
    #   if (odds) {
    #     rownames(stats_team_mat) <- stats_team_mat[, 1]
    #     mat_m <- stats_team_mat[, -1]
    #     df_team_odds <- get_team_odds(mat_m, num_stats = 6)
    #     return(get_gt_odds_team(df_team_odds, teams[1], teams[2]))
    #   } else {
    #     stats_team_mat[, -1] <- round(stats_team_mat[, -1], 1)
    #     return(get_gt_teams(stats_team_mat, teams[1], teams[2]))
    #   }
    # }
  
    # ==========================================================================
    # ========================== Team Stats GT =================================
    
    output$pl_teams_stats <- render_gt({
      
      this_teams_id <- team_stats_pl()
      if(!is.null(this_teams_id)){
      mat_output <- get_team_stats_output()
      
      tab_options(get_gt_teams(mat_output,this_teams_id[[1]][1],this_teams_id[[1]][2]),
                  table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
      }
    })
    
    # output$serie_a_teams_stats <- renderDataTable({
    #   this_teams_id <- team_stats_serie_a()
    #   this_teams_stats <- get_team_stats(all_team_stats, mat, this_teams_id,"2023/2024", "Both")
    #   datatable(this_teams_stats, rownames = FALSE, filter = "none", selection = "single") %>% 
    #     formatRound(columns = 2:6, digits = 2)
    # })
    # 
    # output$la_liga_teams_stats <- renderDataTable({
    #   this_teams_id <- team_stats_la_liga()
    #   this_teams_stats <- get_team_stats(all_team_stats, mat, this_teams_id,"2023/2024", "Both")
    #   datatable(this_teams_stats, rownames = FALSE, filter = "none", selection = "single") %>% 
    #     formatRound(columns = 2:6, digits = 2)
    # })
    # 
    # output$bundesliga_teams_stats <- renderDataTable({
    #   this_teams_id <- team_stats_bundesliga()
    #   this_teams_stats <- get_team_stats(all_team_stats, mat, this_teams_id,"2023/2024", "Both")
    #   datatable(this_teams_stats, rownames = FALSE, filter = "none", selection = "single") %>% 
    #     formatRound(columns = 2:6, digits = 2)
    # })
    # 
    # output$ligue_1_teams_stats <- renderDataTable({
    #   this_teams_id <- team_stats_ligue_1()
    #   this_teams_stats <- get_team_stats(all_team_stats, mat, this_teams_id,"2023/2024", "Both")
    #   datatable(this_teams_stats, rownames = FALSE, filter = "none", selection = "single") %>% 
    #     formatRound(columns = 2:6, digits = 2)
    # })
    
    # ========================== Team Stats GT =================================
    # ==========================================================================
    
    
    # ====================== HOME TEAM matches DT ==============================
    # ==========================================================================
    
    
    output$pl_home_team_matches <- renderDataTable({

      home_team_matches_output <- home_team_matches_pl()
      yellow <- 21
      red <- 22
      if(input$pl_radio_home_team == "For") {
        home_team_matches_output <- home_team_matches_output[,-c(24:36)]
      } else if(input$pl_radio_home_team == "Against") {
        home_team_matches_output <- home_team_matches_output[,-c(11:23)]
      } else {
        home_team_matches_output <-  cbind(home_team_matches_output[,c(1:10)],
                                           home_team_matches_output[,c(11:18,20:22)] +
                                           home_team_matches_output[,c(24:31,33:35)])
        yellow <- 20
        red <- 21
      }
    
      datatable(home_team_matches_output,escape = c(3,4),rownames = FALSE) %>% 
        formatStyle(c("Gh","Ga"),fontWeight = "bolder", fontSize = "20px", textAlign = "center" ) %>% 
        formatStyle("1", backgroundColor = "#83ca89",borderLeft = "0.5rem solid") %>% 
        formatStyle(c("X","2"), backgroundColor = "#83ca89") %>% 
        formatStyle(c("o2.5","u2.5"), backgroundColor = "#a6d9aa") %>% 
        formatStyle(yellow, backgroundColor = "#fbffa1",borderLeft = "solid") %>% 
        formatStyle(11,borderLeft = "solid") %>% 
        formatStyle(red, backgroundColor = "#F47174",borderRight = "solid") %>% 
        formatStyle("Date", backgroundColor = "#83ca89", borderRight = "0.5rem solid") %>% 
        formatStyle(c(1,6:23),fontWeight = "bold")
        
    })
    
    
    
  
    
    
    
    # ==========================================================================
    # ====================== HOME TEAM matches DT ==============================
    
    
    # ==========================================================================
    # ====================== AWAY TEAM matches DT ==============================
    
    
    output$pl_away_team_matches <- renderDataTable({
      
      away_team_matches_output <- away_team_matches_pl()
      yellow <- 21
      red <- 22
      if(input$pl_radio_away_team == "For") {
        away_team_matches_output <- away_team_matches_output[,-c(24:36)]
      } else if(input$pl_radio_away_team == "Against") {
        away_team_matches_output <- away_team_matches_output[,-c(11:23)]
      } else {
        away_team_matches_output <-  cbind(away_team_matches_output[,c(1:10)],
                                           away_team_matches_output[,c(11:18,20:22)] +
                                           away_team_matches_output[,c(24:31,33:35)])
        yellow <- 20
        red <- 21
      }
      
      datatable(away_team_matches_output,escape = c(3,4),rownames = FALSE) %>% 
        formatStyle(c("Gh","Ga"),fontWeight = "bolder", fontSize = "20px", textAlign = "center" ) %>% 
        formatStyle("1", backgroundColor = "#83ca89",borderLeft = "0.5rem solid") %>% 
        formatStyle(c("X","2"), backgroundColor = "#83ca89") %>% 
        formatStyle(c("o2.5","u2.5"), backgroundColor = "#a6d9aa") %>% 
        formatStyle(yellow, backgroundColor = "#fbffa1",borderLeft = "solid") %>% 
        formatStyle(11,borderLeft = "solid") %>% 
        formatStyle(red, backgroundColor = "#F47174",borderRight = "solid") %>% 
        formatStyle("Date", backgroundColor = "#83ca89", borderRight = "0.5rem solid") %>% 
        formatStyle(c(1,6:23),fontWeight = "bold")
    })
    
    
    
    
    
    
  
    # ====================== AWAY TEAM matches DT ==============================
    # ==========================================================================
    
    
    # ==========================================================================
    # ============================ LOGOS =======================================
    
    
    
    get_team_logo <- function(league) {
      
      team <- team_stats_pl()
      if(is.null(team)) return(NULL)
      
        url_home <- rename_teams(team[[1]][1], from = "fbref",to = "logo_local")
        url_away <- rename_teams(team[[1]][2], from = "fbref",to = "logo_local")
        
        list_home <- list(src = url_home,
                          alt = team[[1]][1])
        
        list_away <- list(src = url_away,
                          alt = team[[1]][2])
        
        
        return(list(list_home,list_away))
      
    }
    
    pl_logo <- reactive({ get_team_logo("pl") })
    
    output$hometeam_logo <- renderImage({
      
      pl_logo()[[1]]
    },deleteFile=FALSE)
    
    
    output$awayteam_logo <- renderImage({
      
      pl_logo()[[2]]
    },deleteFile=FALSE)
    
    
    output$hometeam_name <- renderText({
      hometeam <- team_stats_pl()
      paste(hometeam[[1]][1])
    })
    
    output$awayteam_name <- renderText({
      awayteam <- team_stats_pl()
      paste(awayteam[[1]][2])
    })
    
    
    output$date <- renderText({
      this_date <- team_stats_pl()
      paste(this_date[[3]][1])
    })
    
    
    
    
    get_odds <- function(league) {
      
      teams <- team_stats_pl()
      if(is.null(teams)) return(NULL)
      odds <- get_odds_kambi("england/premier_league", 12579,teams[[1]])
      
      return(odds)
    }
    
    
    pl_odds <- reactive({ get_odds("pl") })
    
    
    output$odds1kambi <- renderText({
      odds <- pl_odds()
      paste(odds[1])
    })
    
    
    output$oddsXkambi <- renderText({
      odds <- pl_odds()
      paste(odds[2])
    })
    
    
    output$odds2kambi <- renderText({
      odds <- pl_odds()
      paste(odds[3])
    })
    
    
    # ============================ LOGOS  ======================================
    # ==========================================================================
    
    
  
    
    
    # ==========================================================================
    # ============================= ODDS GT ====================================
    
    # output$pl_teams_odds <- render_gt({
    #   tab_options(generate_team_stats_output("pl", odds = TRUE), table.width = "500px",
    #               table.font.size = 14, column_labels.font.weight = "bold")
    # })
    # 
    # output$serie_a_teams_odds <- render_gt({
    #   tab_options(generate_team_stats_output("serie_a", odds = TRUE), table.width = "500px",
    #               table.font.size = 14, column_labels.font.weight = "bold")
    # })
    # 
    # output$la_liga_teams_odds <- render_gt({
    #   tab_options(generate_team_stats_output("la_liga", odds = TRUE), table.width = "500px",
    #               table.font.size = 14, column_labels.font.weight = "bold")
    # })
    # 
    # output$bundesliga_teams_odds <- render_gt({
    #   tab_options(generate_team_stats_output("bundesliga", odds = TRUE), table.width = "500px",
    #               table.font.size = 14, column_labels.font.weight = "bold")
    # })
    # 
    # output$ligue_1_teams_odds <- render_gt({
    #   tab_options(generate_team_stats_output("ligue_1", odds = TRUE), table.width = "500px",
    #               table.font.size = 14, column_labels.font.weight = "bold")
    # })
    
    # ============================= ODDS GT ====================================
    # ==========================================================================
    
    get_player_stats <- function(home = TRUE, league) {
      teams_ex <- strsplit(input[[paste0(league, "_select")]], " vs ")[[1]]
      team <- ifelse(home, gsub("^[0-9-]+\\s", "", teams_ex[1]), teams_ex[2])
      
      if(home){
        season <- input[[paste0(league,"_home_player_season")]]
      } else {
        season <- input[[paste0(league,"_away_player_season")]]
      }
      
      min_minutes <- ifelse(home, input[[paste0(league,"_home_player_minutes")]],
                            input[[paste0(league,"_away_player_minutes")]])
      
      player_pos_ALL <- ifelse(home, input[[paste0(league,"_home_player_pos_ALL")]],
                               input[[paste0(league,"_away_player_pos_ALL")]])
      
      team_id <- id_teams_2023[id_teams_2023$hteam == team, 2]
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
    output$pl_home_player <- renderDataTable({
      render_data_table(player_stats_reactive_home_pl(),round = TRUE) 
    }, server = TRUE)
    
    output$pl_home_player_per_match  <- renderDataTable({
      out <- player_stats_2_reactive_home_pl()
      datatable(out, escape = c(1,4), rownames = FALSE) %>% 
        formatStyle(c("Gh","Ga"),fontWeight = "bolder", fontSize = "20px", textAlign = "center" ) %>% 
        formatStyle("A_team",borderRight = "0.5rem solid" ) %>% 
        formatStyle(14, backgroundColor = "#fbffa1",borderLeft = "solid") %>% 
        formatStyle(15, backgroundColor = "#F47174",borderRight = "solid") %>% 
        formatStyle("Date", backgroundColor = "#83ca89", borderRight = "0.5rem solid") %>% 
        formatStyle(c(1,5:15),fontWeight = "bold")
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
      datatable(player_stats_2_reactive_away_pl())
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
      datatable(player_stats_2_reactive_home_serie_a(),escape())
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
      datatable(player_stats_2_reactive_away_serie_a())
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
      datatable(player_stats_2_reactive_home_la_liga())
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
      datatable(player_stats_2_reactive_away_la_liga())
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
      datatable(player_stats_2_reactive_home_bundesliga())
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
      datatable(player_stats_2_reactive_away_bundesliga())
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
      datatable(player_stats_2_reactive_home_ligue_1())
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
      datatable(player_stats_2_reactive_away_ligue_1())
    }, server = TRUE)
    
    output$ligue_1_away_player_odds <- render_gt({
      mat <- player_stats_reactive_away_ligue_1()
      mat <- mat[input$ligue_1_away_player_rows_selected,]
      tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    
    
    onStop(function() {
      dbDisconnect(con)
    })

}
# source("app.R")
# shinyApp(ui = ui, server = server)

