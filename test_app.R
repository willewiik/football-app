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






html_content <- readLines("match.html", warn = FALSE)

ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("Football Leagues App signed by willewiik"),
                includeCSS("style.css"),
                
                navbarPage("Home",
                           tabPanel("Premier League",
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Matchup",
                                                 selectInput("pl_select", "Select Match", choices = NULL),
                                                 gt_output("pl_teams_stats"),
                                                 gt_output("pl_teams_odds"),
                                                 includeHTML("match.html")),
                                        
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
                                                 textOutput("pl_home_player_name"),
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
                                                 gt_output("serie_a_teams_stats"),
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
                                                 gt_output("la_liga_teams_stats"),
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
                                                 gt_output("bundesliga_teams_stats"),
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
                                                 gt_output("ligue_1_teams_stats"),
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
  
  
  league_table <- get_league_table(res)
  
  
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
  
  # Anslutningsuppgifter till databasen
  dbHost <- "localhost"
  dbPort <- 3306  # Portnummer
  dbName <- "sql_workbench"
  dbUser <- "root"
  dbPassword <- Sys.getenv("Key1")
  
  # Skapa anslutningssträng
  con <- dbConnect(MySQL(), host = dbHost, port = dbPort, dbname = dbName, user = dbUser, password = dbPassword)
  id_teams_2023 <- readRDS("rds_files/id_teams_2023.rds")
  id_teams_2023$hteam <- rename_teams(id_teams_2023$hteam, from = "fbref_full", to = "fbref")
  
  
  
  # Define a function to retrieve player statistics based on home or away
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
    print(team_id)
    player_stats_DT <- sql_querys_team(con, team_id, season, min_minutes, !player_pos_ALL)
    print(player_stats_DT)
    return(player_stats_DT)
  }
  
  # Define a function to retrieve specific player's statistics
  get_specific_player_stats <- function(home = TRUE, league) {
    
    row <- ifelse(home,input[[paste0(league,"_home_player_rows_selected")]],
                  input[[paste0(league,"_away_player_rows_selected")]])
    season <- ifelse(home, input[[paste0(league, "_home_player_season")]],
                     input[[paste0(league, "_away_player_season")]])
    
    player_stats_DT <- get_player_stats(home, league)
    this_player <- player_stats_DT[row, 1]
    output$pl_home_player_name <- renderText({ unlist(this_player) }) 
    this_player_df <- sql_querys_player(con, this_player, season)
    print(this_player_df)
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
    render_data_table(player_stats_2_reactive_home_pl())
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
    render_data_table(player_stats_2_reactive_away_pl())
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
    render_data_table(player_stats_2_reactive_home_serie_a())
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
    render_data_table(player_stats_2_reactive_away_serie_a())
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
    render_data_table(player_stats_2_reactive_home_la_liga())
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
    render_data_table(player_stats_2_reactive_away_la_liga())
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
    render_data_table(player_stats_2_reactive_home_bundesliga())
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
    render_data_table(player_stats_2_reactive_away_bundesliga())
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
    render_data_table(player_stats_2_reactive_home_ligue_1())
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
    render_data_table(player_stats_2_reactive_away_ligue_1())
  }, server = TRUE)
  
  output$ligue_1_away_player_odds <- render_gt({
    mat <- player_stats_reactive_away_ligue_1()
    mat <- mat[input$ligue_1_away_player_rows_selected,]
    tab_options(get_gt_odds_players(mat), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
  })
  
  
  # ============================================================================
  # ============================================================================
  
  
  # Filter the desired columns for Premier League
  
  # ==========================================================================
  # ============================ 45 SEC WAIT =================================
  # ==========================================================================
  

  
  
  # ==========================================================================
  # ============================ 45 SEC WAIT =================================
  # ==========================================================================
  
  
 
  
  
  

  
  
  
  # ==========================================================================
  # ========================== Team Stats GT =================================
  
 
  
  # ========================== Team Stats GT =================================
  # ==========================================================================
  
  
  # ==========================================================================
  # ============================= ODDS GT ====================================
  
 
  
  # ============================= ODDS GT ====================================
  # ==========================================================================
  
  onStop(function() {
    dbDisconnect(con)
  })
  
  
  
}

shinyApp(ui = ui, server = server)





