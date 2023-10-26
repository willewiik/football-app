
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


#setwd("C:\\Users\\willi\\OneDrive\\Skrivbord\\football_app")



ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("Football Leagues App signed by willewiik"),
  
  navbarPage("Home",
    tabPanel("Premier League",
      mainPanel(
        tabsetPanel(
          tabPanel("Matchup",
           selectInput("pl_select", "Select Match", choices = NULL),
           gt_output("pl_teams_stats"),
           gt_output("pl_teams_odds")),
          
          tabPanel("League Table",
           DTOutput("pl_table")))
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
                    DTOutput("serie_a_table")))
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
                          DTOutput("la_liga_table")))
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
                          DTOutput("bundesliga_table")))
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
                          DTOutput("ligue_1_table")))
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
 
  # ============================================================================
  # ============================================================================
  

    # Filter the desired columns for Premier League
    pl_league <- cbind(1:20,league_table[league_table$Competition_Name == "Premier League",-1 ])

    output$pl_table <- renderDT({
      datatable(pl_league,
                 options = list(
                    ordering = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,  # Allow automatic width
                    initComplete = JS('function(settings, json) {$(this.api().table().node()).css("width", "50%");}'),
                    searching = FALSE
                     ),
                  selection = 'none',
                  class = 'cell-border stripe',
                  escape = FALSE,
                  rownames = FALSE,
                  filter = "none",
                  width = 50
               )
    })
    
    # ==========================================================================
    # ============================ 45 SEC WAIT =================================
    # ==========================================================================
    
    # Cards, Offsides, Fouls
    misc <- fb_season_team_stats(c("ENG","ITA","ESP","GER","FRA"),
                                    "M",2024,"1st","misc", time_pause = 3)
    misc <- misc %>%  mutate("Cards" = ((CrdY + (CrdR*2)) / Mins_Per_90),
                                   "Offside" = (Off / Mins_Per_90),
                                   "Fouls" = (Fls / Mins_Per_90))
    
    # Shots, sot
    shots <- fb_season_team_stats(c("ENG","ITA","ESP","GER","FRA"),
                                     "M",2024,"1st","shooting", time_pause = 3)
    shots <- shots %>%  mutate("Shots" = ((Sh_Standard) / Mins_Per_90),
                                     "ShotsOnTarget" = (SoT_Standard / Mins_Per_90))
    
    
    # Tackles
    tackles <- fb_season_team_stats(c("ENG","ITA","ESP","GER","FRA"),
                                       "M",2024,"1st","defense", time_pause = 3)
    tackles <- tackles %>%  mutate("Tackles" = ((Tkl_Tackles) / Mins_Per_90))
                                    
    # ==========================================================================
    # ============================ 45 SEC WAIT =================================
    # ==========================================================================
                                  
    
    # Empty matrix for teams stats
    mat <- matrix(NA, 6, 6) %>% 
      as.data.frame() %>% 
      `colnames<-`(c("Stats","HomeTeam_For", "HomeTeam_Against",
                     "AwayTeam_For", "AwayTeam_Against", "Total"))
    
    mat$Stats <- c("Cards", "Offside", "Fouls", "Shots", "ShotsOnTarget", "Tackles")
    
  
    team_stats <- function(league) {
      teams <- c()
      teams_ex <- strsplit(input[[paste0(league, "_select")]], " vs ")[[1]]
      print(paste0(league, "_select"))
    
  
      # Extract home team and away team, removing the date part for the home team
      teams[1] <- gsub("^[0-9-]+\\s", "", teams_ex[1])
      teams[2] <- teams_ex[2]
      
      # Get the data
      stats_team_mat <- get_team_stats(teams, misc, shots, tackles, mat)
      
      return(list(teams = teams, stats = stats_team_mat))
    }
    
    team_stats_pl <- reactive({ team_stats("pl") })
    team_stats_serie_a <- reactive({ team_stats("serie_a") })
    team_stats_la_liga <- reactive({ team_stats("la_liga") })
    team_stats_bundesliga <- reactive({ team_stats("bundesliga") })
    team_stats_ligue_1 <- reactive({ team_stats("ligue_1") })
    
    

    
    
    generate_team_stats_output <- function(league, odds = FALSE) {
      data <- switch(league,
                     "pl" = team_stats_pl(),
                     "serie_a" = team_stats_serie_a(),
                     "la_liga" = team_stats_la_liga(),
                     "bundesliga" = team_stats_bundesliga(),
                     "ligue_1" = team_stats_ligue_1()
      )
      teams <- data$teams
      stats_team_mat <- data$stats
      
      if (odds) {
        rownames(stats_team_mat) <- stats_team_mat[, 1]
        mat_m <- stats_team_mat[, -1]
        df_team_odds <- get_team_odds(mat_m, num_stats = 6)
        return(get_gt_odds_team(df_team_odds, teams[1], teams[2]))
      } else {
        stats_team_mat[, -1] <- round(stats_team_mat[, -1], 1)
        return(get_gt_teams(stats_team_mat, teams[1], teams[2]))
      }
    }
    
    
    
    
    output$pl_teams_stats <- render_gt({
      tab_options(generate_team_stats_output("pl"),
                  table.width = "500px",
                  table.font.size = 14,
                  column_labels.font.weight = "bold")
    })
    
    output$serie_a_teams_stats <- render_gt({
      tab_options(generate_team_stats_output("serie_a"),
                  table.width = "500px",
                  table.font.size = 14,
                  column_labels.font.weight = "bold")
    })
    
    output$la_liga_teams_stats <- render_gt({
      tab_options(generate_team_stats_output("la_liga"),
                  table.width = "500px",
                  table.font.size = 14,
                  column_labels.font.weight = "bold")
    })
    
    output$bundesliga_teams_stats <- render_gt({
      tab_options(generate_team_stats_output("bundesliga"),
                  table.width = "500px",
                  table.font.size = 14,
                  column_labels.font.weight = "bold")
    })
    
    output$ligue_1_teams_stats <- render_gt({
      tab_options(generate_team_stats_output("ligue_1"),
                  table.width = "500px",
                  table.font.size = 14,
                  column_labels.font.weight = "bold")
    })
    
    
    
    # ============================= ODDS GT ====================================
    output$pl_teams_odds <- render_gt({
      tab_options(generate_team_stats_output("pl", odds = TRUE), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$serie_a_teams_odds <- render_gt({
      tab_options(generate_team_stats_output("serie_a", odds = TRUE), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$la_liga_teams_odds <- render_gt({
      tab_options(generate_team_stats_output("la_liga", odds = TRUE), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$bundesliga_teams_odds <- render_gt({
      tab_options(generate_team_stats_output("bundesliga", odds = TRUE), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    output$ligue_1_teams_odds <- render_gt({
      tab_options(generate_team_stats_output("ligue_1", odds = TRUE), table.width = "500px", table.font.size = 14, column_labels.font.weight = "bold")
    })
    
    # ============================= ODDS GT ====================================
    
   

}

shinyApp(ui = ui, server = server)




#hej <- fb_season_team_stats(c("ENG","ITA","ESP","GER","FRA"),"M",2024,"1st","misc", time_pause = 3)





