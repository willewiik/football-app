
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


setwd("C:\\Users\\willi\\OneDrive\\Skrivbord\\football_app")



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
         # DTOutput('tbl')),
          tabPanel("League Table",
           DTOutput("pl_table")))
      )
    ),
    tabPanel("Serie A",
             mainPanel(
               selectInput("serie_a_select", "Select Match", choices = NULL),
               DTOutput("serie_a_table"))
    ),
    tabPanel("La Liga",
             mainPanel(
               selectInput("la_liga_select", "Select Match", choices = NULL),
               DTOutput("la_liga_table"))
    ),
    tabPanel("Bundesliga",
             mainPanel(
               selectInput("bundesliga_select", "Select Match", choices = NULL),
               DTOutput("bundesliga_table"))
    ),
    tabPanel("Ligue 1",
             mainPanel(
               selectInput("ligue_1_select", "Select Match", choices = NULL),
               DTOutput("ligue_1_table"))
    )
  )
  
        # tabPanel("Serie A",
        #          selectInput("serie_a_select", "Select Serie A Matches", choices = NULL),
        #          DTOutput("serie_a_table")),
        # tabPanel("La Liga",
        #          selectInput("la_liga_select", "Select La Liga Matches", choices = NULL),
        #          DTOutput("la_liga_table")),
        # tabPanel("Bundesliga",
        #          selectInput("bundesliga_select", "Select Bundesliga Matches", choices = NULL),
        #          DTOutput("bundesliga_table")),
        # tabPanel("Ligue 1",
        #          selectInput("ligue_1_select", "Select Ligue 1 Matches", choices = NULL),
        #          DTOutput("ligue_1_table"))

)

server <- function(input, output, session) {
  
  
  source("worldfootball.R", local = TRUE)
  res <- load_match_results(country = c("ENG","ITA","ESP","GER","FRA"),
                            gender ="M",
                            season_end_year = 2024,
                            tier = "1st")
  
  
  league_table <- get_league_table(res)
  
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  
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
  
    
    columns <- c("Date", "Home", "HomeGoals", "Home_xG", "Away", "AwayGoals", "Away_xG")
  
  
  
    
    # Filter the desired columns for Premier League
    pl_league <- cbind(1:20,league_table[league_table$Competition_Name == "Premier League",-1 ])

    output$pl_table <- renderDT({
      datatable(pl_league,
                 options = list(
                    #dom = "t",
                    ordering = FALSE,
                    paging = FALSE,
                   # columnDefs = list(list(width = '50px', targets = "_all")),
                    autoWidth = FALSE,  # Allow automatic width
                    initComplete = JS('function(settings, json) {$(this.api().table().node()).css("width", "50%");}'),
                    searching = FALSE
                    #headerCallback = JS(headerCallbackRemoveHeaderFooter)
                     ),
                  selection = 'none',
                  callback = JS(
                    "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                      ),
                  class = 'cell-border stripe',
                  escape = FALSE,
                  rownames = FALSE,
                  filter = "none",
                  width = 50
               )
     
      
    })
    
   
    # Cards, Offsides, Fouls
    pl_misc <- fb_season_team_stats("ENG","M",2024,"1st","misc")
    pl_misc <- pl_misc %>%  mutate("Cards" = ((CrdY + (CrdR*2)) / Mins_Per_90),
                                   "Offside" = (Off / Mins_Per_90),
                                   "Fouls" = (Fls / Mins_Per_90))
    
    # Shots, sot
    pl_shots <- fb_season_team_stats("ENG","M",2024,"1st","shooting")
    pl_shots <- pl_shots %>%  mutate("Shots" = ((Sh_Standard) / Mins_Per_90),
                                     "ShotsOnTarget" = (SoT_Standard / Mins_Per_90))
    
    
    # Tackles
    pl_tackles <- fb_season_team_stats("ENG","M",2024,"1st","defense")
    pl_tackles <- pl_tackles %>%  mutate("Tackles" = ((Tkl_Tackles) / Mins_Per_90))
                                    
                                  
    
    mat <- matrix(NA, 6, 6) %>% 
      as.data.frame() %>% 
      `colnames<-`(c("Stats","HomeTeam_For", "HomeTeam_Against",
                     "AwayTeam_For", "AwayTeam_Against", "Total"))
    
    mat$Stats <- c("Cards", "Offside", "Fouls", "Shots", "ShotsOnTarget", "Tackles")
    
  
    
    output$pl_teams_stats <- render_gt({
      
      teams <- c()
      teams_ex <- strsplit(input$pl_select, " vs ")[[1]]
      
      # Extract home team and away team, removing the date part for the home team
      teams[1] <- gsub("^[0-9-]+\\s", "", teams_ex[1])
      teams[2] <- teams_ex[2]
  
      get_team_stats <- function(team, pl_misc, pl_shots, pl_tackles) {
        pl_misc %>%
          filter(Squad %in% c(team, str_c("vs ", team))) %>%
          select(Team_or_Opponent, Cards, Offside, Fouls) %>%
          full_join(
            pl_shots %>%
              filter(Squad %in% c(team, str_c("vs ", team))) %>%
              select(Team_or_Opponent, Shots, ShotsOnTarget),
            by = 'Team_or_Opponent'
          ) %>%
          full_join(
            pl_tackles %>%
              filter(Squad %in% c(team, str_c("vs ", team))) %>%
              select(Team_or_Opponent, Tackles),
            by = 'Team_or_Opponent'
          )
      }
      
      teams_stats <- lapply(teams, get_team_stats, pl_misc, pl_shots, pl_tackles) 
     
      mat$HomeTeam_For <- teams_stats[[1]][1,-1] %>% unlist()
      mat$HomeTeam_Against <- teams_stats[[1]][2,-1]  %>% unlist()
      mat$AwayTeam_For <- teams_stats[[2]][1,-1]  %>% unlist()
      mat$AwayTeam_Against <- teams_stats[[2]][2,-1]  %>% unlist()
      mat$Total <- (mat$HomeTeam_For + mat$AwayTeam_Against) / 2 + 
                   (mat$AwayTeam_For + mat$HomeTeam_Against) / 2
      
      mat[,-1] <- round(mat[,-1], 1)
      
     
      
      tab_options(get_gt_teams(mat,teams[1],teams[2]),
                  table.width = "500px",
                  table.font.size = 14,
                   column_labels.font.weight = "bold"
  
                  
      )
      
    
      
    })
    
    
    output$pl_teams_odds <- render_gt({
      
      teams <- c()
      teams_ex <- strsplit(input$pl_select, " vs ")[[1]]
      
      # Extract home team and away team, removing the date part for the home team
      teams[1] <- gsub("^[0-9-]+\\s", "", teams_ex[1])
      teams[2] <- teams_ex[2]
      
      get_team_stats <- function(team, pl_misc, pl_shots, pl_tackles) {
        pl_misc %>%
          filter(Squad %in% c(team, str_c("vs ", team))) %>%
          select(Team_or_Opponent, Cards, Offside, Fouls) %>%
          full_join(
            pl_shots %>%
              filter(Squad %in% c(team, str_c("vs ", team))) %>%
              select(Team_or_Opponent, Shots, ShotsOnTarget),
            by = 'Team_or_Opponent'
          ) %>%
          full_join(
            pl_tackles %>%
              filter(Squad %in% c(team, str_c("vs ", team))) %>%
              select(Team_or_Opponent, Tackles),
            by = 'Team_or_Opponent'
          )
      }
      
      teams_stats <- lapply(teams, get_team_stats, pl_misc, pl_shots, pl_tackles) 
      
      mat$HomeTeam_For <- teams_stats[[1]][1,-1] %>% unlist()
      mat$HomeTeam_Against <- teams_stats[[1]][2,-1]  %>% unlist()
      mat$AwayTeam_For <- teams_stats[[2]][1,-1]  %>% unlist()
      mat$AwayTeam_Against <- teams_stats[[2]][2,-1]  %>% unlist()
      mat$Total <- (mat$HomeTeam_For + mat$AwayTeam_Against) / 2 + 
        (mat$AwayTeam_For + mat$HomeTeam_Against) / 2
      
     # mat[,-1] <- round(mat[,-1], 1)
      rownames(mat) <- mat[,1]
      mat_m <- mat[,-1]
      num_stats <- 6
      resultat <- unlist(apply(cbind(((mat_m[,1]+ mat_m[,4])/ 2),
                                     ((mat_m[,2]+ mat_m[,3])/ 2),
                                     mat_m[,5]), 1:2, calculate_betting_odds))
      
      dff <- NULL
      for (i in seq(1, length(resultat), 9)) {
        matris <- matrix(resultat[i:(i+8)], nrow = 3)
        if (is.null(dff)) {
          dff <- as.data.frame(matris)
        } else {
          dff <- rbind(dff, as.data.frame(matris))
        }
      }
      df_delat_m <- NULL
      for (i in seq(1, nrow(dff), num_stats*3)) {
        subset_df <- dff[i:(i+(num_stats*3)-1), ]
        if (is.null(df_delat_m)) {
          df_delat_m <- subset_df
        } else {
          df_delat_m <- cbind(df_delat_m, subset_df)
        }
      }
      
      df_complete <- cbind(rep(rownames(mat_m), each = 3),df_delat_m)
      
      
      
      
      tab_options(get_gt_odds_team(df_complete,teams[1],teams[2]),
                  table.width = "500px",
                  table.font.size = 14,
                  column_labels.font.weight = "bold"
                  
                  
      )
      
      
    })
   

}

shinyApp(ui = ui, server = server)











