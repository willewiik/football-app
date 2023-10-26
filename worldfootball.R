

library(shiny)
library(DT)
library(dplyr)
library(worldfootballR)
library(shinythemes)
library(tidyr)


filtered_data <- function(week, df) {
  res_filtered <- df[df$Wk == week, ]
  return(res_filtered)
}


get_league_table <- function(df) {
  
  
  league_table_res <- df %>% filter_at(vars(HomeGoals), all_vars(!is.na(.))) 
  
  filtered_data <- league_table_res %>%
    mutate(
      HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals == AwayGoals, 1, 0)),
      AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals == AwayGoals, 1, 3)),
    ) %>% 
    select(Competition_Name,Home, Away, HomeGoals, AwayGoals,HomePoints,AwayPoints)
  
  # Calculate total points for each team in each competition
  total_points <- filtered_data %>%
    group_by(Competition_Name, Team = Home) %>%
    summarize(TotalPoints = sum(HomePoints),
              GD = sum(HomeGoals - AwayGoals)) %>%
    bind_rows(filtered_data %>%
                group_by(Competition_Name, Team = Away) %>%
                summarize(TotalPoints = sum(AwayPoints),
                          GD = sum(AwayGoals - HomeGoals))) %>% 
    group_by(Competition_Name, Team) %>%
    summarize(TotalPoints = sum(TotalPoints),
              GD = sum(GD))
  
  
  # Arrange the data to form a league table
  league_table <- total_points %>%
    arrange(desc(Competition_Name), desc(TotalPoints),desc(GD))
  
  
  return(league_table)
}


calculate_betting_odds <- function(X) {
  
  odds_X_minus_1 <- 1 / (1 - ppois(floor(X-1), X))
  odds_X <- 1 / (1 - ppois(floor(X), X))
  odds_X_plus_1 <- 1 / (1 - ppois(floor(X+1), X))
  odds_max_X_minus_1 <- 1 / ppois(floor(X-1), X)
  odds_max_X <- 1 / ppois(floor(X), X)
  odds_max_X_plus_1 <- 1 / ppois(floor(X+1), X)
  
  X <- floor(X)
  
  mat <- matrix(NA,3,3)
  colnames(mat) <- c("Lina","Över","Under")
  mat[,1] <- c((X-0.5),X+0.5,X+1.5)
  mat[,2] <- round(c(odds_X_minus_1,odds_X,odds_X_plus_1),2)
  mat[,3] <- round(c(odds_max_X_minus_1,odds_max_X,odds_max_X_plus_1),2)
  
  odds_df <- as.data.frame(mat)
  
  return(odds_df)
}


get_team_stats <- function(teams, misc, shots, tackles, mat) {

  if(!length(teams) == 2) stop("Teams needs to be a vector with TWO teams!")
  
  inner_function <- function(team, misc, shots, tackles) {
    misc %>%
      filter(Squad %in% c(team, str_c("vs ", team))) %>%
      select(Team_or_Opponent, Cards, Offside, Fouls) %>%
      full_join(
        shots %>%
          filter(Squad %in% c(team, str_c("vs ", team))) %>%
          select(Team_or_Opponent, Shots, ShotsOnTarget),
        by = 'Team_or_Opponent'
      ) %>%
      full_join(
        tackles %>%
          filter(Squad %in% c(team, str_c("vs ", team))) %>%
          select(Team_or_Opponent, Tackles),
        by = 'Team_or_Opponent'
      )
  }
  
  teams_stats <- lapply(teams, inner_function, misc, shots, tackles) 
  
  mat$HomeTeam_For <- teams_stats[[1]][1,-1] %>% unlist()
  mat$HomeTeam_Against <- teams_stats[[1]][2,-1]  %>% unlist()
  mat$AwayTeam_For <- teams_stats[[2]][1,-1]  %>% unlist()
  mat$AwayTeam_Against <- teams_stats[[2]][2,-1]  %>% unlist()
  mat$Total <- (mat$HomeTeam_For + mat$AwayTeam_Against) / 2 + 
    (mat$AwayTeam_For + mat$HomeTeam_Against) / 2
  
  return(mat)
}



get_team_odds <- function(mat, num_stats) {
  
  resultat <- unlist(apply(cbind(((mat[,1]+ mat[,4])/ 2),
                                 ((mat[,2]+ mat[,3])/ 2),
                                   mat[,5]), 1:2, calculate_betting_odds))
  
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
  
  df_complete <- cbind(rep(rownames(mat), each = 3),df_delat_m)
  return(df_complete)
}






