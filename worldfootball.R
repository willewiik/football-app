

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



rename_pos <- function(pos) {
  
  res <- c()
  for(i in 1:length(pos)) {
    
    if(pos[i] == "AMC" || pos[i] == "AML" ||pos[i] == "AMR") {
      res[i] <- "AMC/AML/AMR"
    } else if(pos[i] == "DMC" || pos[i] == "DML" ||pos[i] == "DMR"){
      res[i] <- "DMC/DML/DMR"
    } else if(pos[i] == "FW" || pos[i] == "FWL" ||pos[i] == "FWR"){
      res[i] <- "FW/FWL/FWR"
    } else if(pos[i] == "ML" || pos[i] == "MR" ){
      res[i] <- "ML/MR"
    } else if(pos[i] == "DL" || pos[i] == "DR" ){
      res[i] <- "DL/DR"
    } else {
      res[i] <- pos[i]
    }
    
  }
  return(res)
}


rename_teams <- function(teams,from = "understat", to = "fbref") {
  
  # fbref full name
  fbref_name_full <- c("Manchester City","Arsenal","Newcastle United","Manchester United","Liverpool",
                       "Brighton & Hove Albion", "Tottenham Hotspur", "Aston Villa", "Brentford", "Fulham", "Chelsea",
                       "Crystal Palace", "Wolverhampton Wanderers", "Bournemouth","West Ham United","Nottingham Forest",
                       "Everton","Leeds United","Leicester City","Southampton",
                       "Burnley","Luton Town", "Sheffield United",
                       
                       "Sampdoria","Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                       "Salernitana","Hellas Verona","Juventus","Torino","Udinese","Internazionale","Sassuolo",   #SERIE A
                       "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                       "Frosinone", "Genoa", "Cagliari",
                       
                       "Osasuna","Celta Vigo","Valladolid", "Barcelona","Cádiz", "Valencia", "Almería",    
                       "Athletic Club", "Getafe", "Real Betis", "Espanyol", "Sevilla", "Mallorca", "Atlético Madrid", #LA LIGA
                       "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
                       "Granada","Las Palmas", "Alavés",
                       
                       "Lyon","Strasbourg","Clermont Foot", "Toulouse", "Lens", "Angers" ,"Lille",      
                       "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris Saint-Germain","Ajaccio" ,  # Ligue 1
                       "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                       "Metz", "Le Havre",
                       
                       "Eintracht Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "Mönchengladbach", "Bochum", "Dortmund",  
                       "Stuttgart", "Köln", "Freiburg", "Hoffenheim", "Werder Bremen", "Bayer Leverkusen", "RB Leipzig",  # Bundes
                       "Hertha BSC", "Schalke 04", "Mainz 05", "Bayern Munich",
                       "Darmstadt 98", "Heidenheim")
  
  #fbref
  fbref_name <- c("Manchester City", "Arsenal", "Newcastle Utd", "Manchester Utd", "Liverpool",
                  "Brighton","Tottenham","Aston Villa", "Brentford","Fulham","Chelsea","Crystal Palace",
                  "Wolves","Bournemouth","West Ham", "Nott'ham Forest", "Everton","Leeds United",
                  "Leicester City", "Southampton",
                  "Burnley","Luton Town", "Sheffield Utd",
                  
                  "Sampdoria","Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                  "Salernitana","Hellas Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
                  "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                  "Frosinone", "Genoa", "Cagliari",
                  
                  "Osasuna","Celta Vigo","Valladolid", "Barcelona","Cádiz", "Valencia", "Almería",    
                  "Athletic Club", "Getafe", "Betis", "Espanyol", "Sevilla", "Mallorca", "Atlético Madrid", #LA LIGA
                  "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
                  "Granada","Las Palmas", "Alavés",
                  
                  "Lyon","Strasbourg","Clermont Foot", "Toulouse", "Lens", "Angers" ,"Lille",      
                  "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris S-G","Ajaccio" ,  # Ligue 1
                  "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                  "Metz", "Le Havre",
                  
                  "Eint Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "M'Gladbach", "Bochum", "Dortmund",  
                  "Stuttgart", "Köln", "Freiburg", "Hoffenheim", "Werder Bremen", "Leverkusen", "RB Leipzig",  # Bundes
                  "Hertha BSC", "Schalke 04", "Mainz 05", "Bayern Munich",
                  "Darmstadt 98", "Heidenheim"
  )
  
  
  
  understat_name <- c("Manchester City", "Arsenal", "Newcastle United", "Manchester United", "Liverpool",
                      "Brighton","Tottenham","Aston Villa", "Brentford","Fulham","Chelsea","Crystal Palace",
                      "Wolverhampton Wanderers","Bournemouth","West Ham", "Nottingham Forest", "Everton","Leeds",
                      "Leicester", "Southampton",
                      "Burnley","Luton", "Sheffield United",
                      
                      "Sampdoria","AC Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                      "Salernitana","Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
                      "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                      "Frosinone", "Genoa", "Cagliari",
                      
                      "Osasuna","Celta Vigo","Real Valladolid", "Barcelona","Cadiz", "Valencia", "Almeria",    
                      "Athletic Club", "Getafe", "Real Betis", "Espanyol", "Sevilla", "Mallorca", "Atletico Madrid", #LA LIGA
                      "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
                      "Granada", "Las Palmas", "Alaves",
                      
                      "Lyon","Strasbourg","Clermont Foot", "Toulouse", "Lens", "Angers" ,"Lille",      
                      "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris Saint Germain","Ajaccio" ,  # Ligue 1
                      "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                      "Metz", "Le Havre",
                      
                      "Eintracht Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "Borussia M.Gladbach", "Bochum", "Borussia Dortmund",  
                      "VfB Stuttgart", "FC Cologne", "Freiburg", "Hoffenheim", "Werder Bremen", "Bayer Leverkusen", "RasenBallsport Leipzig",  
                      "Hertha Berlin", "Schalke 04", "Mainz 05", "Bayern Munich",
                      "Darmstadt", "FC Heidenheim")
  
  
  #football data
  footballData_name <- c("Man City", "Arsenal", "Newcastle", "Man United", "Liverpool",
                         "Brighton","Tottenham","Aston Villa", "Brentford","Fulham","Chelsea","Crystal Palace",
                         "Wolves","Bournemouth","West Ham", "Nott'm Forest", "Everton","Leeds",
                         "Leicester", "Southampton",
                         "Burnley","Luton", "Sheffield United",
                         
                         "Sampdoria","Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                         "Salernitana","Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
                         "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                         "Frosinone", "Genoa", "Cagliari",
                         
                         "Osasuna","Celta","Valladolid", "Barcelona","Cadiz", "Valencia", "Almeria",    
                         "Ath Bilbao", "Getafe", "Betis", "Espanol", "Sevilla", "Mallorca", "Ath Madrid", #LA LIGA
                         "Sociedad", "Elche", "Girona", "Vallecano", "Real Madrid","Villarreal",
                         "Granada", "Las Palmas", "Alaves",
                         
                         "Lyon","Strasbourg","Clermont", "Toulouse", "Lens", "Angers" ,"Lille",      
                         "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris SG","Ajaccio" ,  # Ligue 1
                         "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                         "Metz", "Le Havre",
                         
                         "Ein Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "M'gladbach", "Bochum", "Dortmund",  
                         "Stuttgart", "FC Koln", "Freiburg", "Hoffenheim", "Werder Bremen", "Leverkusen", "RB Leipzig",  # Bundes
                         "Hertha", "Schalke 04", "Mainz", "Bayern Munich",
                         "Darmstadt", "Heidenheim")
  
  
  
  if (from == "understat" & to == "fbref") {
    
    new_teams <- lapply(teams, function(team) fbref_name[which(team == understat_name)]) %>% unlist()
    return(new_teams)
    
  } else if (from == "football_data" & to == "fbref") {
    
    new_teams <- lapply(teams, function(team) fbref_name[which(team == footballData_name)]) %>% unlist()
    return(new_teams)
    
  } else if (from == "fbref" & to == "fbref_full") {
    
    new_teams <- lapply(teams, function(team) fbref_name_full[which(team == fbref_name)]) %>% unlist()
    return(new_teams)
    
  } else if (from == "fbref" & to == "understat") {
    
    new_teams <- lapply(teams, function(team) understat_name[which(team == fbref_name)]) %>% unlist()
    return(new_teams)
    
  } else if (from == "fbref_full" & to == "understat") {
    
    new_teams <- lapply(teams, function(team) understat_name[which(team == fbref_name_full)]) %>% unlist()
    return(new_teams)
    
  } else if (from == "fbref_full" & to == "fbref") {
    
    new_teams <- lapply(teams, function(team) fbref_name[which(team == fbref_name_full)]) %>% unlist()
    return(new_teams)
    
  } else {
    return(NULL)
  }
  
  
}


sql_querys_team <- function(con, teamid, season, min_minutes ,only_show_pos_ALL = FALSE) {
  
  
  if(length(season) == 1) {
    
    if(season == "2022/2023") {
      
      season <- "AND m.season = '22/23'"
      
    } else {
      
      season <- "AND m.season = '2023/2024'"
      
    }
  } else {
    
    season <- ""
    
  }
  
  sql_query <-  paste0("SELECT
                        p.player_name,
                        ps.position,
                        COUNT(ps.match_id) AS total_matches,
                        SUM(ps.minutes_played) AS total_minutes_played,
                        SUM(ps.goals) AS total_goals,
                        SUM(ps.assists) AS total_assists,
                        SUM(ps.shots) AS total_shots,
                        SUM(ps.sot) AS total_sot,
                        SUM(ps.tackles) AS total_tackles,
                        SUM(ps.pass) AS total_pass,
                        (SUM(ps.yellow) + (2*SUM(ps.red))) AS total_cards
                        FROM
                        players p
                        INNER JOIN
                        player_stats ps ON p.player_id = ps.player_id
                        INNER JOIN
                        teams t ON p.team_id = t.team_id
                        INNER JOIN
                        matches m ON ps.match_id = m.match_id
                        WHERE ps.team_id = '",teamid,"'",
                       season,
                       "GROUP BY p.player_name, ps.position;")
  
  player_stats <- dbGetQuery(con, sql_query)
  
  player_stats <- player_stats[!is.na(player_stats$position),] # removing NA
  
  player_stats_no_pos <- player_stats %>%
    group_by(player_name) %>%
    summarise(matches = sum(total_matches),
              minutes_played = sum(total_minutes_played),
              across(total_shots:total_cards, ~ sum(.)/sum(total_minutes_played)*90)) %>%
    filter(minutes_played >= min_minutes) %>%
    mutate(position = "ALL")
  
  player_stats_with_pos <- player_stats %>%
    mutate(position = rename_pos(position)) %>%
    group_by(player_name, position) %>%
    summarise(matches = sum(total_matches),
              minutes_played = sum(total_minutes_played),
              across(total_shots:total_cards, ~ sum(.)/sum(total_minutes_played)*90)) %>%
    filter(minutes_played >= min_minutes)
  
  player_stats_merge <- rbind(player_stats_with_pos, player_stats_no_pos) %>%
    arrange(player_name)
  
  colnames(player_stats_merge) <- c("Name", "Position", "Matches", "Minutes played",
                                    "Shots/90",  "Sot/90", "Tackles/90", "Pass/90", "Card/90")
  
  dublicated_players <-  player_stats_merge[duplicated(player_stats_merge[,-2]),1] %>% unlist()
  
  player_stats_merge <- player_stats_merge[!(player_stats_merge$Name %in% dublicated_players &
                                               player_stats_merge$Position != "ALL") ,] 
  
  
  if(only_show_pos_ALL) {
    player_stats_merge <- player_stats_merge %>% filter(Position == "ALL")
  } else {
    player_stats_merge <- player_stats_merge %>% filter(Position != "ALL")
  }
  
  
  
  return(player_stats_merge)
  
}



sql_querys_player <- function(con, player, season) {
  
  if(length(season) == 1) {
    
    if(season == "2022/2023") {
      
      season <- "AND m.season = '22/23';"
      
    } else {
      
      season <- "AND m.season = '2023/2024';"
      
    }
  } else {
    
    season <- ""
    
  }
  
  sql_query <-  paste0("SELECT m.event_date, t1.team_name AS home_team,
                t2.team_name AS away_team, ps.position, ps.minutes_played, ps.goals, ps.assists,
                ps.shots, ps.sot, ps.tackles, ps.pass, ps.yellow, ps.red
                FROM players AS p
                JOIN player_stats AS ps ON p.player_id = ps.player_id
                JOIN matches AS m ON ps.match_id = m.match_id
                JOIN teams AS t1 ON m.home_team_id = t1.team_id
                JOIN teams AS t2 ON m.away_team_id = t2.team_id
                WHERE p.player_name = '", player, "'", season)
  
  player_stats <- dbGetQuery(con, sql_query)
  player_stats <- player_stats %>% arrange(desc(event_date))
  
 
  colnames(player_stats) <- c("Date", "Home team", "Away team", "Position",
                                    "MInutes played",  "Goals", "Assists", "Shots", "Sot",
                              "Tackles", "Passes", "Yellow", "Red")
  

  return(player_stats)
  
}


