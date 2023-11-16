

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
  
  
  # KAMBI
  KAMBI_name <- c("Manchester City", "Arsenal", "Newcastle", "Manchester United", "Liverpool",
                  "Brighton","Tottenham","Aston Villa", "Brentford","Fulham","Chelsea","Crystal Palace",
                  "Wolves","Bournemouth","West Ham", "Nottingham Forest", "Everton","Leeds",
                  "Leicester", "Southampton",
                  "Burnley","Luton Town", "Sheffield United",
                  
                  "Sampdoria","AC Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                  "Salernitana","Hellas Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
                  "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                  "Frosinone", "Genoa", "Cagliari",
                  
                  "Osasuna","Celta Vigo","Valladolid", "FC Barcelona","Cadiz", "Valencia", "Almeria",    
                  "Athletic Bilbao", "Getafe", "Real Betis", "Espanyol", "Sevilla", "Mallorca", "Atlético Madrid", #LA LIGA
                  "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
                  "Granada", "UD Las Palmas", "Alavés",
                  
                  "Lyon","Strasbourg","Clermont", "Toulouse", "Lens", "Angers" ,"Lille",      
                  "Montpellier", "Rennes", "Marseille", "Nantes", "AS Monaco","Paris SG","Ajaccio" ,  # Ligue 1
                  "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                  "Metz", "Le Havre",
                  
                  "Eintracht Frankfurt", "VfL Wolfsburg", "FC Augsburg", "1. FC Union Berlin", "Borussia Mönchengladbach", "VfL Bochum", "Borussia Dortmund",  
                  "VfB Stuttgart", "1. FC Köln", "SC Freiburg", "TSG Hoffenheim", "Werder Bremen", "Bayer Leverkusen", "RB Leipzig",  # Bundes
                  "Hertha Berlin", "Schalke 04", "Mainz 05", "Bayern Munich",
                  "Darmstadt 98", "1. FC Heidenheim")
  
  
  
  
  # log name
  logo_name <- c("Manchester City","Arsenal FC","Newcastle United","Manchester United","Liverpool FC",
                       "Brighton & Hove Albion", "Tottenham Hotspur", "Aston Villa", "Brentford FC", "Fulham FC", "Chelsea FC",
                       "Crystal Palace", "Wolverhampton Wanderers", "AFC Bournemouth","West Ham United","Nottingham Forest",
                       "Everton FC","Leeds United","Leicester City","Southampton FC",
                       "Burnley FC","Luton Town", "Sheffield United",
                       
                       "UC Sampdoria","AC Milan","AC Monza","US Lecce","ACF Fiorentina","SS Lazio","Spezia Calcio" ,      
                       "US Salernitana 1919","Hellas Verona","Juventus FC","Torino FC","Udinese Calcio","Inter Milan","US Sassuolo",   #SERIE A
                       "FC Empoli","SSC Napoli","Bologna FC 1909","Atalanta BC","AS Roma","US Cremonese",
                       "Frosinone Calcio", "Genoa CFC", "Cagliari Calcio",
                       
                       "CA Osasuna","Celta de Vigo","Real Valladolid CF", "FC Barcelona","Cádiz CF", "Valencia CF", "UD Almería",    
                       "Athletic Bilbao", "Getafe CF", "Real Betis Balompié", "RCD Espanyol Barcelona", "Sevilla FC", "RCD Mallorca", "Atlético de Madrid", #LA LIGA
                       "Real Sociedad", "Elche CF", "Girona FC", "Rayo Vallecano", "Real Madrid","Villarreal CF",
                       "Granada CF","UD Las Palmas", "Deportivo Alavés",
                       
                       "Olympique Lyon","RC Strasbourg Alsace","Clermont Foot 63", "FC Toulouse", "RC Lens", "Angers SCO" ,"LOSC Lille",      
                       "Montpellier HSC", "Stade Rennais FC", "Olympique Marseille", "FC Nantes", "AS Monaco","Paris Saint-Germain","AC Ajaccio" ,  # Ligue 1
                       "Stade Reims", "AJ Auxerre","ESTAC Troyes","OGC Nice","Stade Brestois 29","FC Lorient",
                       "FC Metz", "Le Havre AC",
                       
                       "Eintracht Frankfurt", "VfL Wolfsburg", "FC Augsburg", "1.FC Union Berlin", "Borussia Mönchengladbach", "VfL Bochum", "Borussia Dortmund",  
                       "VfB Stuttgart", "1. FC Köln", "SC Freiburg", "TSG 1899 Hoffenheim", "SV Werder Bremen", "Bayer 04 Leverkusen", "RB Leipzig",  # Bundes
                       "Hertha BSC", "FC Schalke 04", "1.FSV Mainz 05", "Bayern Munich",
                       "SV Darmstadt 98", "1.FC Heidenheim 1846")
  
  
  logo_local <- c()
  logo_web <-c()
  logo_local[1:23] <-  paste0("logos/GB1/",logo_name[1:23],".png")
  logo_local[24:46] <- paste0("logos/GB1/",logo_name[24:46],".png")
  logo_local[47:69] <-paste0("logos/GB1/",logo_name[47:69],".png")
  logo_local[70:91] <- paste0("logos/GB1/",logo_name[70:91] ,".png")
  logo_local[92:111] <- paste0("logos/GB1/", logo_name[92:111],".png")
  
  logo_web[18:20] <-  paste0("https://raw.githubusercontent.com/luukhopman/football-logos/master/logos/2022-23/GB1/",logo_name[18:20],".png")
  logo_web[c(1:17,21:23)] <- paste0("https://raw.githubusercontent.com/luukhopman/football-logos/master/logos/GB1/",logo_name[c(1:17,21:23)],".png")
  logo_web[24:46] <- paste0("https://raw.githubusercontent.com/luukhopman/football-logos/master/logos/IT1/",logo_name[24:46],".png")
  logo_web[47:69] <- paste0("https://raw.githubusercontent.com/luukhopman/football-logos/master/logos/ES1/",logo_name[47:69],".png")
  logo_web[70:91] <- paste0("https://raw.githubusercontent.com/luukhopman/football-logos/master/logos/FR1/",logo_name[70:91],".png")
  logo_web[92:111] <- paste0("https://raw.githubusercontent.com/luukhopman/football-logos/master/logos/L1/",logo_name[92:111],".png")
  
 
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
    
  } else if (from == "kambi" & to == "fbref") {
    
    new_teams <- lapply(teams, function(team) fbref_name[which(team == KAMBI_name)]) %>% unlist()
    return(new_teams)
    
  }else if (from == "fbref" & to == "logo_local") {
    
    new_teams <- lapply(teams, function(team) logo_local[which(team == fbref_name)]) %>% unlist()
    return(new_teams)
    
  }else if (from == "fbref_full" & to == "logo_web") {
    
    new_teams <- lapply(teams, function(team) logo_web[which(team == fbref_name_full)]) %>% unlist()
    return(new_teams)
    
  } else {
    return(NULL)
  }
  
  
}



# ==============================================================================
# PLAYER SECTION ===============================================================
# ==============================================================================
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
  
  suppressWarnings({
  player_stats <- dbGetQuery(con, sql_query)
  })
  
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
  
  sql_query <-  paste0("SELECT m.event_date, t1.team_name AS home_team, m.h_goals, m.a_goals,
                t2.team_name AS away_team, ps.position, ps.minutes_played, ps.goals, ps.assists,
                ps.shots, ps.sot, ps.tackles, ps.pass, ps.yellow, ps.red
                FROM players AS p
                JOIN player_stats AS ps ON p.player_id = ps.player_id
                JOIN matches AS m ON ps.match_id = m.match_id
                JOIN teams AS t1 ON m.home_team_id = t1.team_id
                JOIN teams AS t2 ON m.away_team_id = t2.team_id
                WHERE p.player_name = '", player, "'", season)
  
  suppressWarnings({
  player_stats <- dbGetQuery(con, sql_query)
  })
  player_stats <- player_stats %>% arrange(desc(event_date))
  
  print(player_stats)
  
  url_home <- rename_teams(player_stats$home_team, from = "fbref_full",to = "logo_web")
  url_away <- rename_teams(player_stats$away_team, from = "fbref_full",to = "logo_web")
  player_stats <- player_stats %>%  
    mutate(
      home_team = paste0(
        "<img src=\"",
        url_home,
        "\" style=\"width: 25px; height: 33px; display: block; margin: 0 auto;\">"
      ),
      away_team = paste0(
        "<img src=\"",
        url_away,
        "\" style=\"width: 25px; height: 33px; display: block; margin: 0 auto;\">"
      )
    )
  
  colnames(player_stats) <- c("Date", "H_team","Gh","Ga", "A_team", "Pos",
                              "Min played",  "Goals", "Ass", "Shots", "Sot",
                              "Tkl", "Passes", "Y", "R")

  return(player_stats)
  
}



# ==============================================================================
# MATCHUP SECTION ==============================================================
# ==============================================================================

get_team_stats <- function(all_team_stats, mat, teams,
                           season_home_team, match_location_home_team,
                           season_away_team, match_location_away_team) {
  
  if(!length(teams) == 2) stop("Teams needs to be a vector with TWO teams!")
  
  
  team1 <- get_one_team_specific_stats(all_team_stats, teams[1],
                                       season_home_team, match_location_home_team)
  team2 <- get_one_team_specific_stats(all_team_stats, teams[2],
                                       season_away_team, match_location_away_team)
  
  mat[,2:3] <- team1
  mat[,4:5] <- team2
  mat$Total <- (mat$HomeTeam_For + mat$AwayTeam_Against) / 2 + 
    (mat$AwayTeam_For + mat$HomeTeam_Against) / 2
  
  return(mat)
}

get_all_team_specific_stats <- function(con) {
  

  
sql_query1 <-  paste0("SELECT
    t.team_id,
    t.team_name,
    m.season,
    'Home' AS match_location,
    AVG(m.h_fouls) AS avg_fouls,
    AVG(m.h_corners) AS avg_corners,
    AVG(m.h_tackles) AS avg_tackles,
    AVG(m.h_offsides) AS avg_offsides,
    AVG(m.h_goal_kicks) AS avg_goal_kicks,
    AVG(m.h_throw_ins) AS avg_throw_ins,
    AVG(m.h_shots) AS avg_shots,
    AVG(m.h_sot) AS avg_sot,
    AVG(m.h_possession) AS avg_possession,
    AVG(m.h_goals) AS avg_goals,
    AVG(m.h_xg) AS avg_xg,
    AVG(m.h_yellow) AS avg_yellow,
    AVG(m.h_red) AS avg_red,
    AVG(m.h_odds) AS avg_odds1x2,
    AVG(m.o2_5odds) AS avg_oddsOver,
    AVG(m.a_fouls) AS avg_fouls_A,
    AVG(m.a_corners) AS avg_corners_A,
    AVG(m.a_tackles) AS avg_tackles_A,
    AVG(m.a_offsides) AS avg_offsides_A,
    AVG(m.a_goal_kicks) AS avg_goal_kicks_A,
    AVG(m.a_throw_ins) AS avg_throw_ins_A,
    AVG(m.a_shots) AS avg_shots_A,
    AVG(m.a_sot) AS avg_sot_A,
    AVG(m.a_possession) AS avg_possession_A,
    AVG(m.a_goals) AS avg_goals_A,
    AVG(m.a_xg) AS avg_xg_A,
    AVG(m.a_yellow) AS avg_yellow_A,
    AVG(m.a_red) AS avg_red_A,
    AVG(m.a_odds) AS avg_odds1x2_A,
    AVG(m.o2_5odds) AS avg_oddsOver_A
FROM
    teams t
JOIN
    matches m ON t.team_id = m.home_team_id
GROUP BY
    t.team_id, t.team_name, match_location,m.season

UNION

SELECT
    t.team_id,
    t.team_name,
    m.season,
    'Away' AS match_location,
    AVG(m.a_fouls) AS avg_fouls,
    AVG(m.a_corners) AS avg_corners,
    AVG(m.a_tackles) AS avg_tackles,
    AVG(m.a_offsides) AS avg_offsides,
    AVG(m.a_goal_kicks) AS avg_goal_kicks,
    AVG(m.a_throw_ins) AS avg_throw_ins,
    AVG(m.a_shots) AS avg_shots,
    AVG(m.a_sot) AS avg_sot,
    AVG(m.a_possession) AS avg_possession,
    AVG(m.a_goals) AS avg_goals,
    AVG(m.a_xg) AS avg_xg,
    AVG(m.a_yellow) AS avg_yellow,
    AVG(m.a_red) AS avg_red,
    AVG(m.a_odds) AS avg_odds1x2,
    AVG(m.o2_5odds) AS avg_oddsOver,
    AVG(m.h_fouls) AS avg_fouls_A,
    AVG(m.h_corners) AS avg_corners_A,
    AVG(m.h_tackles) AS avg_tackles_A,
    AVG(m.h_offsides) AS avg_offsides_A,
    AVG(m.h_goal_kicks) AS avg_goal_kicks_A,
    AVG(m.h_throw_ins) AS avg_throw_ins_A,
    AVG(m.h_shots) AS avg_shots_A,
    AVG(m.h_sot) AS avg_sot_A,
    AVG(m.h_possession) AS avg_possession_A,
    AVG(m.h_goals) AS avg_goals_A,
    AVG(m.h_xg) AS avg_xg_A,
    AVG(m.h_yellow) AS avg_yellow_A,
    AVG(m.h_red) AS avg_red_A,
    AVG(m.h_odds) AS avg_odds1x2_A,
    AVG(m.o2_5odds) AS avg_oddsOver_A
FROM
    teams t
JOIN
    matches m ON t.team_id = m.away_team_id
GROUP BY
    t.team_id, t.team_name, match_location, m.season;
")


sql_query2 <-  paste0("SELECT
    t.team_id,
    t.team_name,
    'All' AS season,
    'Home' AS match_location,
    AVG(m.h_fouls) AS avg_fouls,
    AVG(m.h_corners) AS avg_corners,
    AVG(m.h_tackles) AS avg_tackles,
    AVG(m.h_offsides) AS avg_offsides,
    AVG(m.h_goal_kicks) AS avg_goal_kicks,
    AVG(m.h_throw_ins) AS avg_throw_ins,
    AVG(m.h_shots) AS avg_shots,
    AVG(m.h_sot) AS avg_sot,
    AVG(m.h_possession) AS avg_possession,
    AVG(m.h_goals) AS avg_goals,
    AVG(m.h_xg) AS avg_xg,
    AVG(m.h_yellow) AS avg_yellow,
    AVG(m.h_red) AS avg_red,
    AVG(m.h_odds) AS avg_odds1x2,
    AVG(m.o2_5odds) AS avg_oddsOver,
    AVG(m.a_fouls) AS avg_fouls_A,
    AVG(m.a_corners) AS avg_corners_A,
    AVG(m.a_tackles) AS avg_tackles_A,
    AVG(m.a_offsides) AS avg_offsides_A,
    AVG(m.a_goal_kicks) AS avg_goal_kicks_A,
    AVG(m.a_throw_ins) AS avg_throw_ins_A,
    AVG(m.a_shots) AS avg_shots_A,
    AVG(m.a_sot) AS avg_sot_A,
    AVG(m.a_possession) AS avg_possession_A,
    AVG(m.a_goals) AS avg_goals_A,
    AVG(m.a_xg) AS avg_xg_A,
    AVG(m.a_yellow) AS avg_yellow_A,
    AVG(m.a_red) AS avg_red_A,
    AVG(m.a_odds) AS avg_odds1x2_A,
    AVG(m.o2_5odds) AS avg_oddsOver_A
FROM
    teams t
JOIN
    matches m ON t.team_id = m.home_team_id
GROUP BY
    t.team_id, t.team_name, match_location

UNION

SELECT
    t.team_id,
    t.team_name,
    'All' AS season,
    'Away' AS match_location,
 AVG(m.a_fouls) AS avg_fouls,
    AVG(m.a_corners) AS avg_corners,
    AVG(m.a_tackles) AS avg_tackles,
    AVG(m.a_offsides) AS avg_offsides,
    AVG(m.a_goal_kicks) AS avg_goal_kicks,
    AVG(m.a_throw_ins) AS avg_throw_ins,
    AVG(m.a_shots) AS avg_shots,
    AVG(m.a_sot) AS avg_sot,
    AVG(m.a_possession) AS avg_possession,
    AVG(m.a_goals) AS avg_goals,
    AVG(m.a_xg) AS avg_xg,
    AVG(m.a_yellow) AS avg_yellow,
    AVG(m.a_red) AS avg_red,
    AVG(m.a_odds) AS avg_odds1x2,
    AVG(m.o2_5odds) AS avg_oddsOver,
    AVG(m.h_fouls) AS avg_fouls_A,
    AVG(m.h_corners) AS avg_corners_A,
    AVG(m.h_tackles) AS avg_tackles_A,
    AVG(m.h_offsides) AS avg_offsides_A,
    AVG(m.h_goal_kicks) AS avg_goal_kicks_A,
    AVG(m.h_throw_ins) AS avg_throw_ins_A,
    AVG(m.h_shots) AS avg_shots_A,
    AVG(m.h_sot) AS avg_sot_A,
    AVG(m.h_possession) AS avg_possession_A,
    AVG(m.h_goals) AS avg_goals_A,
    AVG(m.h_xg) AS avg_xg_A,
    AVG(m.h_yellow) AS avg_yellow_A,
    AVG(m.h_red) AS avg_red_A,
    AVG(m.h_odds) AS avg_odds1x2_A,
    AVG(m.o2_5odds) AS avg_oddsOver_A
FROM
    teams t
JOIN
    matches m ON t.team_id = m.away_team_id
GROUP BY
    t.team_id, t.team_name, match_location
")

  
  
  
sql_query3 <-  paste0("SELECT
    t.team_id,
    t.team_name,
    m.season,
    'Both' AS match_location,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_fouls ELSE m.a_fouls END) AS avg_fouls,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_corners ELSE m.a_corners END) AS avg_corners,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_tackles ELSE m.a_tackles END) AS avg_tackles,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_offsides ELSE m.a_offsides END) AS avg_offsides,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_goal_kicks ELSE m.a_goal_kicks END) AS avg_goal_kicks,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_throw_ins ELSE m.a_throw_ins END) AS avg_throw_ins,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_shots ELSE m.a_shots END) AS avg_shots,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_sot ELSE m.a_sot END) AS avg_sot,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_possession ELSE m.a_possession END) AS avg_possession,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_goals ELSE m.a_goals END) AS avg_goals,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_xg ELSE m.a_xg END) AS avg_xg,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_yellow ELSE m.a_yellow END) AS avg_yellow,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_red ELSE m.a_red END) AS avg_red,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_odds ELSE m.a_odds END) AS avg_odds1x2,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.o2_5odds ELSE m.o2_5odds END) AS avg_oddsOver,
    
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_fouls ELSE m.h_fouls END) AS avg_fouls_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_corners ELSE m.h_corners END) AS avg_corners_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_tackles ELSE m.h_tackles END) AS avg_tackles_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_offsides ELSE m.h_offsides END) AS avg_offsides_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_goal_kicks ELSE m.h_goal_kicks END) AS avg_goal_kicks_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_throw_ins ELSE m.h_throw_ins END) AS avg_throw_ins_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_shots ELSE m.h_shots END) AS avg_shots_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_sot ELSE m.h_sot END) AS avg_sot_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_possession ELSE m.h_possession END) AS avg_possession_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_goals ELSE m.h_goals END) AS avg_goals_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_xg ELSE m.h_xg END) AS avg_xg_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_yellow ELSE m.h_yellow END) AS avg_yellow_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_red ELSE m.h_red END) AS avg_red_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_odds ELSE m.h_odds END) AS avg_odds1x2_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.o2_5odds ELSE m.o2_5odds END) AS avg_oddsOver_A
FROM
    teams t
LEFT JOIN
    matches m ON t.team_id = m.home_team_id OR t.team_id = m.away_team_id
GROUP BY
    t.team_id, t.team_name, m.season;
")




sql_query4 <-  paste0("SELECT
    t.team_id,
    t.team_name,
    'All' AS season,
    'Both' AS match_location,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_fouls ELSE m.a_fouls END) AS avg_fouls,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_corners ELSE m.a_corners END) AS avg_corners,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_tackles ELSE m.a_tackles END) AS avg_tackles,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_offsides ELSE m.a_offsides END) AS avg_offsides,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_goal_kicks ELSE m.a_goal_kicks END) AS avg_goal_kicks,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_throw_ins ELSE m.a_throw_ins END) AS avg_throw_ins,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_shots ELSE m.a_shots END) AS avg_shots,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_sot ELSE m.a_sot END) AS avg_sot,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_possession ELSE m.a_possession END) AS avg_possession,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_goals ELSE m.a_goals END) AS avg_goals,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_xg ELSE m.a_xg END) AS avg_xg,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_yellow ELSE m.a_yellow END) AS avg_yellow,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_red ELSE m.a_red END) AS avg_red,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.h_odds ELSE m.a_odds END) AS avg_odds1x2,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.o2_5odds ELSE m.o2_5odds END) AS avg_oddsOver,
    
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_fouls ELSE m.h_fouls END) AS avg_fouls_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_corners ELSE m.h_corners END) AS avg_corners_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_tackles ELSE m.h_tackles END) AS avg_tackles_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_offsides ELSE m.h_offsides END) AS avg_offsides_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_goal_kicks ELSE m.h_goal_kicks END) AS avg_goal_kicks_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_throw_ins ELSE m.h_throw_ins END) AS avg_throw_ins_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_shots ELSE m.h_shots END) AS avg_shots_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_sot ELSE m.h_sot END) AS avg_sot_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_possession ELSE m.h_possession END) AS avg_possession_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_goals ELSE m.h_goals END) AS avg_goals_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_xg ELSE m.h_xg END) AS avg_xg_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_yellow ELSE m.h_yellow END) AS avg_yellow_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_red ELSE m.h_red END) AS avg_red_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.a_odds ELSE m.h_odds END) AS avg_odds1x2_A,
    AVG(CASE WHEN t.team_id = m.home_team_id THEN m.o2_5odds ELSE m.o2_5odds END) AS avg_oddsOver_A
FROM
    teams t
LEFT JOIN
    matches m ON t.team_id = m.home_team_id OR t.team_id = m.away_team_id
GROUP BY
    t.team_id, t.team_name;
")
  
  suppressWarnings({
    team_stats_1 <- dbGetQuery(con, sql_query1)
    team_stats_2 <- dbGetQuery(con, sql_query2)
    team_stats_3 <- dbGetQuery(con, sql_query3)
    team_stats_4 <- dbGetQuery(con, sql_query4)
  })
  
  all_team_stats <- rbind(team_stats_1,team_stats_2,team_stats_3,team_stats_4)
  
  return(all_team_stats)
}


get_one_team_specific_stats <- function(all_team_stats, team, season_string, home_away) {
  
  
    if(season_string == "2022/2023") {
      
      season_string <- '22/23'
      
    } else if (season_string == "2023/2024"){
      
      season_string <- '2023/2024'
      
    } else {
    
      season_string <- 'All'
      
    }
    print(season_string)
  df <- all_team_stats %>%  filter(team_id == team) %>% filter(season == season_string) %>% 
    filter(match_location == home_away) %>% select(-c(1:4))  %>% t() 


  df <- cbind(df[1:(nrow(df)/2),],df[-(1:(nrow(df)/2)),])
  colnames(df) <- c("For","Against")
  rownames(df) <- c("Fouls","Corners","Tackles","Offsides","Goal kicks",
                    "Throw ins","Shots","Shots on target","Posession","Goals",
                    "xG","Yellow cards","Red cards","Odds 1x2","Odds Over 2.5")
  return(df)
}


get_one_team_matches <- function(con, team, season_string, home_away) {
  
  
  
  if(season_string == "2022/2023") {
    
    season_string <- " m.season = '22/23' AND"
    
  } else if (season_string == "2023/2024"){
    
    season_string <- " m.season = '2023/2024' AND"
    
  } else {
    
    season_string <- ""
    
  }
  

  sql_query <- paste0("SELECT
  m.event_date,
  t1.team_name AS home_team,
  m.h_goals,
  m.a_goals,
  t2.team_name AS away_team,
  m.home_team_id,
  m.away_team_id,
  m.h_odds,
  m.x_odds,
  m.a_odds,
  m.o2_5odds,
  m.u2_5odds,
  m.h_fouls,
  m.h_corners,
  m.h_tackles,
  m.h_offsides,
  m.h_goal_kicks,
  m.h_throw_ins,
  m.h_shots,
  m.h_sot,
  m.h_possession,
  m.h_xg,
  m.h_yellow,
  m.h_red,
  m.h_formation,
  m.a_fouls,
  m.a_corners,
  m.a_tackles,
  m.a_offsides,
  m.a_goal_kicks,
  m.a_throw_ins,
  m.a_shots,
  m.a_sot,
  m.a_possession,
  m.a_xg,
  m.a_yellow,
  m.a_red,
  m.a_formation
  FROM
  matches m
  JOIN teams AS t1 ON m.home_team_id = t1.team_id
  JOIN teams AS t2 ON m.away_team_id = t2.team_id
  WHERE",season_string,"
  (m.home_team_id = '",team,"'OR m.away_team_id = '",team,"');")
  
  suppressWarnings({
    matches <- dbGetQuery(con, sql_query)
  })
  
  away_for_stats <- matches[matches$away_team_id == team,c(26:38)]
  away_aga_stats <- matches[matches$away_team_id == team,c(13:25)]
  matches[matches$away_team_id == team,c(13:25)] <- away_for_stats
  matches[matches$away_team_id == team,c(26:38)] <- away_aga_stats
  
  if(home_away == "Home") {
    
    matches <- matches[matches$home_team_id == team,]
    
  } else if (home_away == "Away") {
    
    matches <- matches[matches$away_team_id == team,]
    
  } else {
    
  }
  # removing id
  matches <- matches[,-c(6:7)]
  
  url_home <- rename_teams(matches$home_team, from = "fbref_full",to = "logo_web")
  url_away <- rename_teams(matches$away_team, from = "fbref_full",to = "logo_web")
  matches <- matches %>%  
    mutate(
      home_team = paste0(
        "<img src=\"",
        url_home,
        "\" style=\"width: 40px; height: 52px; display: block; margin: 0 auto;\">"
      ),
      away_team = paste0(
        "<img src=\"",
        url_away,
        "\" style=\"width: 40px; height: 52px; display: block; margin: 0 auto;\">"
      )
    )
  
  colnames(matches) <- c("Date","Home","Gh","Ga","Away","1","X","2","o2.5","u2.5","fouls",
                         "cor","tkl","off","gk","ti","s","sot","pos","xG","y","r","form",
                         "foulsA",
                         "corA","tklA","offA","gkA","tiA","sA","sotA","posA","xGA","yA","rA","formA")
  
  return(matches)
  
}




# ==============================================================================
# ODDS =========================================================================
# ==============================================================================


get_odds_kambi <- function(comp, category=12579,teams) {
  
  Sys.sleep(3)
  url <-  str_c("https://eu-offering.kambicdn.org/offering/v2018/ub/listView/football/",
                comp,".json?lang=en_US&market=IT&category=",category)
  
  res <-  GET(url, fileEncoding = "UTF-8")
  json_file <- fromJSON(rawToChar(res$content))
  
  
  home <- lapply(json_file$events, function(x){x$event$homeName})  %>% unlist() %>%
    rename_teams(., from = "kambi", to = "fbref")
  away <- lapply(json_file$events, function(x){x$event$awayName}) %>% unlist() %>% 
    rename_teams(., from = "kambi", to = "fbref")
  
  
  val1 <- (teams[1] == home)
  val2 <- (teams[2] == away)
  
  event <- intersect(which(val1), which(val2))
  h_odds <- json_file$events[[event]]$betOffers[[1]]$outcomes[[1]]$odds/1000
  x_odds <- json_file$events[[event]]$betOffers[[1]]$outcomes[[2]]$odds/1000
  a_odds <- json_file$events[[event]]$betOffers[[1]]$outcomes[[3]]$odds/1000
  
  return(c(h_odds,x_odds,a_odds))
  
  
}






