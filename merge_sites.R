

# This script merges fbref, understat, and footballdata matches




library(understatr)
library(worldfootballR)
library(dplyr)
library(stringr)


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
    
  } else {
    return(NULL)
  }
  
  
}



season <- 2023 # season start year
season_end_year <- season + 1
leagues <- c("EPL", "Serie A", "La liga", "Bundesliga", "Ligue 1")
countries <- c("ENG","ITA","ESP","GER","FRA")


# ==============================================================================
# UNDERSTAT ====================================================================
# ==============================================================================
matches_understat <- lapply(leagues, understat_league_match_results,
                           season_start_year = season)

matches_understat <- do.call("rbind", matches_understat) %>% 
                     select(home_team, away_team, match_id, league) 

matches_understat$home_team <- rename_teams(matches_understat$home_team,
                                       from = "understat", to = "fbref")

matches_understat$away_team <- rename_teams(matches_understat$away_team,
                                            from = "understat", to = "fbref")


# ==============================================================================
# FBREF ========================================================================
# ==============================================================================
matches_fbref <- fb_match_results(country = countries,
                                  gender = "M",
                                  season_end_year = season_end_year,
                                  tier="1st")




matches_fbref <-  matches_fbref %>% 
  select(Date, Home, Away, MatchURL) %>% 
  filter(as.Date(Date) < Sys.Date()) %>% 
  na.omit() # Removing matches that should have played but they have not


cat("=======================THESE MATCHES ARE NOT COUNTED=======================")
print(matches_fbref[str_detect(matches_fbref$MatchURL,"https://fbref.com/en/stathead/matchup"),1:3])
cat("==========================================================================")



matches_fbref <- matches_fbref %>% 
  filter(str_detect(MatchURL,"https://fbref.com/en/matches")) %>% 
  rename(datetime = Date, 
         home_team = Home, away_team = Away, 
         match_id = MatchURL) %>% 
  mutate(match_id = str_sub(match_id,30,37))



# ==============================================================================
# MERGING ======================================================================
# ==============================================================================

# Fbref using better dates than understat
merged_matches <- merge(matches_fbref, matches_understat,
                        by = c("home_team", "away_team")) 

colnames(merged_matches) <- c(colnames(merged_matches)[1:3],
                              "match_id_fbref","match_id_understat","league")


# Check missing matches
# matches_fbref[!(matches_fbref$match_id %in% merged_matches$match_id.x), ]
# matches_understat[!(matches_understat$match_id %in% merged_matches$match_id.y), ]

#saveRDS(merged_matches, file = "rds_files/id_matches_2023.rds")




# ==============================================================================
# Football data ================================================================
# ==============================================================================
# [Please note that the odds are collected for the downloadable weekend fixtures 
# on Fridays afternoons generally not later than 17:00 British Standard Time.
# Odds for midweek fixtures are collected Tuesdays not later than 13:00 British Standard Time.]



columns_of_interest <- c("HomeTeam", "AwayTeam", "AvgCH", "AvgCD", "AvgCA",
                         "AvgC.2.5", "AvgC.2.5.1", "AHCh", "AvgCAHH", "AvgCAHA")


read_and_transform_data <- function(league_code) {
  
  # File path
  data <- read.csv(paste0("odds_matches/", league_code, ".csv"))[, columns_of_interest]
  data$HomeTeam <- sapply(data$HomeTeam, FUN = rename_teams, from = "football_data", to = "fbref")
  data$AwayTeam <- sapply(data$AwayTeam, FUN = rename_teams, from = "football_data", to = "fbref")
  return(data)
  
}

# List of league codes
league_codes <- c("E0", "I1", "SP1", "F1", "D1")

matches_footballData <- lapply(league_codes, read_and_transform_data)

matches_footballData <- do.call("rbind", matches_footballData) 
colnames(matches_footballData)[1:2] <- c("home_team", "away_team")


matches_footballData <- merge(matches_fbref, matches_footballData,
                               by = c("home_team", "away_team")) 


# saveRDS(matches_footballData, file = "rds_files/odds_matches_2024.rds")


# ==============================================================================
# Team IDS from fbref ==========================================================
# ==============================================================================



library(rvest)

get_team_id <- function(url, rows) {
  hteam <- url %>%
    read_html() %>%
    html_nodes(".right a") %>%
    html_text() %>%
    unique()
  
  teamid <- url %>%
    read_html() %>%
    html_nodes(".right a") %>%
    html_attr("href") %>%
    stringr::str_sub(12, 19) %>%
    unique()
  
  return(data.frame(hteam, teamid))
}

urls <- c(
  "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures",
  "https://fbref.com/en/comps/11/schedule/Serie-A-Scores-and-Fixtures",
  "https://fbref.com/en/comps/12/schedule/La-Liga-Scores-and-Fixtures",
  "https://fbref.com/en/comps/20/schedule/Bundesliga-Scores-and-Fixtures",
  "https://fbref.com/en/comps/13/schedule/Ligue-1-Scores-and-Fixtures"
)

rows <- c(380, 380, 380, 306, 306)

league_data <- lapply(1:length(urls), function(i) get_team_id(urls[i], rows[i]))
team_id <- do.call(rbind, league_data)
team_id$hteam <- rename_teams(team_id$hteam, from = "fbref", to = "fbref_full")
#saveRDS(team_id, file = "rds_files/id_teams_2023.rds")

# ==============================================================================
# ID PLAYERS ===================================================================
# ==============================================================================


merge_dataframes <- function(df1, df2, min_name = 3) {
  merged_df <- data.frame()
  
  diff <- nrow(df1) - nrow(df2)
  if(!(diff==0)){
    if(diff>0){
      # ta bort rad från df1
      df1 <- df1[-order(df1$total_minutes_played)[1:abs(diff)], ]
    }else{
      # ta bort rad från df2
      print("hjehjhe")
      df2 <- df2[-order(df2$total_minutes_played)[1:abs(diff)], ]
    }
  }
  
  # Loopa igenom spelarnamnen i df1
  gg <- 1
  wp <- 1
  players_untaken_index_1 <- c()
  players_untaken_index_2 <- c()
  
  for (i in 1:nrow(df1)) {
    
    player_name1 <- df1$player_name[i]
    total_minutes_played1 <- df1$total_minutes_played[i]
    total_shots1 <- df1$total_shots[i]
    player_id1 <- df1$player_id[i]
    
    # Hitta bästa matchning i df2 baserat på spelarnamn och "score"
    best_match_index <- NA
    min_name_distance <- Inf
    valid <- TRUE
    
    
    
    for (j in 1:nrow(df2)) {
      player_name2 <- df2$player_name[j]
      total_minutes_played2 <- df2$total_minutes_played[j]
      total_shots2 <- df2$total_shots[j]
      player_id2 <- df2$player_id[j]
      
      name_distance <- stringdist::stringdist(player_name1, player_name2)
      
      
      if(name_distance < min_name_distance){
        min_name_distance <- name_distance
        best_match_index <- j
      }
      
    }
    
    
    # Slå samman observationerna om en matchning hittades
    if (min_name_distance < min_name) {
      
      players_untaken_index_2[gg] <- best_match_index
      gg <- gg + 1
      
      merged_row <- data.frame(
        player_id1 = player_id1,
        player_id2 = df2$player_id[best_match_index],
        player_name1 = player_name1,
        player_name2 = df2$player_name[best_match_index],
        valid = valid
        # total_minutes_played1 = total_minutes_played1,
        # total_minutes_played2 = df2$total_minutes_played[best_match_index],
        # total_shots1 = total_shots1,
        # total_shots2 = df2$total_shots[best_match_index]
      )
      
      merged_df <- rbind(merged_df, merged_row)
    }else{
      
      
      players_untaken_index_1[wp] <- i
      wp <- wp + 1
    }
  }
  
  # GALEN IMPLEMENTERING AV LOL KOD
  vec1 <- 1:nrow(df2)
  vec2 <- players_untaken_index_2
  common_elements <- intersect(vec1, vec2)
  
  players_untaken_index_2 <- c(vec1[!duplicated(vec1) & !vec1 %in% common_elements], 
                               vec2[!duplicated(vec2) & !vec2 %in% common_elements])
  
  print(players_untaken_index_1)
  print(players_untaken_index_2)
  if(!(length(players_untaken_index_1) == 0)) {
    for(i in players_untaken_index_1){
      
      player_name1 <- df1$player_name[i]
      total_minutes_played1 <- df1$total_minutes_played[i]
      total_shots1 <- df1$total_shots[i]
      player_id1 <- df1$player_id[i]
      
      valid <- FALSE
      best_match_index <- NA
      best_score <- Inf
      
      for (jj in players_untaken_index_2) {
        #  print(jj)
        player_name2 <- df2$player_name[jj]
        total_minutes_played2 <- df2$total_minutes_played[jj]
        total_shots2 <- df2$total_shots[jj]
        
        minutes_difference <- abs(total_minutes_played1 - total_minutes_played2)
        shots_difference <- abs(total_shots1 - total_shots2)
        
        score <-  1 * minutes_difference +
          75 * shots_difference
        
        if (score < best_score) {
          best_match_index <- jj
          best_score <- score
        }
      }
      
      merged_row <- data.frame(
        player_id1 = player_id1,
        player_id2 = df2$player_id[best_match_index],
        player_name1 = player_name1,
        player_name2 = df2$player_name[best_match_index],
        valid = valid
        # total_minutes_played1 = total_minutes_played1,
        # total_minutes_played2 = df2$total_minutes_played[best_match_index],
        # total_shots1 = total_shots1,
        # total_shots2 = df2$total_shots[best_match_index]
      )
      
      merged_df <- rbind(merged_df, merged_row)
    }
  }
  return(merged_df)
}



# //////////////////////////////////////////////////////////////////////////////
# /////////// FÅR UT ALLLA SPELARE I UNDERSTAT MED ID SOM I FBREF //////////////
# //////////////////////////////////////////////////////////////////////////////
{
  
  id_teams_2023 <- readRDS("rds_files/id_teams_2023.rds")
  id_players_old <- readRDS("rds_files/id_players_2023.rds")
  
  
 # unique_teams <- id_teams_2023[c(1,4,20,21,24,39,44,57,58,72,75,88,93),]
  unique_teams <- id_teams_2023
  
  
  id_players <- data.frame(player_id1 = character(), player_id2 = character(),
                           player_name1 = character(), player_name2 = character(),
                           stringsAsFactors = FALSE)
  
  
  # FBREF ========================================================================
  fbref_players <- fb_big5_advanced_season_stats(season_end_year= 2024,
                                                 stat_type= "standard",
                                                 team_or_player= "player")
  
  fbref_players$Squad <- rename_teams(fbref_players$Squad, from = "fbref", "fbref_full")
  
  fbref_players_shooting <- fb_big5_advanced_season_stats(season_end_year= 2024,
                                                          stat_type= "shooting",
                                                          team_or_player= "player")
  
  fbref_players_shooting$Squad <- rename_teams(fbref_players_shooting$Squad, from = "fbref", "fbref_full")
  
  fbref_players_shooting <- fbref_players_shooting %>% 
    filter(Squad %in% unique_teams$hteam) %>% 
    select(Player, Sh_Standard) 
  
  fbref_players <- fbref_players %>% select(Url, Player, Squad, Min_Playing) %>% 
    filter(Squad %in% unique_teams$hteam) %>% mutate(Url = str_sub(Url, 30,37))
  
  fbref_players  <- merge(fbref_players, fbref_players_shooting, by = "Player") %>% 
    relocate(Url, Player, Min_Playing, Sh_Standard, Squad)
  
  
  fbref_players <- fbref_players[!(fbref_players$Url %in% id_players_old$player_id1),] # removing players that already exict in database
  
  # ==============================================================================
  
  
  
  for(i in 1:nrow(unique_teams)){
    
    
    understat_players <- get_team_players_stats(rename_teams(unique_teams[i,1], from = "fbref_full", to = "understat"),2023)
    understat_players <- understat_players[,c(1,2,4,9)]
    
    if(unique_teams[i,1] == "Barcelona") understat_players <- understat_players[!(understat_players$player_id==11471),]
    if(unique_teams[i,1] == "Athletic Club") understat_players[understat_players$player_id==10981,2] <- "Malcom Abdulai Ares Djalo"
    if(unique_teams[i,1] == "Mallorca") understat_players[understat_players$player_id==9838,2] <- "Josep Antoni Gayá"
    
    if(unique_teams[i,1] == "Troyes") understat_players[understat_players$player_id==10824,2] <- "Jordan Dongmo"
    
    understat_players <- understat_players[!(understat_players$player_id %in% id_players_old$player_id2),]
    
    fbref_players_subset <- fbref_players[fbref_players$Squad == unique_teams[i,1],-5]
    
    
    # If there 0 players, no need to merge
    if(nrow(understat_players) == 0 | nrow(fbref_players_subset) == 0) {
      
      print(paste("nrow understat=",nrow(understat_players),
                  "nrow fbref=",nrow(fbref_players_subset)))
      print(unique_teams[i,1])
      
    } else {
    
      # ==============================================================================
      
      colnames(fbref_players_subset) <- colnames(understat_players) <- c("player_id","player_name","total_minutes_played","total_shots")
      
      merge_df <-  merge_dataframes(fbref_players_subset,understat_players)
      
      id_players <- rbind(id_players,merge_df)
    
    }
    
  }
  
  
  # remove strange players
  # id_players <- id_players[-c(196,222,223),]
  # id_players <- id_players[-c(19,56,131,181,217,254),]
  # 
  # manuell_players <- list(c("6a713852","9098", "Robert Sánchez", "Robert Sánchez", TRUE),
  # c("ed383e43", "12086", "Alex Matos", "Alex Matos", TRUE),
  # c("674cfc5a", "7944", "Gabriel Strefezza", "Gabriel Strefezza", TRUE),
  # c("7c4b4e36", "11947", "Rareș-Cătălin Burnete", "Rares Burnete", TRUE),
  # c("f7c12fbf", "8334", "Hugo Guillamón", "Hugo Guillamón", TRUE),
  # c("20a65cdf", "12139", "Yarek Hernandis", "Yarek Gasiorowski", TRUE),
  # c("0e0102eb", "8393", "Omar Marmoush", "Omar Marmoush", TRUE),
  # c("58468842", "12107", "Nacho Ferri", "Nacho Ferri", TRUE),
  # c("8455ad90", "7313", "Lee Kang-in", "Lee Kang-in", TRUE),
  # c("b70afb45", "10794", "Thijs Dallinga", "Thijs Dallinga", TRUE))
  # 
  # manuell_players <- do.call("rbind",manuell_players) %>% as.data.frame()
  # colnames(manuell_players) <- colnames(id_players)
  
  
  #id_players <- rbind(id_players,manuell_players)
  
  old <- readRDS(file="rds_files/id_players_2023.rds")
  id_players <- rbind(id_players, old)
  
  #saveRDS(id_players, file = "rds_files/id_players_2023.rds")
  
}


