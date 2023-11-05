library(understatr)
library(dplyr)
library(listviewer)
library(rvest)
library(stringr)
library(lubridate)
library(tibble)
library(writexl)
library(RSelenium)
library(httr)
library(jsonlite)
library(RJSONIO)
library(huxtable)
library(colorDF)
library(knitr)
library(stringi)
library(kableExtra)
library(lme4)
library(lmerTest)
library(sjstats)
library(lme4)
library(stringdist)




# //////////////////////////////////////////////////////////////////////////////
# /////////// FÅR UT ALLLA SPELARE I UNDERSTAT MED ID SOM I FBREF //////////////
# //////////////////////////////////////////////////////////////////////////////

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





{

id_teams_2023 <- readRDS("id_teams_2023.rds")
  

unique_teams <- id_teams_2023[c(1,4,20,21,24,39,44,57,58,72,75,88,93),]


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




# ==============================================================================



for(i in 1:nrow(unique_teams)){
  
 
  understat_players <- get_team_players_stats(rename_teams(unique_teams[i,1], from = "fbref_full", to = "understat"),2023)
  understat_players <- understat_players[,c(1,2,4,9)]
  
  if(unique_teams[i,1] == "Barcelona") understat_players <- understat_players[!(understat_players$player_id==11471),]
  if(unique_teams[i,1] == "Athletic Club") understat_players[understat_players$player_id==10981,2] <- "Malcom Abdulai Ares Djalo"
  if(unique_teams[i,1] == "Mallorca") understat_players[understat_players$player_id==9838,2] <- "Josep Antoni Gayá"
  
  if(unique_teams[i,1] == "Troyes") understat_players[understat_players$player_id==10824,2] <- "Jordan Dongmo"
  
  fbref_players_subset <- fbref_players[fbref_players$Squad == unique_teams[i,1],-5]
  
  # ==============================================================================
  
  colnames(fbref_players_subset) <- colnames(understat_players) <- c("player_id","player_name","total_minutes_played","total_shots")
  
  merge_df <-  merge_dataframes(fbref_players_subset,understat_players)
  
  id_players <- rbind(id_players,merge_df)

}


# remove strange players
# id_players <- id_players[-c(196,222,223),]

old <- readRDS(file="id_players_2023.rds")
id_players <- rbind(id_players, old)

#saveRDS(id_players, file = "id_players_2023.rds")

}







