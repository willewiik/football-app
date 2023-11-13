

library(RMySQL)
library(dplyr)
library(DBI)
library(RSQLite)
library(tidyr)
library(dbplyr)


# ==============================================================================
file_name <- str_c("rds_files/stats_list_",Sys.Date(),".rds")
stats <- readRDS(file = file_name) 

cutoff <- c()
for(pp in 1:5) {
  for(p in 1:length(stats[[pp]])){
    if(as.character(stats[[pp]][[p]][1]) == "NULL"){
      cutoff[pp] <- p - 1
      break
    }
  }
}
for(pp in 1:5) stats[[pp]] <- stats[[pp]][1:cutoff[pp]]
# ==============================================================================


# Anslutningsuppgifter till databasen
dbHost <- "localhost"
dbPort <- 3306  # Portnummer
dbName <- "sql_workbench"
dbUser <- "root"
dbPassword <- "&2;6DcH+O{jnVct"

# Skapa anslutningssträng
con <- dbConnect(MySQL(), host = dbHost, port = dbPort, dbname = dbName, user = dbUser, password = dbPassword)





#dbDisconnect(con)




## LÄGGER IN TABELLER I DATABAS ------------------------------------------------
{
  
  
  unique_teams <- data.frame(team_id = character(),
                             team_name = character(), stringsAsFactors = FALSE)
  
  for(kk in 1:5){
    for (match in stats[[kk]]) {
      home_team <- data.frame(team_id = match$home.team.id,
                              team_name = match$home.team, stringsAsFactors = FALSE)
      away_team <- data.frame(team_id = match$away.team.id,
                              team_name = match$away.team, stringsAsFactors = FALSE)
      
      unique_teams <- rbind(unique_teams, home_team, away_team)
    }
  }
  
  # Connecting to teams table to be able to know which teams already in database
  teams <- dbGetQuery(con, sql("SELECT team_id FROM teams"))
  

  unique_teams <- unique(unique_teams)
  unique_teams <- unique_teams[!(unique_teams$team_id %in% teams$team_id),]
  
  dbSendQuery(con, "SET GLOBAL local_infile = true;")
  
  if(nrow(unique_teams=! 0)) {

    dbWriteTable(
      con,
      name = "teams",
      value = unique_teams,
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE,
      allow.keywords = FALSE
    )
    
  }
  
  
  # TABELL MATCHES -------------------------------------------------------------
  matches <- data.frame(matrix(ncol = 57, nrow = 0))
  for(kk in 1:5){
    for (i in 1:length(stats[[kk]])) {
      match_data <- unlist(stats[[kk]][[i]][1:57])
      matches <- rbind(matches, match_data)
    }
  }
  colnames(matches) <- names(stats[[kk]][[1]])[1:57]
  colnames(matches) <- gsub("\\.", "_", colnames(matches))
  colnames(matches)[c(5,44,48,49)] <- c("event_date","a_aerials_won",
                                        "a_throw_ins","a_long_balls")
  
  
  #dbSendQuery(con, "SET GLOBAL local_infile = true;")
  dbWriteTable(
    con,
    name = "matches",
    value = matches[,-c(15,16)], # removing team names
    row.names = FALSE,
    overwrite = FALSE,
    append = TRUE,
    allow.keywords = FALSE
  )
  
  
  # TABELL PLAYERS ---------------------------------------------------------------
  
  players <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(players) <- c("player_id","player_name","team_id")
  for(kk in 1:5){
    for (i in 1:length(stats[[kk]])) {
      
      for (j in 1:length(stats[[kk]][[i]][[58]])) {
        
        id <- stats[[kk]][[i]][[58]][[j]][1]
        
        if ((!any(players$player_id == id))) {
          
          players_data <- unlist(stats[[kk]][[i]][[58]][[j]][1:2])
          players_data <- c(players_data, stats[[kk]][[i]][3])
          names(players_data) <- c("player_id","player_name","team_id")
          players <- rbind(players, players_data)
          
        } else if ((any(players$player_id == id)) & !(players[players$player_id == id,3] == stats[[kk]][[i]][3])) {
       
          players[players$player_id == id,3] <- stats[[kk]][[i]][3]
          
        }
        
      }
      
      
      
      for (k in 1:length(stats[[kk]][[i]][[59]])) {
        
        id <- stats[[kk]][[i]][[59]][[k]][1]
        
        if ((!any(players$player_id == id))) {
          
          players_data <- unlist(stats[[kk]][[i]][[59]][[k]][1:2])
          players_data <- c(players_data, stats[[kk]][[i]][4])
          names(players_data) <- c("player_id","player_name","team_id")
          players <- rbind(players, players_data)
        } else if ((any(players$player_id == id)) & !(players[players$player_id == id,3] == stats[[kk]][[i]][4])) {
      
          
          players[players$player_id == id,3] <- stats[[kk]][[i]][4]
          
        }
        
      }
      
    }
  }
  
  
  # Connecting to teams table to be able to know which players already in database
  players_in_database <- dbGetQuery(con, sql("SELECT player_id FROM players"))
  
  players <- players[!(players$player_id %in% players_in_database$player_id), ]
  
  
  
  dbWriteTable(
    con,
    name = "players",
    value = players,
    row.names = FALSE,
    overwrite = FALSE,
    append = TRUE,
    allow.keywords = FALSE
  )
  
  
  # TABELL PLAYERS_STATS ---------------------------------------------------------
  
  players_stats <- data.frame(matrix(ncol = 16, nrow = 0))
  for(kk in 1:5){
    for (i in 1:length(stats[[kk]])) {
      
      for (j in 1:length(stats[[kk]][[i]][[58]])) {
        
        players_data <- unlist(stats[[kk]][[i]][[58]][[j]][c(1,3:15)])
        players_data <- c(stats[[kk]][[i]][1],stats[[kk]][[i]][3], players_data)
        names(players_data)[2] <- "team_id"
        if(length(players_data)==16){
        players_stats <- rbind(players_stats, players_data)
        }else{
          print(players_data$player.id)
        }
        
        
      }
      
      
      for (k in 1:length(stats[[kk]][[i]][[59]])) {
        
        players_data <- unlist(stats[[kk]][[i]][[59]][[k]][c(1,3:15)])
        players_data <- c(stats[[kk]][[i]][1],stats[[kk]][[i]][4], players_data)
        names(players_data)[2] <- "team_id"
        if(length(players_data)==16){
          players_stats <- rbind(players_stats, players_data)
        }else{
          print(players_data$player.id)
        }
        
        
      }
      
    }
  }
  colnames(players_stats) <- c("match_id","team_id", "player_id","position","minutes_played",
                               "goals", "assists","shots","sot",
                               "tackles", "npxg","xAG","xgChain","pass",
                               "yellow", "red")
  

  
  
  dbWriteTable(
    con,
    name = "player_stats",
    value = players_stats,
    row.names = FALSE,
    overwrite = FALSE,
    append = TRUE,
    allow.keywords = FALSE
  )
  
  
  
  dbListTables(con)
  
  dbDisconnect(con)
}








# TEST AV DATABAS -------------------------------------------------------------








