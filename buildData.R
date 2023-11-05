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


# ==============================================================================
# Epmty list ===================================================================
# ==============================================================================

num_layers <- 5
num_elements_layer <- c(380, 380, 380, 306, 306)
stats_list <- vector(mode = 'list', num_layers)

for (i in 1:num_layers) {
  stats_list[[i]] <- vector(mode = 'list', num_elements_layer[i])
  for (j in 1:num_elements_layer[i]) {
    stats_list[[i]][[j]] <- vector(mode = 'list', 59)
  }
}

names(stats_list) <- c("PL","SerieA","LaLiga","Bundesliga","Ligue1")
for(a in 1:length(stats_list)){
  names(stats_list[[a]]) <- paste0("Match", seq(1, length(stats_list[[a]])))
  for(b in 1:length(stats_list[[a]])){
    names(stats_list[[a]][[b]]) <- c("match.id","league.id","home.team.id","away.team.id","date","season","h.odds","x.odds","a.odds",
                                   "o2.5odds","u2.5odds","mainline","h.odds.mainline","a.odds.mainline",
                                   "home.team","away.team","h.fouls","h.corners","h.crosses","h.touches","h.tackles",
                                   "h.interceptions","h.aerials.won","h.clearances","h.offsides","h.goal.kicks",
                                   "h.throw.ins","h.long.balls","h.shots","h.sot","h.possession","h.goals","h.xg","h.yellow","h.red","h.formation",
                                   "referee",
                                   "a.fouls","a.corners","a.crosses","a.touches","a.tackles",
                                   "a.interceptions","a.aerials won","a.clearances","a.offsides","a.goal.kicks",
                                   "a.throw ins","a.long balls","a.shots","a.sot","a.possession","a.goals","a.xg","a.yellow","a.red","a.formation",
                                   "h.players","a.players")
  }
}

# ==============================================================================
# ==============================================================================


get_team_id <- function(team) {
  
  id <- id_teams_2023[id_teams_2023$hteam == team,2]
  return(id)
  
}

convert_date <- function(input_date) {
  parts <- strsplit(input_date, " ")[[1]]
  
  year <- parts[4]
  month <- match(tolower(parts[2]), tolower(month.name))
  day <- gsub(",", "", parts[3])
  
  date_obj <- as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")
  
  output_date <- format(date_obj, "%Y-%m-%d")
  
  return(output_date)
}

get_team_formation <- function(vec) {
  
  vektor <- vec[!(vec == "Sub")]
  vektor <- vektor[!is.na(vektor)]
  
  if(length(vektor) == 11){
    
    if((sum(vektor == "DC") == 3) & (sum(vektor == "DR") == 0)){
      # 3 backslinje
      if(((sum(vektor == "DMR") == 1) & (sum(vektor == "DML") == 1)) | ((sum(vektor == "MR") == 1) & (sum(vektor == "ML") == 1))){
        # 2 wings
        if((sum(vektor == "MC") == 2)){
          # 3-4-?
          if((sum(vektor == "AMC") == 2)){
            return("3-4-2-1")
          }else if((sum(vektor == "AMC") == 1)){
            return("3-4-1-2")
          }else {
            return("3-4-3")
          }
          
        }else if((sum(vektor == "MC") == 3)){
          return("3-5-2")
        }else{
          return("NA")
          print("Dont know this formation")
        }
        
      }else if((sum(vektor == "DMC") == 2)){
        return("3-2-4-1")
      }else if((sum(vektor == "DMC") == 1)){
        print("3-1-4-1, GG WP")
        return("3-1-4-1")
      }else{
        print("Dont know this formation")
        return("NA")
      }
      
      
    }else if((sum(vektor == "DC") == 2) & (sum(vektor == "DR") == 1) & (sum(vektor == "DL") == 1)){
      # 4 backslinje
      if((sum(vektor == "DMC") == 2)){
        # 4-2-?
        if((sum(vektor == "FW") == 1)){
          return("4-2-3-1")
        }else {
          print("4-2-2-2, GG WP")
          return("4-2-2-2")
        }
        
      }else if((sum(vektor == "MC") == 3)){
        # 4-3-?
        if((sum(vektor == "FW") == 1) & (sum(vektor == "FWR") == 1) & (sum(vektor == "FWL") == 1)){
          return("4-3-3")
        }else{
          return("4-3-2-1")
        }
      }else if((sum(vektor == "DMC") == 1)){
        # 4-1-?
        if((sum(vektor == "FW") == 1)){
          print("4-1-4-1, GG WP")
          return("4-1-4-1")
        }else{
          print("4-1-2-1-2, GG WP")
          return("4-1-2-1-2")
        }
      }else if((sum(vektor == "MC") == 2) & (sum(vektor == "MR") == 1) & (sum(vektor == "ML") == 1)){
        # 4-4-?
        if((sum(vektor == "FW") == 2)){
          return("4-2-2")
        }else{
          print("4-4-1-1, GG WP")
          return("4-4-1-1")
        }
      }else{
        print("Dont know this formation")
        return("NA")
      }
      
    }else{
      # 5 backslinje
      if((sum(vektor == "FW") == 1)){
        return("5-4-1")
      }else{
        return("5-3-2")
      }
    }
  
  } else {
    print("Dont have the whole eleven in database")
    return("NA")
  }
}



id_players_2023 <- readRDS("id_players_2023.rds")
id_players_2023 <- unique(id_players_2023)
id_matches_2023 <- readRDS("id_matches_2023.rds") 
odds_2023 <- readRDS("odds_matches_2023.rds") 
id_teams_2023 <- readRDS("id_teams_2023.rds") 

match_id <- id_matches_2023$match_id_fbref


leagues <- c("EPL", "Serie_A", "La_liga", "Bundesliga", "Ligue_1")

x_matches <- sapply(leagues, function(league) sum(id_matches_2023$league == league))
match_id <- lapply(leagues, function(league) (id_matches_2023[id_matches_2023$league == league, 4]))



missing_players <- c()
missing <- 1


stats_list_old <- stats_list
for(ii in 1:5){
  
  
  if(ii %% 1 == 0){
    Sys.sleep(20)
  }
  
  for(i in 1:x_matches[[ii]]){
 
    
    print(i)
    if(i %% 5 == 0){
      Sys.sleep(5)
    }
    
    # ////////////////////////////// HÄMTAR DATA /////////////////////////////////
    
    url <- str_c("https://fbref.com/en/matches/", match_id[[ii]][i])
    url <- read_html(url)
    teams <- url %>% html_nodes(".logo+ strong a") %>%  html_text()
    stats <-  url %>% html_nodes(str_c("#team_stats_extra div:nth-child(13) , ",
                                       "#team_stats_extra div:nth-child(7) , ",
                                       "#team_stats_extra div:nth-child(10) , ",
                                       "#team_stats_extra div:nth-child(4) , ",
                                       "#team_stats_extra div:nth-child(12) , ",
                                       "#team_stats_extra div:nth-child(9) , ",
                                       "#team_stats_extra div:nth-child(15) , ",
                                       "#team_stats_extra div:nth-child(6)")) %>%  html_text()
    
    team_id.h <- get_team_id(teams[1])
    team_id.b <- get_team_id(teams[2])
    
    # Pen behövs pga sidans logik om skott (straffskott ej inkluderat)
    pen.h <-  url %>%  html_nodes(str_c("#stats_",team_id.h,"_summary tbody .right:nth-child(10)")) %>% html_text() %>% as.numeric()
    pen.a <-  url %>%  html_nodes(str_c("#stats_",team_id.b,"_summary tbody .right:nth-child(10)")) %>% html_text() %>% as.numeric()
    
    
    skott <- url %>% html_nodes("tr:nth-child(7) td > div > div:nth-child(1)") %>%  html_text()
    split_str <- strsplit(skott, "—")
    
    sot_h <-   substr(split_str[[1]][1],1, regexpr(" of ", split_str[[1]][1]) - 1) %>% as.numeric()
    sot_h <- as.character(sot_h+sum(pen.h))
    shots_h <- substr(split_str[[1]][1],   regexpr(" of ", split_str[[1]][1]) + 4, nchar(split_str[[1]][1])-1) %>% as.numeric()
    shots_h <- as.character(shots_h+sum(pen.h))
    
    sot_a <-   substr(split_str[[2]][2],2, regexpr(" of ", split_str[[2]][2]) - 1) %>% as.numeric()
    sot_a <- as.character(sot_a+sum(pen.a))
    shots_a <- substr(split_str[[2]][2],   regexpr(" of ", split_str[[2]][2]) + 4, nchar(split_str[[2]][2])) %>% as.numeric()
    shots_a <- as.character(shots_a+sum(pen.a))
    
    poss <-  url %>% html_nodes("tr:nth-child(3) strong") %>%  html_text() %>% substr(1,2)
    goals <- url %>% html_nodes(".score") %>%  html_text()
    xg <- url %>% html_nodes(".score_xg") %>%  html_text()
    referee <- url %>% html_nodes(".scorebox_meta span:nth-child(1)") %>%  html_text() %>% substr(1,str_length(.) - 10)
    
    date <- url %>% html_nodes(".scorebox_meta strong a") %>%  html_text() %>% convert_date()
    cards.h <- url %>% html_nodes(str_c("#stats_",team_id.h,"_summary tfoot .right:nth-child(14) , #stats_",team_id.h,"_summary tfoot .right:nth-child(13)")) %>%  html_text() 
    cards.a <- url %>% html_nodes(str_c("#stats_",team_id.b,"_summary tfoot .right:nth-child(14) , #stats_",team_id.b,"_summary tfoot .right:nth-child(13)")) %>%  html_text() 
    
    
    
    # //////////////////////////// LÄGGER IN MATCHSTATS //////////////////////////
    
    
    
    stats_list[[ii]][[i]][1] <- match_id[[ii]][i] # match id
    stats_list[[ii]][[i]][2] <- paste0("000",ii) # liga id
    stats_list[[ii]][[i]][3] <- team_id.h # h.team id
    stats_list[[ii]][[i]][4] <- team_id.b # a.team id
    stats_list[[ii]][[i]][5] <- date # date
    stats_list[[ii]][[i]][6] <- str_c(year(Sys.Date()),"/",year(Sys.Date())+1) # säsong
    
    # ODDS ////////////////////////////////////////////////////////////

    odds_stats <- odds_2023[odds_2023$match_id==stats_list[[ii]][[i]][1],]

    
    stats_list[[ii]][[i]][7] <- odds_stats$AvgCH # hemmalag odds
    stats_list[[ii]][[i]][8] <- odds_stats$AvgCD # oavgjort odds
    stats_list[[ii]][[i]][9] <- odds_stats$AvgCA # bortalag odds
    
    stats_list[[ii]][[i]][10] <- odds_stats$AvgC.2.5 # över 2.5
    stats_list[[ii]][[i]][11] <- odds_stats$AvgC.2.5.1 # under 2.5
    
    stats_list[[ii]][[i]][12] <- odds_stats$AHCh # asien lina
    stats_list[[ii]][[i]][13] <- odds_stats$AvgCAHH # asien odds hemmalag
    stats_list[[ii]][[i]][14] <- odds_stats$AvgCAHA # asien odds bortalag
    
    
    
    stats_list[[ii]][[i]][c(15:35,37:56)] <- c(teams, stats[c(TRUE, FALSE)],
                                             shots_h, sot_h, poss[1], goals[1], xg[1], cards.h, referee,
                                             stats[c(FALSE, TRUE)], shots_a, sot_a, poss[2], goals[2], xg[2], cards.a)
    
    
    # ////////////////////////// HÄMTAR IN SPELARESTATS //////////////////////////
    
    # HEMMALAG
    team_id <- team_id.h
    player <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary th a")) %>% html_text()
    
    goals <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(7)")) %>% html_text()
    ass <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(8)")) %>% html_text()
    
    shots <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(11)")) %>% html_text() %>% as.numeric()
    shots <- as.character(shots + pen.h)
    sot <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(12)")) %>% html_text() %>% as.numeric()
    sot <- as.character(sot + pen.h)
    
    yellow <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(13)")) %>% html_text()
    red <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(14)")) %>% html_text()
    tkl <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(16)")) %>% html_text()
    npxg <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(20)")) %>% html_text()
    xAG <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(21)")) %>% html_text()
    pass <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(25)")) %>% html_text()
    
    player.id <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary th a")) %>% html_attr("href") %>% str_sub(13,20)
    
    # understat STATS ////////////////////////////////////////////////////////////////////////
    
    understat_ids <- c()
    
    for(t in 1:length(player.id)) {
      
      understat_id <- id_players_2023[which(player.id[t] == id_players_2023[,1]),2][1]
      
      if(length(understat_id) == 0) {
        
        understat_ids[t] <- NA
        print(paste(player[t], "not in database"))
        missing_players[missing] <- player[t]
        missing <- missing + 1
        
      } else {
        
        understat_ids[t] <- understat_id
        
      }
      
    } 
    
   
    pos <- c()
    xgChain <- c()
    min <- c()
    p <- 1
    stats_list[[ii]][[i]][[16]]
    understat_match_id <- id_matches_2023[which(stats_list[[ii]][[i]][[1]] == id_matches_2023[,"match_id_fbref"]),"match_id_understat"]
    if(length(understat_match_id) == 0) print(paste(stats_list[[ii]][[i]][[15]], "vs",
                                                    stats_list[[ii]][[i]][[16]], "not in understat"))
    
    
    for(p in 1:length(understat_ids)){
      
      if(is.na(understat_ids[p])) {
        
        pos[p] <- NA
        xgChain[p] <- NA
        min[p] <- NA
        p <- p + 1
        
      } else {
        
        suppressMessages({
          understat_player <- get_player_matches_stats(understat_ids[p])
        })
        understat_player <- understat_player[understat_player$match_id == understat_match_id,]
        if(!(nrow(understat_player)==0)){
          
          pos[p] <- understat_player[,8]
          xgChain[p] <- understat_player[,21]
          min[p] <- understat_player[,7]
          p <- p + 1
        } else {
          print("nrow(understat_player)==0, BÖR UNDERSÖKAS")
        }
        
      }
    }
    
    # ////////////////////////////////////////////////////////////////////////////////////////
    
    h.df <- cbind(player.id,player,pos,min,goals,ass,shots,sot,tkl,npxg,xAG,xgChain,pass,yellow,red) %>%  as.data.frame()
    
    stats_list[[ii]][[i]][[36]] <- get_team_formation(unlist(pos)) # Formation
    
    
    
    # BORTALAG
    team_id <- team_id.b
    player <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary th a")) %>% html_text()
    
    goals <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(7)")) %>% html_text()
    ass <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(8)")) %>% html_text()
    
    shots <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(11)")) %>% html_text() %>% as.numeric()
    shots <- as.character(shots + pen.a)
    sot <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(12)")) %>% html_text() %>% as.numeric()
    sot <- as.character(sot + pen.a)
    
    yellow <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(13)")) %>% html_text()
    red <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(14)")) %>% html_text()
    tkl <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(16)")) %>% html_text()
    npxg <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(20)")) %>% html_text()
    xAG <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(21)")) %>% html_text()
    pass <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(25)")) %>% html_text()
    
    player.id <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary th a")) %>% html_attr("href") %>% str_sub(13,20)
    
    # understat STATS ////////////////////////////////////////////////////////////////////////
    
    understat_ids <- c()
    
    for(t in 1:length(player.id)) {
      
      understat_id <- id_players_2023[which(player.id[t] == id_players_2023[,1]),2][1]
      
      if(length(understat_id) == 0) {
        
        understat_ids[t] <- NA
        print(paste(player[t], "not in database"))
        missing_players[missing] <- player[t]
        missing <- missing + 1
        
      } else {
        
        understat_ids[t] <- understat_id
        
      }
      
    } 
    
    pos <- c()
    xgChain <- c()
    min <- c()
    p <- 1
    
    for(p in 1:length(understat_ids)){
      
      if(is.na(understat_ids[p])) {
        
        pos[p] <- NA
        xgChain[p] <- NA
        min[p] <- NA
        p <- p + 1
        
      } else {
        
        suppressMessages({
          understat_player <- get_player_matches_stats(understat_ids[p])
        })
        understat_player <- understat_player[understat_player$match_id == understat_match_id,]
        if(!(nrow(understat_player)==0)){
          
          pos[p] <- understat_player[,8]
          xgChain[p] <- understat_player[,21]
          min[p] <- understat_player[,7]
          p <- p + 1
        } else {
          print("nrow(understat_player)==0, BÖR UNDERSÖKAS")
        }
        
      }
    }
    
    # ////////////////////////////////////////////////////////////////////////////////////////
    
    a.df <- cbind(player.id,player,pos,min,goals,ass,shots,sot,tkl,npxg,xAG,xgChain,pass,yellow,red) %>%  as.data.frame()
    
    stats_list[[ii]][[i]][[57]] <- get_team_formation(unlist(pos)) # Formation
    
    
    # ////////////////////////// LÄGGER IN SPELARESTATS /////////////////////////
    
    # lägger in vektor med längd = antal spelare
    stats_list[[ii]][[i]][[58]] <- vector(mode='list',nrow(h.df)) 
    stats_list[[ii]][[i]][[59]] <- vector(mode='list',nrow(a.df))
    names(stats_list[[ii]][[i]][[58]]) <- h.df[,2]
    names(stats_list[[ii]][[i]][[59]]) <- a.df[,2]
    
    for(j in 1:nrow(h.df)) stats_list[[ii]][[i]][[58]][[j]] <- unlist(as.vector(h.df[j,], mode='list'), recursive = FALSE)
    
    
    for(j in 1:nrow(a.df)) stats_list[[ii]][[i]][[59]][[j]] <- unlist(as.vector(a.df[j,], mode='list'), recursive = FALSE)
    
    # ///////////////////////////////////////////////////////////////////////////
    
    
    
  }
}



saveRDS(stats_list, file = "stats_list_2023.rds")















