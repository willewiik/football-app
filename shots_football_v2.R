
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

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

rename_teams <- function(team,opt=1){
  
  #fbref
  full_name <- c("Manchester City","Arsenal","Newcastle United","Manchester United","Liverpool",
                 "Brighton & Hove Albion", "Tottenham Hotspur", "Aston Villa", "Brentford", "Fulham", "Chelsea",
                 "Crystal Palace", "Wolverhampton Wanderers", "Bournemouth","West Ham United","Nottingham Forest",
                 "Everton","Leeds United","Leicester City","Southampton",
                 
                 "Sampdoria","Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                 "Salernitana","Hellas Verona","Juventus","Torino","Udinese","Internazionale","Sassuolo",   #SERIE A
                 "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                 
                 "Osasuna","Celta Vigo","Valladolid", "Barcelona","Cádiz", "Valencia", "Almería",    
                 "Athletic Club", "Getafe", "Real Betis", "Espanyol", "Sevilla", "Mallorca", "Atlético Madrid", #LA LIGA
                 "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
                 
                 "Lyon","Strasbourg","Clermont Foot", "Toulouse", "Lens", "Angers" ,"Lille",      
                 "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris Saint-Germain","Ajaccio" ,  # Ligue 1
                 "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                 
                 "Eintracht Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "Mönchengladbach", "Bochum", "Dortmund",  
                 "Stuttgart", "Köln", "Freiburg", "Hoffenheim", "Werder Bremen", "Bayer Leverkusen", "RB Leipzig",  # Bundes
                 "Hertha BSC", "Schalke 04", "Mainz 05", "Bayern Munich")
  
  #fbref
  short_name <- c("Manchester City", "Arsenal", "Newcastle Utd", "Manchester Utd", "Liverpool",
                  "Brighton","Tottenham","Aston Villa", "Brentford","Fulham","Chelsea","Crystal Palace",
                  "Wolves","Bournemouth","West Ham", "Nott'ham Forest", "Everton","Leeds United",
                  "Leicester City", "Southampton",
                  
                  "Sampdoria","Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                  "Salernitana","Hellas Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
                  "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                  
                  "Osasuna","Celta Vigo","Valladolid", "Barcelona","Cádiz", "Valencia", "Almería",    
                  "Athletic Club", "Getafe", "Betis", "Espanyol", "Sevilla", "Mallorca", "Atlético Madrid", #LA LIGA
                  "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
                  
                  "Lyon","Strasbourg","Clermont Foot", "Toulouse", "Lens", "Angers" ,"Lille",      
                  "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris S-G","Ajaccio" ,  # Ligue 1
                  "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                  
                  "Eint Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "M'Gladbach", "Bochum", "Dortmund",  
                  "Stuttgart", "Köln", "Freiburg", "Hoffenheim", "Werder Bremen", "Leverkusen", "RB Leipzig",  # Bundes
                  "Hertha BSC", "Schalke 04", "Mainz 05", "Bayern Munich")
  
  #egna
  name <- c("ManC","Arsenal","Newcastle","ManU","Pool","Brighton","Spurs","Aston Villa","Brentford",
            "Fulham","Chelsea","Crystal Palace","Wolves","Bournemouth","West Ham","Nottingham","Everton",
            "Leeds","Leicester","Southampton",
            
            "Sampdoria","Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
            "Salernitana","Hellas Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
            "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
            
            "Osasuna","Celta Vigo","Valladolid", "Barcelona","Cádiz", "Valencia", "Almería",    
            "Athletic Club", "Getafe", "Betis", "Espanyol", "Sevilla", "Mallorca", "Atlético Madrid", #LA LIGA
            "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
            
            "Lyon","Strasbourg","Clermont Foot", "Toulouse", "Lens", "Angers" ,"Lille",      
            "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris S-G","Ajaccio" ,  # Ligue 1
            "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
            
            "Eintracht Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "Mönchengladbach", "Bochum", "Dortmund",  
            "Stuttgart", "Köln", "Freiburg", "Hoffenheim", "Werder Bremen", "Bayer Leverkusen", "RB Leipzig",  # Bundes
            "Hertha BSC", "Schalke 04", "Mainz 05", "Bayern Munich")
  
  #understat
  understat <- c("Manchester City", "Arsenal", "Newcastle United", "Manchester United", "Liverpool",
                 "Brighton","Tottenham","Aston Villa", "Brentford","Fulham","Chelsea","Crystal Palace",
                 "Wolverhampton Wanderers","Bournemouth","West Ham", "Nottingham Forest", "Everton","Leeds",
                 "Leicester", "Southampton",
                 
                 "Sampdoria","AC Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                 "Salernitana","Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
                 "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                 
                 "Osasuna","Celta Vigo","Real Valladolid", "Barcelona","Cadiz", "Valencia", "Almeria",    
                 "Athletic Club", "Getafe", "Real Betis", "Espanyol", "Sevilla", "Mallorca", "Atletico Madrid", #LA LIGA
                 "Real Sociedad", "Elche", "Girona", "Rayo Vallecano", "Real Madrid","Villarreal",
                 
                 "Lyon","Strasbourg","Clermont Foot", "Toulouse", "Lens", "Angers" ,"Lille",      
                 "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris Saint Germain","Ajaccio" ,  # Ligue 1
                 "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                 
                 "Eintracht Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "Borussia M.Gladbach", "Bochum", "Borussia Dortmund",  
                 "VfB Stuttgart", "FC Cologne", "Freiburg", "Hoffenheim", "Werder Bremen", "Bayer Leverkusen", "RasenBallsport Leipzig",  
                 "Hertha Berlin", "Schalke 04", "Mainz 05", "Bayern Munich") # Bundes
  
  
  #football data
  football <- c("Man City", "Arsenal", "Newcastle", "Man United", "Liverpool",
                  "Brighton","Tottenham","Aston Villa", "Brentford","Fulham","Chelsea","Crystal Palace",
                  "Wolves","Bournemouth","West Ham", "Nott'm Forest", "Everton","Leeds",
                  "Leicester", "Southampton",
                  
                  "Sampdoria","Milan","Monza","Lecce","Fiorentina","Lazio","Spezia" ,      
                  "Salernitana","Verona","Juventus","Torino","Udinese","Inter","Sassuolo",     #SERIE A
                  "Empoli","Napoli","Bologna","Atalanta","Roma","Cremonese",
                  
                  "Osasuna","Celta","Valladolid", "Barcelona","Cadiz", "Valencia", "Almeria",    
                  "Ath Bilbao", "Getafe", "Betis", "Espanol", "Sevilla", "Mallorca", "Ath Madrid", #LA LIGA
                  "Sociedad", "Elche", "Girona", "Vallecano", "Real Madrid","Villarreal",
                  
                  "Lyon","Strasbourg","Clermont", "Toulouse", "Lens", "Angers" ,"Lille",      
                  "Montpellier", "Rennes", "Marseille", "Nantes", "Monaco","Paris SG","Ajaccio" ,  # Ligue 1
                  "Reims", "Auxerre","Troyes","Nice","Brest","Lorient",
                  
                  "Ein Frankfurt", "Wolfsburg", "Augsburg", "Union Berlin", "M'gladbach", "Bochum", "Dortmund",  
                  "Stuttgart", "FC Koln", "Freiburg", "Hoffenheim", "Werder Bremen", "Leverkusen", "RB Leipzig",  # Bundes
                  "Hertha", "Schalke 04", "Mainz", "Bayern Munich")
  
  
  
  
  if(opt==1){
    for(i in 1:length(full_name)){
      if(team == full_name[i] | team == short_name[i]){
        return(name[i])
        break
      }
    }
  } else if(opt==10){
    for(i in 1:length(full_name)){
      if(team == full_name[i]){
        return(short_name[i])
        break
      }
    }
    
    
  } else if(opt==100){
    for(i in 1:length(full_name)){
      if(team == full_name[i] | team == name[i]| team == short_name[i] ){
        return(understat[i])
        break
      }
    }
    
    
  }else if(opt==1000){
    for(i in 1:length(full_name)){
      if(team == football[i]){
        return(short_name[i])
        break
      }
    }
    
    
  } else{
    
    for(i in 1:length(full_name)){
      if(team == name[i] |team == short_name[i]){
        return(full_name[i])
        break
      }
    }
  }
  
  return(team)
}





  

  


# SKOTT
{
team_rank <- function(team,num=1){
  
  if(num==1){ # SERIE A
  rank1 <- c("Juventus","AC Milan","Atalanta","Roma","Lazio","Napoli")
  rank2 <- c("Bologna","Fiorentina","Sassuolo","Udinese","Torino")
  rank3 <- c("Spezia","Lecce","Sampdoria","Cremonese","Empoli",
             "Verona","Monza","Salernitana","Cagliari","Venezia",
             "Crotone","Genoa","Benevento","Parma Calcio 1913",
             "SPAL 2013","Brescia","Frosinone")
  rank <- 0
  
  if(any(rank1 %in% team)) rank <- 1
  else if(any(rank2 %in% team)) rank <- 1
  else rank <- 2
  
  return(rank)
  
  }else if(num == 2){ # PL
    
  rank1 <- c("Liverpool","Chelsea","Tottenham","Arsenal","Newcastle United",
             "Manchester City","Manchester United","Brighton")
  rank2 <- c("Wolverhampton Wanderers","Leicester","Crystal Palace","West Ham","Brentford","Aston Villa","Leeds")
  rank3 <- c("Everton","Bournemouth","Southampton","Nottingham Forest","Fulham")
  rank <- 0
  
  if(any(rank1 %in% team)) rank <- 1
  else if(any(rank2 %in% team)) rank <- 2
  else rank <- 2
  
  return(rank)
    
    
  }else if(num == 3){ # BUNDESLIGA
    
    rank1 <- c("Bayern Munich","Bayer Leverkusen","Borussia Dortmund","Eintracht Frankfurt",
               "RasenBallsport Leipzig")
    rank2 <- c("Borussia M.Gladbach","Wolfsburg","Mainz 05","Union Berlin","Freiburg")
    rank3 <- c("Bochum","Hertha Berlin","VfB Stuttgart","Augsburg","Werder Bremen","Hoffenheim",
               "Schalke 04","FC Cologne")
    rank <- 0
    
    if(any(rank1 %in% team)) rank <- 1
    else if(any(rank2 %in% team)) rank <- 2
    else rank <- 2
    
    return(rank)
    
    
  }else if(num == 4){ # Ligue 1
    
    rank1 <- c("Paris Saint Germain","Lens","Marseille","Monaco","Lille","Rennes")
    rank2 <- c("Lorient","Nice","Reims","Lyon")
    rank3 <- c("Angers","Ajaccio","Troyes","Strasbourg","Brest",
               "Auxerre","Nantes","Clermont Foot","Montpellier","Toulouse")
    rank <- 0
    
    if(any(rank1 %in% team)) rank <- 1
    else if(any(rank2 %in% team)) rank <- 1
    else rank <- 2
    
    return(rank)
    
    
  }else{ # La liga
    
    rank1 <- c("Barcelona","Real Madrid","Atletico Madrid","Real Sociedad","Villarreal")
    rank2 <- c("Real Betis","Athletic Club","Osasuna")
    rank3 <- c("Rayo Vallecano","Celta Vigo","Girona","Mallorca","Sevilla",
               "Cadiz","Getafe","Almeria","Real Valladolid","Valencia",
               "Espanyol","Elche")
    rank <- 0
    
    if(any(rank1 %in% team)) rank <- 1
    else if(any(rank2 %in% team)) rank <- 1
    else rank <- 2
    
    return(rank)
    
    
    
  }
}


pos_to_int <- function(pos){
  int <- 0
  if(pos == "GK" | pos == "S"){
    int <- 1
  }else if(pos == "DML" | pos == "DMR" | pos == "DL" | pos == "DC"| pos == "DR"){
    int <- 2
  
  }else if(pos == "FW"  | pos == "FWL" | pos == "FWR"){
    int <- 4
  }else{
    int <- 3
  }
return(int)
}

# EN STOR LISTA
lista <- lapply(my.list<-vector(mode = 'list',5),function(x) 
  x <- lapply(x<-vector(mode = 'list',20),function(x) 
    x <- lapply(x<-vector(mode = 'list',4),function(x)
      x <- lapply(x<-vector(mode = 'list',14),function(x)
        x <- lapply(x<-vector(mode = 'list',2),function(x)
          x <- lapply(x<-vector(mode = 'list',4),function(x)
           x <- vector(mode='list',6)))))))

# NAMNGER LISTAN
names(lista) <- c("Serie A","EPL","Bundesliga","Ligue 1","La liga")
#names(lista) <- c("EPL","Serie A","Bundesliga","Ligue 1","La liga")

for(a in 1:5){
  for(b in 1:20){
   
     names(lista[[a]][[b]]) <- c("GK","Försvarare","Mittfält","Anfallare")
     
     for(c in 1:4){
       for(d in 1:12){
         names(lista[[a]][[b]][[c]][[d]]) <- c("bra_lag","dåligt_lag")
         for(e in 1:2){
           names(lista[[a]][[b]][[c]][[d]][[e]]) <- c("S_90","SOT_90","S_70","SOT_70")
           for(f in 1:4){
               names(lista[[a]][[b]][[c]][[d]][[e]][[f]]) <- c("0.5","1.5","2.5","3.5","4.5","p-value")
           }
         }
       }
     }
    
  }
}





# serieA <- get_league_teams_stats("Serie A", 2023)
# serieA_teams <- unique(serieA$team_name)

for(v in 5:5){
  
  liga <- get_league_teams_stats(names(lista)[v], 2023)
  liga_teams <- unique(liga$team_name)
  
  
    for(i in 1:length(liga_teams)){ # 
     
      if(i > 1){
      lista[[v]][[i-1]][1] <- lapply(lista[[v]][[i-1]][1], function(x) x[-c(gk:14)])
      lista[[v]][[i-1]][2] <- lapply(lista[[v]][[i-1]][2], function(x) x[-c(def:14)])
      lista[[v]][[i-1]][3] <- lapply(lista[[v]][[i-1]][3], function(x) x[-c(mid:14)])
      lista[[v]][[i-1]][4] <- lapply(lista[[v]][[i-1]][4], function(x) x[-c(anf:14)])
      
      if(length(lista[[v]][[i-1]][["GK"]]) == 0){lista[[v]][[i-1]][["GK"]] <- NULL}
      if(length(lista[[v]][[i-1]][["Försvarare"]]) == 0){lista[[v]][[i-1]][["Försvarare"]] <- NULL}
      if(length(lista[[v]][[i-1]][["Mittfält"]]) == 0){lista[[v]][[i-1]][["Mittfält"]] <- NULL}
      if(length(lista[[v]][[i-1]][["Anfallare"]]) == 0){lista[[v]][[i-1]][["Anfallare"]] <- NULL}
      }
      
      team <- get_team_players_stats(liga_teams[i],2023) %>% quiet()
      names(lista[[v]])[i] <- unique(team$team_name)
      gk <- 1
      def <- 1
      mid <- 1
      anf <- 1
      ind <- 1
      
      for(j in 1:nrow(team)){
       
     
        
        result <- tryCatch(
          expr = get_player_shots(as.numeric(team[j,1]))%>% suppressMessages(),
          error = function(e) {
            message("Ett fel uppstod när du försökte hämta spelarskott")
            NULL
          }
        )
        
        if(!is.null(result)) {
    
        shots <- get_player_shots(as.numeric(team[j,1])) %>% suppressMessages()
       
        stats <- get_player_matches_stats(as.numeric(team[j,1])) %>% suppressMessages()
        stats <- stats[stats$year==2022,]
        
        s <- sum(shots$result == "Goal" | shots$result == "SavedShots"| shots$result == "SavedShot") / nrow(shots)
    
        stats$opp <- ifelse(stats$h_team == liga_teams[i],stats$a_team,stats$h_team)
        stats <- stats[!stats$position=="Sub",] # tar bort inhopp pga skevhet
        
        if(nrow(stats) > 9){
    
          pos <- names(sort(table(stats$position),decreasing = TRUE))[1]
      
          stats$avgShots90 <- (stats$shots / stats$time) * 90
          stats$teamRank <- unlist(lapply(stats$opp, FUN = team_rank, num = v))
          
          if(length(unique(stats$teamRank)) == 1){
            model <- lm(avgShots90 ~ 1 , data = stats)
            p_value <- 1
            avg_top <- sum(coefficients(model) * c(1))
            avg_bottom <- sum(coefficients(model) * c(1))
          }else{
            model <- lm(avgShots90 ~ as.factor(teamRank) , data = stats)
            p_value <- summary(model)$coefficients[2,4]
            avg_top <- sum(coefficients(model) * c(1,0))
            avg_bottom <- sum(coefficients(model) * c(1,1))
          }
          
      
          ind <- switch(pos_to_int(pos), gk, def, mid, anf)
         
         names(lista[[v]][[i]][[pos_to_int(pos)]])[ind] <- unique(stats$player_name)
          print( unique(stats$player_name))
          
          # ////////////////////////////////////////////////////////////////////////////
          
          top_odds <- 1 - ppois(c(0, 1, 2, 3, 4), lambda = avg_top)
          bottom_odds <- 1 - ppois(c(0, 1, 2, 3, 4), lambda = avg_bottom)
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[1]][1:5] <- 
            paste("ODDS O:", round((1/top_odds),3), "| | ODDS U:", round((1/(1-top_odds)),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[1]][1:5] <- 
            paste("ODDS O:", round((1/bottom_odds),3), "| | ODDS U:", round((1/(1-bottom_odds)),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[1]][[6]] <- round(p_value,3)
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[1]][[6]] <- round(p_value,3)
    
          # ////////////////////////////////////////////////////////////////////////////
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[3]][1:5] <- 
            paste("ODDS O:", round((1/(top_odds*(70/90))),3), "| | ODDS U:",round((1/(1-top_odds*(70/90))),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[3]][1:5] <- 
            paste("ODDS O:", round((1/(bottom_odds*(70/90))),3), "| | ODDS U:",round((1/(1-bottom_odds*(70/90))),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[3]][[6]] <- round(p_value,3)
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[3]][[6]] <- round(p_value,3)
          
          # ////////////////////////////////////////////////////////////////////////////
          
          top_odds <- 1 - ppois(c(0, 1, 2, 3, 4), lambda = avg_top*s)
          bottom_odds <- 1 - ppois(c(0, 1, 2, 3, 4), lambda = avg_bottom*s)
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[2]][1:5] <- 
            paste("ODDS O:", round((1/top_odds),3), "| | ODDS U:", round((1/(1-top_odds)),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[2]][1:5] <- 
            paste("ODDS O:", round((1/bottom_odds),3), "| | ODDS U:", round((1/(1-bottom_odds)),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[2]][[6]] <- paste("SOT %:",round(s,3))
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[2]][[6]] <- paste("SOT %:",round(s,3))
          
          
          # ////////////////////////////////////////////////////////////////////////////
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[4]][1:5] <- 
            paste("ODDS O:", round((1/(top_odds*(70/90))),3), "| | ODDS U:",round((1/(1-top_odds*(70/90))),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[4]][1:5] <- 
            paste("ODDS O:", round((1/(bottom_odds*(70/90))),3), "| | ODDS U:",round((1/(1-bottom_odds*(70/90))),3))
          
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[1]][[4]][[6]] <- paste("SOT %:",round(s,3))
          lista[[v]][[i]][[pos_to_int(pos)]][[ind]][[2]][[4]][[6]] <- paste("SOT %:",round(s,3))
          
     
          # ////////////////////////////////////////////////////////////////////////////
          
          if(pos_to_int(pos) == 1) gk <- gk + 1
          else if(pos_to_int(pos) == 2) def <- def + 1
          else if(pos_to_int(pos) == 3) mid <- mid + 1
          else anf <- anf + 1
           
        }
        }
      }
      
        if(i == length(liga_teams)){
            lista[[v]][[i]][1] <- lapply(lista[[v]][[i]][1], function(x) x[-c(gk:14)])
            lista[[v]][[i]][2] <- lapply(lista[[v]][[i]][2], function(x) x[-c(def:14)])
            lista[[v]][[i]][3] <- lapply(lista[[v]][[i]][3], function(x) x[-c(mid:14)])
            lista[[v]][[i]][4] <- lapply(lista[[v]][[i]][4], function(x) x[-c(anf:14)])
        
            if(length(lista[[v]][[i]][["GK"]]) == 0){lista[[v]][[i]][["GK"]] <- NULL}
            if(length(lista[[v]][[i]][["Försvarare"]]) == 0){lista[[v]][[i]][["Försvarare"]] <- NULL}
            if(length(lista[[v]][[i]][["Mittfält"]]) == 0){lista[[v]][[i]][["Mittfält"]] <- NULL}
            if(length(lista[[v]][[i]][["Anfallare"]]) == 0){lista[[v]][[i]][["Anfallare"]] <- NULL}
        }
      
        
      
      
    }
}



# P_VÄRDE UNDER 0.05

# for(a in 1:5){
#   for(b in 1:length(lista[[a]])){
#     for(c in 1:length(lista[[a]][[b]])){
#       for(d in 1:length(lista[[a]][[b]][[c]])){
#        
#        
#        # print(paste(a,b,c,d))
#         if(!(lista[[a]][[b]][[c]][[d]][[1]][[1]][6] == "NULL")){
#           if( !(is.na(lista[[a]][[b]][[c]][[d]][[1]][[1]][6] < 0.05)) & 
#              lista[[a]][[b]][[c]][[d]][[1]][[1]][6] < 0.05){
#             print(paste(names(lista[[a]][[b]][[c]][d]), " PVÄRDE=",lista[[a]][[b]][[c]][[d]][[1]][[1]][6] ))
#           }
#         }
#       }
#     }
#     
#   }
# }





jsonedit(lista[["La liga"]])



shots <- get_player_shots(8461) 
shots <- shots[-c(1:175),]
stats <- get_player_matches_stats(8607) %>% quiet()
s <- sum(shots$result == "Goal" | shots$result == "SavedShots"| shots$result == "SavedShot") / nrow(shots)




gam <- matrix(NA,3,3)
gam[1,] <- c(1,-1,3)
gam[2,] <- c(6,2,-2)
gam[3,] <- c(3,5,-3)
  

s <- seq(0,1,0.1)


for(i in 1:length(s)){
  
  print(paste("s=",s[i]))
  for(kol in 1:3){
    
    bool <- (gam[2,kol] * s[i] + (1-s[i]) * gam[3,kol]) >= gam[1,kol]
    print(bool)
    
    
  }
  
  
}
  




s <-0.5
for(i in 1:length(s)){
  
  print(paste("s=",s[i]))
  for(rad in 1:3){
    
    bool <- (gam[rad,2] * s[i] + (1-s[i]) * gam[rad,3]) <= gam[rad,1]
    print(gam[rad,2] * s[i] + (1-s[i]) * gam[rad,3])
    print(gam[rad,2])
    print(gam[rad,3])
    print(bool)
    
    
  }
  
  
}


}



# HÄMTNING AV DATA
{


# [200 000 objekt i listan, 37.7mb, PL]
num_layers <- 5
num_elements_layer <- c(380, 380, 380, 380, 306)
stats_pl <- vector(mode = 'list', num_layers)

for (i in 1:num_layers) {
  stats_pl[[i]] <- vector(mode = 'list', num_elements_layer[i])
  for (j in 1:num_elements_layer[i]) {
    stats_pl[[i]][[j]] <- vector(mode = 'list', 59)
  }
}

names(stats_pl) <- c("PL","SerieA","LaLiga","Ligue1","Bundesliga")
for(a in 1:length(stats_pl)){
  names(stats_pl[[a]]) <- paste0("Match", seq(1, length(stats_pl[[a]])))
  for(b in 1:length(stats_pl[[a]])){
    names(stats_pl[[a]][[b]]) <- c("match.id","league.id","home.team.id","away.team.id","date","season","h.odds","x.odds","a.odds",
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

# HÄMTAR ID CONV FÖR SPELARE
id_players_2023 <- readRDS(file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\id_players_2023.rds")
id_players_2023 <- unique(id_players_2023) 
id_players_2023[(id_players_2023$player_id1=="c8387671"),1] <- "b9e1436c"  # Harvey Elliott
id_players_2023[(id_players_2023$player_id1=="6a713852"),1] <- "cce694d4"  # Robert Sanchez

# HÄMTAR ID CONV FÖR MATCH
id_matches_2023 <- readRDS(file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\id_matches_2023.rds") 



# HÄMTAR MATCH ID
url_home <- "https://fbref.com/en/comps/9/2022-2023/schedule/2022-2023-Premier-League-Scores-and-Fixtures"
url_home <- read_html(url_home)
match_id <- url_home %>% html_nodes("td.left~ .left+ .left a") %>% html_attr("href")
match_id <- match_id[1:380]
PL_match_id <- match_id[str_sub(match_id,1,11) == "/en/matches"]
hteam <- url_home %>% html_nodes(".right a") %>%  html_text()
hteam <- hteam[1:380]
team_global <-  unique(hteam) # HÄMTAR TEAM ID
teamid <- url_home %>% html_nodes(".right a") %>% html_attr("href") %>% str_sub(12,19)
team_id_global <- unique(teamid)

url_home <- "https://fbref.com/en/comps/11/schedule/Serie-A-Scores-and-Fixtures"
url_home <- read_html(url_home)
match_id <- url_home %>% html_nodes("td.left~ .left+ .left a") %>% html_attr("href")
match_id <- match_id[1:380]
SERIEA_match_id <- match_id[str_sub(match_id,1,11) == "/en/matches"]
hteam <- url_home %>% html_nodes(".right a") %>%  html_text()
hteam <- hteam[1:380]
team_global <-  c(team_global,unique(hteam)) # HÄMTAR TEAM ID
teamid <- url_home %>% html_nodes(".right a") %>% html_attr("href") %>% str_sub(12,19)
teamid <- teamid[1:380]
team_id_global <- c(team_id_global,unique(teamid))

url_home <- "https://fbref.com/en/comps/12/2022-2023/schedule/2022-2023-La-Liga-Scores-and-Fixtures"
url_home <- read_html(url_home)
match_id <- url_home %>% html_nodes("td.left~ .left+ .left a") %>% html_attr("href")
match_id <- match_id[1:380]
LALIGA_match_id <- match_id[str_sub(match_id,1,11) == "/en/matches"]
hteam <- url_home %>% html_nodes(".right a") %>%  html_text()
hteam <- hteam[1:380]
team_global <-  c(team_global,unique(hteam)) # HÄMTAR TEAM ID
teamid <- url_home %>% html_nodes(".right a") %>% html_attr("href") %>% str_sub(12,19)
teamid <- teamid[1:380]
team_id_global <- c(team_id_global,unique(teamid))

url_home <- "https://fbref.com/en/comps/13/2022-2023/schedule/2022-2023-Ligue-1-Scores-and-Fixtures"
url_home <- read_html(url_home)
match_id <- url_home %>% html_nodes("td.left~ .left+ .left a") %>% html_attr("href")
match_id <- match_id[1:380]
LIGUE1_match_id <- match_id[str_sub(match_id,1,11) == "/en/matches"]
hteam <- url_home %>% html_nodes(".right a") %>%  html_text()
hteam <- hteam[1:380]
team_global <-  c(team_global,unique(hteam)) # HÄMTAR TEAM ID
teamid <- url_home %>% html_nodes(".right a") %>% html_attr("href") %>% str_sub(12,19)
teamid <- teamid[1:380]
team_id_global <- c(team_id_global,unique(teamid))

url_home <- "https://fbref.com/en/comps/20/schedule/Bundesliga-Scores-and-Fixtures"
url_home <- read_html(url_home)
match_id <- url_home %>% html_nodes("td.left~ .left+ .left a") %>% html_attr("href")
match_id <- match_id[1:306]
BUNDESLIGA_match_id <- match_id[str_sub(match_id,1,11) == "/en/matches"]
hteam <- url_home %>% html_nodes(".right a") %>%  html_text()
hteam <- hteam[1:306]
team_global <-  c(team_global,unique(hteam)) # HÄMTAR TEAM ID
teamid <- url_home %>% html_nodes(".right a") %>% html_attr("href") %>% str_sub(12,19)
teamid <- teamid[1:306]
team_id_global <- c(team_id_global,unique(teamid))

footballdata <- readRDS(file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\footballdata\\footballdata_2023_stats.rds")


get_team_formation <- function(vec) {
  
  vektor <- vec[!(vec == "Sub")]
  
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
  
  
}

get_team_id <- function(team){
  
  team <- rename_teams(team,opt=10)
  return(team_id_global[team == team_global ])
  
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




match_id <- list(PL_match_id,SERIEA_match_id,LALIGA_match_id,
                 LIGUE1_match_id,BUNDESLIGA_match_id)

x_matches <- c(rep(380,4),306)
for(ii in 1:5){
  
  if(ii %% 1 == 0){
    Sys.sleep(20)
  }
  
  for(i in 1:x_matches[[ii]]){
    
    print(i)
    if(i %% 20 == 0){
      
    }
    
    # ////////////////////////////// HÄMTAR DATA /////////////////////////////////
    
    url <- str_c("https://fbref.com", match_id[[ii]][i])
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
   
  
    
    stats_pl[[ii]][[i]][1] <- str_sub(match_id[[ii]][i],13,20) # match id
    stats_pl[[ii]][[i]][2] <- paste0("000",ii) # liga id
    stats_pl[[ii]][[i]][3] <- team_id.h # h.team id
    stats_pl[[ii]][[i]][4] <- team_id.b # a.team id
    stats_pl[[ii]][[i]][5] <- date # date
    stats_pl[[ii]][[i]][6] <- "22/23" # säsong
    
    # ODDS ////////////////////////////////////////////////////////////
    
    odds_stats <- footballdata[footballdata$match_id==stats_pl[[ii]][[i]][1],]
    
    
    stats_pl[[ii]][[i]][7] <- odds_stats$AvgCH # hemmalag odds
    stats_pl[[ii]][[i]][8] <- odds_stats$AvgCD # oavgjort odds
    stats_pl[[ii]][[i]][9] <- odds_stats$AvgCA # bortalag odds
    
    stats_pl[[ii]][[i]][10] <- odds_stats$AvgC.2.5 # över 2.5
    stats_pl[[ii]][[i]][11] <- odds_stats$AvgC.2.5.1 # under 2.5
    
    stats_pl[[ii]][[i]][12] <- odds_stats$AHCh # asien lina
    stats_pl[[ii]][[i]][13] <- odds_stats$AvgCAHH # asien odds hemmalag
    stats_pl[[ii]][[i]][14] <- odds_stats$AvgCAHA # asien odds bortalag
    
    
    
    stats_pl[[ii]][[i]][c(15:35,37:56)] <- c(teams, stats[c(TRUE, FALSE)],
                             shots_h, sot_h, poss[1], goals[1], xg[1], cards.h, referee,
                             stats[c(FALSE, TRUE)], shots_a, sot_a, poss[2], goals[2], xg[2], cards.a)
    
    
    # ////////////////////////// HÄMTAR IN SPELARESTATS //////////////////////////
  
     # HEMMALAG
     team_id <- team_id.h
     player <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary th a")) %>% html_text()
   # pos <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary .poptip+ td")) %>% html_text()
     
     # if (!(length(player) == length(pos))) {
     #   pos <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .center:nth-child(4)")) %>% html_text()
     # }
     # 
   #  min <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(6)")) %>% html_text()
     
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
     for(t in 1:length(player.id)) understat_ids[t] <- id_players_2023[which(player.id[t] == id_players_2023[,1]),2]

     pos <- c()
     xgChain <- c()
     min <- c()
     p <- 1

     understat_match_id <- id_matches_2023[which(stats_pl[[ii]][[i]][[1]] == id_matches_2023[,1]),2]
     for(p in 1:length(understat_ids)){
       suppressMessages({
       understat_player <- get_player_matches_stats(understat_ids[p])
       })
       understat_player <- understat_player[understat_player$match_id == understat_match_id,]
       if(!(nrow(understat_player)==0)){

         pos[p] <- understat_player[,8]
         xgChain[p] <- understat_player[,21]
         min[p] <- understat_player[,7]
             p <- p + 1
       }

     }
   
     # ////////////////////////////////////////////////////////////////////////////////////////
     
     h.df <- cbind(player.id,player,pos,min,goals,ass,shots,sot,tkl,npxg,xAG,xgChain,pass,yellow,red) %>%  as.data.frame()
     
     stats_pl[[ii]][[i]][[36]] <- get_team_formation(unlist(pos)) # Formation
    
     
    
     # BORTALAG
     team_id <- team_id.b
     player <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary th a")) %>% html_text()
    # pos <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary .poptip+ td")) %>% html_text()
     
     # if (!(length(player) == length(pos))) {
     #   pos <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .center:nth-child(4)")) %>% html_text()
     # }
     # 
    # min <-  url %>%  html_nodes(str_c("#stats_",team_id,"_summary tbody .right:nth-child(6)")) %>% html_text()
     
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

     for(t in 1:length(player.id)) understat_ids[t] <- id_players_2023[which(player.id[t] == id_players_2023[,1]),2]

     pos <- c()
     xgChain <- c()
     min <- c()
     p <- 1

     for(p in 1:length(understat_ids)){
       suppressMessages({
       understat_player <- get_player_matches_stats(understat_ids[p])
       })
       understat_player <- understat_player[understat_player$match_id == understat_match_id,]
       if(!(nrow(understat_player)==0)){

         pos[p] <- understat_player[,8]
         xgChain[p] <- understat_player[,21]
         min[p] <- understat_player[,7]
         p <- p + 1
       }

     }
 
     # ////////////////////////////////////////////////////////////////////////////////////////
    
     a.df <- cbind(player.id,player,pos,min,goals,ass,shots,sot,tkl,npxg,xAG,xgChain,pass,yellow,red) %>%  as.data.frame()
     
     stats_pl[[ii]][[i]][[57]] <- get_team_formation(unlist(pos)) # Formation

  
     # ////////////////////////// LÄGGER IN SPELARESTATS /////////////////////////
     
     # lägger in vektor med längd = antal spelare
     stats_pl[[ii]][[i]][[58]] <- vector(mode='list',nrow(h.df)) 
     stats_pl[[ii]][[i]][[59]] <- vector(mode='list',nrow(a.df))
     names(stats_pl[[ii]][[i]][[58]]) <- h.df[,2]
     names(stats_pl[[ii]][[i]][[59]]) <- a.df[,2]
     
     for(j in 1:nrow(h.df)) stats_pl[[ii]][[i]][[58]][[j]] <- unlist(as.vector(h.df[j,], mode='list'), recursive = FALSE)
  
     
     for(j in 1:nrow(a.df)) stats_pl[[ii]][[i]][[59]][[j]] <- unlist(as.vector(a.df[j,], mode='list'), recursive = FALSE)
        
     # ///////////////////////////////////////////////////////////////////////////
     
    
    
  }
}

}

#saveRDS(stats_pl, file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\2023_all_stats.rds")
# saveRDS(stats_pl, file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\PL2023_all_stats.rds")
# saveRDS(lista, file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\TOP5_oddsshots.rds")
# saveRDS(stats_pl, file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\PLstats.rds")
# stats_pl <- readRDS(file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\PLstats.rds") 
#stats_pl <- readRDS(file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\PL2023_all_stats.rds") 
stats_shots <- readRDS(file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\TOP5_oddsshots.rds") 
  
stats_pl <- readRDS(file = "C:\\Users\\willi\\OneDrive\\Skrivbord\\r_objekt\\PLstats.rds") 

# värdering på lagen
get_xGD <- function(colname = "opponent"){
  
  url_home <- "https://fbref.com/en/comps/9/Premier-League-Stats"
  url_home <- read_html(url_home)
  xGD <- url_home %>% html_nodes(".force_mobilize .right:nth-child(15) , img+ a") %>% html_text()
  xGD <- xGD[2:41]
  mat <- matrix(NA,20,2)
  colnames(mat) <- c(colname,"xgd")
  mat[,1] <- unlist(sapply(xGD[c(TRUE,FALSE)],rename_teams, opt = 2))
  mat[,2] <-  sapply(xGD[c(FALSE,TRUE)], function(x) ifelse(substr(x, 1, 1) == "+", gsub("\\+", "", x), x))
  return(as.data.frame(mat))
  
}

# värdering på domaren
get_ref_stats <- function(){
  
  ref_stats_2022 <- read.csv2("https://raw.githubusercontent.com/Oggestor/data/main/ref_2022.csv") %>% 
    mutate(referee = sub("^\\d+\\.(.*)", "\\1", Referee)) %>% 
    mutate(Fouls = round(Apps * as.numeric(Fouls.pg))) %>% 
    mutate(Yellow = round(Apps * as.numeric(Yel.pg))) %>% 
    select(-c(3:9))
  
  ref_stats_2023 <- read.csv2("https://raw.githubusercontent.com/Oggestor/data/main/ref_2023.csv") %>% 
    mutate(referee = sub("^\\d+\\.(.*)", "\\1", Referee)) %>% 
    mutate(Fouls = round(Apps * as.numeric(Fouls.pg))) %>% 
    mutate(Yellow = round(Apps * as.numeric(Yel.pg))) %>% 
    select(-c(3:9))
  
  ref_stats <- bind_rows(ref_stats_2022, ref_stats_2023) %>%
    group_by(referee) %>%
    summarize(Apps = sum(Apps), Fouls = sum(Fouls), Yellow = sum(Yellow)) %>% 
    mutate(Fouls.pg = (Fouls/Apps)) %>% 
    mutate(Yellow.pg = (Yellow/Apps)) %>% 
    filter(Apps >= 10) %>% 
    mutate(fouls.pg = (Fouls.pg - mean(Fouls.pg)) / sd(Fouls.pg))
  
  # url_home <- "https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/9075/Stages/20934/RefereeStatistics/England-Premier-League-2022-2023"
return(as.data.frame(ref_stats[,c(1,7)]))
  
}

overall_p <- function(my_model) {
  
  if (!(is.null(summary(my_model)$fstatistic))) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
  
  } else {
    return(NULL)
  }
  
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

matchup <- function(team1, team2, df, odds = TRUE, p = 0.1, ref = "Anthony Taylor", mod = "multi"){
  
 team1 <- rename_teams(team1,2)
 team2 <- rename_teams(team2,2)

  stats_t1 <- as.data.frame(do.call(rbind, stats_pl[[team1]][1:35]))
  stats_t2 <- as.data.frame(do.call(rbind, stats_pl[[team2]][1:35]))
  for (kolumn in c(1,2,20)){
    stats_t1[, kolumn] <- (as.character(stats_t1[, kolumn]))
    stats_t2[, kolumn] <- (as.character(stats_t2[, kolumn]))
  } 
 
  colnames(stats_t1)[1] <- "team"
  colnames(stats_t2)[1] <- "team"
  stats_t1[, 20] <- stri_trans_general(stats_t1[, 20], "Latin-ASCII")
  stats_t2[, 20] <- stri_trans_general(stats_t2[, 20], "Latin-ASCII")
  
  get_xgd <- get_xGD()
  colnames(get_xgd) <- c("xgd","team")
  stats_t1$team <- "Aston Villa"
  stats_t2$team <- "Aston Villa"
 
  get_ref <- get_ref_stats()
  merge_stats_1 <- merge(stats_t1,get_xgd, by = "team") 
  merge_stats_2 <- merge(stats_t2,get_xgd, by = "team")

  
  merge_stats_1 <- merge(merge_stats_1,get_ref, by = "referee", all.x = TRUE) %>% 
                         relocate(referee, .after = xg)
  merge_stats_2 <- merge(merge_stats_2,get_ref, by = "referee", all.x = TRUE) %>% 
                         relocate(referee, .after = xg)

  # Lägger in 0 på fouls index pga domaren har dömt för få matcher (<10)
  merge_stats_1$fouls.pg <- ifelse(is.na(merge_stats_1$fouls.pg), 0, merge_stats_1$fouls.pg)
  merge_stats_2$fouls.pg <- ifelse(is.na(merge_stats_2$fouls.pg), 0, merge_stats_2$fouls.pg)
  
  

  
  for (kolumn in c(3:19,21:38)){
    merge_stats_1[, kolumn] <- as.numeric(as.character(merge_stats_1[, kolumn]))
    merge_stats_2[, kolumn] <- as.numeric(as.character(merge_stats_2[, kolumn]))
  } 
  
  stats_t1 <- merge_stats_1[,c(2,3,7,11:13,15,16,21,25,29:31,33,34,37,38)]
  stats_t2 <- merge_stats_2[,c(2,3,7,11:13,15,16,21,25,29:31,33,34,37,38)]
  stats_t1$`home ground` <- as.factor(stats_t1$`home ground`)
  stats_t2$`home ground` <- as.factor(stats_t2$`home ground`)
  
  colnames(stats_t1) <- make.names(colnames(stats_t1))
  colnames(stats_t2) <- make.names(colnames(stats_t2))
                                   
  # summary(lm(sot ~ xgd, data = stats_t2))   
  # summary(lm(A_sot ~ xgd, data = stats_t1)) 
  
  # MODELLER ///////////////////////////////////////////////////////////////////
  
  
  # Linjär reg /////////////////////////////////////////////////////////////////
  
  if (mod == "lm"){
    modeller <- lapply(my.list<-vector(mode = 'list',2),function(x) 
                                x <- vector(mode='list',14))
    names(modeller) <- c("team_1","team_2")
    for(a in 1:2) names(modeller[[a]]) <- colnames(stats_t1)[2:15]
    
    for (team in names(modeller)){
      
      if (team == "team_1"){
        stats <- stats_t1
      } else {
        stats <- stats_t2
      }
      
      
      for (respons in colnames(stats_t1)[2:15]) {
      
      
      
      # /////////////////////////////// 3 variabler ////////////////////////////////
      
      modell <- lm(as.formula(paste0(respons, " ~ xgd +home.ground + fouls.pg")), data = stats)
      p_values_alla <- summary(modell)$coefficients[, 4]
        
      if (all(p_values_alla[2:4] < p)) {
        modeller[[team]][[respons]] <- modell
        next
      }  
      
      # /////////////////////////////// 2 variabler ////////////////////////////////
      
      modell_1 <- lm(as.formula(paste0(respons, " ~ xgd + home.ground")), data = stats)
      p_values_alla_1 <- summary(modell_1)$coefficients[, 4]
      f_value_1 <- summary(modell_1)$fstatistic[1]
      
      modell_2 <- lm(as.formula(paste0(respons, " ~ xgd + fouls.pg")), data = stats)
      p_values_alla_2 <- summary(modell_2)$coefficients[, 4]
      f_value_2 <- summary(modell_2)$fstatistic[1]
      
      modell_3 <- lm(as.formula(paste0(respons, " ~ home.ground + fouls.pg")), data = stats)
      p_values_alla_3 <- summary(modell_3)$coefficients[, 4]
      f_value_3 <- summary(modell_3)$fstatistic[1]
      
      valid_models <- c()
      
      # Kontrollera om p-värdena är mindre än p för någon modell
      if (all(p_values_alla_1[2:3] < p)) {
        valid_models <- c(valid_models, "Model 1")
      }
      
      if (all(p_values_alla_2[2:3] < p)) {
        valid_models <- c(valid_models, "Model 2")
      }
      
      if (all(p_values_alla_3[2:3] < p)) {
        valid_models <- c(valid_models, "Model 3")
      }
      
      # Kontrollera om det finns minst en giltig modell
      if (length(valid_models) > 0) {
    
        # Skapa en vektor med de giltiga modellernas F-värden
        valid_f_values <- c(ifelse("Model 1" %in% valid_models, f_value_1, -Inf),
                            ifelse("Model 2" %in% valid_models, f_value_2, -Inf),
                            ifelse("Model 3" %in% valid_models, f_value_3, -Inf))
        
        # Hitta modellen med högst F-värde bland de giltiga modellerna
        best_model <- switch(which.max(valid_f_values),
                             "Model 1" = modell_1,
                             "Model 2" = modell_2,
                             "Model 3" = modell_3)
        
        modeller[[team]][[respons]] <- best_model
        next
        
      } else {
       
        # /////////////////////////////// 1 variabler //////////////////////////////
        
        modell_1 <- lm(as.formula(paste0(respons, " ~ xgd")), data = stats)
        p_values_alla_1 <- summary(modell_1)$coefficients[, 4]
        f_value_1 <- summary(modell_1)$fstatistic[1]
        
        modell_2 <- lm(as.formula(paste0(respons, " ~ fouls.pg")), data = stats)
        p_values_alla_2 <- summary(modell_2)$coefficients[, 4]
        f_value_2 <- summary(modell_2)$fstatistic[1]
        
        modell_3 <- lm(as.formula(paste0(respons, " ~ home.ground")), data = stats)
        p_values_alla_3 <- summary(modell_3)$coefficients[, 4]
        f_value_3 <- summary(modell_3)$fstatistic[1]
        
        valid_models <- c()
        
        # Kontrollera om p-värdena är mindre än p för någon modell
        if (all(p_values_alla_1[2] < p)) {
          valid_models <- c(valid_models, "Model 1")
        }
        
        if (all(p_values_alla_2[2] < p)) {
          valid_models <- c(valid_models, "Model 2")
        }
        
        if (all(p_values_alla_3[2] < p)) {
          valid_models <- c(valid_models, "Model 3")
        }
        
        # Kontrollera om det finns minst en giltig modell
        if (length(valid_models) > 0) {
          
          # Skapa en vektor med de giltiga modellernas F-värden
          valid_f_values <- c(ifelse("Model 1" %in% valid_models, f_value_1, -Inf),
                              ifelse("Model 2" %in% valid_models, f_value_2, -Inf),
                              ifelse("Model 3" %in% valid_models, f_value_3, -Inf))
          
          # Hitta modellen med högst F-värde bland de giltiga modellerna
          best_model <- switch(which.max(valid_f_values),
                               "Model 1" = modell_1,
                               "Model 2" = modell_2,
                               "Model 3" = modell_3)
          
          modeller[[team]][[respons]] <- best_model
          next
          
        } else {
          
          # /////////////////////////////// 0 variabler //////////////////////////////
          
          modeller[[team]][[respons]] <- lm(as.formula(paste0(respons, " ~ 1")), data = stats)
          
          
        }
        
      }
      
      }
    }
    
  } else {
    
   # MultiLevel /////////////////////////////////////////////////////////////////
    
    all_stats <- get_big_df_2(get_xgd, get_ref)
    
   
    model_shots <- lmerTest::lmer(shots ~  home.ground + (1 + xgd | org.team), data = all_stats)
   # model_shots <- lmerTest::lmer(shots ~  xgd + home.ground + (1 | org.team), data = all_stats)
    model_shots_A <- lmerTest::lmer(A_shots ~  home.ground + (1 + xgd | org.team), data = all_stats)
   # model_shots_A <- lmerTest::lmer(A_shots ~  xgd + home.ground + (1 | org.team), data = all_stats)
    
    model_sot <- lmerTest::lmer(sot ~  home.ground + (1 + xgd | org.team), data = all_stats)
   # model_sot <- lmerTest::lmer(sot ~  xgd + home.ground + (1 | org.team), data = all_stats)
    model_sot_A <- lmerTest::lmer(A_sot ~  home.ground + (1 + xgd | org.team), data = all_stats)
    # model_sot_A <- lmerTest::lmer(A_sot ~  xgd + home.ground + (1 | org.team), data = all_stats)
    
    model_fouls <- lmerTest::lmer(fouls ~  fouls.pg + (1 | org.team), data = all_stats)
    model_fouls_A <- lmerTest::lmer(A_fouls ~  fouls.pg + (1 | org.team), data = all_stats)
    
    model_throws <- lmerTest::lmer(thorow.ins ~ xgd  + (1 | org.team), data = all_stats)
    model_throws_A <- lmerTest::lmer(A_thorow.ins ~ xgd  + (1 | org.team), data = all_stats)
    
    model_goalkicks <- lmerTest::lmer(goal.kicks ~ xgd + home.ground + (1 | org.team), data = all_stats)
    model_goalkicks_A <- lmerTest::lmer(A_goal.kicks ~ xgd + home.ground + (1 | org.team), data = all_stats)
    
    model_tackles <- lmerTest::lmer(tackles ~ 1 + (1 | org.team), data = all_stats) # endast intercept
    model_tackles_A <- lmerTest::lmer(A_tackles ~ 1 + (1 | org.team), data = all_stats) # endast intercept
    
    model_offside <- lmerTest::lmer(offsides ~ 1 + (1 | org.team), data = all_stats) # encdast intercept
    model_offside_A <- lmerTest::lmer(A_offsides ~ 1 + (1 | org.team), data = all_stats) # encdast intercept
    

    # summary(model_offside_A)
    # ranova(model_offside)
    # drop1(model)
    # performance::icc(model)
    
    
  }
  
  mat_m <- matrix(NA,8,5)
  colnames(mat_m) <- c(str_c(team1,"(for)"),str_c(team1,"(against)"),
                     str_c(team2,"(for)"),str_c(team2,"(against)"),"Total")
  rownames(mat_m) <- c(colnames(stats_t1)[2:8],"free.kicks")
  
  
  if (any(ref == get_ref[,1])) {
  ny_data_1 <- data.frame(xgd = as.numeric(get_xgd[get_xgd == team2,2]),
                          home.ground = factor("yes", levels = levels(stats_t1$home.ground)),
                          fouls.pg = as.numeric(get_ref[get_ref == ref,2]),
                          org.team = team1)
  
  ny_data_2 <- data.frame(xgd = as.numeric(get_xgd[get_xgd == team1,2]),
                          home.ground = factor("no", levels = levels(stats_t2$home.ground)),
                          fouls.pg = as.numeric(get_ref[get_ref == ref,2]),
                          org.team = team2)
  } else {
  print(paste("Domare: ",ref," har ej dömt fler än 10 matcher i PL och kan ej användas i analysen"))
  ny_data_1 <- data.frame(xgd = as.numeric(get_xgd[get_xgd == team2,2]),
                          home.ground = factor("yes", levels = levels(stats_t1$home.ground)),
                          fouls.pg = 0,
                          org.team = team1)
  
  ny_data_2 <- data.frame(xgd = as.numeric(get_xgd[get_xgd == team1,2]),
                          home.ground = factor("no", levels = levels(stats_t2$home.ground)),
                          fouls.pg = 0,
                          org.team = team2)  
  }
  
  pred_team_1 <- c()
  pred_team_2 <- c()
  pred_team_1_A <- c()
  pred_team_2_A <- c()

  if (mod == "lm") {
    
    for (i in 1:7) {
      pred_team_1[i] <- predict(modeller[["team_1"]][[rownames(mat_m)[i]]], newdata = ny_data_1)
      pred_team_2[i] <- predict(modeller[["team_2"]][[rownames(mat_m)[i]]], newdata = ny_data_2)
    } 
    for (i in 1:7) {
      pred_team_1_A[i] <- predict(modeller[["team_1"]][[colnames(stats_t1)[i+8]]], newdata = ny_data_1)
      pred_team_2_A[i] <- predict(modeller[["team_2"]][[colnames(stats_t2)[i+8]]], newdata = ny_data_2)
    }
  } else {
    
    pred_team_1[1] <- predict(model_fouls, newdata = ny_data_1)
    pred_team_2[1] <- predict(model_fouls, newdata = ny_data_2)
    pred_team_1_A[1] <- predict(model_fouls_A, newdata = ny_data_1)
    pred_team_2_A[1] <- predict(model_fouls_A, newdata = ny_data_2)
    
    pred_team_1[2] <- predict(model_tackles, newdata = ny_data_1)
    pred_team_2[2] <- predict(model_tackles, newdata = ny_data_2)
    pred_team_1_A[2] <- predict(model_tackles_A, newdata = ny_data_1)
    pred_team_2_A[2] <- predict(model_tackles_A, newdata = ny_data_2)
    
    pred_team_1[3] <- predict(model_offside, newdata = ny_data_1)
    pred_team_2[3] <- predict(model_offside, newdata = ny_data_2)
    pred_team_1_A[3] <- predict(model_offside_A, newdata = ny_data_1)
    pred_team_2_A[3] <- predict(model_offside_A, newdata = ny_data_2)
    
    pred_team_1[4] <- predict(model_goalkicks, newdata = ny_data_1)
    pred_team_2[4] <- predict(model_goalkicks, newdata = ny_data_2)
    pred_team_1_A[4] <- predict(model_goalkicks_A, newdata = ny_data_1)
    pred_team_2_A[4] <- predict(model_goalkicks_A, newdata = ny_data_2)
    
    pred_team_1[5] <- predict(model_throws, newdata = ny_data_1)
    pred_team_2[5] <- predict(model_throws, newdata = ny_data_2)
    pred_team_1_A[5] <- predict(model_throws_A, newdata = ny_data_1)
    pred_team_2_A[5] <- predict(model_throws_A, newdata = ny_data_2)
    
    pred_team_1[6] <- predict(model_shots, newdata = ny_data_1)
    pred_team_2[6] <- predict(model_shots, newdata = ny_data_2)
    pred_team_1_A[6] <- predict(model_shots_A, newdata = ny_data_1)
    pred_team_2_A[6] <- predict(model_shots_A, newdata = ny_data_2)
    
    pred_team_1[7] <- predict(model_sot, newdata = ny_data_1)
    pred_team_2[7] <- predict(model_sot, newdata = ny_data_2)
    pred_team_1_A[7] <- predict(model_sot_A, newdata = ny_data_1)
    pred_team_2_A[7] <- predict(model_sot_A, newdata = ny_data_2)
    
    
    
  }
  
  mat_m[1:7,1] <- pred_team_1
  mat_m[1:7,2] <- pred_team_1_A
  mat_m[1:7,3] <- pred_team_2
  mat_m[1:7,4] <- pred_team_2_A
  mat_m[8,1:2] <- mat_m[1,3:4] + mat_m[3,3:4] - 0.14 # Frispark (fouls+offside-straff)
  mat_m[8,3:4] <- mat_m[1,1:2] + mat_m[3,1:2] - 0.14 # Frispark (fouls+offside-straff)
  mat_m[,5] <- (((mat_m[,1]+ mat_m[,4])/ 2) + 
                ((mat_m[,2]+ mat_m[,3])/ 2))

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
  for (i in seq(1, nrow(dff), 24)) {
    subset_df <- dff[i:(i+23), ]
    if (is.null(df_delat_m)) {
      df_delat_m <- subset_df
    } else {
      df_delat_m <- cbind(df_delat_m, subset_df)
    }
  }
  
  
  if(mod == "lm"){
    terms <- c(mapply(terms, modeller[[1]], SIMPLIFY = FALSE),
               mapply(terms, modeller[[2]], SIMPLIFY = FALSE))
    
    terms_string <- unlist(lapply(terms, function(x){ paste(attr(x, "term.labels"), collapse =" + ")}))
    
    p_values_list <- c(mapply(function(model) { summary(model)$coefficients[-1, 4] }, modeller[[1]], SIMPLIFY = FALSE),
                       mapply(function(model) { summary(model)$coefficients[-1, 4] }, modeller[[2]], SIMPLIFY = FALSE))
    p_values_string <- sapply(p_values_list, function(p_values) {
      formatted_p_values <- sprintf("%.3f", p_values)
      paste(formatted_p_values, collapse = ", ")
    })
    
    p_values_F <- c(mapply(function(model) { sprintf("%.3f",overall_p(model))}, modeller[[1]], SIMPLIFY = TRUE),
                    mapply(function(model) { sprintf("%.3f",overall_p(model))}, modeller[[2]], SIMPLIFY = TRUE))
    p_values_F_string <- unlist(lapply(p_values_F, function(x) ifelse(identical(x, character(0)), "", x)))
  
    
    mod_info <- c()
    for (i in 1:length(terms_string)) {
      mod_info <- c(mod_info, terms_string[i], p_values_string[i], p_values_F_string[i])
    }
    mod_info <- split(mod_info, ceiling(seq_along(mod_info) / (length(mod_info) / 4)))
    
    
    
    df_complete <- cbind(rep(rownames(mat_m), each = 3),df_delat_m,
                      c(mod_info[[1]],rep("",3)), c(mod_info[[2]],rep("",3)),
                      c(mod_info[[3]],rep("",3)), c(mod_info[[4]],rep("",3)))
                          
    colnames(df_complete) <- c("Action",str_c("Line ",team1),"Odds overr","Odds underr",
                            str_c("Line ",team2),"Odds oveer","Odds undeer",
                            str_c("Line Total"),"Odds over","Odds under",
                            "Info (For)","Info (Against)","Info (for)","Info (against)")
    
    df_complete <- df_complete %>%
      relocate(`Info (For)`, .after = Action) %>% 
      relocate(`Info (Against)`, .after = `Info (For)`) %>% 
      relocate(`Info (for)`, .after = `Odds underr`) %>% 
      relocate(`Info (against)`, .after = `Info (for)`) 
    
  } else {
    
    df_complete <- cbind(rep(rownames(mat_m), each = 3),df_delat_m)
    
    colnames(df_complete) <- c("Action",str_c("Line ",team1),"Odds overr","Odds underr",
                               str_c("Line ",team2),"Odds oveer","Odds undeer",
                               str_c("Line Total"),"Odds over","Odds under")
    
  }
  # PRINTS /////////////////////////////////////////////////////////////////////
  

 
  if(odds == TRUE & mod == "lm"){
    
 # rows <- c(2,3,5,6,8,9,11,12,14,15,17,18,20,21)
 # cols <- c(2,3,7,8)
  

  kbl(df_complete,linesep = "",
      digits = 2) %>%
    kable_classic() %>%
    row_spec(0, font_size = 15, bold = TRUE) %>% 
    column_spec(c(5,6,10,11,13,14), italic  = TRUE ) %>% 
    row_spec(c(1:3,7:9,13:15,19:21),background = "#FFD580",bold = FALSE) %>%
    collapse_rows(columns = 1, latex_hline = "none", valign = "middle")%>%
    row_spec(c(4:6,10:12,16:18,22:24),background = "#c4e6bb",bold = FALSE) %>% 
    column_spec(1, border_right = TRUE, bold = TRUE) %>% 
  #  cell_spec(rows = c(2,3,5,6,8,9,11,12,14,15,17,18,20,21),cols = c(2,3,7,8),color  = "red") %>%
    column_spec(c(6,11), border_right = TRUE, bold = FALSE) %>%
    column_spec(c(2,3,7,8), width = "20cm") %>% 
    column_spec(c(4,9,12), bold = TRUE) 
  } else if (odds == TRUE & mod == "multi") {
    
    kbl(df_complete,linesep = "",
        digits = 2) %>%
      kable_classic() %>%
      row_spec(0, font_size = 15, bold = TRUE) %>% 
      column_spec(c(3,4,6,7,9,10), italic  = TRUE ) %>% 
      row_spec(c(1:3,7:9,13:15,19:21),background = "#FFD580",bold = FALSE) %>%
      collapse_rows(columns = 1, latex_hline = "none", valign = "middle")%>%
      row_spec(c(4:6,10:12,16:18,22:24),background = "#c4e6bb",bold = FALSE) %>% 
      column_spec(1, border_right = TRUE, bold = TRUE) %>% 
      column_spec(c(4,7), border_right = TRUE, bold = FALSE) %>%
      column_spec(c(2,5,8), bold = TRUE) 
    
    
  } else {


  kbl(mat_m,
      booktabs = T,linesep = "",
      longtable = T, digits = 2) %>%
    kable_classic() %>%
    row_spec(0, font_size = 15, bold = TRUE) %>% 
    column_spec(1, border_right = TRUE, bold = TRUE) %>% 
    column_spec(2, image = spec_boxplot(as.list(stats_t1[2:8]),col = "black",res=200, same_lim = FALSE,height = 75)) %>% 
    column_spec(4, image = spec_boxplot(as.list(stats_t1[9:15]),col = "black",res=200, same_lim = FALSE,height = 75)) %>% 
    column_spec(6, image = spec_boxplot(as.list(stats_t2[2:8]),col = "black",res=200, same_lim = FALSE,height = 75)) %>% 
    column_spec(8, image = spec_boxplot(as.list(stats_t2[9:15]),col = "black",res=200, same_lim = FALSE,height = 75)) %>% 
    column_spec(c(2,3,6,7),background = "#c4e6bb",bold = TRUE) %>% 
    column_spec(c(4,5,8,9),background = "#FFD580",bold = FALSE) %>%
    column_spec(10,background = "#F47174",bold = TRUE) %>%
    kable_styling(latex_options = c("repeat_header","striped"))
  }

}


matchup("Leicester","West Ham",
        stats_pl, odds = TRUE, ref = "Simon Hooper", p = 0, mod = "multi")



team1 <- "Leicester"
team2 <-  "West Ham"
df <- stats_pl




dfff <- as.data.frame(do.call(rbind, stats_pl[[rename_teams("Bournemouth",2)]][1:35]))
for (kolumn in c(3:19,21:36)) dfff[, kolumn] <- as.numeric(as.character(dfff[, kolumn]))
rownames(dfff) <- (i*35-34):(i*35)

dfff$free.kicks <- dfff$fouls + dfff$offsides - 0.14
dfff$A_free.kicks <- dfff$A_fouls + dfff$A_offsides - 0.14

df_2 <- dfff
  

dfff <- as.data.frame(do.call(rbind, stats_pl[[rename_teams("Everton",2)]][1:35]))
for (kolumn in c(3:19,21:36)) dfff[, kolumn] <- as.numeric(as.character(dfff[, kolumn]))
rownames(dfff) <- (i*35-34):(i*35)

dfff$free.kicks <- dfff$fouls + dfff$offsides - 0.14
dfff$A_free.kicks <- dfff$A_fouls + dfff$A_offsides - 0.14








#-------------------------------------------------------------------------------
#-------------------------------------XGBOOST-----------------------------------
#-------------------------------------------------------------------------------
{
set.seed(990420)
train_ind <- sample(nrow(big_df),nrow(big_df)*0.7)
df_xg_test<- big_df[-train_ind,c(3:19,21:36)]
df_xg_train <- big_df[train_ind,c(3:19,21:36)]
df_xg_train <- df_xg_train[!is.na(df_xg_train$offsides), ]


# 88 !!!! %%%%%%%

print(data.matrix(df_xg_train))
# test
model <- xgboost(data = data.matrix(df_xg_train[,-9]),
                      label = df_xg_train$offsides,
                      max.depth = 3,
                      nround = 10,
                      eta = 0.2,
                      gamma = 0.8,
                      colsample_bytree = 1,
                      min_child_weight = 1,
                      subsample = 1
                      ) %>% quiet()

p<-predict(model, newdata = data.matrix(df_xg_test[,-9]))
true_labels <- df_xg_test$offsides

# Calculate evaluation metrics
mse <- mean((true_labels - p)^2)  # Mean Squared Error
rmse <- sqrt(mse)  # Root Mean Squared Error
mae <- mean(abs(true_labels - p))  # Mean Absolute Error
r2 <- 1 - sum((true_labels - p)^2) / sum((true_labels - mean(true_labels))^2)  # R-squared

# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared (R2):", r2, "\n")




summary(model)
library(xgboost)
library(ggplot2)
library(tidyverse)
library(caret)


imp <- xgb.importance(colnames(df_xg_train)[-9],model = model)
print(imp)
# 
# xgb.importance(model = model) %>%
#   ggplot(aes(x = fct_reorder(Feature, Gain), y = Gain)) +
#   geom_pointrange(aes(ymin = 0, ymax = Gain), color = "cadetblue", size = .3) +
#   theme_minimal() +
#   coord_flip() +
#   labs(y = "Relativ variabelvikt", x = "", title = "Viktiga variabler för byggbranschen, XGBoosting")
# 



# 
# #Skapa grid med kombinationer av parametervärden att prova
# grid <- expand.grid(max_depth = seq(2,10,1),
#                     nrounds = seq(2,10,1),
#                     eta = seq(0.1,0.9,0.1),
#                     gamma = seq(0.1,0.9,0.1),
#                     colsample_bytree = 1,
#                     min_child_weight =1,
#                     subsample = 1)
# 
# # Definiera träningskontrollen
# ctrl <- trainControl(method = "cv", number = 5)
# 
# # Anropa xgb.train() i train()
# xgb_model <- train(offsides ~ ., data = df_xg_train,
#                    trControl = ctrl,
#                    tuneGrid = grid,
#                    method = "xgbTree",
#                    na.action = na.omit,
#                    nthread = 2,
#                    verbose = 0)


}









