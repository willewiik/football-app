


library(RJSONIO)
library(httr)
library(stringr)
library(dplyr)




BTTS <- 11942
double_chanse <- 12220
ht <- 11927
ht_goals <- 11928
DNB <- 11929
handicap_3 <- 16819
asian <- 12319
home_goals <- 11930
away_goals <- 11931
cornerns <- 19260
cards <- 19261
red_card <- 18919
penalty <- 18899
moneyline <- 12579
OverUnder <- 12580


pl <- "england/premier_league"
serieA <- "italy/serie_a"
laliga <- "spain/la_liga"
ligue1 <- "france/ligue_1"
bundesliga <- "germany/bundesliga"

get_odds_kambi <- function(comp, category=12579) {

  
  Sys.sleep(3)
  url <-  str_c("https://eu-offering.kambicdn.org/offering/v2018/ub/listView/football/",
          comp,".json?lang=en_US&market=IT&category=",category)
  
  res <-  GET(url, fileEncoding = "UTF-8")
  json_file <- fromJSON(rawToChar(res$content))
  
  
  return(json_file)

}




home <- lapply(json_file$events, function(x){x$event$homeName})  %>% unlist() %>%
  rename_teams(., from = "kambi", to = "fbref")
away <- lapply(json_file$events, function(x){x$event$awayName}) %>% unlist() %>% 
  rename_teams(., from = "kambi", to = "fbref")


val1 <- (hteam == home)
val2 <- (ateam == away)

event <- intersect(which(val1), which(val2))
h_odds <- json_file$events[[1]]$betOffers[[1]]$outcomes[[1]]$odds/1000
x_odds <- json_file$events[[1]]$betOffers[[1]]$outcomes[[2]]$odds/1000
a_odds <- json_file$events[[1]]$betOffers[[1]]$outcomes[[3]]$odds/1000






