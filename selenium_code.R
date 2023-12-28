library(RSelenium)
library(wdman)
library(netstat)
library(dplyr)
library(stringr)
library(rvest)






run_selenium <- function(hteam = "Burnley", ateam = "Manchester City", league = "PL",
                         browser = "firefox") {
  
  
  if(browser == "chrome") {
    
    remote_driver <- rsDriver(browser = "chrome",
                              chromever = "114.0.5735.90",
                              port = free_port())
  } else {
    
    remote_driver <- rsDriver(browser = "firefox",
                          chromever = NULL,
                          verbose = F,
                          port = free_port())
  }

  
  
  remDrv <- remote_driver$client
  remDrv$maxWindowSize()
  
  if(league == "PL") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/england/premier_league/all/matches"
  else if(league == "Serie A") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/italy/serie_a/all/matches"
  else if(league == "La Liga") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/spain/la_liga/all/matches"
  else if(league == "Bundesliga") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/germany/bundesliga/all/matches"
  else if(league == "Ligue 1") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/france/ligue_1/all/matches"
  
  remDrv$navigate(unibet_url)
  
  Sys.sleep(3)
  # click accept cookies
  remDrv$findElements("id", "CybotCookiebotDialogBodyLevelButtonLevelOptinAllowAll")[[1]]$clickElement()
  
  xpath_selector <- sprintf("//div[@class='_6548b']//div[@class='c539a' and @data-test-name='teamName' and contains(., '%s')]",
                            hteam)
  # Click the match
  remDrv$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
  
  
  
  # ---------------------------- TEAM SHOTS AND SOT --------------------------------
  Sys.sleep(2)
  # Click Match and Team Shots
  
  markets <-  remDrv$findElements("class name", "KambiBC-filter-menu__option")
  markets <- lapply(markets, function(x) x$getElementText()) %>% unlist()
  
  if(!any("Skott i matchen och för laget" == markets)) stop("Unibet did not have market 'Skott i matchen och för laget'")
  
  index_shots <- which("Skott i matchen och för laget" == markets)
  xpath_selector <- sprintf("//*[starts-with(@id, 'react-tabs-')]/div[1]/div[1]/div/div/div/div/ul/li[%s]", index_shots)
  remDrv$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
  
  
  
  
  # XPath selector for the button with the specified class
  xpath_selector <- "//button[@class='KambiBC-outcomes-list__toggler-toggle-button down']"
  
  # Find the button "Visa lista" to show more odds
  button_element <- remDrv$findElements(using = "xpath", value = xpath_selector)
  Sys.sleep(1)
  # Check if the button element is found
  if (!(length(button_element) == 0)) {
    remDrv$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
  } else {
    print("Button not found.")
  }
  
  
  html_content <- remDrv$getPageSource()[[1]]
  html_parsed <- read_html(html_content)
  
  
  css_selector_odds <- "div.sc-iBYQkv.hYDhjM"
  css_selector_over <- "div.sc-dkrFOg.erQOWi .sc-hLBbgP.ermjoq"
  css_selector_value <- "div.sc-dkrFOg.erQOWi .sc-eDvSVe.hfwXzz"
  css_selector_subtitle <- "h3.KambiBC-bet-offer-subcategory__label span"
  
  odds <- html_text(html_nodes(html_parsed, css_selector_odds))
  over <- html_text(html_nodes(html_parsed, css_selector_over))
  value <- html_text(html_nodes(html_parsed, css_selector_value))
  subtitles <- html_text(html_nodes(html_parsed, css_selector_subtitle))
  
  
  
  
  sekvens_o <- c()
  sekvens_u <- c()
  odds_o <- c()
  odds_u <- c()
  df_odds <- data.frame(team = character(),
                        type = character(),
                        line = numeric(),
                        over_odds = numeric(),
                        under_odds = numeric(),
                        stringsAsFactors = FALSE)
  
  process_odds_data <- function(type, team) {
    ind <- which(over != over[1])[1] - 1
    
    line <- value[1:ind]
    odds_o <- odds[1:ind]
    odds_u <- odds[(ind + 1):(ind + ind)]
    
    df_odds <<- rbind(df_odds, data.frame("team" = rep(team, ind),
                                          "type" = rep(type, ind),
                                          "line" = line,
                                          "over odds" = odds_o,
                                          "under odds" = odds_u))
    
    # Stop if it is last iteration
    if(!(length(over)/2 == ind)) {
      over <<- over[(1 + ind * 2):length(over)]
      odds <<- odds[(1 + ind * 2):length(odds)]
      value <<- value[(1 + ind * 2):length(value)]
    }
  }
  
  
  print("hje")
  
  process_odds_data("Sot", "total")
  process_odds_data("Sot", "hteam")
  process_odds_data("Sot", "ateam")
  
  if(length(subtitles) == 6) {
    
    process_odds_data("Shots", "total")
    process_odds_data("Shots", "hteam")
    process_odds_data("Shots", "ateam")
    
  }
  
  
  remote_driver$server$stop()
  # print(df_odds)
  return(df_odds)
  
}






run_selenium_betsson <- function(hteam = "Fulham", ateam = "Manchester City") {
  
  
  

  
  remote_driver <- rsDriver(browser = "chrome",
                            chromever = "114.0.5735.90",
                            port = free_port())
  
  remote_driver$client$maxWindowSize()
  remote_driver$client$navigate("https://www.betsson.com/sv/odds/fotboll/england/premier-league-epl")
  
  Sys.sleep(3)
  # click accept cookies
  remote_driver$client$findElements("id", "onetrust-accept-btn-handler")[[1]]$clickElement()
  
  xpath_selector <-  sprintf("//span[@class='obg-event-info-participant-name' and text()='%s']/ancestor::div[@class='obg-event-info-participant-label ng-star-inserted']",
                             hteam)
  
  # Click the match
  remote_driver$client$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
  
  Sys.sleep(2)
  # Click Matchstatistik
  label_text <- "Matchstatistik"
  # Use XPath to find the obg-tab-label element based on the label text
  xpath_expression <- sprintf("//span[@class='label-text ng-star-inserted' and text()='%s']/ancestor::div[@class='ng-star-inserted']", label_text)
  remote_driver$client$findElements(using = "xpath", value = xpath_expression)[[1]]$clickElement()
  # 
  # # XPath selector for the button with the specified class
  # xpath_selector <- "//button[@class='KambiBC-outcomes-list__toggler-toggle-button down']"
  # 
  # # Find the button "Visa lista" to show more odds
  # button_element <- remote_driver$client$findElements(using = "xpath", value = xpath_selector)
  # Sys.sleep(1)
  # # Check if the button element is found
  # if (!(length(button_element) == 0)) {
  #   remote_driver$client$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
  # } else {
  #   print("Button not found.")
  # }
  
  
  html_content <- remote_driver$client$getPageSource()[[1]]
  html_parsed <- read_html(html_content)
  
  
  css_selector_odds <- "div.sc-iBYQkv.hYDhjM"
  css_selector_over <- "div.sc-dkrFOg.erQOWi .sc-hLBbgP.ermjoq"
  css_selector_value <- "div.sc-dkrFOg.erQOWi .sc-eDvSVe.hfwXzz"
  css_selector_subtitle <- "h3.KambiBC-bet-offer-subcategory__label span"
  
  odds <- html_text(html_nodes(html_parsed, css_selector_odds))
  over <- html_text(html_nodes(html_parsed, css_selector_over))
  value <- html_text(html_nodes(html_parsed, css_selector_value))
  subtitles <- html_text(html_nodes(html_parsed, css_selector_subtitle))
  
  
  sekvens_o <- c()
  sekvens_u <- c()
  odds_o <- c()
  odds_u <- c()
  df_odds <- data.frame(team = character(),
                        type = character(),
                        line = numeric(),
                        over_odds = numeric(),
                        under_odds = numeric(),
                        stringsAsFactors = FALSE)
  
  
  process_odds_data <- function(type, team) {
    ind <- which(over != over[1])[1] - 1
    
    line <- value[1:ind]
    odds_o <- odds[1:ind]
    odds_u <- odds[(ind + 1):(ind + ind)]
    
    df_odds <<- rbind(df_odds, data.frame("team" = rep(team, ind),
                                          "type" = rep(type, ind),
                                          "line" = line,
                                          "over odds" = odds_o,
                                          "under odds" = odds_u))
    
    over <<- over[(1 + ind * 2):length(over)]
    odds <<- odds[(1 + ind * 2):length(odds)]
    value <<- value[(1 + ind * 2):length(value)]
  }
  
  
  process_odds_data("sot", "Total")
  process_odds_data("sot", hteam)
  process_odds_data("sot", ateam)
  process_odds_data("shots", "Total")
  process_odds_data("shots", hteam)
  process_odds_data("shots", ateam)
  
  remote_driver$server$stop()
  print(df_odds)
  return(df_odds)
  
}










run_EV_bets <- function(all_team_stats, models, sot_ratio, id_teams_2023, league = "PL",
                        browser = "firefox") {
  
  process_odds_data <- function(type, team) {
    ind <- which(over != over[1])[1] - 1
    
    line <- value[1:ind]
    odds_o <- odds[1:ind]
    odds_u <- odds[(ind + 1):(ind + ind)]
    
    df_odds <<- rbind(df_odds, data.frame("team" = rep(team, ind),
                                          "type" = rep(type, ind),
                                          "line" = line,
                                          "over odds" = odds_o,
                                          "under odds" = odds_u))
    
    # Stop if it is last iteration
    if(!(length(over)/2 == ind)) {
      over <<- over[(1 + ind * 2):length(over)]
      odds <<- odds[(1 + ind * 2):length(odds)]
      value <<- value[(1 + ind * 2):length(value)]
    }
  }
  
  df_pick_EV <- data.frame("League" = character(),
                           "Hteam" = character(), "Ateam" = character(),
                           "Date" =  character(), "Market" = character(),
                           "Pick" = character(),"Odds_Kambi" = numeric(),
                           "Correct_odds" = numeric(), "EV" = numeric(),
                           stringsAsFactors = FALSE)
  # all_team_stats <- get_all_team_specific_stats(con)
  # mod <-  models_shots_sot(con)
  
  # models <-  mod[[1]]
  # sot_ratio <-  mod[[2]]
  
  mat <- matrix(NA, 15, 6) %>% 
    as.data.frame() %>% 
    `colnames<-`(c("Stats","HomeTeam_For", "HomeTeam_Against",
                   "AwayTeam_For", "AwayTeam_Against", "Total"))
  
  mat$Stats <- c("Fouls","Corners","Tackles","Offsides","Goal kicks",
                 "Throw ins","Shots","Shots on target","Posession","Goals",
                 "xG","Yellow cards","Red cards","Odds 1x2","Odds Over 2.5")
  
  if(browser == "chrome") {
    
    remote_driver <- rsDriver(browser = "chrome",
                              chromever = "114.0.5735.90",
                              port = free_port())
  } else {
    
    remote_driver <- rsDriver(browser = "firefox",
                              chromever = NULL,
                              verbose = F,
                              port = free_port())
  }
  
  
  remDrv <- remote_driver$client
  remDrv$maxWindowSize()
  
  
  if(league == "PL") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/england/premier_league/all/matches"
  else if(league == "Serie A") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/italy/serie_a/all/matches"
  else if(league == "La Liga") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/spain/la_liga/all/matches"
  else if(league == "Bundesliga") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/germany/bundesliga/all/matches"
  else if(league == "Ligue 1") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/france/ligue_1/all/matches"
  
  
  remDrv$navigate(unibet_url)
  
  Sys.sleep(3)
  # click accept cookies
  remDrv$findElements("id", "CybotCookiebotDialogBodyLevelButtonLevelOptinAllowAll")[[1]]$clickElement()
  
  # check which matches that are available
  html_content <- remDrv$getPageSource()[[1]]
  html_parsed <- read_html(html_content)
  
  teams <- html_parsed %>% html_nodes(".c539a") %>%  html_text()
  
  df_matchup_kambi <- data.frame(hteam = teams[c(T,F)], ateam = teams[c(F,T)])
  df_matchup_kambi$hteam_fbref <- rename_teams(df_matchup_kambi$hteam, from = "kambi_2", to = "fbref")
  df_matchup_kambi$ateam_fbref <- rename_teams(df_matchup_kambi$ateam, from = "kambi_2", to = "fbref")
  
  
  index_team <- c()
  for(i in 1 :nrow(df_matchup_kambi)) {
    
    if(i != 1) {
    this_team <- df_matchup_kambi[i,1]
    team_to_check <- c(df_matchup_kambi$hteam[1:(i-1)],
                       df_matchup_kambi$ateam[1:(i-1)])
    
    index_team[i] <- sum(this_team == team_to_check) + 1
    } else {
      index_team[i] <- 1
    }
    
  }
  df_matchup_kambi$index <- index_team
  df_matchup_kambi <- df_matchup_kambi %>%
    distinct(hteam, ateam, .keep_all = TRUE)
  
  df_matchup_kambi$markets <- TRUE
 
  
  updateProgressBar(
    session = session,
    id = "progress_bar",
    value = 0,
    total = nrow(df_matchup_kambi)
    )
  #   range_value = c(0,nrow(df_matchup_kambi))
  # )
  
  for(matchup in 2:nrow(df_matchup_kambi)) {
 # for(matchup in 1:2) {
    
    
    Sys.sleep(5)
    
    updateProgressBar(
      session = session,
      id = "progress_bar",
      value = matchup,
      total = nrow(df_matchup_kambi),
      title = paste(df_matchup_kambi$hteam[matchup]," vs ", df_matchup_kambi$ateam[matchup])
    )
    
    
    xpath_selector <- sprintf("//div[@class='_6548b']//div[@class='c539a' and @data-test-name='teamName' and contains(., '%s')]",
                              df_matchup_kambi$hteam[matchup])
    
    index <- df_matchup_kambi$index[matchup]
    element_match <- remDrv$findElements(using = "xpath", value = xpath_selector)[[index]]$clickElement()
    
    Sys.sleep(5)
    
    html_1x2 <- remDrv$getPageSource()[[1]]
    html_1x2 <- read_html(html_1x2)
    odds_1x2 <- html_1x2 %>% html_nodes(".KambiBC-outcomes-list--columns-3 .hYDhjM") %>%  html_text()
    date_match <- html_1x2 %>% html_node("span._443d2 time") %>% html_text()
    
    
    # Click Match and Team Shots
    
    markets <-  remDrv$findElements("class name", "KambiBC-filter-menu__option")
    markets <- lapply(markets, function(x) x$getElementText()) %>% unlist()
    
    if(!any("Skott i matchen och för laget" == markets)) {
      print("Unibet did not have market 'Skott i matchen och för laget'")
      df_matchup_kambi$markets[matchup] <- FALSE
      break
    } 
    
    index_shots <- which("Skott i matchen och för laget" == markets)
    xpath_selector <- "//*[starts-with(@id, 'react-tabs-')]/div[1]/div[1]/div/div/div/div/ul/li[%s]" %>% sprintf(index_shots)
    
    remDrv$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
    
    
    # XPath selector for the button with the specified class
    xpath_selector <- "//button[@class='KambiBC-outcomes-list__toggler-toggle-button down']"
    
    # Find the button "Visa lista" to show more odds
    button_element <- remDrv$findElements(using = "xpath", value = xpath_selector)
    Sys.sleep(2)
    # Check if the button element is found
    if (!(length(button_element) == 0)) {
      remDrv$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
    } else {
      #print("Button not found.")
    }
    
    
    html_content <- remDrv$getPageSource()[[1]]
    html_parsed <- read_html(html_content)
    
    
    css_selector_odds <- "div.sc-iBYQkv.hYDhjM"
    css_selector_over <- "div.sc-dkrFOg.erQOWi .sc-hLBbgP.ermjoq"
    css_selector_value <- "div.sc-dkrFOg.erQOWi .sc-eDvSVe.hfwXzz"
    css_selector_subtitle <- "h3.KambiBC-bet-offer-subcategory__label span"
    
    odds <- html_text(html_nodes(html_parsed, css_selector_odds))
    over <- html_text(html_nodes(html_parsed, css_selector_over))
    value <- html_text(html_nodes(html_parsed, css_selector_value))
    subtitles <- html_text(html_nodes(html_parsed, css_selector_subtitle))
    
    sekvens_o <- c()
    sekvens_u <- c()
    odds_o <- c()
    odds_u <- c()
    df_odds <- data.frame(team = character(),
                          type = character(),
                          line = numeric(),
                          over_odds = numeric(),
                          under_odds = numeric(),
                          stringsAsFactors = FALSE)
    
    
    process_odds_data("Sot", "total")
    process_odds_data("Sot", "hteam")
    process_odds_data("Sot", "ateam")
    
    if(length(subtitles) == 6) {
      
      process_odds_data("Shots", "total")
      process_odds_data("Shots", "hteam")
      process_odds_data("Shots", "ateam")
      
    }
    
    
    # CALCULATE ODDS ===========================================================
    
    teams <- c(df_matchup_kambi$hteam_fbref[matchup], df_matchup_kambi$ateam_fbref[matchup])
    
    teams_id <- c()
    teams_id[1] <- id_teams_2023[id_teams_2023$hteam == teams[1],2]
    teams_id[2] <- id_teams_2023[id_teams_2023$hteam == teams[2],2]
    
    
    teams_stats <- get_team_stats(all_team_stats, mat, teams_id,
                                  "2023/2024", "Both",
                                  "2023/2024", "Both") 
    rownames(teams_stats) <- teams_stats[, 1]
    rownames(teams_stats)[c(8,12)] <- c("Sot","Cards")
    teams_stats[12,-1] <- teams_stats[12,-1] + (teams_stats[13,-1]*2) # card
    teams_stats <- teams_stats[-c(9,11,13,14,15), -1]
    
    correct_odds <- get_team_odds(teams_stats,
                          num_stats = 10, model = TRUE, teams = teams,
                          odds = odds_1x2,
                          models = models,
                          sot_ratio = sot_ratio)
    
    # ==========================================================================
    
    EV_match <- get_gt_odds_team_kambi(correct_odds, teams[1], teams[2], df_odds, raw = TRUE)
    orginal_row_num <- nrow(EV_match)
    colnames(EV_match)[-1] <- rep(c("Line","Over","Under","O_kambi","U_kambi","EV"),3)
    EV_match <- rbind(EV_match[,1:7], EV_match[,c(1,8:13)], EV_match[,c(1,14:19)])
    EV_match$team <- c(rep(teams[1], orginal_row_num),rep(teams[2], orginal_row_num),
                       rep("Total", orginal_row_num))
    
    EV_match <- EV_match[complete.cases(EV_match), ]
    EV_match$O_kambi <- as.numeric(EV_match$O_kambi)
    EV_match$U_kambi <- as.numeric(EV_match$U_kambi)
    
    # Which (over/under) is more EV
    which_side <- (EV_match$O_kambi / EV_match$Over) < (EV_match$U_kambi / EV_match$Under)
    
    
    EV_match$pick <-ifelse(which_side,
                           str_c("Under ", EV_match$Line," ", EV_match$Action, " ", EV_match$team),
                           str_c("Over ", EV_match$Line," ", EV_match$Action, " ", EV_match$team))
    
    df_pick_EV <- rbind(df_pick_EV, data.frame("League"  = league,
                                               "Hteam" = teams[1],
                                               "Ateam" = teams[2],
                                               "Date" = date_match,
                                               "Market" = EV_match$Action,
                                               "Odds_Kambi" = ifelse(which_side,
                                                                     EV_match$U_kambi,
                                                                     EV_match$O_kambi),
                                               "Correct_odds" = ifelse(which_side,
                                                                       EV_match$Under,
                                                                       EV_match$Over), 
                                               "Pick" = EV_match$pick,
                                               "EV" = EV_match$EV*100))
    
    remDrv$navigate(unibet_url)
  }
  
  
  remote_driver$server$stop()
  return(df_pick_EV)
}


















