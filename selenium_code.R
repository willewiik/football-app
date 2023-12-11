library(RSelenium)
library(wdman)
library(netstat)
library(dplyr)
library(stringr)
library(rvest)



run_selenium <- function(hteam = "Luton Town", ateam = "Manchester City", league = "PL") {
  
  remote_driver <- rsDriver(browser = "chrome",
                            chromever = "114.0.5735.90",
                            port = free_port())
  
  remote_driver$client$maxWindowSize()
  
  if(league == "PL") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/england/premier_league/all/matches"
  else if(league == "Serie A") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/italy/serie_a/all/matches"
  else if(league == "La Liga") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/spain/la_liga/all/matches"
  else if(league == "Bundesliga") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/germany/bundesliga/all/matches"
  else if(league == "Ligue 1") unibet_url <- "https://www.unibet.se/betting/sports/filter/football/france/ligue_1/all/matches"
  
  remote_driver$client$navigate(unibet_url)
  
  Sys.sleep(3)
  # click accept cookies
  remote_driver$client$findElements("id", "CybotCookiebotDialogBodyLevelButtonLevelOptinAllowAll")[[1]]$clickElement()
  
  xpath_selector <- sprintf("//div[@class='_6548b']//div[@class='c539a' and @data-test-name='teamName' and contains(., '%s')]",
                            hteam)
  
  # Click the match
  remote_driver$client$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
  
  Sys.sleep(2)
  # Click Match and Team Shots
  remote_driver$client$findElements(using = "xpath", value = '//li[@data-label="Match and Team Shots"]/a')[[1]]$clickElement()
  
  # XPath selector for the button with the specified class
  xpath_selector <- "//button[@class='KambiBC-outcomes-list__toggler-toggle-button down']"
  
  # Find the button "Visa lista" to show more odds
  button_element <- remote_driver$client$findElements(using = "xpath", value = xpath_selector)
  Sys.sleep(1)
  # Check if the button element is found
  if (!(length(button_element) == 0)) {
    remote_driver$client$findElements(using = "xpath", value = xpath_selector)[[1]]$clickElement()
  } else {
    print("Button not found.")
  }

  
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
  
  
  process_odds_data("Sot", "total")
  process_odds_data("Sot", "hteam")
  process_odds_data("Sot", "ateam")
  process_odds_data("Shots", "total")
  process_odds_data("Shots", "hteam")
  process_odds_data("Shots", "ateam")
  
  remote_driver$server$stop()
  print(df_odds)
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









