# 
# library(worldfootballR)
# 
# 
# gg<-fotmob_get_league_ids()
# fotmob_get_match_players(4230533)
# fotmob_get_match_info(4230533)
# fotmob_get_match_details()
# 
# try({
#   library(dplyr)
#   library(tidyr)
#   
#   results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
#   results %>%
#     dplyr::select(primary_id, ccode, league_name = name, match_id)
# })
# browseVignettes("worldfootballR")
# test <- worldfootballR::fb_big5_advanced_season_stats(2024,"possession","team")
# 
# 
# 
# hej <-fb_match_lineups("https://fbref.com/en/matches/67ed3ba2/Brentford-Tottenham-Hotspur-August-13-2023-Premier-League")
# 
# jesus <- fb_league_stats(
#               country = "ENG",
#               gender = "M",
#               season_end_year = 2024,
#               tier = "1st",
#               stat_type = "shooting",
#               team_or_player = "team"
#             )
#   
# devtools::install_github("JaseZiv/worldfootballR")
# 
# 
# mapped_players <- player_dictionary_mapping()
# lo <-understat_league_match_results("EPL",2023)
# 
# 
# library(dplyr)
# 
# 
# 
# 
# res <- load_match_results(country = c("ENG","ITA","ESP","GER","FRA"),
#                           gender ="M",
#                           season_end_year = 2024,
#                           tier = "1st")
# 
# #res <- res %>% filter_at(vars(HomeGoals), all_vars(!is.na(.))) # get results
# 
# index_20_next_matches <- which(is.na(res$HomeGoals))[20]
# 
# res <- res %>%  filter(row_number() %in% 1:index_20_next_matches)
# 
# 




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





