

mapped_players <- player_dictionary_mapping()

unique_urls <- unique(mapped_players$PlayerFBref)
mapped_players <- mapped_players[mapped_players$PlayerFBref %in% unique_urls, ]


df_injuries <- data.frame()
country_names <- c("England","Italy","Spain","Germany","France")
for (country in country_names) {
  current_injuries <- tm_league_injuries(country_name = country)
  
  df_injuries <- bind_rows(df_injuries, current_injuries)
}


players_not_mapped <- df_injuries[which(!df_injuries$player_url %in% mapped_players$UrlTmarkt),c(4,7)]
df_injuries <- df_injuries[which(df_injuries$player_url %in% mapped_players$UrlTmarkt),]
df_injuries <- df_injuries[,c(1,4,5,11:13)]




index_player <- unlist(lapply(df_injuries$player_url,
                              function(x) which(x == mapped_players$UrlTmarkt)))



df_injuries$player_name <- mapped_players$PlayerFBref[index_player]







