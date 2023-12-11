

# RUN APP


source("app.R")


server <- function(input, output, session) {
  
  output$pl_hometeam_logo <- renderImage({
    
    list(src = "logos/GB1/Liverpool FC.png",
         alt = "ERROR")
  },deleteFile=FALSE)
  
  output$serie_a_hometeam_name <- renderText({
    
    paste("Liverpool")
  })
  
  output$pl_date <- renderText({
    
    paste("2023-11-15")
  })
  
  output$pl_odds1kambi <- renderText({
    
    paste("2.50")
  })
  
  
}

shinyApp(ui = ui, server = server)






# 
# binman::list_versions("chromedriver")
# 
# sel_obj <- selenium(retcommand = T, check = F)


# 
# html_content <- remote_driver$client$getPageSource()[[1]]
# html_parsed <- read_html(html_content)
# 
# teams <- html_parsed %>% html_nodes(".c539a") %>%  html_text()
# 
# df_matchup_kambi <- data.frame(hteam = teams[c(T,F)], ateam = teams[c(F,T)])
# 
# index_which_match <- 1
# team_name_to_find <- df_matchup_kambi$hteam[index_which_match]

# Use XPath to locate the div element with the specified team name







