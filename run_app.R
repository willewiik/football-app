

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



# binman::list_versions("chromedriver")
# 
# sel_obj <- selenium(retcommand = T, check = F)




library(soccermatics)
raheem_sterling_shots <- understat_player_shots(player_url = "https://understat.com/player/618")
raheem_sterling_shots$X <- raheem_sterling_shots$X * 100
raheem_sterling_shots$Y <- raheem_sterling_shots$Y * 100

raheem_sterling_shots$xG <- raheem_sterling_shots$xG %>% round(.,2)

library(ggsoccer)




# ggplot code (unchanged from your example)
yo <- ggplot(raheem_sterling_shots) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(aes(x = X, y = Y, size = xG, color = season),
             alpha = 0.7) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 101)) +
  scale_y_reverse() +
  ggtitle("Simple shotmap",
          "ggsoccer example")

# Convert ggplot to Plotly and use the defined layout
plotly_obj <- plotly::ggplotly(yo) 

# Show the Plotly object
plotly_obj



plotly::ggplotly(yo)
