

# RUN APP


source("app.R")


server <- function(input, output, session) {

  output$hometeam_logo <- renderImage({

    list(src = "logos/GB1/Liverpool FC.png",
         alt = "ERROR")
  },deleteFile=FALSE)

  output$hometeam_name <- renderText({

    paste("Liverpool")
  })

  output$date <- renderText({

    paste("2023-11-15")
  })

  output$odds1kambi <- renderText({

    paste("2.50")
  })

}

shinyApp(ui = ui, server = server)

