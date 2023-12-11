




library(gt)


get_gt_teams <- function(mat, home, away){
  
    mat[,-1] <- round(mat[,-1],2)
    colnames(mat) <- c(colnames(mat)[1],
                       str_c(home,"_For"),str_c(home,"_Against"),
                       str_c(away,"_For"),str_c(away,"_Against"),
                       colnames(mat)[6])
    
    actual_colnames <- colnames(mat)
    desired_colnames <- actual_colnames |> 
      str_remove(str_c("(",home,"|",away,")_")) |> 
      str_to_title()
    names(desired_colnames) <- actual_colnames
    
    
    
    gt_result <- gt(mat, rowname_col = "Stats") %>% 
      cols_label(.list = desired_colnames) %>% 
      tab_header(
      title = paste("Statistics for", home, "-", away)
       )  %>%
        tab_spanner(
          label = md(str_c("**",home,"**")),
          columns = 2:3
        ) %>% 
        tab_spanner(
          label = md(str_c("**",away,"**")),
          columns = 4:5
        ) %>% 
        tab_spanner(
          label =  md('**Total**'),
          columns = 6
        ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "green", alpha = 0.1)
        ),
        locations = cells_body(
          columns = 2:3,
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "blue", alpha = 0.1)
        ),
        locations = cells_body(
          columns = 4:5,
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "red", alpha = 0.1)
        ),
        locations = cells_body(
          columns = 6,
        )
      ) %>% 
      opt_stylize(style = 1, color = 'gray')
    gt_result  
    
    return(gt_result)
      
}


get_gt_odds_team <- function(mat, home, away){
  
  
  
  colnames(mat) <- c("Action",str_c(home,"_Line"),str_c(home,"_over"),str_c(home,"_under"),
                     str_c(away,"_Line"),str_c(away,"_over"),str_c(away,"_under"),
                     "Total_Line","Total_over","Total_under")
  
  
  actual_colnames <- colnames(mat)
  desired_colnames <- actual_colnames |> 
    str_remove(str_c("(",home,"|",away,"|","Total",")_")) |> 
    str_to_title()
  names(desired_colnames) <- actual_colnames
  
  desired_colnames[1] <- ""
  
 # mat <- mat[19:21,]
  
  gt_result <- gt(mat, rowname_col = "Stats") %>% 
    cols_label(.list = desired_colnames) %>% 
    tab_header(
      title = paste("Odds for", home, "-", away)
    )  %>%
    tab_spanner(
      label = md(str_c("**",home,"**")),
      columns = 2:4
    ) %>% 
    tab_spanner(
      label = md(str_c("**",away,"**")),
      columns = 5:7
    ) %>% 
    tab_spanner(
      label =  md('**Total**'),
      columns = 8:10
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#FC766AFF", alpha = 0.8)
      ),
      locations = cells_body(
        rows = c(1:3, 7:9, 13:15, 19:21, 25:27),
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#5B84B1FF", alpha = 0.1)
      ),
      locations = cells_body(
        rows = c(4:6, 10:12, 16:18, 22:24, 28:30),
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(1,2,5,8),
      )
    ) %>% 
    tab_style(
      style = list(
        cell_borders(sides  = "left")
      ),
      locations = cells_body(
        columns = c(2,5,8),
      )
    ) %>% 
    opt_stylize(style = 1, color = 'gray')
  gt_result  
  
  return(gt_result)
  
}



get_gt_odds_team_kambi <- function(mat, home, away, df_odds){
  
  
    new_mat <- matrix(NA,nrow(mat),19)
    
    colnames(new_mat) <- c("Action",str_c(home,"_Line"),str_c(home,"_over"),str_c(home,"_under"),
                           str_c(home,"_o_kambi"),str_c(home,"_u_kambi"),str_c(home,"_EV"),
                           str_c(away,"_Line"),str_c(away,"_over"),str_c(away,"_under"),
                           str_c(away,"_o_kambi"),str_c(away,"_u_kambi"),str_c(away,"_EV"),
                           "Total_Line","Total_over","Total_under",
                           "Total_o_kambi","Total_u_kambi", "Total_EV")
    
    # Adding the predicted odds
    
    new_mat <- as.data.frame(new_mat)
    new_mat[,c(1:4,8:10,14:16)] <- as.data.frame(mat[,1:10])
    
    for (i in 1:2) {
      type <- ifelse(i == 1, "Sot", "Shots")
      
      for (j in 1:3) {
        col <- c(2, 8, 14)[j]
        team <- c("hteam", "ateam", "total")[j]
        
        for (k in 1:3) {
          is_line_kambi <- new_mat[new_mat[, 1] == type, col][k] == df_odds[df_odds$team == team & df_odds$type == type, 3]
          
          if (any(is_line_kambi)) {
            new_mat[new_mat[, 1] == type, (col + 3):(col + 4)][k, ] <- (
              df_odds[df_odds$team == team & df_odds$type == type, 4:5][which(is_line_kambi), ])
            
            EV <- calculate_EV(odds = new_mat[new_mat[, 1] == type, (col + 1):(col + 4)][k, ])
            new_mat[new_mat[, 1] == type, (col + 5)][k] <- EV
          } else {
            # the line is not on kambi, set to NA or handle accordingly
          }
        }
      }
    }
    
    
    
  
  
  actual_colnames <- colnames(new_mat)
  desired_colnames <- actual_colnames %>% 
    str_remove(str_c("(",home,"|",away,"|","Total",")_")) %>% 
    str_to_title()
  names(desired_colnames) <- actual_colnames
  desired_colnames[desired_colnames == "Ev"] <- "EV"
  desired_colnames[1] <- ""
  
  # mat <- mat[19:21,]
  new_mat <- new_mat[19:24,]
  
  gt_result <- gt(new_mat, rowname_col = "Stats") %>% 
    cols_label(.list = desired_colnames) %>% 
    tab_header(
      title = paste("Odds for", home, "-", away," with kambi odds")
    )  %>%
    tab_spanner(
      label = md(str_c("**",home,"**")),
      columns = 2:7
    ) %>% 
    tab_spanner(
      label = md(str_c("**",away,"**")),
      columns = 8:13
    ) %>% 
    tab_spanner(
      label =  md('**Total**'),
      columns = 14:19
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#FC766AFF", alpha = 0.8)
      ),
      locations = cells_body(
        rows = c(1:3),
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#5B84B1FF", alpha = 0.1)
      ),
      locations = cells_body(
        rows = c(4:6),
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(1,2,8,14),
      )
    ) %>% 
    tab_style(
      style = list(
        cell_borders(sides  = "left")
      ),
      locations = cells_body(
        columns = c(2,8,14),
      )
    ) %>% 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>% 
    fmt_percent(
      columns = c(7,13,19),
      decimals = 0
    ) %>% 
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = cells_body(
        columns = c(7,13,19),
      )
    )  %>% 
    opt_stylize(style = 1, color = 'gray')
  gt_result  
  
  return(gt_result)
  
}



get_gt_odds_players <- function(mat){
  
  player <- mat[1]
  stats <- mat[5:9]
  resultat <- lapply(stats,calculate_betting_odds)
  resultat <- do.call("rbind",resultat ) 
  resultat <- resultat[-c(13,15),]
  resultat$Action <- c(rep("Shots",3), rep("Sot",3), rep("Tackles",3 ), rep("Passes", 3),"Card")
  resultat <- resultat %>% relocate("Action")

  
  gt_result <- gt(resultat) %>% 
    tab_header(
      title = paste("Odds for", player)
    )  %>%
    tab_style(
      style = list(
        cell_fill(color = "#FC766AFF", alpha = 0.8)
      ),
      locations = cells_body(
        rows = c(1:3, 7:9, 13),
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#5B84B1FF", alpha = 0.1)
      ),
      locations = cells_body(
        rows = c(4:6, 10:12),
      )
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(1,2),
      )
    ) %>% 
    opt_stylize(style = 1, color = 'gray')
  gt_result  
  
  
}
  


get_plotly_ref <- function(data) {
  


  if(!is.null(input$referee_selector)) {
    
    data$size_ref <- ifelse(data$referee %in% input$referee_selector, 30, 10)
    data$opa_ref <- ifelse(data$referee %in% input$referee_selector, 1, 0.3)
   
  } else {
    
    data$size_ref <- 10
    data$opa_ref <- 1
    
  }
  
  
  fig <- plot_ly()  %>% 
    add_trace(data, x = ~data[data$league_id==1,4],
              y = ~data[data$league_id==1,5],
              type = 'scatter', mode = 'markers',
              marker = list(
                size = ~data[data$league_id==1,8],
                opacity = ~data[data$league_id==1,9],
                color = ~data[data$league_id==1,7]  
              ),
              text = ~data[data$league_id==1,2],
              name = 'PL') %>% 
    add_trace(data, x = ~data[data$league_id==2,4],
              y = ~data[data$league_id==2,5],
              type = 'scatter', mode = 'markers',
              marker = list(
                size = ~data[data$league_id==2,8],
                opacity = ~data[data$league_id==2,9],
                color = ~data[data$league_id==2,7]  
              ),
              text = ~data[data$league_id==2,2],
              name = 'Serie A') %>% 
    add_trace(data, x = ~data[data$league_id==3,4],
              y = ~data[data$league_id==3,5],
              type = 'scatter', mode = 'markers',
              marker = list(
                size = ~data[data$league_id==3,8],
                opacity = ~data[data$league_id==3,9],
                color = ~data[data$league_id==3,7]  
              ),
              text = ~data[data$league_id==3,2],
              name = 'La Liga') %>% 
    add_trace(data, x = ~data[data$league_id==4,4],
              y = ~data[data$league_id==4,5],
              type = 'scatter', mode = 'markers',
              marker = list(
                size = ~data[data$league_id==4,8],
                opacity = ~data[data$league_id==4,9],
                color = ~data[data$league_id==4,7]  
              ),
              text = ~data[data$league_id==4,2],
              name = 'Bundesliga') %>% 
    add_trace(data, x = ~data[data$league_id==5,4],
              y = ~data[data$league_id==5,5],
              type = 'scatter', mode = 'markers',
              marker = list(
                size = ~data[data$league_id==5,8],
                opacity = ~data[data$league_id==5,9],
                color = ~data[data$league_id==5,7]  
              ),
              text = ~data[data$league_id==5,2],
              name = 'Ligue 1') %>% 
    layout(
      title = "Referee Performance",
      xaxis = list(title = "Average Total Fouls"),
      yaxis = list(title = "Average Total Cards")
    )
  
  
}


# mat <- matrix(10, 6, 6) %>% 
#   as.data.frame() %>% 
#   `colnames<-`(c("Stats","HomeTeam_For", "HomeTeam_Against",
#                  "AwayTeam_For", "AwayTeam_Against", "Total"))
# 
# mat$Stats <- c("Cards", "Offside", "Fouls", "Shots", "ShotsOnTarget", "Tackles")












# 
# num_stats <- 6
# mat_m <- matrix(10.2,num_stats,5)
# team1 <- "Arsenal"
# team2 <- "Inter"
# colnames(mat_m) <- c(str_c(team1,"(for)"),str_c(team1,"(against)"),
#                      str_c(team2,"(for)"),str_c(team2,"(against)"),"Total")
# rownames(mat_m) <- c("Cards", "Offside", "Fouls", "Shots", "ShotsOnTarget", "Tackles")
# 
# resultat <- unlist(apply(cbind(((mat_m[,1]+ mat_m[,4])/ 2),
#                                ((mat_m[,2]+ mat_m[,3])/ 2),
#                                mat_m[,5]), 1:2, calculate_betting_odds))
# 
# dff <- NULL
# for (i in seq(1, length(resultat), 9)) {
#   matris <- matrix(resultat[i:(i+8)], nrow = 3)
#   if (is.null(dff)) {
#     dff <- as.data.frame(matris)
#   } else {
#     dff <- rbind(dff, as.data.frame(matris))
#   }
# }
# df_delat_m <- NULL
# for (i in seq(1, nrow(dff), num_stats*3)) {
#   subset_df <- dff[i:(i+(num_stats*3)-1), ]
#   if (is.null(df_delat_m)) {
#     df_delat_m <- subset_df
#   } else {
#     df_delat_m <- cbind(df_delat_m, subset_df)
#   }
# }
# 
# df_complete <- cbind(rep(rownames(mat_m), each = 3),df_delat_m)








