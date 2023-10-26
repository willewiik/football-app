






library(gt)


get_gt_teams <- function(mat, home, away){
  

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
      title = paste("Statistics for", home, "-", away, "& total (per match)")
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
  


mat <- matrix(10, 6, 6) %>% 
  as.data.frame() %>% 
  `colnames<-`(c("Stats","HomeTeam_For", "HomeTeam_Against",
                 "AwayTeam_For", "AwayTeam_Against", "Total"))

mat$Stats <- c("Cards", "Offside", "Fouls", "Shots", "ShotsOnTarget", "Tackles")





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



num_stats <- 6
mat_m <- matrix(10.2,num_stats,5)
team1 <- "Arsenal"
team2 <- "Inter"
colnames(mat_m) <- c(str_c(team1,"(for)"),str_c(team1,"(against)"),
                     str_c(team2,"(for)"),str_c(team2,"(against)"),"Total")
rownames(mat_m) <- c("Cards", "Offside", "Fouls", "Shots", "ShotsOnTarget", "Tackles")

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
for (i in seq(1, nrow(dff), num_stats*3)) {
  subset_df <- dff[i:(i+(num_stats*3)-1), ]
  if (is.null(df_delat_m)) {
    df_delat_m <- subset_df
  } else {
    df_delat_m <- cbind(df_delat_m, subset_df)
  }
}

df_complete <- cbind(rep(rownames(mat_m), each = 3),df_delat_m)






df_complete


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
        rows = c(1:3, 7:9, 13:15),
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#5B84B1FF", alpha = 0.1)
      ),
      locations = cells_body(
        rows = c(4:6, 10:12, 16:18),
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


