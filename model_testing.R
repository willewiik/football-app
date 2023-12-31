


# dbHost <- "localhost"
# dbPort <- 3306  # Portnummer
# dbName <- "sql_workbench"
# dbUser <- "root"
# dbPassword <- Sys.getenv("Key1")

dbHost <- "127.0.0.1" 
dbPort <- 3306  # Portnummer
dbName <- "newschema"
dbUser <- "root"
dbPassword <- "pt2hGuPBwxwVRB"

# Skapa anslutningssträng
con <- dbConnect(MySQL(), host = dbHost, port = dbPort, dbname = dbName,
                 user = dbUser, password = dbPassword)


dbDisconnect(con)
odds_to_prob <- function(h,x,a, two = FALSE) {
  
  if(two) {
    
    total_vig_prob <- (1/h + 1/x)
    
    h <- 1/h / total_vig_prob
    x <- 1/x / total_vig_prob
    
    return(list(h,x))
    
    
  } else {
    
    total_vig_prob <- (1/h + 1/x +1/a)
    
    h <- 1/h / total_vig_prob
    x <- 1/x / total_vig_prob
    a <- 1/a / total_vig_prob
    
    return(list(h,x,a))
    
  }
  
}


all_matches <- dbGetQuery(con,  "SELECT  * FROM matches")
all_teams <- dbGetQuery(con,  "SELECT  * FROM teams")
all_matches <- all_matches %>%  select(home_team_id, away_team_id, h_odds,
                                       x_odds, a_odds, o2_5odds, u2_5odds,
                                       h_shots, h_sot, a_shots, a_sot, event_date) 

probs <- odds_to_prob(all_matches$h_odds,all_matches$x_odds,all_matches$a_odds)
all_matches$h_prob <- probs[[1]]
all_matches$a_prob <- probs[[3]]

all_matches$o2_5_prob <- odds_to_prob(all_matches$o2_5odds,
                                      all_matches$u2_5odds, two = TRUE)[[1]]


all_matches$home_team_id <- lapply(all_matches$home_team_id,
                            function(x){all_teams[all_teams$team_id == x,2]}) %>% unlist()
all_matches$away_team_id <- lapply(all_matches$away_team_id,
                            function(x){all_teams[all_teams$team_id == x,2]}) %>% unlist()

home_teams <- all_matches %>%  select(home_team_id,away_team_id, h_prob,
                                      o2_5_prob, h_shots, h_sot,event_date)
away_teams <- all_matches %>%  select(away_team_id,home_team_id, a_prob,
                                      o2_5_prob, a_shots, a_sot,event_date)
colnames(home_teams) <- colnames(away_teams) <- c("team","opponent","prob_win",
                                                  "over2.5_goals_prob","shots","sot","event_date")


data_shots <- rbind(home_teams,away_teams)
data_shots$team <- as.factor(data_shots$team)
data_shots$opponent <- as.factor(data_shots$opponent)
data_shots$prob_win <- data_shots$prob_win * 100
data_shots$over2.5_goals_prob <- data_shots$over2.5_goals_prob * 100
data_shots$event_date_num <- as.Date(data_shots$event_date) %>%  as.numeric()

data_shots$event_date_num <- (data_shots$event_date_num - max(data_shots$event_date_num)) * -1
decay_parameter <- 200
data_shots$weights <- exp(-(data_shots$event_date_num - max(data_shots$event_date_num))/decay_parameter)


grouped_data <- data_shots %>%
  group_by(team) %>%
  summarise(total_shots = sum(shots), total_sot = sum(sot)) %>% 
  mutate(sot_ratio = total_sot / total_shots)

library(MASS)
b <- boxcox(data_shots$prob_win ~ 1)
lambda <- b$x[which.max(b$y)]

new_x_exact <- (data_shots$prob_wi^lambda - 1) / lambda

data_shots$prob_win_tr <-  (data_shots$prob_win*10)^1.1

hist(data_shots$prob_win_tr)




library(lme4)



# ==============================================================================


# Split the data into training (80%) and testing (20%)
set.seed(125)  # for reproducibility
sample_index <- sample(1:nrow(data_shots), 0.7 * nrow(data_shots))

data_shots_2 <- data_shots


train_data <- data_shots_2[sample_index, ]
test_data <- data_shots_2[-sample_index, ]

# Train the model on the training set
trained_model <- lmer(shots ~ prob_win_tr + (1 | team) + (1 | opponent),
                      data = train_data)

# Make predictions on the testing set
predictions <- predict(trained_model, newdata = test_data)


#new_new <- data.frame(team = "Manchester City", opponent = "Liverpool", prob_win = 59)
#new_new <- data.frame(team = "Liverpool", opponent = "Manchester City", prob_win = 21)

# Evaluate the model
# You can use metrics like Mean Squared Error (MSE) or R-squared
mse <- mean((test_data$shots - predictions)^2)
rsquared <- 1 - (sum((test_data$shots - predictions)^2) / sum((test_data$shots - mean(test_data$shots))^2))

# Print the metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", rsquared, "\n")

predictions[which(test_data$team=="Manchester City")]
test_data$shots[1:20]
test_data[which(test_data$team=="Manchester City"),]




# # KORSVALIDERING --------------------------------------------------------------

# range <- seq(0,1,0.05)
# 
# MSE_vec <- c()
# RMSE_vec <- c()
# MAE_vec <- c()
# R2_vec <- c()
# 
# m <- 1
# for(i in range){
#   
#   model <- xgboost(data = data.matrix(train_data[,-c(5,6)]),
#                    label = train_data$shots,
#                    max.depth = 4,
#                    nround = 30,
#                    eta = 0.25,
#                    gamma = 0.8,
#                    colsample_bytree = 1,
#                    min_child_weight = 5,
#                    subsample = 1
#   )
#   
#   p<-predict(model, newdata = data.matrix(test_data[,-c(5,6)]))
#   true_labels <- test_data$shots
#   
#   # Calculate evaluation metrics
#   MSE_vec[m] <- mean((true_labels - p)^2)  # Mean Squared Error
#   RMSE_vec[m] <- sqrt(mse)  # Root Mean Squared Error
#   MAE_vec[m] <- mean(abs(true_labels - p))  # Mean Absolute Error
#   R2_vec[m] <- 1 - sum((true_labels - p)^2) / sum((true_labels - mean(true_labels))^2)  # R-squared
#   
#   
# 
#   m <- m + 1
# 
# }
# 
# 
# 
# # scale(as.data.frame(cbind(MSE_vec,RMSE_vec,MAE_vec,R2_vec))) %>% ggplot(aes(x=range,y=MSE_vec)) +
# # geom_line(aes(color="MSE_vec")) +
# # geom_line(aes(y = RMSE_vec,color="RMSE_vec")) +
# # geom_line(aes(y = MAE_vec,color="MAE_vec")) +
# # geom_line(aes(y = R2_vec,color="R2_vec")) +
# # theme_bw()
# 
# as.data.frame(R2_vec) %>% ggplot(aes(x=range,y=R2_vec)) +
#   geom_line(aes(color="R2")) +
#   theme_bw()
# 





################ FOULS #########################################################



















