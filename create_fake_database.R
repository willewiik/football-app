
make_fake_con <- function(){
      create_teams <- "CREATE TABLE teams (
      team_id CHAR(8) PRIMARY KEY,
      team_name VARCHAR(255)
    );"
      
      
      
      create_match <- "CREATE TABLE matches (
      match_id CHAR(8),
      league_id INT,
      home_team_id CHAR(8),
      away_team_id CHAR(8),
      event_date CHAR(10),
      season CHAR(10),
      h_odds FLOAT,
      x_odds FLOAT,
      a_odds FLOAT,
      o2_5odds FLOAT,
      u2_5odds FLOAT,
      mainline FLOAT,
      h_odds_mainline FLOAT,
      a_odds_mainline FLOAT,
      h_fouls INT,
      h_corners INT,
      h_crosses INT,
      h_touches INT,
      h_tackles INT,
      h_interceptions INT,
      h_aerials_won INT,
      h_clearances INT,
      h_offsides INT,
      h_goal_kicks INT,
      h_throw_ins INT,
      h_long_balls INT,
      h_shots INT,
      h_sot INT,
      h_possession INT,
      h_goals INT,
      h_xg FLOAT,
      h_yellow INT,
      h_red INT,
      h_formation VARCHAR(25),
      referee VARCHAR(255),
      a_fouls INT,
      a_corners INT,
      a_crosses INT,
      a_touches INT,
      a_tackles INT,
      a_interceptions INT,
      a_aerials_won INT,
      a_clearances INT,
      a_offsides INT,
      a_goal_kicks INT,
      a_throw_ins INT,
      a_long_balls INT,
      a_shots INT,
      a_sot INT,
      a_possession INT,
      a_goals INT,
      a_xg FLOAT,
      a_yellow INT,
      a_red INT,
      a_formation VARCHAR(25),
      PRIMARY KEY (match_id),
      FOREIGN KEY (home_team_id) REFERENCES teams (team_id),
      FOREIGN KEY (away_team_id) REFERENCES teams (team_id)
    );"
      
      create_index <- "CREATE INDEX idx_match_id ON matches (match_id);"
      
      create_players <- "CREATE TABLE players (
    player_id CHAR(8) PRIMARY KEY,
    player_name VARCHAR(255),
    team_id VARCHAR(255),
     FOREIGN KEY (team_id) REFERENCES teams (team_id)
    );"
      
      create_player_stats <- "CREATE TABLE player_stats (
      player_id CHAR(8),
      team_id CHAR(8),
      match_id CHAR(8),
      position VARCHAR(50),
      minutes_played INT,
      goals INT,
      assists INT,
      shots INT,
      sot INT,
      tackles INT,
      npxg FLOAT,
      xAG FLOAT,
      xgChain FLOAT,
      pass INT,
      yellow INT,
      red INT,
      PRIMARY KEY (player_id, match_id),
      FOREIGN KEY (player_id) REFERENCES players (player_id),
      FOREIGN KEY (match_id) REFERENCES matches (match_id)
    );"
      
      
      alter_1 <- "ALTER TABLE player_stats
    ADD CONSTRAINT fk_player_id
    FOREIGN KEY (player_id)
    REFERENCES players (player_id);"
      
      
      alter_2 <- "ALTER TABLE player_stats
    ADD CONSTRAINT fk_match_id
    FOREIGN KEY (match_id)
    REFERENCES matches (match_id);"
      
    
    
    
    
    con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    dbExecute(con, create_teams)
    dbExecute(con, create_match)
    dbExecute(con, create_index)
    dbExecute(con, create_players)
    dbExecute(con, create_player_stats)
    
    
    get_sql_code_from_file <- function(file_path){
      
      # Get contents of file
      file_lines <- readLines(file_path)
      
      # Remove single line comments
      file_lines <- file_lines[grepl(pattern = "^--", file_lines) == FALSE]
      
      # Note indices of lines with multi-line comments start/ends
      multi_line_start_indices <- grep(pattern = "^/\\*", file_lines)
      multi_line_end_indices <- grep(pattern = "\\*/", file_lines)
      
      # Check if any multi-line comments found
      if (length(multi_line_end_indices) != 0) {
        
        # Note lines defining and within multi-line comments
        multi_line_indices <- sapply(
          seq_along(multi_line_start_indices),
          FUN = function(index, multi_line_start_indices, multi_line_end_indices){
            return(multi_line_start_indices[index]:multi_line_end_indices[index])
          },
          multi_line_start_indices, multi_line_end_indices
        )
        
        # Remove multi-line_comments
        file_lines <- file_lines[-multi_line_indices]
      }
      
      # Combine into single string
      file_string <- paste(file_lines, collapse = "\n")
      
      return(file_string)
    }
    
    
    # Specify the directory containing your SQL files
    sql_files_dir <- "C:/Users/William Wiik/Desktop/databas_backup/"
    
    # List all SQL files in the directory
    sql_files <- list.files(path = sql_files_dir, pattern = "\\.sql", full.names = TRUE)
    
    # Regular expression to match "INSERT INTO" statements
    insert_regex <- "INSERT INTO[^;]+;"
    for(i in 1:4) {
      sql_files[i] <- get_sql_code_from_file(sql_files[i])
    }
    
   
    
    
    insert_match <- regmatches(sql_files[1], regexpr(insert_regex, sql_files[1], perl = TRUE))
    
    insert_player_stat <- regmatches(sql_files[2], regexpr(insert_regex, sql_files[2], perl = TRUE))
    
    insert_players <- gsub("([A-Za-z])\\\\'", "", regmatches(sql_files[3], regexpr(insert_regex, sql_files[3], perl = TRUE)))
    
    insert_teams <- regmatches(sql_files[4], regexpr(insert_regex, sql_files[4], perl = TRUE))
    
    
    dbExecute(con, insert_teams)
    dbExecute(con, insert_match)
    dbExecute(con, insert_players)
    dbExecute(con, insert_player_stat)
    return(con)
    
}
