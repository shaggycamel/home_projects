# Start function
player_name <- function(URL) {
  teamPass <- read_html(URL)
  player_name <- teamPass %>% html_nodes("#players-graph") %>% html_nodes("span.name") %>% html_text()
  player_id <- teamPass %>% html_nodes("#players-graph") %>% html_nodes("span.btn-compare") %>% html_attr("data-id")
  players <- as.data.frame(cbind(player_id, player_name))
}
# End function

# Start function
player_bio <- function(URL){
  playerPass <- read_html(URL)
  player_position <- playerPass %>% html_nodes("div.show-990") %>% html_nodes("div.positions") %>% html_text() %>% str_trim()
  player_bio <- playerPass %>% html_nodes("div.show-990") %>% html_nodes("div.measurement") %>% html_text() %>% str_replace(c("Height","Weight","DOB"), "") %>% str_trim()
  # This should really be gathered from historical records present in db.
  # player_exp <- playerPass %>% html_nodes("div.show-990") %>% html_nodes("div.player-teams") %>% html_nodes("span.title") %>% html_text() %>% str_trim()
  player <- append(player_bio, player_position, 0)
}
# End function

# Start function
# df must contain columns: "tournament_name", "team_name", "season"
get_player_data <- function(df){
  
  # Empty dataframe
  players <- data.frame()
  
  # Loop over tournaments in df
  for (tournament_row in 1:nrow(df)) {
    tournament <- df[tournament_row, "tournament_name"] %>% str_to_lower() %>% str_replace_all(" ", "-")
    team <- df[tournament_row, "team_name"] %>% str_to_lower() %>% str_replace_all(" ", "-")
    URL <- paste0("https://index.rugbypass.com/rpi/", tournament, "/", team, "/all/7-days/high-to-low/players/")
    
    # call player name function()
    players_temp <- player_name(URL)
    
    # Emtpy dataframe for loop
    p_bio <- data.frame()
    
    # Loop over each player in team to get info
    for (player_row in 1:nrow(players_temp)) {
      id <- players_temp[player_row, "player_id"]
      name <- players_temp[player_row, "player_name"] %>% str_to_lower() %>% str_replace_all(" ", "-")
      URL <- paste0("https://index.rugbypass.com/rpi/", tournament, "/", team, "/", name)
      
      # call player bio function
      p_bio_tmp <- player_bio(URL)
      p_bio_tmp <- as.data.frame(t(append(p_bio_tmp, id, 0)))
      
      if(length(p_bio_tmp) == 1){
        colnames(p_bio_tmp) <- "player_id"
      } else {
        colnames(p_bio_tmp) <- c("player_id", "position", "height_m", "weight_kg", "dob_ymd")
      }
      # collate rows
      p_bio <- bind_rows(p_bio, p_bio_tmp)
    }
    p_bio$tournament_id <- as.list(df[tournament_row, "tournament_id"])
    p_bio$team_id <- as.list(df[tournament_row, "team_id"])
    p_bio$season <- as.list(df[tournament_row, "season"])
    players_temp <- left_join(players_temp, p_bio, by = "player_id")
    # players_temp$dob_ymd <- as.character(players_temp$dob_ymd)
    players <- rbind(players, players_temp)
  }
  
  # Cleaning
  players <- players %>% select(season, tournament_id, team_id, player_id, player_name, position, dob_ymd, height_m, weight_kg)
  players$tournament_id <- unlist(players$tournament_id)
  players$team_id <- unlist(players$team_id)
  players$season <- unlist(players$season)
  players$weight_kg <- players$weight_kg %>% str_replace("kg", "")
  players$height_m <- players$height_m %>% str_replace("m", "")
  players <- na_if(players, "")
  players <- na_if(players, "---")
  players$height_m <- as.double(players$height_m)
  players$weight_kg <- as.integer(players$weight_kg)
  players$dob_ymd <- as.Date(players$dob_ymd, format = "%d %b %Y")
  players
}
# End function