library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)

setwd("C:/Users/olive/Desktop/N-SYNC")
source("rugby_pass_scrape_functions.R")

paths_allowed(paths = "https://index.rugbypass.com/") 
rPass <- read_html("https://index.rugbypass.com/")

tournament_info <- rPass %>% html_nodes("#tournaments-drop-down") %>% html_nodes("li")
tournament_id <- tournament_info %>% html_attr(name="data-value")
tournament_name <- tournament_info %>% html_text() %>% str_trim()
season <- "Jun-Aug 2020"
tournaments <- as.data.frame(cbind(tournament_id, tournament_name, season))

team_info <- rPass %>% html_nodes("#teams-drop-down") %>% html_nodes("li")
team_id <- team_info %>% html_attr(name="data-value")
team_name <- team_info %>% html_text()  %>% str_trim()
tournament_id <- team_info %>% html_attr(name="data-tournaments")
teams <- as.data.frame(cbind(team_id, team_name, tournament_id))
teams <- separate_rows(teams, tournament_id)

df <- left_join(teams, tournaments, by = "tournament_id")
df <- df[, c("season", "team_tournament_id", "tournament_name", "team_id", "team_name")] %>% 
  subset(grepl("aotearoa", df$tournament_name, ignore.case = TRUE) & team_name == "Blues")

for (tournament_row in 1:nrow(df)) {
  tournament <- df[tournament_row, "tournament_name"]
  team <- df[tournament_row, "team_name"]
  URL <- paste0("https://index.rugbypass.com/rpi/", tournament, "/", team, "/all/7-days/high-to-low/players/")
  
  # call player name function()
  players <- player_name(URL)
  
  # Reformat variable names for next url
  tournament <- tournament %>% str_to_lower() %>% str_replace_all(" ", "-")
  team <- team %>% str_to_lower() %>% str_replace_all(" ", "-")
  p_bio <- data.frame()
  
  for (player_row in 1:nrow(players)) {
    id <- players[player_row, "player_id"]
    name <- players[player_row, "player_name"] %>% str_to_lower() %>% str_replace_all(" ", "-")
    URL <- paste0("https://index.rugbypass.com/rpi/", tournament, "/", team, "/", name)
    
    # call player bio function
    p_bio_tmp <- player_bio(URL)
    p_bio_tmp <- as.data.frame(t(append(p_bio_tmp, id, 0)))

    if(length(p_bio_tmp) == 1){
      colnames(p_bio_tmp) <- "player_id"
    } else {
      colnames(p_bio_tmp) <- c("player_id", "position", "height", "weight", "dob")
    }
    rownames(p_bio_tmp) <- player_row
    
    # collate rows
    p_bio <- bind_rows(p_bio, p_bio_tmp)
  }
  # Join p_bio onto players df
  players <- left_join(players, p_bio, by = "player_id")
  players$team_id <- as.list(df[tournament_row, "team_id"])
  players$season <- as.list(df[tournament_row, "season"])
  players <- players %>% select(season, team_id, player_id, player_name, position, dob, height, weight)
}

