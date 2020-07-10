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

source("rugby_pass_scrape_functions.R")

paths_allowed(paths = "https://index.rugbypass.com/") 
rPass <- read_html("https://index.rugbypass.com/")

# Get tournament information
# tournament_id <- rPass %>% html_nodes("#tournaments-drop-down") %>% html_nodes("li") %>% html_attr(name="data-value")
# tournament_name <- rPass %>% html_nodes("#tournaments-drop-down") %>% html_nodes("li") %>% html_text() %>% str_trim()
# tournaments <- as.data.frame(cbind(tournament_id, tournament_name, season))
tournaments <- read.csv("tournaments.csv", encoding = "UTF-8",
                        col.names = c("tournament_id", "tournament_name", "season", "geolocation"),
                        colClasses = c("character", "character", "character", "character"))

# Get team information
team_id <- rPass %>% html_nodes("#teams-drop-down") %>% html_nodes("li") %>% html_attr(name="data-value")
team_name <- rPass %>% html_nodes("#teams-drop-down") %>% html_nodes("li") %>% html_text()  %>% str_trim()
tournament_id <- rPass %>% html_nodes("#teams-drop-down") %>% html_nodes("li") %>% html_attr(name="data-tournaments")
teams <- as.data.frame(cbind(team_id, team_name, tournament_id))
teams <- separate_rows(teams, tournament_id)

df <- left_join(teams, tournaments, by = "tournament_id")
df <- df[complete.cases(df),]
df <- df[, c("season", "tournament_id", "tournament_name", "team_id", "team_name")] %>% 
  subset(grepl("super", tournament_name, ignore.case = TRUE) & season == "2020")

# Get player information
players <- data.frame()
for (tournament_row in 1:nrow(df)) {
  tournament <- df[tournament_row, "tournament_name"] %>% str_replace_all(" ", "-") %>% str_to_lower()
  team <- df[tournament_row, "team_name"] %>% str_to_lower() %>% str_replace_all(" ", "-")
  URL <- paste0("https://index.rugbypass.com/rpi/", tournament, "/", team, "/all/7-days/high-to-low/players/")
  
  # call player name function()
  players_temp <- player_name(URL)
  
  
  # Emtpy dataframe for loop
  p_bio <- data.frame()
  
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
  players <- rbind(players, players_temp)
}

# Cleaning
players <- players %>% select(season, tournament_id, team_id, player_id, player_name, position, dob_ymd, height_m, weight_kg)
players$weight_kg <- players$weight_kg %>% str_replace("kg", "")
players$height_m <- players$height_m %>% str_replace("m", "")
players <- na_if(players, "")
players <- na_if(players, "---")
players$height_m <- as.double(players$height_m)
players$weight_kg <- as.integer(players$weight_kg)
players$dob_ymd <- as.Date(players$dob_ymd, format = "%d %b %Y")


