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
library(RMySQL)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("rugby_pass_scrape_functions.R")

# Check if bots are allowed on website. If TRUE then okay to proceed
paths_allowed(paths = "https://index.rugbypass.com/") 

# Get html of rugby pass website
rPass <- read_html("https://index.rugbypass.com/")

# Get tournament information
tournament_id <- rPass %>% html_nodes("#tournaments-drop-down") %>% html_nodes("li") %>% html_attr(name="data-value")
tournament_name <- rPass %>% html_nodes("#tournaments-drop-down") %>% html_nodes("li") %>% html_text() %>% str_trim()
tournament_season <- read.csv("tournament_season.csv")
tournament_season$tournament_id <- as.character(tournament_season$tournament_id)
tournaments <- as.data.frame(cbind(tournament_id, tournament_name))
tournaments <- left_join(tournaments, tournament_season, by = c("tournament_id"))
tournaments <- tournaments %>% select(tournament_id, tournament_name.x, season, geolocation)
colnames(tournaments) <- c("tournament_id", "tournament_name", "season", "geolocation")


# Get team information
team_id <- rPass %>% html_nodes("#teams-drop-down") %>% html_nodes("li") %>% html_attr(name="data-value")
team_name <- rPass %>% html_nodes("#teams-drop-down") %>% html_nodes("li") %>% html_text()  %>% str_trim()
tournament_id <- rPass %>% html_nodes("#teams-drop-down") %>% html_nodes("li") %>% html_attr(name="data-tournaments")
teams <- as.data.frame(cbind(team_id, team_name, tournament_id))
teams <- separate_rows(teams, tournament_id)

# Prepare df to get player data
df <- left_join(teams, tournaments, by = "tournament_id")
df <- df[complete.cases(df),]
df <- df[, c("season", "tournament_id", "tournament_name", "team_id", "team_name")] %>% 
  subset(grepl("aotearoa", tournament_name, ignore.case = TRUE) & season == "2020")

# Get player information
players <- get_player_data(df)

# Establish connection to database
db_connection <- dbConnect(MySQL(), user="oli", password=Sys.getenv("MySQL_password"), dbname="rugby_analysis", host="localhost")

# Write tables to database
dbWriteTable(db_connection, "tournaments", tournaments)
dbWriteTable(db_connection, "teams", teams)
dbWriteTable(db_connection, "players", players)

# Read table
dbReadTable(db_connection, "players")