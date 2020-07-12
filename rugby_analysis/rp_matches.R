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
library(data.table)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check if bots are allowed on website. If TRUE then okay to proceed
paths_allowed("https://www.rugbypass.com/super-rugby-aotearoa/matches/")

# Get html of rugby pass matches website
rPassMatches <- read_html("https://www.rugbypass.com/super-rugby-aotearoa/matches/")

fixture_results <- data.frame()
rPassMatchesRounds <- as.list(rPassMatches %>% html_nodes("div.games") %>% html_children())
for(round in rPassMatchesRounds){
  match_id <- round %>% html_nodes("div[itemscope]") %>% html_attr("data-id")
  match_date <- round %>% html_nodes("meta[itemprop=startDate]") %>% html_attr("content")
  home_team <- round %>% html_nodes("span.teams") %>% 
          html_node("span[itemprop=homeTeam]") %>% html_nodes("a.name") %>% html_text(trim = TRUE)
  home_score <- round %>% html_nodes("span.teams") %>% html_nodes("span.home") %>% html_text(trim = TRUE)
  away_team <- round %>% html_nodes("span.teams") %>% 
          html_node("span[itemprop=awayTeam]") %>% html_nodes("a.name") %>% html_text(trim = TRUE)
  away_score <- round %>% html_nodes("span.teams") %>% html_nodes("span.away") %>% html_text(trim = TRUE)
  if(is_empty(home_score)){
    home_score <- vector(mode="character", length=length(match_id))
  }
  if(is_empty(away_score)){
    away_score <- vector(mode="character", length=length(match_id))
  }
  
  tmp <- cbind(match_id, match_date, home_team, home_score, away_team, away_score)
  fixture_results <- fixture_results %>% rbind(tmp)
}
fixture_results$home_score <- as.integer(fixture_results$home_score)
fixture_results$away_score <- as.integer(fixture_results$away_score)
fixture_results$match_date <- as_datetime(fixture_results$match_date , tz="NZ")
fixture_results$match_time <- as.ITime(fixture_results$match_date)
fixture_results$match_date <- as.IDate(fixture_results$match_date)
fixture_results <- fixture_results %>% select(match_id, match_date, match_time, home_team, home_score, away_team, away_score)


# Establish connection to database
db_connection <- dbConnect(MySQL(), user="oli", password=Sys.getenv("MySQL_password"), dbname="rugby_analysis", host="localhost")

# Read table
dbReadTable(db_connection, "players")


