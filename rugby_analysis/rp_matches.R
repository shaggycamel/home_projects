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

paths_allowed("https://www.rugbypass.com/super-rugby-aotearoa/matches/")





# Establish connection to database
db_connection <- dbConnect(MySQL(), user="oli", password=Sys.getenv("MySQL_password"), dbname="rugby_analysis", host="localhost")

# Read table
dbReadTable(db_connection, "players")


