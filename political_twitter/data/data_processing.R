
# Oliver Eaton
# Begun: 2021-05-14

# Driver file to gather tweets from political leaders across
# Australia and New Zealand

# Environ -----------------------------------------------------------------

# Start timer
start_time <- Sys.time()

library(tidyverse)
library(rtweet)
library(RMySQL)

# rtweet configuration
source(here::here("data", "config", ".rtweet_config.R"))

# Connection to MySQL database
sql_con = dbConnect(MySQL(), user = "oli", dbname = "political_twitter")

# Preprocessing -----------------------------------------------------------

source(here::here("data", "preprocessing", "influencer_twitter_id.R"))
source(here::here("data", "preprocessing", "influencer_twitter_details.R"))
source(here::here("data", "preprocessing", "influencer_twitter_tweets.R"))
source(here::here("data", "preprocessing", "influencer_twitter_friends.R"))
source(here::here("data", "preprocessing", "influencer_twitter_mentions.R"))


# Write to Database -------------------------------------------------------

write_to_db <- function(table){
  dbWriteTable(
    sql_con
    , deparse(substitute(table))
    , table
    , row.names = FALSE
    , overwrite = TRUE
  )
}

write_to_db(influencer_id)
write_to_db(influencer_twitter_details)
write_to_db(influencer_tweets)
write_to_db(influencer_friends)
write_to_db(influencer_mentions)


# Log File ----------------------------------------------------------------

# End timer
end_time <- Sys.time()

# Print number of rows to log file
write(
  paste(
    Sys.Date()
    , round(end_time - start_time, 2)
    , nrow(influencer_id)
    , nrow(influencer_twitter_details)
    , nrow(influencer_tweets)
    , nrow(influencer_friends)
    , nrow(influencer_mentions)
    , sep = "\t"
  )
  , file = here::here(".log")
  , append = TRUE
)

# Print complete message
print("political_twitter database updated...")



