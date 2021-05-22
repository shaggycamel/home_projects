
# Oliver Eaton
# Begun: 2021-05-14
# Last updated:

# File to:
# Gather polictal tweets from Australia and New Zealand

# Environ -----------------------------------------------------------------

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
