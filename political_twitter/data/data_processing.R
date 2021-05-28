
# Oliver Eaton
# Begun: 2021-05-14

# Driver file to gather tweets from political leaders across
# Australia and New Zealand

# Environ -----------------------------------------------------------------

# Start timer
start_time <- Sys.time()

library(tidyverse)

# rtweet configuration
source(here::here("data", "config", ".rtweet_config.R"))

# database functions
source(here::here("data", "preprocessing", "database_functions.R"))

# Preprocessing -----------------------------------------------------------

source(here::here("data", "preprocessing", "influencer_twitter_id.R"))
source(here::here("data", "preprocessing", "influencer_twitter_details.R"))
source(here::here("data", "preprocessing", "influencer_twitter_tweets.R"))
source(here::here("data", "preprocessing", "influencer_twitter_friends.R"))
source(here::here("data", "preprocessing", "influencer_twitter_mentions.R"))

# Write to Database -------------------------------------------------------

write_table(influencer_twitter_details)
write_table(influencer_tweets)
write_table(influencer_friends)
write_table(influencer_mentions)

# Log File ----------------------------------------------------------------

# End timer
end_time <- Sys.time()

# Print to log file
write(
  paste(
    start_time
    , end_time
    , round(difftime(end_time, start_time, units = "secs"),2)
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
print(paste0(Sys.Date(), ": political_twitter database updated..."))

