
# Oliver Eaton
# Begun: 2021-05-14
# Last updated:

# File to:
# Gather polictal tweets from Australia and New Zealand


# Environ -----------------------------------------------------------------

library(tidyverse)
library(rtweet)
library(RMySQL)

source(here::here("data", "rtweet_config"))
source(here::here("data", "influencer_config.R"))

sql_con = dbConnect(MySQL(), user = "oli", dbname = "political_twitter")

# Get Twitter id ----------------------------------------------------------

influencer_id <- dbReadTable(sql_con, "influencer_id")


# Get Twitter Details -----------------------------------------------------

# First read in details from db
old_dt <- dbReadTable(sql_con, "influencer_twitter_details")

# Second gather details from twitter
new_dt <- lookup_users(influencer_id$user_id) %>% 
  select(
    user_id
    , account_created_at
    , screen_name
    , name
    , location
    , description
    , latest_tweet_id = status_id
    , followers_count
    , friends_count
    , listed_count
    , statuses_count
    , favourites_count
  ) %>% 
  mutate(as_of_date = Sys.Date()) %>% 
  relocate(as_of_date)

# Third compare lists from first and second step
# influencer_twitter_details

setdiff(old_dt, new_dt)


# Get Tweets --------------------------------------------------------------

# First read in tweets from database
old_tw <- dbReadTable(sql_con, "influencer_tweets")

# Second get latest tweet_id for each user


# Lastly limit tweet collection from last tweet sent by each user
influencer_tweets <- get_timelines(influencer_id$user_id, retryonratelimit = TRUE) %>% 
  mutate(across(where(is.list), as.character))


# Get Freinds -------------------------------------------------------------

# First read in friends from db
old_fr <- dbReadTable(sql_con, "influencer_friends")

# Second gather friends from twitter
new_fr <- get_friends(influencer_id$user_id, retryonratelimit = TRUE) %>%  
  rename(user_id = user, friend_user_id = user_id) %>% 
  mutate(
    date_friendship_began = NA_character_
    , date_friendship_ceased = NA_character_
  )

# Third compare lists from first and second step
influencer_friends



# Database ----------------------------------------------------------------

# Influencer ID
dbWriteTable(
  sql_con
  , "influencer_id"
  , influencer_id
  , field.types = c(research_inclusion_date = "date", research_exclusion_date = "date")
  , row.names = FALSE
  , overwrite = TRUE
)

# Influencer details
dbWriteTable(
  sql_con
  , "influencer_twitter_details"
  , influencer_twitter_details
  , field.types = c(as_of_date = "date", account_created_at = "datetime")
  , row.names = FALSE
  , overwrite = TRUE
)

# Influencer Tweets
dbWriteTable(
  sql_con
  , "influencer_tweets"
  , influencer_tweets
  , field.types = c(created_at = "datetime")
  , row.names = FALSE
  , overwrite = TRUE
)

# Influencer Friends
dbWriteTable(
  sql_con
  , "influencer_friends"
  , influencer_friends
  , field.types = c(date_friendship_began = "date", date_friendship_ceased = "date")
  , row.names = FALSE
  , overwrite = TRUE
)

