
# Oliver Eaton
# Begun: 2021-05-14
# Last updated:

# File to:
# Gather polictal tweets from Australia and New Zealand


# Environ -----------------------------------------------------------------

library(tidyverse)
library(rtweet)

source(here::here("data", "r_twitter_setup.R"))
source(here::here("data", "twitter_influencer_db_setup.R"))

# Get Twitter id ----------------------------------------------------------

influencer_id <- lookup_users(influencers) %>% 
  select(user_id, account_created_at) %>%
  mutate(research_inclusion_date = Sys.Date())

# Get Twitter Details -----------------------------------------------------

influencer_details <- lookup_users(influencer_id$user_id) %>% 
  select(
    user_id
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


# Get Tweets --------------------------------------------------------------

tweets <- get_timeline(influencer_id$user_id)

# Get Freinds -------------------------------------------------------------

friends <- map_df(influencer_id$user_id, get_friends, retryonratelimit = TRUE)
  
  