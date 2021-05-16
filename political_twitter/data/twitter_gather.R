
# Oliver Eaton
# Begun: 2021-05-14
# Last updated:

# File to:
# Gather polictal tweets from Australia and New Zealand


# Environ -----------------------------------------------------------------

library(tidyverse)
library(rtweet)
library(RMySQL)

source(here::here("data", "rtweet_config.R"))
# source(here::here("data", "influencer_config.R"))

sql_con = dbConnect(MySQL(), user = "oli", dbname = "political_twitter")

# Get Twitter id ----------------------------------------------------------

influencer_id <- dbReadTable(sql_con, "influencer_id") %>% 
  arrange(user_id) %>% 
  mutate(across(ends_with("date"), as.Date))


# Get Twitter Details -----------------------------------------------------

# First read in details from db
old_dt <- dbReadTable(sql_con, "influencer_twitter_details") %>% 
  mutate(
    as_of_date = as.Date(as_of_date)
    , account_created_at = as.POSIXct(account_created_at)
  )

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

# Third compare lists second and first step
# get indicies of differences in new_dt and append onto old_dt.
new_indicies <- setdiff(
  new_dt %>% select(-as_of_date)
  , old_dt %>% select(-as_of_date)
) %>% rownames()

# Influencer twitter details
influencer_twitter_details <- rbind(
  new_dt[new_indicies, ]
  , old_dt
)


# Get Tweets --------------------------------------------------------------

# First read in tweets from database
old_tw <- dbReadTable(sql_con, "influencer_tweets") %>% 
  mutate(created_at = as.POSIXct(created_at))

# Second get latest tweet_id for each user
tw_id <- old_tw %>% 
  group_by(user_id) %>% 
  slice_max(order_by = ~ -status_id, with_ties = FALSE) %>% 
  arrange(user_id) %>% 
  select(user_id, status_id)

# Lastly limit tweet collection from last tweet sent by each user
new_tw <- map2_df(
  .x = tw_id$user_id
  , .y = tw_id$status_id
  , ~ get_timelines(user = .x, retryonratelimit = TRUE, since_id = .y)
) %>% 
  mutate(across(where(is.list), as.character))

# Influencer tweets
influencer_tweets <- rbind(
  new_tw
  , old_tw
)

# Get Freinds -------------------------------------------------------------

# First read in friends from db
old_fr <- dbReadTable(sql_con, "influencer_friends") %>% 
  mutate(across(starts_with("date"), as.Date))

# Second gather friends from twitter
new_fr_t <- get_friends(influencer_id$user_id, retryonratelimit = TRUE) %>%  
  rename(user_id = user, friend_user_id = user_id) 

# Third compare lists from first and second step
# New friendships
new_fr <- map_df(influencer_id$user_id, ~ {
  
  setdiff(
    new_fr_t %>% filter(user_id == .x)
    , old_fr %>% select(contains("user_id")) %>% filter(user_id == .x)
  ) %>% 
    mutate(
      date_friendship_began = Sys.Date()
      , date_friendship_ceased = NA_character_
    )
  
})

# Removed friendships
walk(influencer_id$user_id, ~{
  
  f_rem <- setdiff(
    old_fr %>% select(contains("user_id")) %>% filter(user_id == .x)
    , new_fr %>% filter(user_id == .x)
  )
  
  old_fr$date_friendship_ceased[
    old_fr$user_id == .x & 
      old_fr$friend_user_id %in% f_rem$friend_user_id
  ] <<- Sys.Date()
  
})

# Influencer friends
influencer_friends <- rbind(
  old_fr
  , new_fr
)


# Database ----------------------------------------------------------------

# Influencer ID
dbWriteTable(
  sql_con
  , "influencer_id"
  , influencer_id
  , row.names = FALSE
  , overwrite = TRUE
  # , field.types = c(research_inclusion_date = "date", research_exclusion_date = "date")
)

# Influencer details
dbWriteTable(
  sql_con
  , "influencer_twitter_details"
  , influencer_twitter_details
  , row.names = FALSE
  , overwrite = TRUE
  # , field.types = c(as_of_date = "date", account_created_at = "datetime")
)

# Influencer Tweets
dbWriteTable(
  sql_con
  , "influencer_tweets"
  , influencer_tweets
  , row.names = FALSE
  , overwrite = TRUE
  # , field.types = c(created_at = "datetime")
)

# Influencer Friends
dbWriteTable(
  sql_con
  , "influencer_friends"
  , influencer_friends
  , row.names = FALSE
  , overwrite = TRUE
  # , field.types = c(date_friendship_began = "date", date_friendship_ceased = "date")
)

