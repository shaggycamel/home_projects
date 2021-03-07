
library(tidyverse)
library(rtweet)



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

t <- get_timeline(
  influencer_id$user_id
  # , since_id = t1 %>% filter(created_at == max(created_at)) %>% pull(status_id)
  # , = 3200
)
tweets <- rbind(t, tweets)


# Get Freinds -------------------------------------------------------------

for(i in 1:nrow(influencer_id)){
  f <- get_friends(
    influencer_id$user_id[i]
    , retryonratelimit = TRUE 
  )
  
  if_else(
    !exists("friends")
    , friends <- rbind(f, friends)
    , friends <- f
  )
  
}
