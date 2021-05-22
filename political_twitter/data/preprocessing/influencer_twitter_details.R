
# Get Twitter Details -----------------------------------------------------

# First read in details from db
dt_old <- dbReadTable(sql_con, "influencer_twitter_details") %>% 
  mutate(
    as_of_date = as.Date(as_of_date)
    , account_created_at = as.POSIXct(account_created_at)
  )

# Second gather details from twitter
dt_new <- lookup_users(influencer_id$user_id) %>% 
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

# Influencer twitter details
influencer_twitter_details <- bind_rows(dt_new, dt_old)