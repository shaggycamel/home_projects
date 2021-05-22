
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

# Figure out how to handle influencers that have been
# removed from the study


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


# Get Tweets --------------------------------------------------------------

# First read in tweets from database
tw_old <- dbReadTable(sql_con, "influencer_tweets") %>% 
  mutate(across(contains("created_at"), as.POSIXct))

# Second get latest tweet_id for each user from old_tw
tw_id <- tw_old %>% 
  group_by(user_id) %>% 
  slice_max(order_by = ~ -status_id, with_ties = FALSE) %>% 
  select(user_id, status_id, created_at)

# Lastly limit tweet collection from last tweet sent by each user
tw_new <- map2_df(
  .x = tw_id$user_id
  , .y = tw_id$status_id
  , ~ get_timelines(user = .x, retryonratelimit = TRUE, since_id = .y)
) %>% 
  mutate(across(where(is.list), as.character))

# Influencer tweets
influencer_tweets <- bind_rows(tw_new, tw_old)


# Get Friends -------------------------------------------------------------

# Gather Friends function
g_frnds <- function(u_id){
  
  n_friends = lookup_users(u_id)$friends_count
  curr_page = -1
  fetched_friends = 0
  i = 0
  all_friends = NULL
  
  while(fetched_friends < n_friends)  {
    
    if(rate_limit("get_friends")$remaining == 0) {
      print(paste0("API limit reached. Reseting at ", rate_limit("get_friends")$reset_at))
      Sys.sleep(as.numeric((rate_limit("get_friends")$reset + 0.1) * 60))
    }
    
    curr_friends <- get_friends(u_id, n = 5000, retryonratelimit = TRUE, page = curr_page)
    i <- i + 1
    all_friends <- rbind(all_friends, curr_friends)
    fetched_friends <- nrow(all_friends)
    print(paste0(i, ". ", fetched_friends, " out of ", n_friends, " fetched."))
    curr_page <- next_cursor(curr_friends)
  }
  distinct(all_friends)
}

# First read in friends from db
dbReadTable(sql_con, "influencer_friends") %>% 
  mutate(across(starts_with("date"), as.Date)) %>% {
    fr_csd <<- filter((.), !is.na(date_friendship_ceased))
    fr_old <<- filter((.), is.na(date_friendship_ceased))
  }
# Ceased friendships are separated so it doesn't affect the situation
# where a user re-kindles that friendship.

# Second gather friends from twitter
fr_new_tmp <- map_df(.x = influencer_id$user_id, ~ g_frnds(u_id = .x)) %>%  
  rename(user_id = user, friend_user_id = user_id) 

# Third compare lists from first and second step
# New friendships
fr_new <- map_df(influencer_id$user_id, ~ {
  
  setdiff(
    fr_new_tmp %>% filter(user_id == .x)
    , fr_old %>% select(contains("user_id")) %>% filter(user_id == .x)
  ) %>% 
    mutate(
      date_friendship_began = Sys.Date()
      , date_friendship_ceased = NA
    )
  
})

# Removed friendships
walk(influencer_id$user_id, ~{
  
  fr_rem <- setdiff(
    fr_old %>% filter(user_id == .x) %>% select(contains("user_id"))
    , fr_new_tmp %>% filter(user_id == .x)
  )
  
  fr_old$date_friendship_ceased[
    fr_old$user_id == .x & 
      fr_old$friend_user_id %in% fr_rem$friend_user_id
  ] <<- Sys.Date()
  
})

# Influencer friends
influencer_friends <- bind_rows(fr_csd, fr_old, fr_new)



# Get Mentions ------------------------------------------------------------
# Need to find a way to join twitter_user_id to mentions table

# First read in mentions from database
mnt_old <- dbReadTable(sql_con, "influencer_mentions") %>%
  mutate(across(contains("created_at"), as.POSIXct))

# Second get latest mention_id for each user from mnt_old
mnt_id <- mnt_old %>%
  group_by(mentions_user_id_single) %>%
  slice_max(order_by = ~ -status_id, with_ties = FALSE) %>% 
  select(mentions_user_id_single, mention_status_id = status_id, mention_created_at = created_at)

# Get screen names
screen_names_mnt_id <- filter(influencer_twitter_details, as_of_date == max(as_of_date)) %>% 
  select(as_of_date, user_id, screen_name) %>% 
  left_join(mnt_id, by =  c("user_id"="mentions_user_id_single"))

# Lastly limit mention collection from last mention of each user
mnt_new <- map2_df(
  .x = paste0("@", screen_names_mnt_id$screen_name)
  , .y = screen_names_mnt_id$mention_status_id
  , ~ tweets_with_users(search_tweets(.x, since_id = .y, parse = FALSE))
) %>% 
  mutate(
    # Convert lists to character
    across(where(is.list), as.character)
    
    # Split user_ids mentioned into a single cell
    , mentions_user_id_single = str_replace_all(
        string = mentions_user_id
        , pattern = c(
          "^c" = ""
          , "\\(" = ""
          , "\\)$" = ""
          , '"' = ""
        )
    ) 
  ) %>% 
  #Seperate rows of user_ids into single cell
  separate_rows(mentions_user_id_single) %>%
  relocate(mentions_user_id_single, .after = mentions_user_id) %>% 
  
  # Filter to influencers and exclude retweets
  filter(
    mentions_user_id_single %in% (influencer_id$user_id)
    , is_retweet == FALSE
  ) %>% 
  distinct()


# Influencer mentions
influencer_mentions <- bind_rows(mnt_old, mnt_new) %>% 
  group_by(status_id, mentions_user_id_single) %>% 
  slice_max(
    order_by = c(statuses_count, quoted_retweet_count, quoted_statuses_count) 
    , with_ties = FALSE
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

# Influencer Mentions
dbWriteTable(
  sql_con
  , "influencer_mentions"
  , influencer_mentions
  , row.names = FALSE
  , overwrite = TRUE
  # , field.types = c(date_friendship_began = "date", date_friendship_ceased = "date")
)

