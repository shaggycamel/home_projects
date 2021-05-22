
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