
# Oliver Eaton
# Begun: 2021-05-27

# Analysis of political twitter data

# Environ -----------------------------------------------------------------

library(tidyverse)

# Database functions
source(here::here("data", "preprocessing", "database_functions.R"))

# Read in data
details <- read_table("influencer_twitter_details")
tweets <- read_table("influencer_tweets")
mentions <- read_table("influencer_mentions")
friends <- read_table("influencer_friends")

pols <- read_csv(
  here::here("data", "influencer_political_details.csv")
  , col_types = cols(twitter_id = col_character())
)

# Friendship change rate --------------------------------------------------

details <- left_join(
  details
  , pols %>% select(twitter_id, country)
  , by = c("user_id"="twitter_id")
)

details %>% 
  filter(name == "Adam Bandt") %>% 
  ggplot(aes(x = as_of_date, y = friends_count))

