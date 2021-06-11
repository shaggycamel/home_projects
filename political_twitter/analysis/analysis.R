
# Oliver Eaton
# Begun: 2021-05-27

# Analysis of political twitter data

# Environ -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(cld2)

# Database functions
source(here::here("data", "preprocessing", "database_functions.R"))

# Read in data
tweets <- read_table("influencer_tweets")
mentions <- read_table("influencer_mentions")
friends <- read_table("influencer_friends")
details <- read_table("influencer_twitter_details") |> 
  group_by(user_id) |> 
  slice_max(as_of_date)

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
  ggplot(aes(x = as_of_date, y = friends_count)) +
  geom_line()


# ( Current f# / Initial f# ) * 100

friend_index <- details |> 
  select(user_id, screen_name, as_of_date, friends_count) |> 
  left_join(
    (details |> 
      group_by(user_id) |> 
      filter(as_of_date == min(as_of_date)) |> 
      select(user_id, f_c_base = friends_count))
    , by = "user_id"
  ) |> 
  mutate(friend_index = (friends_count / f_c_base) * 100)

ggplot(friend_index, aes(x = as_of_date, y = friend_index, colour = screen_name)) +
  geom_line()


# Mention Sentiment -------------------------------------------------------
# Remove URLs, remove foreign language tweets.
# Concat tweets that have the same tweet_id but different text, try find out 
# which tweet should come first in that sequence 

# To detect language cld2 package (could also try franc)


mnt_anl <- filter(mentions,  is_retweet==FALSE) |> 
  select(status_id, created_at, mentions_user_id_single, text) |> 
  mutate(
    # Remove handles mentioned
    text = str_to_lower(str_squish(str_remove_all(text, "@[\\w\\d]+")))
    # Remove URLS
    , text = str_squish(str_remove_all(text, "https?\\S*"))
    # Detect language
    , language = detect_language(text)
  ) |> 
  filter(language == "en")




  # unnest_tokens(word, text, token = "sentences") |> 
  # anti_join(get_stopwords()) |> 
  # inner_join(get_sentiments("bing"), by = "word")


sentiment_ratio <- mnt_anl |> 
  group_by(mentions_user_id_single) |> 
  count(sentiment) |> 
  left_join(
    details |> select(user_id, name)
    , by = c("mentions_user_id_single"="user_id")
  )





