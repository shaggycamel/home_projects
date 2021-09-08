
# Oliver Eaton
# Begun: 2021-09-08

# Hashtag analysis

# Environ -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(textclean)

# Database functions
source(here::here("data", "preprocessing", "database_functions.R"))

# Read in data
influencer_tweets <- read_table("influencer_tweets")
influencer_mentions <- read_table("influencer_mentions")


# Set dataframes for analysis ---------------------------------------------

# Working df
df <- bind_rows(
  distinct(influencer_tweets, status_id, text)
  , distinct(influencer_mentions, status_id, text)
) |> 
  filter(str_detect(text, "#"))

# hashtag df
hashtag_df <- unnest_tweets(df, input = text, output = hashtag) |> 
  filter(str_starts(hashtag, "#")) |> 
  distinct()

# word df
word_df <- mutate(df, language = cld2::detect_language(text)) |> 
  filter(language == "en") |> 
  mutate(
    text = str_replace_all(text, "&amp;", "and")
    , text = str_remove_all(text, "(@|#)[_a-z0-9]+")
    , text = replace_names(text)
    , text = replace_email(text)
    , text = replace_non_ascii(text)
    , text = replace_word_elongation(text)
    , text = replace_contraction(text)
    , text = replace_hash(text)
    , text = str_remove_all(text, "[:digit:]")
  ) |> 
  na.omit() |> 
  filter(strip(text) != "") |> 
  select(-language) |> 
  unnest_tweets(input = text, output = word, strip_url = TRUE) |> 
  anti_join(stop_words, by = "word") |> 
  filter(str_detect(word, "\\W", negate = TRUE)) |> 
  mutate(word = textstem::stem_words(word)) |> 
  {\(.) left_join(., count(., word), by = "word")}() |> 
  filter(n > 3) |> 
  select(-n)
             

# Bag of words ------------------------------------------------------------

# Dummy cols
