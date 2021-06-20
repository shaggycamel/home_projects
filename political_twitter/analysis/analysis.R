
# Oliver Eaton
# Begun: 2021-05-27

# Analysis of political twitter data

# Environ -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(cld2)
library(SnowballC)
library(textstem)
library(wordcloud2)
library(wordcloud)
library(textclean)

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

# Think about removing retweets from database
table(mentions$is_retweet)
table(tweets$is_retweet)

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


# Tweet topic modelling ---------------------------------------------------

z <- "@AxelWilkeNZ @loksabhaspeaker 😢, $100 too brb afk" |> str_to_lower()
x <- c('look', 'noooooo!', 'real coooool!', "it's sooo goooood", 'fsdfds', 
       'fdddf', 'as', "aaaahahahahaha", "aabbccxccbbaa", 'I said heyyy!',
       "I'm liiiike whyyyyy me?", "Wwwhhatttt!", NA)

replace_word_elongation(x)
replace_word_elongation(x, impart.meaning = TRUE)
  

# Remove punctuation, emojis, urls, handles, stopwords, numbers
# stem, lem, potentially remove words less than 3 in length
# remove hashtag words
# having a bit of trouble with emojis
twt_anl <- filter(tweets, is_retweet == FALSE) |> 
  # select(user_id, screen_name, status_id, created_at, text) |> 
  select(text) |> 
  mutate(
    text = str_to_lower(text)
    # Replace ampersands with "and"
    , text = str_replace_all(text, "&amp;", "and")
    # Remove twitter handles and hashtags
    , text = str_remove_all(text, "(@|#)[_a-z0-9]+")
    # Remove URLS
    , text = str_remove_all(text, "https?\\S*")
    # Replace contractions - turns "it's" into "it is"
    , text = replace_contraction(text)
    # Replace internet slang
    , text = replace_internet_slang(text)
    # Replace word elongation
    , text = replace_word_elongation(text)
    # Remove punctuation
    , text = str_remove_all(text, "[:punct:]")
    # Remove numbers
    , text = str_remove_all(text, "[:digit:]")
    # Remove emoji (ascii characters)
    , text = replace_non_ascii(text)
    # Final chance at non-word characters
    , text = str_replace_all(text, "\\W", " ")
    # Clean up
    , text = str_squish(text)
  ) |>
  filter(text != "") |> 
  # Cant get this working
  mutate(text = str_replace_all(text, c(
    "nz"="new-zealand"
    , "new-zealand"="new zealand"
    , "aus" = "autralia"
  ))) |> 
  unnest_tokens(word, text, token = "words") |>
  anti_join(get_stopwords(), by = "word") |>
  mutate(
    stem_text = wordStem(word)
    , lem_text = lemmatize_words(word)
  )

wordcloud(filter(count(twt_anl, word), n > 400))


# Now do tf-idf
