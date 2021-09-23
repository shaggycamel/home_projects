
# Oliver Eaton
# Begun: 2021-09-08

# Hashtag analysis

# Environ -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(textclean)
library(widyr)
library(tidymodels)

source(here::here("functions", "database_functions.R")) # Database functions
source(here::here("functions", "help.R")) # Helper functions

# Read in data
influencer_tweets <- read_table("influencer_tweets")
influencer_mentions <- read_table("influencer_mentions")


# Set dataframes for analysis ---------------------------------------------

df_prep <- function(df, hash) {
  tmp <- hash_at_link_ratio(df) |> 
    filter(created_at >= (Sys.Date() - 30), str_detect(text, "#", negate = !hash)) |> 
    distinct(status_id, text)
}

# Working df
df <- bind_rows(df_prep(influencer_tweets, TRUE), df_prep(influencer_mentions, TRUE))

# hashtag df
hashtag_df <- unnest_tweets(df, input = text, output = hashtag) |> 
  filter(str_starts(hashtag, "#")) |> 
  distinct() |> 
  mutate(hashtag = str_remove_all(hashtag, pattern = "[:emoji:]"))

# word df
word_df <- mutate(df, language = cld2::detect_language(text)) |> 
  filter(language == "en") |> 
  mutate(
    text = str_replace_all(text, "&amp;", "and")
    , text = str_remove_all(text, "(@|#)[_a-z0-9]+")
    # , text = replace_names(text)
    , text = replace_email(text)
    , text = replace_word_elongation(text)
    , text = replace_contraction(text)
    , text = replace_non_ascii(text)
    # , text = replace_hash(text)
    , text = str_remove_all(text, "[:digit:]")
  ) |> 
  na.omit() |> 
  filter(strip(text) != "") |> 
  select(-language) |> 
  unnest_tweets(input = text, output = word, strip_url = TRUE) |> 
  filter(str_detect(word, "\\W", negate = TRUE), word != "") |> 
  mutate(word = textstem::stem_words(word))


# Bag of words ------------------------------------------------------------

# tf_idf
tfidf <- left_join(hashtag_df, word_df, by ="status_id") |> 
  select(-status_id) |>
  na.omit() |> 
  with_groups(c(hashtag, word), ~ summarise(.x, n = n())) |> 
  bind_tf_idf(word, hashtag, n) |> 
  anti_join(stop_words, by = "word")

# cosine similarity
similarity <- pairwise_similarity(tfidf, hashtag, word, tf_idf) |> 
  na.omit() |> 
  filter(similarity >= 0.05) |> 
  arrange(item2, desc(similarity))

# K-means  ->>>>>> Need to fine tune this. There is always one large group
set.seed(123)
# km <- map(5:20, ~ widely_kmeans(similarity, item1, item2, similarity, k = .x, fill = 0))
# map(km, ~ count(.x, cluster))
km <- widely_kmeans(similarity, item1, item2, similarity, k = 12, fill = 0)
count(km, cluster)


# Decision Tree -----------------------------------------------------------

# Join hash clusters to word groups & pivot wider
sp <- left_join(
  select(km, .cluster = cluster, .hashtag = item1)
  , select(tfidf, hashtag, word, n)
  , by = c(".hashtag"="hashtag")
) |> 
  arrange(.cluster, .hashtag, word) |> 
  pivot_wider(names_from = word, values_from = n)



test_df <- bind_rows(df_prep(influencer_tweets, FALSE), df_prep(influencer_mentions, FALSE)) |> 
  clean_text() |> 
  filter(word != "") |> 
  semi_join(data.frame(word = colnames(sp))) |> 
  count(status_id, word) |> 
  arrange(word) |> 
  pivot_wider(names_from = word, values_from = n)

# columns in train that aren't in test
setdiff(colnames(sp), colnames(test_df))
t <- select(sp, .cluster, all_of(colnames(test_df)[-1]))

tree <- decision_tree(mode = "classification", tree_depth = 5)
tree_fit <- fit(tree, .cluster ~ ., data = t)
topcs_preds <- predict(tree_fit, new_data = test_df)


count(topcs_preds, .pred_class)
