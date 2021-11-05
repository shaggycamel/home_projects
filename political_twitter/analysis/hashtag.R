
# Oliver Eaton
# Begun: 2021-09-08

# Hashtag analysis

# Environ -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(textclean)
library(janitor)
library(widyr)
library(tidymodels)
library(Matrix)

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
df <- bind_rows(df_prep(influencer_tweets, TRUE)
                , df_prep(influencer_mentions, TRUE))

# hashtag df
hashtag_df <- unnest_tweets(df, input = text, output = hashtag) |> 
  filter(str_starts(hashtag, "#")) |> 
  distinct() |> 
  mutate(hashtag = str_remove_all(hashtag, pattern = "[:emoji:]"))

# word df
word_df <- clean_text(df) |> 
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
  rename(.h1 = item1, .h2 = item2) |>
  arrange(.h2, desc(similarity)) |> 
  rowwise() |>
  mutate(c = as.character(list(sort(c(.h1, .h2))))) |> 
  with_groups(c, ~ mutate(.x, c_c = row_number())) |> 
  filter(c_c == 1) |> 
  select(!ends_with("c")) |> 
  pivot_wider(names_from = .h2, values_from = similarity, values_fill = 0)
  # Maybe apply min max scaling

#hmmmm
k <- kmeans(column_to_rownames(similarity, ".h1"), centers = 50)
k$withinss
k$size



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


tree <- decision_tree(mode = "classification")
tree_fit <- fit(tree, .cluster ~ ., data = sp)

topcs_preds <- predict(tree_fit, new_data = test_df)



