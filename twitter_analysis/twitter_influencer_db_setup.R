
library(tidyverse)

# TEMP --------------------------------------------------------------------

influencers <- c(
  "TheEllenShow"
  , "KimKardashian"
  , "Cristiano"
  , "BillGates"
  , "BarackObama"
  , "ArianaGrande"
  , "katyperry"
  , "elonmusk"
  , "taylorswift13"
  , "narendramodi"
)


# id ----------------------------------------------------------------------

influencer_id <- data.frame(
  twitter_id = character()
  , date_included = character()
  , stringsAsFactors = FALSE
)
# Convert date_included to date


# details -----------------------------------------------------------------

influencer_details <- data.frame(
  date_twitter_updated = character()
  , twitter_id = character()
  , twitter_screen_name  = character()
  , name = character()
  , twitter_creation_date = character()
  , twitter_description = character()
  , stringsAsFactors = FALSE
)
# Convert date_twitter_updated, twitter_creation_date  to date


# friends_followers -------------------------------------------------------

influencer_friends_followers <- data.frame(
  date = character()
  , twitter_id = character()
  , follower_count = character()
  , friend_ids = character()
  , stringsAsFactors = FALSE
)
# Convert follower_count to double
# Convert friend_ids to JSON


# tweets ------------------------------------------------------------------

influencer_tweets <- data.frame(
  twitter_id = character()
  , tweet_date = character()
  , tweet = character()
  , stringsAsFactors = FALSE
)
# Convert tweet_date to timestamp














for(i in influencers){
  usr <- getUser(i)
  df[nrow(df)+1, ] <- c(
    Sys.Date() %>% toString()
    , usr$id
    , usr$screenName
    , usr$name
    , usr$location
    
  )
}


