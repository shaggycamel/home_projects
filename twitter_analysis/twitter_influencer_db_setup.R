
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
  , research_inclusion_date = character()
  , stringsAsFactors = FALSE
)
# Convert date_included to date


# details -----------------------------------------------------------------

influencer_details <- data.frame(
  twitter_id = character()
  , twitter_screen_name  = character()
  , name = character()
  , location = character()
  , twitter_creation_date = character()
  , twitter_description = character()
  , date_twitter_updated = character()
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


