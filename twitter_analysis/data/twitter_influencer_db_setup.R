

# TEMP --------------------------------------------------------------------

influencers <- c(
  "AlboMP"
  , "AdamBandt"
  , "PeterDutton_MP"
  , "JoshFrydenburg"
  , "GregHuntMP"
  , "ScottMorrisonMP"
  , "tanya_plibersek"
  , "cporterwa"
  , "DaveSharma"
  , "billshortenmp"
  , "SenatorCash"
  , "SenatorDodson"
  , "PaulineHansonOz"
  , "JacquiLambie"
  , "MarisePayne"
  , "SenatorWong"
)


# id ----------------------------------------------------------------------

influencer_id <- tibble(
  twitter_id = character()
  , research_inclusion_date = character()
)
# Convert date_included to date


# details -----------------------------------------------------------------

influencer_details <- tibble(
  twitter_id = character()
  , twitter_screen_name  = character()
  , name = character()
  , location = character()
  , twitter_creation_date = character()
  , twitter_description = character()
  , date_twitter_updated = character()
)
# Convert date_twitter_updated, twitter_creation_date  to date


# friends_followers -------------------------------------------------------

influencer_friends_followers <- tibble(
  date = character()
  , twitter_id = character()
  , follower_count = character()
  , friend_ids = character()
)
# Convert follower_count to double
# Convert friend_ids to JSON


# tweets ------------------------------------------------------------------

influencer_tweets <- tibble(
  twitter_id = character()
  , tweet_date = character()
  , tweet = character()
)
# Convert tweet_date to timestamp


