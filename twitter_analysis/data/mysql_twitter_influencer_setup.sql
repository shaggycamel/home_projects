CREATE TABLE IF NOT EXISTS influencer_id (
    user_id TEXT PRIMARY KEY
    , research_inclusion_date DATE
);


CREATE TABLE IF NOT EXISTS influencer_details (
    as_of_date DATE PRIMARY KEY
    , user_id TEXT PRIMARY KEY
    , account_created_at DATETIME
    , screen_name TEXT
    , name TEXT
    , location TEXT
    , description TEXT
    , latest_tweet_id TEXT
    , followers_count INT
    , friends_count INT
    , listed_count INT
    , statuses_count INT
    , favourites_count INT
);



CREATE TABLE IF NOT EXISTS influencer_tweets (
    twitter_id TEXT PRIMARY KEY
    , date_included TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

