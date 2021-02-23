CREATE TABLE IF NOT EXISTS influencer_id (
    twitter_id TEXT PRIMARY KEY
    , date_included DATE
);


CREATE TABLE IF NOT EXISTS influencer_details (
    twitter_id TEXT PRIMARY KEY
    , screen_name TEXT
    , name TEXT
    , location TEXT
    , 
);



CREATE TABLE IF NOT EXISTS influencer_tweets (
    twitter_id TEXT PRIMARY KEY
    , date_included TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

