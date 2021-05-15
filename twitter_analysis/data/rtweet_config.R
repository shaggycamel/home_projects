
library(rtweet)

access_token <- "1158336653443776512-K5cOdyxK3puSZCu8SrHMr5jFnI5x4k"
access_secret <- "1yGAKsyozNz97wsuDZoetR1xFCdCapwpMLplFWD8qaien"
api_key <- "VOa7h2E3KgDXZJ7SIOyJMtkUu"
api_secret <- "6HKcxiJb9FkK85GLYRcdhiFOsEdIh4xBHQuKguQ8Rit9iPoQuz"
appname <- "mao_zhao"

twitter_token <- create_token(
  app = appname
  , consumer_key = api_key
  , consumer_secret = api_secret
  , access_token = access_token
  , access_secret = access_secret
)


