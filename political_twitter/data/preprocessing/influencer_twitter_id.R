
# Oliver Eaton
# Begun: 2021-05-22

# File to read in twitter ids of political leaders.
# ID's were handpicked and originate from ./data/config/influencer_config.R

# Get Twitter id ----------------------------------------------------------

influencer_id <- dbReadTable(sql_con, "influencer_id") %>% 
  arrange(user_id) %>% 
  mutate(across(ends_with("date"), as.Date))

# Figure out how to handle influencers that have been
# removed from the study
