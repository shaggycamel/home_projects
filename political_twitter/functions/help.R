
# Oliver Eaton
# Begun: 2021-09-11

# Helper functions for the Twitter analysis

# Hash, @, link ratio -----------------------------------------------------

hash_at_link_ratio <-  function(df){
  
  df |> 
    mutate(
      token_count = str_count(str_squish(text), " ")+1
      , hash_at_link_count = str_count(text, "#|@|http")
      , hash_at_link_ratio = hash_at_link_count / token_count
    ) |> 
    filter(token_count >= 10, hash_at_link_ratio <= 0.60)
  
}

