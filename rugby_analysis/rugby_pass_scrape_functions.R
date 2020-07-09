player_name <- function(URL) {
  teamPass <- read_html(URL)
  player_name <- teamPass %>% html_nodes("#players-graph") %>% html_nodes("span.name") %>% html_text()
  player_id <- teamPass %>% html_nodes("#players-graph") %>% html_nodes("span.btn-compare") %>% html_attr("data-id")
  players <- as.data.frame(cbind(player_id, player_name))
}

player_bio <- function(URL){
  playerPass <- read_html(URL)
  player_position <- playerPass %>% html_nodes("div.show-990") %>% html_nodes("div.positions") %>% html_text() %>% str_trim()
  player_bio <- playerPass %>% html_nodes("div.show-990") %>% html_nodes("div.measurement") %>% html_text() %>% str_replace(c("Height","Weight","DOB"), "") %>% str_trim()
  # This should really be gathered from historical records present in db.
  # player_exp <- playerPass %>% html_nodes("div.show-990") %>% html_nodes("div.player-teams") %>% html_nodes("span.title") %>% html_text() %>% str_trim()
  player <- append(player_bio, player_position, 0)
}

