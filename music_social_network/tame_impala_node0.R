library(robotstxt)
library(rvest)
# library(selectr)
# library(xml2)
library(dplyr)
# library(stringr)
# library(forcats)
# library(magrittr)
# library(tidyr)
# library(ggplot2)
# library(lubridate)
# library(tibble)
# library(purrr)
# library(RMySQL)
# library(data.table)


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check if bots are allowed on website. If TRUE then okay to proceed
paths_allowed("https://www.allmusic.com/")

# Get html of inital Arist Tame Impala
initial_url <- "https://www.allmusic.com/artist/tame-impala-mn0002014409"
# inital_tame_imapla <- read_html(paste0(initial_url,"/related"))

initial_similar_to <- inital_tame_imapla %>% html_node("section.related")
initial_similar_atrists_url <- initial_similar_to %>% html_nodes("a") %>% html_attr("href")
initial_similar_atrists_name <- initial_similar_to %>% html_nodes("a") %>% html_text()

# artists <- as.data.frame(rbind(c("Tame Impala", initial_url), cbind(initial_similar_atrists_name, initial_similar_atrists_url)))
artists <- as.data.frame(cbind(initial_similar_atrists_name, initial_similar_atrists_url))
colnames(artists) <- c("artist", "artist_url")

similarity <- data.frame(cbind(list("Tame Impala"), initial_similar_atrists_name)) %>% ungroup()
colnames(similarity) <- c("from", "to")


MAX_DEPTH <- 3
depth <- 0

collect_artists <- function(band_list, depth){
  
  depth <- depth + 1
  message(depth)
  
  while(depth <= MAX_DEPTH){
    
    for(row in 1:nrow(band_list)){
      
      band_name <- band_list[row, 1]
      band_url <- band_list[row, 2]
      
      message(band_name)

      tmp_artist <- read_html(paste0(band_url,"/related"))
      tmp_similar_to <- tmp_artist %>% html_node("section.related")

      tmp_similar_atrists_url <- tmp_similar_to %>% html_nodes("a") %>% html_attr("href")
      tmp_similar_atrists_name <- tmp_similar_to %>% html_nodes("a") %>% html_text()
      tmp <- cbind(tmp_similar_atrists_name, tmp_similar_atrists_url)

      # artists <- rbind(artists, tmp)
      # similarity <- rbind(similarity, cbind(list(band_name), tmp_similar_atrists_name)) %>% ungroup()

      slp <- rnorm(1, mean=35, sd=20) %>% round() %>% abs()
      message(slp)
      Sys.sleep(2)
      collect_artists(tmp, depth)
      
    }
  }
}


collect_artists(artists, depth)



