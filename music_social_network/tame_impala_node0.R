library(robotstxt)
library(rvest)
# library(selectr)
# library(xml2)
# library(dplyr)
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

inital_similar_to <- inital_tame_imapla %>% html_node("section.related")
inital_similar_atrist_url <- similar_to %>% html_nodes("a") %>% html_attr("href")
inital_similar_atrists_name <- similar_to %>% html_nodes("a") %>% html_text()

artists <- data.frame()
similarity <- data.frame()



as.data.frame(rbind(c("Tame Impala", initial_url), cbind(similar_atrists_name, similar_atrist_url)) %>% 
              colnames = names("artist", "artist_url"))


