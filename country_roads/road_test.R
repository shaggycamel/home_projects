# biogeo.ucdavis.edu/data/diva/rds/GRC_rds.zip

setwd(dirname(rstudioapi::documentPath()))

library(tidyverse)
library(sf)
library(raster)
library(rgdal)

world <- spData::world
crs_data <- make_EPSG()

world %>% dplyr::select(name_long)

nzl_rds <- st_read(file.path(getwd(), "NZL_rds", "NZL_roads.shp"), quiet = TRUE) %>% 
  st_transform(crs = "+proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +ellps=intl +units=m +no_defs +type=crs")

nzl_cov <- st_read(file.path(getwd(), "NZL_adm", "NZL_adm0.shp"), quiet = TRUE) %>% 
  st_transform(crs = "+proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +ellps=intl +units=m +no_defs +type=crs")

chn_rds <- st_read(file.path(getwd(), "CHN_rds", "CHN_roads.shp"), quiet = TRUE)
chn_cov <- st_read(file.path(getwd(), "CHN_adm", "CHN_adm0.shp"), quiet = TRUE)
  
afg_rds <- st_read(file.path(getwd(), "AFG_rds", "AFG_roads.shp"), quiet = TRUE)
afg_cov <- st_read(file.path(getwd(), "AFG_adm", "AFG_adm0.shp"), quiet = TRUE)

aus_rds <- st_read(file.path(getwd(), "AUS_rds", "AUS_roads.shp"), quiet = TRUE)
aus_cov <- st_read(file.path(getwd(), "AUS_adm", "AUS_adm0.shp"), quiet = TRUE)

fra_rds <- st_read(file.path(getwd(), "FRA_rds", "FRA_roads.shp"), quiet = TRUE)
fra_cov <- st_read(file.path(getwd(), "FRA_adm", "FRA_adm0.shp"), quiet = TRUE)

idn_rds <- st_read(file.path(getwd(), "IDN_rds", "IDN_roads.shp"), quiet = TRUE)
idn_cov <- st_read(file.path(getwd(), "IDN_adm", "IDN_adm0.shp"), quiet = TRUE)

# plot(nzl_cov[0], main = "Roads: New Zealand")
# plot(nzl_rds[1], col = "black", main = "Roads: ?")

# plot(chn_cov[0], main = "Roads: China")
# plot(chn_rds[1], col = "black", main = "Roads: ?")

# plot(afg_cov[0], main = "Roads: Afganistan")
plot(afg_rds[1], col = "black", main = "Roads: ?")

# plot(aus_cov[0], main = "Roads: Australia")
# plot(aus_rds[1], col = "black", main = "Roads: ?")

# plot(fra_cov[0], main = "Roads: France")
plot(fra_rds[1], col = "black", main = "Roads: ?")

# plot(idn_cov[0], main = "Roads: Indonesia")
plot(idn_rds[1], col = "black", main = "Roads: ?")

st_centroid(idn_rds)
plot(st_centroid(st_union(idn_rds)),add = TRUE, col = "red", pch=19)

idn_box <- st_bbox(idn_rds)
x_min <- c(idn_box$xmin, 0) %>% st_point()
x_max <- c(idn_box$xmax, 0) %>% st_point()
y_min <- c(0, idn_box$ymin) %>% st_point()
y_max <- c(0, idn_box$ymax) %>% st_point()

x_dist <- (st_distance(x_min, x_max) * 100)[1] %>% round()
y_dist <- (st_distance(y_min, y_max) * 100)[1] %>% round()

x_line <- st_sfc(
  st_linestring(rbind(c(0, 0), c(idn_box$xmax, 0)))
  , crs = idn_rds %>% st_crs()
)

y_line <- st_sfc(
  st_linestring(rbind(c(0, 0), c(0, idn_box$ymax)))
  , crs = idn_rds %>% st_crs()
)

plot(idn_rds[1], col = "black", main = "Roads: ?")
plot(x_line, add=TRUE, col="red", pch=100)
plot(y_line, add=TRUE, col="red", pch=100)
     