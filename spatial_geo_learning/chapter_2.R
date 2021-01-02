library(tidyverse)
library(sf)
library(sp) # superceded by sf, but used in some examples
library(raster)
library(spData)
library(spDataLarge)
library(rgdal)

vignette("sf1")          # an introduction to the package

data(spData::world)
names(world)
plot(world)
summary(world)
world[1:2, 1:3]
world$geom

# Convert sf object to sp
world_sp = as_Spatial(world)

# Convert sp object to sf
st_as_sf(world_sp)

# Plot single variable in black
plot(world[,1], col="black")

# Subset to asian countires
world_asia <- world[world$continent=="Asia",]
# Combine asian boundries into one object
asia <- st_union(world_asia)

# Plot asia in red
plot(world["pop"], col="black", reset=FALSE)
plot(asia, add=TRUE, col="red")

# Plot the world by continent and city population
plot(world["continent"], reset=FALSE)
cex <- sqrt(world$pop) / 10000
world_cents <- st_centroid(world, of_largest_polygon = TRUE)
plot(st_geometry(world_cents), add=TRUE, cex=cex)

# Plot India with an emphasis on China (expandBB)
india <- world[world$name_long=="India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col="gray", lwd=3)
plot(world_asia[0], add=TRUE)

# Simple Feature Geometry Objects -----------------------------------------
# Point
plot(st_point(c(5, 2)))

# Multi-point
multipoint_matrix <- rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
plot(st_multipoint(multipoint_matrix))

# Linestring
linestring_matrix <- rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
plot(st_linestring(linestring_matrix))

# Polygon
polygon_list <- list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
plot(st_polygon(polygon_list))

# Polygon with a hole
polygon_border <- rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole <- rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
plot(st_polygon(polygon_with_hole_list))

# Multilinestring
multilinestring_list <- list(
  rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
  , rbind(c(1, 2), c(2, 4))
)
plot(st_multilinestring((multilinestring_list)))

# Multipolygon
multipolygon_list <- list(
  list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
  , list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
)
plot(st_multipolygon(multipolygon_list))

# Geometry collection
gemetrycollection_list <- list(
  st_multipoint(multipoint_matrix)
  , st_linestring(linestring_matrix)
)
plot(st_geometrycollection(gemetrycollection_list))


# Co-ordinate Reference System --------------------------------------------
sfc_crs_eg <- st_sfc(
  st_multilinestring(multilinestring_list)
  , st_multipolygon(multipolygon_list)
  , crs = 4326
)
st_geometry_type(sfc_crs_eg)
st_crs(sfc_crs_eg)
plot(sfc_crs_eg)


# The SF object -----------------------------------------------------------
lnd_point = st_point(c(0.1, 51.5))                 # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object
lnd_attrib = data.frame(                           # data.frame object
  name = "London"
  , temperature = 25
  , date = as.Date("2017-06-21")
)

lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
plot(world["pop"], col="black")
plot(lnd_sf$geometry, col="white", add=TRUE)

# Raster ------------------------------------------------------------------
raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
new_raster <- raster(raster_filepath)
plot(new_raster)

new_raster2 <- raster(
  nrows = 6
  , ncols = 6
  , res = 0.5
  , xmn = -1.5
  , xmx = 1.5
  , ymn = -1.5
  , ymx = 1.5
  , vals = 1:36
)
plot(new_raster2)


# Multifile Raster --------------------------------------------------------
# Brick
multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")
r_brick <- brick(multi_raster_file)
plot(r_brick)

# Stack
raster_on_disk <- raster(r_brick, layer = 1)
raster_in_memory <- raster(xmn = 301905, xmx = 335745,
                          ymn = 4111245, ymx = 4154085, 
                          res = 30)
values(raster_in_memory) <- sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) <- crs(raster_on_disk)

r_stack <- stack(raster_in_memory, raster_on_disk)
plot(r_stack)


# CRS in depth ------------------------------------------------------------
crs_data <- make_EPSG()
crs_data %>% filter(code==2104) %>% pull(prj4)

vector_filepath <- system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector <- st_read(vector_filepath)
st_crs(new_vector)

new_vector <- st_set_crs(new_vector, 4326)
plot(new_vector[0:1])


projection(new_raster)
projection(new_raster) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # set CRS
plot(new_raster)

