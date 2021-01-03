library(tidyverse)
library(sf)
library(raster)
library(spData)
data("nz")
data("nz_height")

canterbury <- nz %>% filter(Name == "Canterbury")
canterbury_height <- nz_height[canterbury, ]

plot(canterbury[1])
plot(canterbury_height[0], add=TRUE)
st_crs(canterbury)[1]

# Supplement the op argument with the desired operation
# st_intersects, st_disjoint, st_contains, st_covers, st_crosses, st_equals
# st_is_within_distance, st_overlaps, st_touches, st_within
nz_height[canterbury, op=st_disjoint][0] %>% 
  plot()

# another way to filter
canterbury_height2 = nz_height %>%
  filter(st_intersects(x = ., y = canterbury, sparse = FALSE))
plot(canterbury_height2)


# Topological Relations ---------------------------------------------------

a_poly <- rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1)) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc()

l_line <- matrix(c(-1, -1, -0.5, 1), ncol = 2) %>% 
  st_linestring() %>% 
  st_sfc()

p_multi <- matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2) %>% 
  st_multipoint() %>% 
  st_sfc() %>% 
  st_cast("POINT")

plot(a_poly)
plot(l_line, add=TRUE)
plot(p_multi, add=TRUE)

st_intersects(p_multi, a_poly, sparse = FALSE)
st_disjoint(p_multi, a_poly, sparse = FALSE)
st_within(p_multi, a_poly, sparse = FALSE)
st_touches(p_multi, a_poly, sparse = FALSE)
st_is_within_distance(p_multi, a_poly, sparse = FALSE, dist = 0.9)

# Spatial Joining ---------------------------------------------------------
# Ten random points spread across earth. Which points are on land, and
# which countries are they in?
set.seed(2018)

(bb_world <- st_bbox(world))

random_points <- tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4])
) %>% 
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS 

plot(world[1], col="white", main=NULL)
plot(random_points, add=TRUE, col="red", pch=19)

plot(world[1], col="white", main=NULL)
plot(world[random_points, ][2], add=TRUE)

random_joined = st_join(random_points, world["name_long"])
plot(world[1], col="white", main=NULL)
plot(random_joined, add=TRUE, pch=19)


# Non-overlapping joins ---------------------------------------------------

plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE) %>% 
  any()

cycle_hire_P <- st_transform(cycle_hire, 27700)
cycle_hire_osm_P <- st_transform(cycle_hire_osm, 27700)
sel <- st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)

z <- st_join(cycle_hire_P, cycle_hire_osm_P, join = st_is_within_distance, dist = 20) %>% 
  # We need to group by and average becuase sometimes
  # there will be more than one match; in which case more
  # than one record is returned
  group_by(id) %>% 
  summarise(capacity = mean(capacity))

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])


# Spatial Data Aggregation ------------------------------------------------

nz_avgheight <- nz %>% 
  st_join(nz_height) %>% 
  group_by(Name) %>% 
  summarise(elevation = mean(elevation))

plot(nz_avgheight["elevation"])


agg_aw <- st_interpolate_aw(
  incongruent["value"]
  , aggregating_zones
  , extensive = TRUE
)
plot(incongruent)
plot(agg_aw)

# Distance Relations ------------------------------------------------------

nz_highest <- nz_height %>% 
  top_n(1, wt=elevation)

canterbury_centroid <- st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

co <- filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

plot(st_geometry(co))
plot(st_geometry(nz_height)[2:3], add = TRUE)


# Spatial Operations on Raster Data ---------------------------------------


