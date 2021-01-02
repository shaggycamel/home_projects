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
