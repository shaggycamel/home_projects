library(tidyverse)
library(sf)
library(raster)
library(spData)
data(world)

# List methods avaiable for sf object
methods(class = "sf")

dim(world) # it is a 2 dimensional object, with rows and columns
nrow(world) # how many rows?
ncol(world) # how many columns?

# Remove geometry col from sf object
world_df <- st_drop_geometry(world)
class(world_df)

# dplyr functions must reference 'dplyr'
# in order to work
w2 <- world %>% dplyr::select(name_long, population = pop)
names(w2)


# Attribute Aggregation ---------------------------------------------------

world_agg1 <- world %>% 
  group_by(continent) %>% 
  summarise(pop = sum(pop, na.rm = TRUE))
# note how the geom col still exists

# In this example geom is dropped
world %>% 
  group_by(continent) %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>% 
  top_n(n = 3, wt = pop) %>%
  arrange(desc(pop)) %>%
  st_drop_geometry()


# Joins -------------------------------------------------------------------

world_coffee <- inner_join(world, coffee_data)
world_coffee %>% class()

plot(world_coffee["coffee_production_2017"])

# Fix name of "Congo"
coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] <- world$name_long %>% str_subset("Dem*.+Congo")
world_coffee <- inner_join(world, coffee_data)

# Combine col info with unite()
world_coffee <- world_coffee %>% 
  unite("con_reg", continent:region_un, sep = ":", remove=TRUE)

# Undo unite with separate()
world_coffee = world_coffee %>% 
  separate(con_reg, c("continent", "region_un"), sep = ":")


# Manipulating Raster Objects ---------------------------------------------

# Raster object containing elevation
elev = raster(
  nrows = 6, ncols = 6, res = 0.5
  , xmn = -1.5, xmx = 1.5
  , ymn = -1.5, ymx = 1.5
  , vals = 1:36
)

# Raster object of grain type
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(
  nrows = 6, ncols = 6, res = 0.5
  , xmn = -1.5, xmx = 1.5
  , ymn = -1.5, ymx = 1.5
  , vals = grain_fact
)

# raster objects can contain values of class numeric, integer, 
# logical or factor, but not character. 
# To use character values, they must first be converted into an 
# appropriate class, for example using the function factor()

# Assign extra attribute to levels
levels(grain)[[1]] <- cbind(
  levels(grain)[[1]]
  , wetness = c("wet", "moist", "dry")
)
levels(grain)

plot(elev)
plot(grain)


# Raster Subsetting -------------------------------------------------------

# Subsetting raster object
elev[1,2]

# # Three different ways to subset stacked raster
r_stack <- stack(elev, grain)
names(r_stack) <- c("elev", "grain")

subset(r_stack, "elev")
r_stack[["elev"]]
r_stack$elev

# Get values of raster object
values(elev)


# Summarising Raster Objects ----------------------------------------------

summary(elev)
cellStats(elev, sd)

summary(brick(elev, grain))
cellStats(brick(elev, grain), sd)

# boxplot, density, hist, pairs
boxplot(elev)


# Exercises ---------------------------------------------------------------

data("us_states")
data("us_states_df")

us_states_name <- us_states %>% 
  dplyr::select(NAME)

us_states %>% 
  dplyr::select(contains("pop"))

# Find states that belong to mid-west region
us_states %>% 
  dplyr::filter(REGION=="Midwest") %>% 
  plot()

# What is the most common grain class
factorValues(grain, modal(values(grain)))
