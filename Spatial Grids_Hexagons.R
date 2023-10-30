# Libraries #

library(hkdatasets)
library(sf)
library(dplyr)
library(mapview)
library(tmap)

# setwd #

setwd("C:/R Portfolio")

# Load Data #

hk_accidents <- read.csv("hk_accidents.csv")

# collisions in Kowloon City District, 2017
test_data = subset(hk_accidents, District_Council_District == "KC" & Year == 2017)

# turn it to sf object
test_points = test_data %>%
  # lng/lat value are missing in some records
  filter(!is.na(Grid_E) & !is.na(Grid_N)) %>%
  st_as_sf(coords = c("Grid_E", "Grid_N"), crs = 2326, remove = FALSE)

# And these are the traffic collisions in the Kowloon City District of Hong Kong in 2017.
 
mapview_test_points <- mapview(test_points, cex = 3, alpha = .5, popup = NULL)
mapview_test_points

# Square grid (fishnet)

# Square grids, sometimes called fishnet, are more common method of tessellation because of its resemblances to orthodox square raster grids in GIS.

# We will first create a grid which the extent equals to the bounding box of the selected points.

area_fishnet_grid <- st_make_grid(test_points, c(200, 200), what = "polygons", square = TRUE)

# To sf and add grid ID
fishnet_grid_sf <- st_sf(area_fishnet_grid) %>%
  # add grid ID
  mutate(grid_id <- 1:length(lengths(area_fishnet_grid)))

# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
fishnet_grid_sf$n_colli = lengths(st_intersects(fishnet_grid_sf, test_points))

# remove grid without value of 0 (i.e. no points in side that grid)
fishnet_count <- filter(fishnet_grid_sf, n_colli > 0)

# And then we have it! To check the result, plot the grid into a interactive thematic map with tmap.

tmap_mode("view")

map_fishnet <- tm_shape(fishnet_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.5,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_fishnet


# Honey comb

area_honeycomb_grid <- st_make_grid(test_points, c(150, 150), what = "polygons", square = FALSE)

# To sf and add grid ID
honeycomb_grid_sf <- st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id <- 1:length(lengths(area_honeycomb_grid)))

# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
honeycomb_grid_sf$n_colli <- lengths(st_intersects(honeycomb_grid_sf, test_points))

# remove grid without value of 0 (i.e. no points in side that grid)
honeycomb_count <- filter(honeycomb_grid_sf, n_colli > 0)

tmap_mode("view")

map_honeycomb = tm_shape(honeycomb_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_honeycomb

area_honeycomb_grid <- st_make_grid(test_points, c(200, 200), what = "polygons", square = FALSE)
mapview(fishnet_grid_sf)

# To sf and add grid ID
fishnet_grid_sf <- st_sf(area_fishnet_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_fishnet_grid)))
fishnet_grid_sf
st_intersects(fishnet_grid_sf, test_points)
lengths(st_intersects(fishnet_grid_sf, test_points))

fishnet_grid_sf$n_colli <- lengths(st_intersects(fishnet_grid_sf, test_points))
