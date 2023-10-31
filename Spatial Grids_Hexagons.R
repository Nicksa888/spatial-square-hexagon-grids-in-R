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

map_fishnet # See Kowloon_Fishnet_Grids png

############################
# Hexagon grid (honeycomb) #
############################

# Hexagon (honeycomb) grids is another popular choice of grids. I find hexagonal girds usually more visually appealing and use them in most of my mapping projects.

# The steps of making a hexagonal grid and summarise the points within each grid is 99% the same from the square grid one shown above. The only difference is changing the argument from square = TRUE to square = FALSE in st_make_grid.

area_honeycomb_grid <- st_make_grid(test_points, 
                                    c(150, 150), 
                                    what = "polygons", 
                                    square = FALSE)
area_honeycomb_grid 

# To sf and add grid ID
honeycomb_grid_sf <- st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id <- 1:length(lengths(area_honeycomb_grid)))


# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
honeycomb_grid_sf$n_colli <- lengths(st_intersects(honeycomb_grid_sf, 
                                                   test_points))
honeycomb_grid_sf$n_colli

# remove grid without value of 0 (i.e. no points in side that grid)
honeycomb_count <- filter(honeycomb_grid_sf, 
                          n_colli > 0)

honeycomb_count

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

map_honeycomb # See Kowloon_Honeycomb_Grid

#######################
# Create tessellation #
#######################

# The tessellation is created using the st_make_grid function.

# test_points is the input spatial data. The function will define the bounding box (processing extent in ArcGIS sense) of the tessellation.
# c(150, 150) is the cell size, in the unit the crs of the spatial data is using. The default option splits the bounding box into 100 grids, 10 rows x 10 columns layout. I prefer stating the cell size, since the sense of distance could differs according to the spatial variable you are analysing.
# square argument indicates whether you are a square grid (TRUE) or hexagon grid (FALSE)
# Caveat: The cell size is in the same unit to the projected coordinate system (pcs) of the spatial data (in this case, meters). If the values are too small, R will produce a disastrous amount of grids that could make your laptop crash.

area_honeycomb_grid <- st_make_grid(test_points, 
                                    c(200, 200), 
                                    what = "polygons", 
                                    square = FALSE)

mapview(fishnet_grid_sf) # See Basic_Tesselation png

# Adding Grid ID
# st_make_grid returns a sfc_POLYGON object. st_sf is used to convert to sf object for easier handling.

# Adding an unique ID to each cell could help identify the cells when further analysis are needed. length(lengths(area_honeycomb_grid)) returns the total number of cells created. Then we could create a columns of grid_id labelling each cell, from 1 to largest.


# To sf and add grid ID
fishnet_grid_sf <- st_sf(area_fishnet_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_fishnet_grid)))
fishnet_grid_sf

#######################################
# Count number of points in each grid #
#######################################

# The tools here is st_intersects, when using polygon (here, grid) as the first input and point (here, traffic collision location) as the second input, st_intersects will return a list of which points are lying inside each grid.

st_intersects(fishnet_grid_sf, test_points)

# But we are not much interested in which point lies in which grid. The things need to know is how many points are within each grid. lengths will return the number of each element of the list.

lengths(st_intersects(fishnet_grid_sf, test_points))

# With this list of “count of points”, we could attach it back to the grid sf object. The dataframe will have a new column named n_colli, storing the number of points (collisions) in each cell.

fishnet_grid_sf$n_colli <- lengths(st_intersects(fishnet_grid_sf, test_points))
fishnet_grid_sf$n_colli

# We would easily check the results by symbolising each grid according to the number of collisions
mapview(fishnet_grid_sf, zcol = "n_colli") # See Kowloon_Collisions_Grid png

##############################
# Remove grids with 0 values #
##############################

# Yet, there are some cells without any collision points inside. Showing them will be redundant and churns out the computer space, especially when the points tends to be clustered within some major areas. I tend to remove those cells with count of points equals to 0. That reduces the amount of information the reader have to understand.

fishnet_count <- filter(fishnet_grid_sf, n_colli > 0)
mapview(fishnet_count, zcol = "n_colli") # See Kowloon_Collision_No_Zeros png
