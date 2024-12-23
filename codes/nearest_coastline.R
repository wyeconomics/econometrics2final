# Install and load the necessary packages
#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Define the coordinates of the city (example: New York City)
city_coords <- st_point(c(-74.0060, 40.7128)) %>% st_sfc(crs = 4326)

# Get global coastline data
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# Transform the coastline data to the same CRS as the city coordinates
coastline <- st_transform(coastline, crs = st_crs(city_coords))

# Extract coordinates of coastal points
coastline_points <- st_cast(coastline, "POINT")

# Calculate distances from the city to each coastal point
distances <- st_distance(city_coords, coastline_points)

# Find the index of the nearest coastal point
nearest_index <- which.min(distances)

# Get the coordinates of the nearest coastal point
nearest_coastal_point <- coastline_points[nearest_index, ]

# Print the coordinates of the nearest coastal point
print(st_coordinates(nearest_coastal_point))
