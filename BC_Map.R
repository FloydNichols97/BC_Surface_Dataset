# Clear Global Environment 
rm(list=ls())

# Load Packages 
library(tidyverse)
library(ggmap)
library(readr)
library(ggrepel)
library(ggsn)


# Load Spatial Data Frame 
BC_df <- Spatial_df <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Mia T. Senior Research/Spreadsheets/Spatial_df.csv")

## British Columbia Province Scale ##
# define box #
BC_Box <- make_bbox(lon = c(-114.03, -139.06), lat = c(48.30, 60.00), f = .05) # input latitude and longitude maximums and minimums to create a bounding box. f adds extra degrees to your bounding box.

# get map #
BC = get_map(location=BC_Box, zoom = 6, source = "stamen", maptype = "terrain") # this package allows for Stamen maps or google maps. Change source to google if you want to use google maps or satellite images

# create map #
BC_map = ggmap(BC, extent = "device")

# Manual Colors #
Colors <- c("Magnesium Sulfate" = "blue", "Sodium Carbonate" = "darkred")

# display map #
BC_map + geom_point(data = BC_df, mapping = aes(x = Longitude, y = Latitude, fill = `Chemical Composition`), size = 0.5, pch = 24) + 
  scale_fill_manual(values = Colors) + 
  labs(x = "Longitude", y = "Latitude") + 
  geom_rect(aes(xmin=-122, xmax=-120.5, ymin=50.5, ymax=51.4), alpha = 0, fill=alpha("grey",0), color = "black", lty = "dashed", size = 0.75) +
  theme(legend.position = "none")


## Cariboo Plateau Regional Scale ##
# define box
BC_Box <- make_bbox(lon = c(-120.5, -122), lat = c(50.5, 51.4), f = .15)

# get map #
BC = get_map(location=BC_Box, zoom = 10, source = "stamen", maptype = "terrain")

# create map
BC_map = ggmap(BC, extent = "panel") 

# display map #
BC_map + 
  geom_point(data = BC_df, mapping = aes(x = Longitude, y = Latitude, fill = `Chemical Composition`), size = 3, pch = 24) + 
  scalebar(x.min = -122, x.max = -120.5, y.min = 50.42, y.max = 51.4, dist = 20, dist_unit = "km", location = "bottomright", transform = TRUE, model = "WGS84") + 
  scale_fill_manual(values = Colors) + 
  labs(x = "Longitude", y = "Latitude") + 
  geom_text(data = BC_df, aes(x = Longitude, y = Latitude, label = Lake), 
            size = 4, vjust = 0, hjust = -0.25, check_overlap = TRUE) + 
  theme(legend.position = c(0.85,0.9), 
        axis.text = (element_text(size = 16)),
        axis.title = (element_text(size = 16))) 

## Currently this code is not functional with adding to ggplot ##
north2(BC_map, .8, .9, symbol = 3, scale = 0.075) # Create north symbol 




