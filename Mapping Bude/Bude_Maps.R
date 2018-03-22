# Author: Michael Rojas
# MA415/615
# 3/21/18

# Western England Maps (Road, WaterColor) of Bude
# Including Bude Itself, cricket Grounds, Local Beaches, Tidal Swimming Pool, and Local Pub

library(ggmap)
library(ggplot2)
library(tidyverse)
library(maps)

# Long and Lat Data
myDF <- data.frame(
  Points_Of_Interest = c(
    "Bude", "Summerleaze Beach", "Bude Tidal Swimming Pool",
    "Crooklets Beach", "The Barrel at Bude", "Bude North Cornwall Cricket Club"
  ),

  longitude = c(-4.543678, -4.5513, -4.5540, -4.553762, -4.543023, -4.552814),
  latitude = c(50.8261, 50.8305, 50.8326, 50.83587, 50.83007, 50.83347),

  Type = c("Bude", "Beach", "Tidal Swimming Pool", "Beach", "Pub", "Cricket Grounds"),
  stringsAsFactors = FALSE
)

# Pub Route
from <- "Bude North Cornwall Cricket Club"
to <- "The Barrel at Bude"
route_df <- route(from, to, structure = "route")

# Road Map
bude_map <- get_map("Summerleaze Beach", zoom = 15)
ggmap(bude_map) +

  # Title and Axis Labeling
  print(ggtitle("Bude, Western England \n Road Map")) +
  print(labs(y = "Latitude", x = "Longitude")) +

  # Pretty Printing (Title,Axis,Legend Modifications)
  theme(
    plot.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold", size = (15), hjust = 0.5),
    legend.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold.italic"),
    legend.text = element_text(family = "Helvetica", colour = "steelblue4", face = "bold.italic"),
    axis.title = element_text(family = "Helvetica", face = "bold.italic", size = (12), colour = "steelblue4")
  ) +

  # Literal Plotted Points of Interest & Pub Route
  geom_point(data = myDF, aes(x = longitude, y = latitude, color = Type), size = 3) +
  geom_path(aes(x = lon, y = lat), colour = "red", size = 1.5, data = route_df, lineend = "round") +
  scale_colour_manual(values = c("blue", "orange", "limegreen", "purple", "turquoise1"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Water Color Map
bude_Watermap <- get_map("Summerleaze Beach", source = "stamen", maptype = "watercolor", zoom = 15)
ggmap(bude_Watermap) +

  # Title and Axis Labeling
  print(ggtitle("Bude, Western England \nWaterColor Map")) +
  print(labs(y = "Latitude", x = "Longitude")) +

  # Pretty Printing (Title,Axis,Legend Modifications)
  theme(
    plot.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold", size = (15), hjust = 0.5),
    legend.title = element_text(family = "Helvetica", colour = "steelblue4", face = "bold.italic"),
    legend.text = element_text(family = "Helvetica", face = "bold.italic", colour = "steelblue4"),
    axis.title = element_text(family = "Helvetica", face = "bold.italic", size = (12), colour = "steelblue4")
  ) +

  # Literal Plotted Points of Interest & Pub Route
  geom_point(data = myDF, aes(x = longitude, y = latitude, color = Type), size = 3) +
  geom_path(aes(x = lon, y = lat), colour = "red", size = 1.5, data = route_df, lineend = "round") +
  scale_colour_manual(values = c("blue", "orange", "limegreen", "purple", "turquoise1"))
