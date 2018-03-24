# Author: Michael Rojas
# MA415/615
# 3/21/18


# Install this update in the terminal before running if you havent done so already
# devtools::install_github("dkahle/ggmap") 

# Google Api Key used to get rid of Query Limit Error.

# googleKey <- "AIzaSyANzSVR1fVBwRLFj8fInL3-U33kqHeeCbg"
# register_google(key = googleKey)
# GoogleKey wont work unless you get your own key.

# Western England Maps (Road, WaterColor) of Bude
# Including Bude Itself, cricket Grounds, Local Beaches, Tidal Swimming Pool, and Local Pub

library(ggmap)
library(ggplot2)
library(tidyverse)
library(maps)

# Longitude and Latitude Data for Points of Interest
myDF <- data.frame(
  Points_Of_Interest = c(
    "Bude", "Summerleaze Beach", "Bude Tidal Swimming Pool",
    "Crooklets Beach", "The Barrel at Bude", "Bude North Cornwall Cricket Club"
  ),

  longitude = c(-4.543678, -4.5513, -4.5540, -4.553762, -4.543023, -4.552814),
  latitude = c(50.8261, 50.8305, 50.8326, 50.83587, 50.83007, 50.83347),

  Type = c("Bude", "Summerleaze Beach", "Tidal Swimming Pool", "Crooklets Beach", "Pub: The Barrel at Bude", "Cricket Grounds"),
  stringsAsFactors = FALSE
)


# Possible Error that might come up for the Pub Route:
# "Error in rep(LETTERS[k], stepsPerRoute[k]) : invalid 'times' argument"
# Dont know why but this error comes up sometimes, Just reRun code

# Pub Route
from <- "Bude North Cornwall Cricket Club"
to <- "The Barrel at Bude"
route_df <- route(from, to, structure = "route")


# Possible Error that might come up for no reason for get_map and DataFrame:
# "Error in data.frame(ll.lat = ll[1], ll.lon = ll[2], ur.lat = ur[1], ur.lon = ur[2]) : arguments imply differing number of rows: 0, 1"
# "Over Query Limit Error"
# Dont know why but these errors come up sometimes, Just reRun code  

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
  geom_path(data = route_df, aes(x = lon, y = lat), colour = "red", size = 1.5, lineend = "round") +
  scale_colour_manual(values = c("sienna2", "limegreen","steelblue4", "red4","steelblue3", "turquoise1"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Possible Error that might come up for no reason for get_map and DataFrame:
# "Error in data.frame(ll.lat = ll[1], ll.lon = ll[2], ur.lat = ur[1], ur.lon = ur[2]) : arguments imply differing number of rows: 0, 1"
# "Over Query Limit Error"
# Dont know why but these errors come up sometimes, Just reRun code  

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
  geom_path(data = route_df, aes(x = lon, y = lat), colour = "red", size = 1.5, lineend = "round") +
  scale_colour_manual(values = c("sienna2", "limegreen","steelblue4", "red4","steelblue3", "turquoise1"))
