library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)


ire_map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -8.240473, lat = 53.419751, zoom = 6)

ire_map