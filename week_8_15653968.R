library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)


########################### Task One ######################
#making the NZ map
nz_map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 174.7762, lat = -41.2865, zoom = 5)

nz_map

########################### Task Two & Three ##########################
#Adding markers for citys in NZ, no dunners because they aren't special enough (I was only doing three like the example)
nz_pop <- data.frame(
  city = c("Auckland", "Wellington", "Christchurch"),
  lat  = c(-36.8485, -41.2865, -43.5321),
  lng  = c(174.7633, 174.7762, 172.6362),
  population = c(1711130, 427516, 410423) 
  #got the populations from https://worldpopulationreview.com/cities/new-zealand
)

#Adding the markers to the map
nz_map_pop <- leaflet(nz_pop) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = 174.77557, lat = -41.28664, zoom = 5) %>%
  addMarkers(~lng, ~lat, popup = ~paste(city, "<br>Population", population)) %>%
  addHeatmap(lng = ~lng,
             lat = ~lat,
             intensity = ~population/500)

nz_map_pop


######################### Task Four ########################

