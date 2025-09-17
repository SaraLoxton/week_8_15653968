library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(stringr)


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
# Read the supplied NZ territories geoJSON into an sf object
geojson_path <- "nz_ta.geojson"
stopifnot(file.exists(geojson_path))
nz_sf <- st_read(geojson_path, quiet = TRUE)
nz_sf <- st_make_valid(nz_sf)

######################### Task Five ########################
ggplot(nz_sf) + geom_sf(color = "grey40", fill = "white", linewidth = 0.2) + theme_void()

######################### Task Six ########################
pop_csv_path <- "nz_territory_2016_population.csv"
stopifnot(file.exists(pop_csv_path))
pop_raw <- read_csv(pop_csv_path, show_col_types = FALSE, na = c("", "NA", "N/A"))

pop <- pop_raw %>%
  rename(
    territory  = nz_territory,
    population = `2016_population`
  ) %>%
  mutate(
    territory  = str_squish(territory),
    population = readr::parse_number(as.character(population))
  ) %>%
  filter(!is.na(territory), !is.na(population)) %>%
  filter(!str_detect(str_to_lower(territory), "total|all|unspecified")) %>%
  arrange(desc(population))

#Quick check be cause im stressed
summary(pop$population)
hist(pop$population, breaks = 20, main = "Population distribution", xlab = "Population")