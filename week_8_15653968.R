library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(stringr)
library(scales)


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
    population = parse_number(as.character(population))
  ) %>%
  filter(!is.na(territory), !is.na(population)) %>%
  filter(!str_detect(str_to_lower(territory), "total|all|unspecified")) %>%
  arrange(desc(population))

#Quick check be cause im stressed
summary(pop$population)
hist(pop$population, breaks = 20, main = "Population distribution", xlab = "Population")

pop <- pop %>%
  filter(
    !str_detect(
      str_to_lower(territory),
      "^new zealand$|^north island$|^south island$|total|overall|combined|unspecified"
    )
  )

#checking again
pop %>% arrange(desc(population)) %>% slice_head(n = 5)

#histogram for the territories only
ggplot(pop, aes(population)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Population distribution (territories only)",
       x = "Population", y = "Count") +
  theme_minimal()

#log view
ggplot(pop, aes(population)) +
  geom_histogram(bins = 20) +
  scale_x_log10(labels = label_comma()) +
  labs(title = "Population distribution (log10)",
       x = "Population (log10)", y = "Count") +
  theme_minimal()

######################### Task Seven ########################
# All the joining stuff for task seven from task six
pop_clean <- pop %>%
  mutate(name_clean = territory %>%
           str_to_lower() %>% 
           str_squish() %>%
           str_replace_all(regex(" district| city| region| territorial authority| council",
                                 ignore_case = TRUE), ""))

name_col <- names(nz_sf)[stringr::str_detect(names(nz_sf), 
                                             "(NAME|Name|TA|REG)")] %>% 
  purrr::pluck(1)

nz_sf_clean <- nz_sf %>%
  mutate(name_clean = get(name_col) %>%
           str_to_lower() %>% 
           str_squish() %>%
           str_replace_all(regex(" district| city| region| territorial authority| council",
                                 ignore_case = TRUE), ""))

nz_joined <- dplyr::left_join(nz_sf_clean, pop_clean, by = "name_clean")

name_col <- if ("TA2016_NAM" %in% names(nz_sf)) "TA2016_NAM" else {
  names(nz_sf)[stringr::str_detect(names(nz_sf), "(?i)name|ta.*name|territ|region")][1]
}

norm_name <- function(x) {
  x |>
    str_to_lower() |>
    str_squish() |>
    str_replace_all(
      regex(" territorial authority| district council| city council| regional council| district| city| region| council",
            ignore_case = TRUE),
      ""
    ) |>
    str_replace_all("[^a-z ]", "")
}

pop_keyed <- pop %>%
  transmute(name_clean = norm_name(territory),
            population  = population)

nz_joined <- nz_sf %>%
  mutate(name_clean = norm_name(.data[[name_col]])) %>%
  left_join(pop_keyed, by = "name_clean")

#Im stressed again and checking
missing <- nz_joined %>% filter(is.na(population)) %>% distinct(.data[[name_col]]) %>% pull()
if (length(missing)) message("Unmatched in geojson: ", paste(missing, collapse = ", "))


######################### Task Eight ########################
stopifnot("population" %in% names(nz_joined))

p_basic <- ggplot(nz_joined) +
  geom_sf(aes(fill = population), color = "white", linewidth = 0.2) +
  theme_void() +
  labs(
    title = "NZ Territories — Population (raw)",
    fill  = "Population"
  )

p_basic


