Alternative Fuel Stations
================
Nick Cruickshank
3/6/2022

![](https://www.fairfaxcounty.gov/landdevelopment/sites/landdevelopment/files/Assets/images/electric-vehicle-charging-stations.jpg)

# Introduction

This weeks [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-03-01)
features data on **Alternative Fueling Stations** across the US. Data
was sourced from the [US
DOT](https://data-usdot.opendata.arcgis.com/datasets/usdot::alternative-fueling-stations/about).

Data dictionary available
[here](https://afdc.energy.gov/data_download/alt_fuel_stations_format).

# Analysis

## Import Libraries and Data

``` r
# libraries
library(cartogram)
library(ggtext)
library(geosphere) # for calculating distance between points
library(readr)
library(rlist) # for extracting list items
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v dplyr   1.0.7
    ## v tibble  3.1.6     v stringr 1.4.0
    ## v tidyr   1.1.4     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(glue)
library(here)
```

    ## here() starts at C:/Users/nccru/OneDrive/Documents/GitHub/nc_r_tidytuesday

``` r
library(jsonlite)
```

    ## 
    ## Attaching package: 'jsonlite'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

``` r
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1; sf_use_s2() is TRUE

``` r
library(showtext)
```

    ## Loading required package: sysfonts

    ## Loading required package: showtextdb

``` r
library(sysfonts)
```

``` r
# data
here <- here()
cities <- fromJSON(glue("{here}/2022/2022-03-01 - Alternative Charging Stations/cities.json"))
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') %>%
  janitor::clean_names()
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 59927 Columns: 70

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (43): FUEL_TYPE_CODE, STATION_NAME, STREET_ADDRESS, INTERSECTION_DIRECTI...
    ## dbl (15): X, Y, OBJECTID, EV_LEVEL1_EVSE_NUM, EV_LEVEL2_EVSE_NUM, EV_DC_FAST...
    ## lgl (12): PLUS4, EV_OTHER_INFO, HYDROGEN_STATUS_LINK, LPG_PRIMARY, E85_BLEND...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Prepare the Data

### Preliminary Tidying

``` r
# trim down to columns of interest
estations <- stations %>%
  # remove irrelevant rows
  filter(
    fuel_type_code == "ELEC",
    str_detect(groups_with_access_code, "Public")
  ) %>%
  # select relevant columns
  select(
    objectid, station_name, city, state, zip, status_code, expected_date, 
    ev_level1_evse_num, ev_level2_evse_num, ev_dc_fast_count,
    ev_network, longitude, latitude, facility_type, ev_pricing
  )
```

### Load biggest city by state

City longitude, latitude, and population sourced from
[Miserlou](https://gist.github.com/Miserlou/c5cd8364bf9b2420bb29).

``` r
# tidy cities
state_mapping1 <- tibble(
  state = append(state.name, "District of Columbia"),
  abb = append(state.abb, "DC")
)

# biggest cities by state
biggest_cities <- cities %>%
  mutate(population = as.double(population)) %>%
  left_join(state_mapping1, by = "state") %>%
  group_by(abb) %>%
  slice_max(population, n = 1) %>%
  ungroup() %>%
  select(abb, city, population, longitude, latitude) %>%
  rename(c(
    "state" = "abb",
    "biggest_city" = "city",
    "city_long" = "longitude",
    "city_lat" = "latitude"
  ))
```

### Find distance between charging station and the biggest city in each state.

The `get_geo_distance()` function shown in the chunk below was sourced
from [Kan
Nishida](https://blog.exploratory.io/calculating-distances-between-two-geo-coded-locations-358e65fcafae).

``` r
# Kudos to Kan Nishida for the function!
get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function. 
  }
  distance
}

# first attempt at using dism within dataframe
# distm(station_coords, c(-71.06, 42.36), fun = distHaversine) %>%
#   as.data.frame() %>%
#   rename(c("dist" = "V1")) %>%
#   mutate(index = row_number()) %>%
#   left_join(estations_ma_test) %>%
#   mutate(dist_m = dist / 1609.344)

reasonable_driving_distance <- 10

# calculate distances and other relevant conversions
estation_dist <- estations %>%
  left_join(biggest_cities, by = "state") %>%
  group_by(objectid) %>%
  mutate(dist_mi = get_geo_distance(longitude, latitude, city_long, city_lat)) %>%
  ungroup() %>%
  filter(dist_mi <= reasonable_driving_distance) %>%
  mutate(
    ev_level1_evse_num = replace_na(ev_level1_evse_num, 0),
    ev_level2_evse_num = replace_na(ev_level2_evse_num, 0),
    ev_dc_fast_count = replace_na(ev_dc_fast_count, 0),
    total_ports = ev_level1_evse_num + ev_level2_evse_num + ev_dc_fast_count
  )

# roll up to state level
ports_per_capita <- estation_dist %>%
  group_by(state, biggest_city, population) %>%
  dplyr::summarise(total_ports = sum(total_ports)) %>%
  ungroup() %>%
  mutate(ports_per_capita = total_ports / population) %>%
  arrange(desc(ports_per_capita)) %>%
  mutate(rank = row_number())
```

    ## `summarise()` has grouped output by 'state', 'biggest_city'. You can override using the `.groups` argument.

``` r
# prepare data frame for visualization by converting to sf file

## retrieve shape file for us states
us_states <- data.frame(spData::us_states) %>%
  janitor::clean_names() %>%
  select(name, geometry)

## redefine mapping of us states for joining with shape file
state_mapping2 <- tibble(
  name = append(state.name, "District of Columbia"),
  state = append(state.abb, "DC")
)

## convert dataframe for dorling cartogram work
ports_cart <- ports_per_capita %>% 
  inner_join(state_mapping2, by = "state") %>%
  inner_join(us_states, by = "name") %>%
  filter(!(name %in% c("AK", "HI"))) %>%
  st_as_sf() %>%
  st_transform(crs = "epsg:3395") %>%
  cartogram_dorling("ports_per_capita")
```

# Visualize

Visualization inspired by [Ansgar
Wolsing](https://github.com/bydata/tidytuesday/blob/main/2022/09/R/dorling_cartogram.R).

``` r
# values

## fonts
font_add_google("Noto Sans")
showtext_auto()
f1 <- "Noto Sans"
```

``` r
# plot details

## per capita best state
best_state_per_capita <- ports_per_capita %>%
  head(1) %>%
  pull(state)

per_capita_text <- ports_per_capita %>%
  filter(state == best_state_per_capita) %>%
  mutate(
    text = paste0(biggest_city, ", ", state, " had the most ports per capita, with ", total_ports, " charging ports for their ", population, " residence (", round(ports_per_capita, 4), " per capita)")
  ) %>%
  pull(text)

## raw total ports best state
best_state_raw <- ports_per_capita %>%
  arrange(desc(total_ports)) %>%
  head(1) %>%
  pull(state)

raw_text <- ports_per_capita %>%
  filter(state == best_state_raw) %>%
  mutate(
    text = paste0(biggest_city, ", ", state, " had the most total ports, with ", total_ports, " charging ports for their ", population, " residence (", round(ports_per_capita, 4), " per capita)")
  ) %>%
  pull(text)
```

``` r
# plot the relationships
ports_cart %>%
  ggplot() + 
  geom_sf(aes(fill = ports_per_capita)) + 
  geom_sf_text(aes(label = paste0("#", rank), size = 0.5 * ports_per_capita), family = f1, vjust = -1.5) +
  geom_sf_text(aes(label = state, size = ports_per_capita), family = f1, fontface = "bold") + 
  geom_sf_text(aes(label = biggest_city, size = 0.5 * ports_per_capita), family = f1, vjust = 2) +
  # scales, themes, etc
  scale_fill_viridis_c(option = "viridis", name = "Electronic Ports Within Driving Distance of Biggest City per City Capita") + 
  scale_size_continuous(guide = NULL) +
  guides(fill = guide_colorsteps(
    title.position = "top",
    barwidth = unit(18, "cm"),
    barheight = unit(0.5, "cm")
  )) + 
  labs(
    title = "Electronic Fueling Stations Across the Continental US",
    subtitle = glue("Total number of electronic charging ports (summarized across all charging stations) within a {reasonable_driving_distance} mile radius of the biggest city in each state per capita (of city population). {per_capita_text}. {raw_text}."),
    caption = "Data Source: <b>US DOT</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  theme_void(base_family = f1) + 
  theme(
    plot.title = element_text(size = 32, face = "bold"),
    plot.subtitle = element_textbox_simple(size = 16, margin = margin(t = 5, b = 5)),
    plot.caption = element_textbox(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    plot.background = element_rect(fill = "seashell")
  )
```

![](Alternative-Charging-Stations_files/figure-gfm/Electronic%20Car%20Ports%20per%20Capita%20Within%2010mi-1.png)<!-- -->
