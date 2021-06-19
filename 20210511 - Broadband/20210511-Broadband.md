20210511 - Broadband
================
Nick Cruickshank
6/17/2021

``` r
# libraries
library(readr)
library(rworldmap)
library(tidycensus)
library(tidyverse)
library(tigris)
library(usmap)
library(zipcodeR)
```

``` r
# data
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv') 
broadband <- broadband %>%
  janitor::clean_names()
broadband_zip <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband_zip.csv')
broadband_zip <- broadband_zip %>%
  janitor::clean_names()
```

# Introduction

This weeks analysis comes from the [R Tidy Tuesday
Community 2021-05-11](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-05-11)
project, which featured data provided by
[Microsoft](https://github.com/microsoft/USBroadbandUsagePercentages)
and [The
Verge](https://www.theverge.com/22418074/broadband-gap-america-map-county-microsoft-data)
on broadband data by country in America.

> If broadband access was a problem before 2020, the pandemic turned it
> into a crisis. As everyday businesses moved online, city council
> meetings or court proceedings became near-inaccessible to anyone whose
> connection couldn’t support a Zoom call. Some school districts started
> providing Wi-Fi hotspots to students without a reliable home
> connection. In other districts, kids set up in McDonald’s parking lots
> just to get a reliable enough signal to do their homework. After years
> of slowly widening, the broadband gap became impossible to ignore.

# Tidy Data

``` r
latlong <- geocode_zip(broadband_zip$postal_code)
bz <- broadband_zip %>%
  rename(c(
    "zipcode" = "postal_code",
    "state" = "st",
    "fips" = "county_id"
    )) %>%
  mutate(
    zipcode = as.character(zipcode),
    group = 1,
    broadband_category = case_when(
      broadband_usage <= 0.1 ~ "<= 10%",
      #broadband_usage < 0.9 ~ NULL,
      broadband_usage >= 0.9 ~ ">= 90%"
    )
    ) %>%
  left_join(latlong)
```

# Exploratory Analysis

## Broadband Distribution by Country

``` r
# this gets close, but can't figure out why it won't go by county
bz_map <- bz %>%
  filter(!(is.na(broadband_category))) %>%
  select(fips, broadband_category) 

#plot_usmap(regions = "states", ata = bz_map, color = "black", size = 1.5) + 
plot_usmap(regions = "counties", data = bz_map, values = "broadband_category", color = "white", size = 0.1) + 
  scale_fill_manual(values = c(
    "<= 10%" = "dodgerblue",
    ">= 90%" = "gold"
  )) +
  labs(
    title = "Broadband Internet Access in America by County (as of October 2020)",
    fill = "Percent of people per county using broadband speed internet"
  ) + 
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "slategray1")
    )
```

![](20210511-Broadband_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Population by County

``` r
#data.frame(tigris::urban_areas())
```

## Average Income By County

## Population x Broadband by County

## Income x Broadband by County
