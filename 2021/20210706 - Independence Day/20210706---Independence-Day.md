20210706 - Independence Day
================
Nick Cruickshank
7/9/2021

  - [Georgios Karamanis Inspired
    Analysis](#georgios-karamanis-inspired-analysis)
      - [Import Libraries and Data](#import-libraries-and-data)
      - [Tidy Data and Reshape](#tidy-data-and-reshape)
      - [Plot](#plot)

# Georgios Karamanis Inspired Analysis

Kudos to [Georgios
Karamanis](https://github.com/gkaramanis/tidytuesday/blob/master/2021/2021-week27/holidays.R)

## Import Libraries and Data

``` r
# libraries
library(tidyverse)
#library(camcorder) #doesn't appaer to be available, hopefully irrelevant
library(lubridate) 
library(geofacet)
library(countrycode)
library(ggtext)
```

``` r
# data
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')
```

## Tidy Data and Reshape

``` r
# tidy and reshape
holiday_months <- holidays %>%
  filter(!is.na(date_parsed)) %>%
  select(country, date_parsed, day) %>%
  mutate(
    month_num = month(ymd(date_parsed)),
    month_label = month(month_num, label = TRUE),
    country_code = countrycode(country, origin = "country.name", destination = "iso3c"),
    country_code = if_else(country == "Micronesia", "FSM", country_code)
  ) %>%
  group_by(month_num, day) %>%
  mutate(
    n = n(),
    all_codes = paste(country_code, collapse = "\n")
  ) %>%
  ungroup() %>%
  complete(month_num, day, fill = list(n = 2)) %>%
  mutate(
    month_label = month(month_num, label = TRUE),
    month_label = fct_rev(month_label)
  ) %>%
  filter(
    !(month_label %in% c("Apr", "Jun", "Sep", "Nov") & day == 31),
    !(month_label == "Feb" & day > 29)
  )
```

``` r
# options
f1 = "KvivType Titling"
f2 = "KyivType Sans"
```

## Plot

``` r
# plot
holiday_months %>%
  ggplot() + 
  # days
  geom_text(
    aes(day, month_label, label = ifelse(!is.na(country), day, "."), size = n),
    stat = "unique", family = f1
  ) + 
  # country codes
  geom_text(
    aes(day, month_label, label = ifelse(!is.na(country), all_codes, "")),
    size = 2, nudge_y = -0.2, stat = "unique",
    family = f2, vjust = 1, lineheight = 0.9
  ) + 
  scale_size_continuous(range = c(3.5,7)) + 
  scale_y_discrete(labels = toupper) + 
  coord_cartesian(clip = "off", expand = FALSE) + 
  labs(
    title = toupper("National Independence Days"),
    caption = "Source: Wikipedia (https://en.wikipedia.org/wiki/List_of_national_independence_days)\nKuods to Georgios Karamanis"
  ) + 
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = "none",
    axis.text.y = element_text(family = f1),
    plot.margin = margin(20,20,10,20),
    plot.title = element_text(family = f2, size = 20, hjust = 0.5, margin = margin(0,0,20,0)),
    plot.caption = element_text(family = f2, size = 8, hjust = 0.5, margin = margin(20,0,0,0))
  )
```

![](20210706---Independence-Day_files/figure-gfm/g%20karamanis%20indepdence%20day%20calendar%20plot-1.png)<!-- -->
