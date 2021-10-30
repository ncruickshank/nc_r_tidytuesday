Ultra Running
================
Nick Cruickshank
10/30/2021

-   [Introduction](#introduction)
-   [Data Analysis](#data-analysis)
    -   [Libraries](#libraries)
    -   [Import Data](#import-data)
    -   [Exploratory Data Analysis](#exploratory-data-analysis)
    -   [Tidy Data](#tidy-data)
    -   [Visualization](#visualization)

![Run Rabbit - rabbit’s michael mcknight sweeps triple crown with
historic moab 240
win](https://cdn.shopify.com/s/files/1/0780/9833/files/MikeMcKnightBlog_1024x1024.png?v=1571285963)

# Introduction

This weeks [Tidy
Tuesday](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-10-26)
project features **Ultra Running** data collected by [Benjamin
Nowak](https://twitter.com/BjnNowak) from the [International Trail
Running Association (ITRA)](https://itra.run/Races/FindRaceResults).

Some highlights from a study performed by
[RunRepeat.com](https://runrepeat.com/state-of-ultra-running):

> Key results  
> Female ultra runners are faster than male ultra runners at distances
> over 195 miles. The longer the distance the shorter the gender pace
> gap. In 5Ks men run 17.9% faster than women, at marathon distance the
> difference is just 11.1%, 100-mile races see the difference shrink to
> just .25%, and above 195 miles, women are actually 0.6% faster than
> men.  
> Participation has increased by 1676% in the last 23 years from 34,401
> to 611,098 yearly participations and 345% in the last 10 years from
> 137,234 to 611,098. There have never been more ultra runners.  
> More ultra runners are competing in multiple events per year. In 1996,
> only 14% of runners participated in multiple races a year, now 41% of
> participants run more than one event per year. There is also a
> significant increase in the % of people who run 2 races a year, 17.2%
> (from 7.7% to 24.9%) and 3 races, 6.7% (from 2.8% to 9.5%).

> Data and methodology  
> The data covers over 85% of ultra running events worldwide.  
> The analysis includes any distance runs longer than 26.2 miles
> including trail runs, mountain runs, and road runs.  
> The analysis includes runs set for any time over 6 hours since a small
> portion of ultra races do not have a singular set distance.  
> In over 5,010,730 finish times only 3.77% of participants, race in a
> timed race compared to a set distance race.  
> The study was done in collaboration with the International Association
> of Ultrarunners (IAU).  
> The data includes the results of 5,010,730 million finishers from over
> 15 thousand races.  
> This study looks at data from 1996-2018

# Data Analysis

## Libraries

``` r
# libraries
library(ggtext)
library(glue)
library(lubridate)
library(readr)
library(tidyverse)
```

## Import Data

``` r
# import data
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')
```

``` r
# ultra rankings head
head(ultra_rankings)
```

    ## # A tibble: 6 x 8
    ##   race_year_id  rank runner           time          age gender nationality time_in_seconds
    ##          <dbl> <dbl> <chr>            <chr>       <dbl> <chr>  <chr>                 <dbl>
    ## 1        68140     1 VERHEUL Jasper   26H 35M 25S    30 M      GBR                   95725
    ## 2        68140     2 MOULDING JON     27H 0M 29S     43 M      GBR                   97229
    ## 3        68140     3 RICHARDSON Phill 28H 49M 7S     38 M      GBR                  103747
    ## 4        68140     4 DYSON Fiona      30H 53M 37S    55 W      GBR                  111217
    ## 5        68140     5 FRONTERAS Karen  32H 46M 21S    48 W      GBR                  117981
    ## 6        68140     6 THOMAS Leigh     32H 46M 40S    31 M      GBR                  118000

``` r
# race head
head(race)
```

    ## # A tibble: 6 x 13
    ##   race_year_id event   race   city   country date       start_time participation
    ##          <dbl> <chr>   <chr>  <chr>  <chr>   <date>     <time>     <chr>        
    ## 1        68140 Peak D~ Mills~ Castl~ United~ 2021-09-03 19:00      solo         
    ## 2        72496 UTMB®   UTMB®  Chamo~ France  2021-08-27 17:00      Solo         
    ## 3        69855 Grand ~ Ultra~ viell~ France  2021-08-20 05:00      solo         
    ## 4        67856 Persen~ PERSE~ Aseno~ Bulgar~ 2021-08-20 18:00      solo         
    ## 5        70469 Runfir~ 100 M~ uluki~ Turkey  2021-08-20 18:00      solo         
    ## 6        66887 Swiss ~ 160KM  Münst~ Switze~ 2021-08-15 17:00      solo         
    ## # ... with 5 more variables: distance <dbl>, elevation_gain <dbl>,
    ## #   elevation_loss <dbl>, aid_stations <dbl>, participants <dbl>

## Exploratory Data Analysis

What does the distribution of elevation gains look like for races?

``` r
race %>%
  filter(elevation_gain != 0) %>%
  mutate(round_elevation = round(elevation_gain, -3)) %>%
  count(round_elevation, sort = TRUE) %>%
  ggplot(aes(round_elevation, n)) + 
  geom_col(aes(fill = round_elevation)) + 
  scale_fill_viridis_c(option = "magma", guide = "none") + 
  scale_x_continuous(name = "Elevation Gain", breaks = seq(1000, 15000, 2000), labels = function(x) paste0(x, "ft")) + 
  scale_y_continuous(name = NULL) +
  labs(
    title = "Counts of various elevation gains for ultra races",
    subtitle = "Elevation gain is default provided to the 10s degree.\nRounding to the nearest 1000s will provide better visualization downstream."
  ) +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
```

![](Ultra-Running_files/figure-gfm/counts%20of%20elevation-1.png)<!-- -->

Rounding elevation gain to the nearest 1000s looks like it will provide
the best base for visualization. Rounding to the nearest 100s creates
too much noise, and may not be as meaningful.

What time of day to most races start at?

``` r
race %>%
  mutate(hour = hour(start_time)) %>%
  count(hour) %>%
  filter(hour != 0) %>%
  ggplot(aes(hour, n)) + 
  geom_col() + 
  scale_x_continuous(name = "Hour of Day", breaks = seq(0, 23, 3)) + 
  scale_y_continuous(name = NULL) +
  labs(
    title = "Counts of race start hours",
    subtitle = "Over 500 races started at 00:00:00 apparently.\nThese were filtered out due to suspected unreliability."
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey50"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
```

![](Ultra-Running_files/figure-gfm/counts%20of%20race%20start%20hours-1.png)<!-- -->

## Tidy Data

``` r
# tidy dataframes

## ultra rankings

### counts by gender by race
race_gender_participation <- ultra_rankings %>%
  group_by(race_year_id, gender) %>%
  dplyr::summarise(n = n_distinct(runner)) %>%
  filter(!is.na(gender)) %>%
  pivot_wider(id_cols = "race_year_id", names_from = "gender", names_prefix = "count_", values_from = "n") %>%
  janitor::clean_names()

### average finishing time by gender by race
race_gender_times <- ultra_rankings %>%
  filter(!is.na(gender)) %>%
  group_by(race_year_id, gender) %>%
  dplyr::summarise(avg_finish_time = mean(time_in_seconds, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "race_year_id", names_from = "gender", names_prefix = "avg_time_", values_from = "avg_finish_time") %>%
  janitor::clean_names()

## race 
race_tidy <- race %>%
  # fix the "distance == 0" problem
  mutate(
    advertised_distance = as.double(str_extract(race, "[0-9]{1,4}")),
    unit_of_measure = str_extract(str_to_lower(race), "(?<=[0-9]{2,4})(\\s)?(miles|mile|m|km|k|kilometer|kilometers|loops|loop)"),
    unit_of_measure = case_when(
      str_detect(unit_of_measure, "(mile|miles|m)") ~ "Miles",
      str_detect(unit_of_measure, "(km|k|kilometer|kilometers)") ~ "Kilometers",
      TRUE ~ "NA"
    ),
    advertised_distance_km = if_else(unit_of_measure == "Kilometers", advertised_distance, advertised_distance * 1.60934)
  ) %>%
  filter(!(is.na(distance))) %>%
  mutate(distance = if_else(distance == 0, advertised_distance_km, distance)) %>%
  rename(c("distance_km" = "distance")) %>%
  mutate(distance_mi = distance_km / 1.60934) %>%
  # filter for just solo participation events (which represent the majority anyway)
  filter(str_to_lower(participation) == "solo") %>%
  select(-advertised_distance, -unit_of_measure, -advertised_distance_km, -participation) %>%
  # filter for only 100 mi races
  filter(round(distance_mi) >= 98 & round(distance_mi) <= 102) %>%
  # create elevation gain bins
  mutate(elevation_bin = round(elevation_gain, -3)) %>%
  # separate participation by gender 
  ## note: participants != M + F from count(ultra_rankings, gender)
  ## best approximation might be to apply the M/F ratio to the participants column
  left_join(race_gender_participation, by = "race_year_id") %>%
  mutate(
    count_m = replace_na(count_m, 0),
    count_w = replace_na(count_w, 0),
    estimated_men = participants * (count_m / (count_m + count_w)),
    estimated_women = participants * (count_w / (count_m + count_w))
  ) %>%
  ## you actually lose a lot of rows here (~50%)
  filter(participants != 0) %>%
  # add in average time to finish by gender and calculate pace
  left_join(race_gender_times, by = "race_year_id") %>%
  mutate(
    # minutes per mile
    pace_m = (avg_time_m / 60) / distance_mi,
    pace_w = (avg_time_w / 60) / distance_mi
  ) %>%
  # final cleanup
  select(-count_m, -count_w)
```

What race distances have the greatest number of participants?

``` r
race_tidy %>%
  mutate(distance = round(distance_mi)) %>%
  count(distance, sort = TRUE) %>%
  filter(distance >= 90 & distance <= 120) %>%
  mutate(
    color = case_when(
      distance == 100 ~ "red",
      distance %in% c(98, 99, 101, 102) ~ "darkred",
      TRUE ~ "grey40"
    )
  ) %>%
  ggplot(aes(distance, n)) + 
  annotate("segment", x = 93, xend = 112, y = seq(from = 40, to = 400, length.out = 10), yend = seq(from = 40, to = 400, length.out = 10), linetype = "dashed", color = "grey50") +
  annotate("text", x = 92, y = seq(from = 40, to = 400, length.out = 10), label = seq(from = 40, to = 400, length.out = 10)) +
  geom_col(aes(fill = color)) +
  scale_fill_identity() + 
  geom_text(aes(x = distance, y = 10, label = paste0(distance, "mi")), size = 3) + 
  labs(
    title = "Counts of various rounded ultra running race distances",
    subtitle = "Vast majority in data set are approximately 100 miles"
  ) +
  theme_void() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
```

![](Ultra-Running_files/figure-gfm/count%20of%20rounded%20race%20distances-1.png)<!-- -->

``` r
# values

## colors
male_color <- "#0245C4"
female_color <- "#EE4B23"
sky_color <- "#87F5F9"
sky_detail_color <- "#54999C"
sky_text_color <- "#335C5D"
mountain_color <- "#1F6F27"
mountain_detail_color <- "#78EF84"
```

## Visualization

``` r
# final reshaping of race_tidy for the visualization
# men
men_elevation <- race_tidy %>%
  group_by(elevation_bin) %>%
  dplyr::summarise(
    races = n(),
    total_participants = sum(estimated_men),
    avg_finish_time = mean(avg_time_m, na.rm = TRUE), # seconds
    avg_pace = mean(pace_m, na.rm = TRUE) # min per mile
  ) %>%
  ungroup() %>% 
  mutate(
    gender = "Male",
    x = elevation_bin,
    y = elevation_bin
  )

# women
women_elevation <- race_tidy %>%
  group_by(elevation_bin) %>%
  dplyr::summarise(
    races = n(),
    total_participants = sum(estimated_women),
    avg_finish_time = mean(avg_time_w, na.rm = TRUE), # seconds
    avg_pace = mean(pace_w, na.rm = TRUE) # min per mile
  ) %>%
  ungroup() %>%
  mutate(
    gender = "Female", 
    x = rev(seq(14001, 27001, 1000)),
    y = 14000 - rev(seq(0, 13000, 1000))
  ) %>%
  arrange(desc(elevation_bin))

elevation <- bind_rows(men_elevation, women_elevation)

start_year <- year(min(race_tidy$date))
end_year <- year(max(race_tidy$date))

elevation_top_hits <- elevation %>%
  group_by(gender) %>%
  slice_max(total_participants, n = 2) %>%
  arrange(desc(total_participants)) %>%
  mutate(
    target_y = total_participants + y,
    x_lab = 4500,
    y_lab = if_else(elevation_bin == 6000, 32000, 26000)
  )

top_races <- race_tidy %>%
  filter(elevation_bin %in% c(2000, 6000)) %>%
  group_by(elevation_bin, event, country) %>%
  dplyr::summarise(
    total_participants = sum(participants),
    total_men = round(sum(estimated_men)),
    total_women = round(sum(estimated_women))
  ) %>%
  ungroup() %>%
  group_by(elevation_bin) %>%
  slice_max(total_participants, n = 3) %>%
  mutate(
    string_col = paste0("<b>", event, "</b> in ", country, "<br>", total_participants, " participants (<b style=color:'{male_color}'>", total_men, " men</b> & <b style=color:'{female_color}'>", total_women, " women</b>)")
  )
  
list_top_races_6000 <- top_races %>%
  filter(elevation_bin == 6000) %>%
  pull(string_col) %>%
  glue_collapse(sep = "<br>")

list_top_races_2000 <- top_races %>%
  filter(elevation_bin == 2000) %>%
  pull(string_col) %>%
  glue_collapse(sep = "<br>")
```

``` r
elevation %>%
  ggplot() +
  # create a participant "grid" outside the mountain
  ## axis text
  annotate("text", x = 14000, y = seq(15000, 19000, 1000), label = paste0(seq(1, 5, 1), "k"), color = sky_detail_color) + 
  ## men
  annotate(
    "segment",
    x = 0, xend = 13000,
    y = seq(1000, 5000, 1000), yend = seq(15000, 19000, 1000),
    color = sky_detail_color, linetype = "aa", alpha = 0.4
  ) +
  ## women
  annotate(
    "segment",
    x = 15000, xend = 27000,
    y = rev(seq(15000, 19000, 1000)), yend = rev(seq(1000, 5000, 1000)),
    color = sky_detail_color, linetype = "aa", alpha = 0.4
  ) +
  # plot the key measure
  ## highlights
  geom_text(data = elevation_top_hits, aes(x, target_y + 1000, label = round(total_participants)), color = sky_detail_color) +
  geom_point(data = elevation_top_hits, aes(x, target_y, fill = as.character(elevation_bin)), size = 8, shape = 21) + 
  ## everything 
  geom_area(aes(x, y+total_participants, color = gender), fill = mountain_color, size = 1.5) +
  # create an elevation "grid" within the mountain
  geom_line(aes(x, y), color = mountain_detail_color, linetype = "longdash", size = 1.2) + 
  annotate("text", x = 14000, y = 1000, label = "Total Elevation Gain (in Meters)", color = mountain_detail_color, fontface = "bold") +
  annotate(
    "segment", 
    x = seq(4000, 12000, 2000), xend = rev(seq(16000, 25000, 2000)), 
    y = seq(3000, 11000, 2000), yend = seq(3000, 11000, 2000), 
    color = mountain_detail_color, linetype = "aa", alpha = 0.4
  ) +
  annotate("text", x = 14000, y = seq(3000, 13000, 2000), label = paste0(seq(3, 13, 2), "k"), color = mountain_detail_color) +
  # scales, themes, texts, etc
  ## title
  annotate("text", x = 14000, y = 45000, 
           label = "Ultra Running\n100 Mile Races", 
           size = 12, fontface = "bold", hjust = 0.5, color = sky_text_color, lineheight = 0.8) + 
  annotate("richtext", x = 14000, y = 41000, 
           label = glue("Difference in <b>total participation</b> between <b style=color:'{male_color}'>men</b><br>and <b style=color:'{female_color}'>women</b> by Elevation Gain Category (grouped<br>into 1000s of meters) from {start_year} to {end_year}"),
           fill = NA, label.color = NA, color = sky_text_color, size = 5) +
  annotate("text", x = 14000, y = 38000,
           label = "Counts of men and women derived by finding the product of the participants\nlogged for each race multiplied by the gender ratios found\nfrom the Ultra Rankings for that same race",
           size = 4, fontface = "italic", hjust = 0.5, color = sky_text_color, lineheight = 0.8) + 
  ## subtitle
  geom_point(data = elevation_top_hits, aes(x = x_lab, y = y_lab, fill = as.character(elevation_bin)), shape = 21, size = 10) + 
  annotate("text", x = 7000, y = c(26000, 32000), label = c("2000\nMeters", "6000\nMeters"), 
           hjust = 0.5, fontface = "bold", size = 4.5, lineheight = 0.8, color = sky_detail_color) +
  annotate("richtext", x = 17000, y = 32000,
           label = glue(list_top_races_6000),
           fill = NA, label.color = NA, hjust = 0.5, size = 4, color = sky_detail_color) + 
  annotate("richtext", x = 17000, y = 26000,
           label = glue(list_top_races_2000),
           fill = NA, label.color = NA, hjust = 0.5, size = 4, color = sky_detail_color) + 
  ## label the male and female side of the graph
  # annotate("text", x = 3500, y = 4500, label = "Men", angle = 45, color = male_color, size = 5, fontface = "bold") + 
  # annotate("text", x = 24500, y = 4500, label = "Female", angle = 315, color = female_color, size = 5, fontface = "bold") + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 48000)) + 
  coord_fixed() + 
  scale_color_manual(values = c(
    "Female" = female_color,
    "Male" = male_color
  )) + 
  scale_fill_manual(values = c("gold", "darkgoldenrod2")) + 
  theme_void() + 
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = sky_color)
  )
```

![](Ultra-Running_files/figure-gfm/Ultra%20Running%20Participation%20by%20Gender%20and%20Elevation-1.png)<!-- -->
