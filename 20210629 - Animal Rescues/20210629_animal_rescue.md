20210629 - Rescue Animals
================
Nick Cruickshank
6/30/2021

  - [Introduction](#introduction)
  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Notes](#notes)
  - [Data Visualization Projects](#data-visualization-projects)
      - [Choropleth of animal rescues](#choropleth-of-animal-rescues)
      - [Timeline of animal rescue](#timeline-of-animal-rescue)
          - [What does an average year look like for animal
            rescue?](#what-does-an-average-year-look-like-for-animal-rescue)
          - [What are the busiest times of
            day?](#what-are-the-busiest-times-of-day)
      - [Where do animals get rescued from the
        most?](#where-do-animals-get-rescued-from-the-most)
      - [What animals cost the most to
        rescue?](#what-animals-cost-the-most-to-rescue)
      - [What kind of properties do animals most routinely get trapped
        in?](#what-kind-of-properties-do-animals-most-routinely-get-trapped-in)

``` r
# libraries
library(forcats)
library(glue)
library(lubridate)
library(readr)
library(tidyverse)
```

``` r
# data
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')
```

# Introduction

Data this week comes from
[London.gov](https://data.london.gov.uk/dataset/animal-rescue-incidents-attended-by-lfb).

> Fox in bedroom, dog trapped in wall. The London Fire Brigade responds
> to hundreds of requests to rescue animals each year. Its
> monthly-updated spreadsheet of such events goes back to 2009; it lists
> the location and type of property, the kind of animal and rescue,
> hours spent, a (very) brief description, and more. \[h/t Soph Warnes\]

Another
[article](https://www.theguardian.com/world/2021/jan/08/animal-rescues-london-fire-brigade-rise-2020-pandemic-year)
found that animals rescues increased by 20% in the year of the pandemic
(2020).

> The London fire brigade (LFB) was involved in 755 such incidents –
> more than two a day. The number of rescues rose by 20% compared with
> 2019 when there were 602, with the biggest rise coming in the number
> of non-domestic animals rescued, according to the data.

``` r
# tidy
## prominent species
top_species <- animal_rescues %>%
  mutate(animal_group_parent = str_to_lower(animal_group_parent)) %>%
  filter(!(str_detect(animal_group_parent, "unknown"))) %>%
  count(animal_group_parent) %>%
  arrange(desc(n)) %>%
  head(6) # very steep drop off after 6 species

top_species_list <- top_species$animal_group_parent
top3_species_list <- head(top_species, 3)$animal_group_parent

## final tidy
ar <- animal_rescues %>%
  filter(
    cal_year != 2021 # this year isn't complete, and therefore might throw off visualization
  ) %>%
  mutate(
    animal_group_parent = str_to_lower(animal_group_parent),
    # time
    date_time_of_call = as.POSIXct(date_time_of_call, format = "%d/%m/%Y %H:%M"),
    date = as.Date(date_time_of_call, format = "%d/%m/%Y"),
    floor_year_month = floor_date(date, "months"),
    time = hour(date_time_of_call),
    month_day = strftime(date_time_of_call, "%m-%d"),
    month = strftime(date_time_of_call, format = "%m\n%b"),
    day = day(date_time_of_call),
    # special service categories
    special_service_type_category = str_to_lower(special_service_type_category),
    service_category = str_remove(special_service_type_category, "animal rescue from")
  )

unredacted <- ar %>%
  filter(final_description != "Redacted")
```

``` r
# values
min_date <- strftime(min(ar$date), format = "%d-%b-%Y")
max_date <- strftime(max(ar$date), format = "%d-%b-%Y")
```

``` r
# functions
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
```

# Exploratory Data Analysis

## Notes

``` r
count(ar, cal_year)
```

    ## # A tibble: 12 x 2
    ##    cal_year     n
    ##       <dbl> <int>
    ##  1     2009   568
    ##  2     2010   611
    ##  3     2011   620
    ##  4     2012   603
    ##  5     2013   585
    ##  6     2014   583
    ##  7     2015   540
    ##  8     2016   604
    ##  9     2017   539
    ## 10     2018   610
    ## 11     2019   604
    ## 12     2020   758

`type_of_incident`: Only ‘Special Service’ represented.

`pump` = truck / unit deployed.

``` r
count(ar, originof_call) %>%
  arrange(desc(n))
```

    ## # A tibble: 8 x 2
    ##   originof_call             n
    ##   <chr>                 <int>
    ## 1 Person (mobile)        4005
    ## 2 Person (land line)     3032
    ## 3 Police                  131
    ## 4 Other FRS                49
    ## 5 Ambulance                 2
    ## 6 Coastguard                2
    ## 7 Not known                 2
    ## 8 Person (running call)     2

I don’t think there is much insight to be gleaned from whether the call
was made from a landline or mobile device.

# Data Visualization Projects

## Choropleth of animal rescues

## Timeline of animal rescue

By species?

``` r
ar %>%
  filter(animal_group_parent %in% top3_species_list) %>%
  mutate(animal_group_parent = str_to_title(animal_group_parent)) %>%
  count(floor_year_month, animal_group_parent) %>%
  ggplot(aes(floor_year_month, n)) + 
  geom_line() + 
  facet_wrap(~ animal_group_parent, ncol = 1) + 
  labs(
    title = "Number of animal rescues per month",
    subtitle = "High seasonality observed for birds and cats, not for dogs",
    x = "Date",
    y = "Monthly Rescues"
  )
```

![](20210629_animal_rescue_files/figure-gfm/overall_rescue_timeline-1.png)<!-- -->

### What does an average year look like for animal rescue?

``` r
ar_monthly_species_mode <- ar %>%
  filter(
    animal_group_parent %in% top3_species_list,
    service_category != "other animal assistance"
  ) %>%
  group_by(month, animal_group_parent) %>%
  dplyr::summarise(mode_service = Mode(service_category))

ar_monthly_species_mode %>%
  filter(animal_group_parent == "dog")
```

    ## # A tibble: 12 x 3
    ## # Groups:   month [12]
    ##    month     animal_group_parent mode_service   
    ##    <chr>     <chr>               <chr>          
    ##  1 "01\nJan" dog                 " water"       
    ##  2 "02\nFeb" dog                 " water"       
    ##  3 "03\nMar" dog                 " below ground"
    ##  4 "04\nApr" dog                 " below ground"
    ##  5 "05\nMay" dog                 " height"      
    ##  6 "06\nJun" dog                 " height"      
    ##  7 "07\nJul" dog                 " height"      
    ##  8 "08\nAug" dog                 " height"      
    ##  9 "09\nSep" dog                 " below ground"
    ## 10 "10\nOct" dog                 " below ground"
    ## 11 "11\nNov" dog                 " water"       
    ## 12 "12\nDec" dog                 " water"

``` r
ar %>%
  mutate(
    month = strftime(date_time_of_call, format = "%m\n%b"),
    day = day(date_time_of_call)
    ) %>%
  filter(animal_group_parent == "dog") %>%
  # group_by(cal_year, month, day, animal_group_parent) %>%
  # dplyr::summarise(n = n()) %>%
  # group_by(month, day, animal_group_parent) %>%
  # mutate(avg = mean(n)) %>%
  # ungroup() %>%
  count(month, day, animal_group_parent) %>%
  left_join(ar_monthly_species_mode) %>%
  ggplot(aes(day, n)) +
  geom_line() +
  geom_area(aes(fill = mode_service)) +
  facet_wrap(~ month, ncol = 12) + 
  labs(
    title = glue("Count of dog rescues each day of an average calendar year (from {min_date} to {max_date})"),
    subtitle = "Number of rescues on each day plotted. Would average be betterto plot on y-axis?",
    caption = "This graph needs to be repeated for each of the other top three species, and then each plot should be stacked."
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )
```

![](20210629_animal_rescue_files/figure-gfm/Rescues%20by%20Species%20and%20Service%20Type%20in%20an%20Average%20Year-1.png)<!-- -->

### What are the busiest times of day?

## Where do animals get rescued from the most?

## What animals cost the most to rescue?

Normalize by hours spent?

## What kind of properties do animals most routinely get trapped in?
