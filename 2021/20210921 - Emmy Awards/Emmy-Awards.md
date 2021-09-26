Emmy Awards
================
Nick Cruickshank
9/25/2021

-   [Introduction](#introduction)
-   [Data Analysis: Battle of the Streaming
    Services](#data-analysis-battle-of-the-streaming-services)
-   [Plot](#plot)

![](https://deadline.com/wp-content/uploads/2020/07/emmys.jpg?w=681&h=383&crop=1)

# Introduction

This weeks [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-09-21)
comes from
[emmys.com](https://www.emmys.com/awards/nominations/award-search).

According to [Wikipedia](https://en.wikipedia.org/wiki/Emmy_Awards):
&gt; An Emmy Award, or simply Emmy, is a trophy presented at one of the
numerous annual American events or competitions that each recognize
achievements in a particular sector of the television industry. The Emmy
is considered one of the four major entertainment awards in the United
States, the others being the Grammy (for music), the Oscar (Academy
Award) (for film), and the Tony (for theatre). The two events that
receive the most media coverage are the Primetime Emmy Awards and the
Daytime Emmy Awards, which recognize outstanding work in American
primetime and daytime entertainment programming, respectively.

# Data Analysis: Battle of the Streaming Services

``` r
# libraries
library(hrbrthemes)
library(readr)
library(tidyverse)
library(waffle)
```

``` r
# data
nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')
```

``` r
# head
head(nominees)
```

    ## # A tibble: 6 x 10
    ##   category logo  production type  title distributor producer  year  page page_id
    ##   <chr>    <chr> <chr>      <chr> <chr> <chr>       <chr>    <dbl> <dbl>   <dbl>
    ## 1 Outstan~ http~ <NA>       Nomi~ blac~ ABC         ABC       2021     0       1
    ## 2 Outstan~ http~ <NA>       Nomi~ Brid~ Netflix     A Netfl~  2021     0       2
    ## 3 Outstan~ http~ <NA>       Nomi~ Fami~ FOX         20th Te~  2021     0       3
    ## 4 Outstan~ http~ <NA>       Nomi~ Arch~ FX Networks FX Prod~  2021     0       4
    ## 5 Outstan~ http~ Elisabeth~ Nomi~ The ~ Hulu        Hulu, M~  2021     0       5
    ## 6 Outstan~ http~ Martha Sp~ Nomi~ The ~ Hulu        Hulu, M~  2021     0       5

``` r
# values

## regexes
outstanding_series_regex <- "^Outstanding*\\s*(Series|Program|Special)$" # needs some work

## lists
top_streaming_services <- c("Netflix", "Hulu", "Prime Video",
                            "Disney+", "HBO", "Paramount+",
                            "Peacock", "Apple TV+")
```

``` r
# tidy

## filter for categories like "Outstanding X Series"
emmy <- nominees %>%
  select(-starts_with("page")) %>%
  mutate(
    category2 = str_to_title(str_remove(category, " - [0-9]{4}")),
    title = str_to_title(title),
    distributor = case_when(
      distributor == "HBO Max" ~ "HBO",
      distributor %in% c("Amazon", "Amazon Instant Video") ~ "Prime Video",
      TRUE ~ distributor
    ),
    host = case_when(
      str_detect(title, "Jon Stewart") ~ "Jon Stewart",
      str_detect(title, "John Oliver") ~ "John Oliver",
      str_detect(title, "Colbert") ~ "Stephen Colbert",
      str_detect(title, "David Letterman") ~ "David Letterman",
      str_detect(title, "Jimmy Kimmel") ~ "Jimmy Kimmel"
    )
  )

shows_of_interest <- c("The Daily Show With Jon Stewart", "Last Week Tonight With John Oliver",
                       "The Late Show With Stephen Colbert", "The Colbert Report",
                       "Late Show With David Letterman", "Jimmy Kimmel Live")

hosts <- c("Jon Stewart", "John Oliver", "Stephen Colbert", "David Letterman", "Jimmy Kimmel")

host_categories <- emmy %>%
  filter(!(is.na(host))) %>%
  distinct(category) %>%
  pull(category)

category_winners <- emmy %>%
  filter(
    category %in% host_categories,
    type == "Winner"
  ) %>%
  mutate(winner = ifelse(host %in% hosts, host, "Other")) %>%
  distinct(category, winner)

df <- emmy %>%
  filter(category %in% host_categories) %>%
  mutate(host = ifelse(host %in% hosts, host, "Other")) %>%
  group_by(category) %>%
  dplyr::summarise(
    nominees = n(),
    `Jon Stewart` = sum(host == "Jon Stewart"),
    `John Oliver` = sum(host == "John Oliver"),
    `Stephen Colbert` = sum(host == "Stephen Colbert"),
    `David Letterman` = sum(host == "David Letterman"),
    `Jimmy Kimmel` = sum(host == "Jimmy Kimmel"),
    `Other` = sum(host == "Other")
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(-category, -nominees)) %>%
  left_join(category_winners, by = "category") %>%
  mutate(result = ifelse(name == winner & name != "Other", 1, 0)) %>%
  separate(category, into = c("category", "year"), sep = " - ")
```

# Plot

``` r
# plot
waffle <- df %>%
  group_by(year, name) %>%
  dplyr::summarise(
    Nomination = sum(value) - sum(result),
    Win = sum(result)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(-year, -name), names_to = "result", values_to = "n") %>%
  mutate(
    fill = factor(paste0(name, " - ", result), levels = c(
      "Stephen Colbert - Win", "Stephen Colbert - Nomination",
      "Jon Stewart - Win", "Jon Stewart - Nomination",
      "John Oliver - Win", "John Oliver - Nomination",
      "Jimmy Kimmel - Win", "Jimmy Kimmel - Nomination",
      "David Letterman - Win", "David Letterman - Nomination",
      "Other - Win", "Other - Nomination"
    )),
    year = as.double(year)
  ) %>%
  filter(
    !(is.na(n)), !(is.na(year)),
    year != 2001
  )

ggplot(waffle, aes(fill = fill, values = n)) + 
  geom_waffle(color = "white", size = .5, n_rows = 10, flip = TRUE) +
  facet_wrap(~ year, nrow = 1, strip.position = "bottom") + 
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() + 
  labs(
    title = "Which Talk Show Hosts have the greatest success at the Emmy's?"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom"
  )
```

![](Emmy-Awards_files/figure-gfm/Talk%20Show%20Waffles-1.png)<!-- -->

``` r
# need to figure out a color schemes for each Talk Show Host (dark for wins, light for nominations)
# also need to figure out if I want to keep the "Other" categories.
```
