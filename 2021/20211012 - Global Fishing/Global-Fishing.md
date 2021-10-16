Global Fishing
================
Nick Cruickshank
10/16/2021

-   [Introduction](#introduction)
-   [Data Analysis](#data-analysis)
    -   [Load Data](#load-data)
    -   [Tidy Dataset](#tidy-dataset)
    -   [Visualization](#visualization)

![<https://www.hakaimagazine.com/features/a-short-history-of-aquaculture-innovation/>](https://www.hakaimagazine.com/wp-content/uploads/header-aquaculture-timeline-2048x984.jpg)

# Introduction

This weeks [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-10-12)
comes from [Our World in
Data](https://ourworldindata.org/fish-and-overfishing).

> -   Increasing pressures on fish populations mean one-third of global
>     fish stocks are overexploited – this has increased from 10% in the
>     1970s.  
> -   The world now produces more than 155 million tonnes of seafood
>     each year.  
> -   There are large differences in per capita fish consumption across
>     the world.  
> -   The world now produces more seafood from aquaculture (fish
>     farming) than from wild catch. This has played a key role in
>     alleviating pressure on wild fish populations.

# Data Analysis

``` r
# libraries
library(ggstream)
library(ggtext)
library(glue)
library(forcats)
library(patchwork)
library(readr)
library(tidyverse)
```

## Load Data

``` r
# data
farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv')
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')
```

``` r
# captured_vs_farmed

## these two dataframes are likely joined to form captured_vs_farmed
## head(farmed)
## head(captured)

head(captured_vs_farmed)
```

    ## # A tibble: 6 x 5
    ##   Entity      Code   Year `Aquaculture production (~ `Capture fisheries product~
    ##   <chr>       <chr> <dbl>                      <dbl>                       <dbl>
    ## 1 Afghanistan AFG    1969                         60                         400
    ## 2 Afghanistan AFG    1970                         60                         400
    ## 3 Afghanistan AFG    1971                         60                         500
    ## 4 Afghanistan AFG    1972                         60                         500
    ## 5 Afghanistan AFG    1973                         60                         500
    ## 6 Afghanistan AFG    1974                         60                         500

``` r
# stock
## sustainable and over exploited columns are percentages (shares)
## which add up to 100
head(stock)
```

    ## # A tibble: 6 x 5
    ##   Entity                   Code   Year `Share of fish stocks~ `Share of fish st~
    ##   <chr>                    <chr> <dbl>                  <dbl>              <dbl>
    ## 1 Eastern Central Atlantic <NA>   2015                   57.1               42.9
    ## 2 Eastern Central Atlantic <NA>   2017                   57.1               42.9
    ## 3 Eastern Central Pacific  <NA>   2015                   86.7               13.3
    ## 4 Eastern Central Pacific  <NA>   2017                   86.7               13.3
    ## 5 Eastern Indian Ocean     <NA>   2015                   73.1               26.9
    ## 6 Eastern Indian Ocean     <NA>   2017                   68.6               31.4

``` r
# production
head(production)
```

    ## # A tibble: 6 x 10
    ##   Entity      Code   Year `Commodity Balanc~ `Commodity Balan~ `Commodity Balan~
    ##   <chr>       <chr> <dbl>              <dbl>             <dbl>             <dbl>
    ## 1 Afghanistan AFG    1961                 NA                NA                NA
    ## 2 Afghanistan AFG    1962                 NA                NA                NA
    ## 3 Afghanistan AFG    1963                 NA                NA                NA
    ## 4 Afghanistan AFG    1964                 NA                NA                NA
    ## 5 Afghanistan AFG    1965                 NA                NA                NA
    ## 6 Afghanistan AFG    1966                 NA                NA                NA
    ## # ... with 4 more variables: ...

## Tidy Dataset

``` r
# values

## filters
countries <- c("China", "Indonesia", "India", "Vietnam", "United States")

## colors
aquaculture_color <- "#6DDDE9"
capture_color <- "#0035AF"
background_color <- "#D0F0FF"
```

`stock` df doesn’t look like it uses the same entities as other dfs.
Consider mapping the relationship between these two dfs, or simply
plotting what is shared between them (i.e., inner join).

I suspect that some entities are nested within bigger entities. For
example, “Southern Asia” is an entity, but so is “China”. “North
America” is an entity“, and so is”Americas". This would likely be a
whole mess, which would need to be sorted manually. Best solution is to
just pick one entity, or a group of entities, where this issue won’t
crop up.

According to
[Wikipedia](https://en.wikipedia.org/wiki/Fishing_industry_by_country),
the nations with the largest fishing industries are as follows (as of
2018):

``` r
wiki_top_countries <- tibble(
  country = c("China", "Indonesia", "India", "Vietnam", "United States"),
  capture = c(17800000, 6584419, 5082332, 2785940, 4931017),
  aquaculture = c(63700000, 1600000, 5703002, 3634531, 444369)
) %>%
  mutate(total = capture + aquaculture)

wiki_top_countries %>%
  arrange(desc(total)) %>%
  mutate(rank = row_number()) %>%
  pivot_longer(cols = c("capture", "aquaculture")) %>%
  ggplot(aes(fct_reorder(country, rank), value)) + 
  geom_bar(aes(fill = name), stat = "identity") +
  scale_fill_manual(values = c(aquaculture_color, capture_color)) +
  theme_minimal() + 
  labs(
    title = "Total fishing industry production by Nation in 2018",
    x = "",
    y = "Production"
  ) + 
  theme(
    legend.position = "top"
  )
```

![](Global-Fishing_files/figure-gfm/wiki%20fish%20nations-1.png)<!-- -->

``` r
# tidy

## stock
# stock_tidy <- stock %>%
#   janitor::clean_names() %>%
#   rename(c(
#     "sustainable_share" = "share_of_fish_stocks_within_biologically_sustainable_levels_fao_2020",
#     "overexploit_share" = "share_of_fish_stocks_that_are_overexploited"
#   )) %>%
#   mutate(
#     sustainable_share = sustainable_share / 100,
#     overexploit_share = overexploit_share / 100
#   )

## production
production_tidy <- production %>%
  filter(Entity %in% countries) %>%
  pivot_longer(cols = -c("Entity", "Code", "Year")) %>%
  separate(name, into = c("a", "b", "species", "c", "d", "e"), sep = " - ") %>%
  janitor::clean_names() %>%
  select(entity, code, year, species, value) %>%
  filter(!(is.na(value)))

## captured_vs_farmed
capt_farm_tidy <- captured_vs_farmed %>%
  janitor::clean_names() %>%
  filter(entity %in% countries) %>%
  rename(c(
    "aquaculture" = "aquaculture_production_metric_tons",
    "capture" = "capture_fisheries_production_metric_tons"
  )) %>%
  mutate(
    aqua_share = aquaculture / (aquaculture + capture),
    capt_share = capture / (aquaculture + capture)
  ) %>%
  select(-aquaculture, -capture) %>%
  pivot_longer(cols = c("aqua_share", "capt_share"))
```

## Visualization

``` r
# fish production by species and nation
prod_plot <- production_tidy %>%
  group_by(entity) %>%
  mutate(normalized_value = 100 * (value / max(value))) %>%
  ungroup() %>%
  ggplot(aes(year, normalized_value)) + 
  geom_stream(aes(fill = species), color = "black") + 
  # scales, themes, guides, etc
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(name = NULL) +
  scale_x_continuous(name = NULL) +
  guides(fill = guide_legend(nrow = 1, title = NULL)) +
  facet_wrap(~ factor(entity, levels = countries), ncol = 5) + 
  theme_minimal() + 
  theme(
    legend.position = "top",
    plot.title = element_textbox(hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = background_color, color = background_color),
    panel.background = element_rect(fill = background_color, color = background_color)
  )
```

``` r
# aquaculture vs captured by nation
prop_plot <- capt_farm_tidy %>%
  ggplot(aes(year, value)) + 
  geom_bar(aes(fill = name), stat = "identity", position = "fill") + 
  # scales, guides, themes, etc
  scale_fill_manual(values = c(aquaculture_color, capture_color)) +
  ## the production df has min x value of 1960 and max x value of 2018
  scale_x_continuous(limits = c(1960, 2018)) +
  facet_wrap(~ factor(entity, levels = countries), ncol = 5) +
  theme_void() + 
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    plot.background = element_rect(fill = background_color, color = background_color)
  )
```

``` r
# analysis

## create a sentence to describe recent production trends
top_nation_2013_strings <- production_tidy %>%
  filter(year == max(year)) %>%
  group_by(entity) %>%
  dplyr::summarise(total_value = sum(value)) %>%
  ungroup() %>% 
  mutate(
    billion_tonnes = round(total_value / 1000000, 2),
    string = paste0(entity, " produced ", billion_tonnes, " billion tonnes of fish")
  ) %>%
  pull(string) %>%
  glue_collapse(sep = ", ", last = ", and ")

analysis <- glue("<b>(A)</b> The following stream plots depict the distribution of species produced as part of the fishing industry over time in nations with the largest fishing industries. It is important to note that these streams are normalized within each nation. The difference in production levels <i>between</i> nations is very pronounced, particularly for China. In 2013, {top_nation_2013_strings}. These streams reveal that China and Vietnam have been rapidly increasing their fishing industries within the past 20 to 30 years. China's production by species has been fairly evenly distributed over time, with the majority of the production being represented by freshwater fish and molluscs. Vietnam has been surging in marine fish, freshwater fish, and crustaceans (i.e., crabs and lobster). Indonesia and India have been increasing their fishing industries at a more steady rate over the time period included in this dataset. Indonesia appears to primarily produce pelagic fish (i.e., herrings, sardines, tuna), while India primarily produces freshwater fish. Finally, the United States fishing industry has remained constant since the 1990s, producing primarily demersal fish (i.e., cod, haddock, bass, etc.).<br><br><b>(B) </b>These bars depict the ratio between <b style=color:'{aquaculture_color}'>aquaculture</b> and <b style=color:'{capture_color}'>captured</b> production of fish over time. China has the most pronounced proportion of their fishing industry being produced from aquaculture, while most other nations have begun to incorporate aquaculture more into their industries starting in the 2000s. Even then, none are as reliant on aquaculture fishing as China. Perhaps China has been able to dominate the fishing industry world over in part due to their utilization of aquaculture.")

timespan <- floor(max((production_tidy$year) - min(production_tidy$year)) / 10) * 10
```

``` r
# patchwork
prod_plot + 
  prop_plot + 
  plot_layout(ncol = 1, heights = c(10,1)) + 
  plot_annotation(
    title = glue("How has the <b>Fishing Industry</b> changed for the top nations over the past {timespan} years?"),
    subtitle = str_wrap(analysis, 100),
    caption = "Data Source: <b>Our World in Data</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday",
    tag_levels = "A",
    theme = theme(
      plot.title = element_textbox(size = 24, hjust = 0.5),
      plot.subtitle = element_textbox(
        face = "italic", size = 12, hjust = 0.5, halign = 0.5,
        width = unit(1, "npc"), margin = margin(b = 15)
      ),
      plot.caption = element_textbox(size = 12, hjust = 0.5),
      panel.background = element_rect(fill = background_color),
      plot.background = element_rect(fill = background_color),
      panel.border = element_blank()
    )
  )
```

![](Global-Fishing_files/figure-gfm/Global%20Fishing-1.png)<!-- -->
