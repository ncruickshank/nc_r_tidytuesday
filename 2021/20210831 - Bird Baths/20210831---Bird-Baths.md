20210831 - Bird Baths
================
Nick Cruickshank
9/2/2021

-   [Introduction](#introduction)
-   [Exploratory Data Analysis](#exploratory-data-analysis)
    -   [What are the most populous
        birds?](#what-are-the-most-populous-birds)
    -   [Which birds had the biggest raw change from Urban to
        Rural?](#which-birds-had-the-biggest-raw-change-from-urban-to-rural)
    -   [Which birds had the biggest raw change from 2014 to
        2015?](#which-birds-had-the-biggest-raw-change-from-2014-to-2015)
    -   [Ideas from Cleary et al 2016](#ideas-from-cleary-et-al-2016)
        -   [Do assemblages at urban and rural bird baths differ across
            bioregions?](#do-assemblages-at-urban-and-rural-bird-baths-differ-across-bioregions)
        -   [Do assemblages at urban and rural bird baths differ within
            bioregions?](#do-assemblages-at-urban-and-rural-bird-baths-differ-within-bioregions)
        -   [Is species richness different at bird baths in rural areas
            compared to urban
            areas?](#is-species-richness-different-at-bird-baths-in-rural-areas-compared-to-urban-areas)

# Introduction

# Exploratory Data Analysis

``` r
# libraries
library(forcats)
library(readr)
library(tidyverse)
```

``` r
# data
bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

## view dataset
bird_baths %>%
  head()
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["survey_year"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["urban_rural"],"name":[2],"type":["chr"],"align":["left"]},{"label":["bioregions"],"name":[3],"type":["chr"],"align":["left"]},{"label":["bird_type"],"name":[4],"type":["chr"],"align":["left"]},{"label":["bird_count"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"2014","2":"Urban","3":"South Eastern Queensland","4":"Bassian Thrush","5":"0"},{"1":"2014","2":"Urban","3":"South Eastern Queensland","4":"Chestnut-breasted Mannikin","5":"0"},{"1":"2014","2":"Urban","3":"South Eastern Queensland","4":"Wild Duck","5":"0"},{"1":"2014","2":"Urban","3":"South Eastern Queensland","4":"Willie Wagtail","5":"0"},{"1":"2014","2":"Urban","3":"South Eastern Queensland","4":"Regent Bowerbird","5":"0"},{"1":"2014","2":"Urban","3":"South Eastern Queensland","4":"Rufous Fantail","5":"0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

## What are the most populous birds?

``` r
bird_counts <- bird_baths %>%
  group_by(bird_type) %>%
  summarise(total = sum(bird_count)) %>%
  arrange(desc(total)) 

# bird_counts %>%
#   ggplot(aes(fct_reorder(bird_type, total), total)) + 
#   geom_hline(yintercept = 300) + 
#   geom_point() + 
#   scale_x_discrete(name = NULL, labels = NULL)

most_populous_birds <- bird_counts %>%
  filter(total > 300) %>%
  pull(bird_type)
```

## Which birds had the biggest raw change from Urban to Rural?

Looks like there’s a fairly decent breaking point around 300 birds
total.

## Which birds had the biggest raw change from 2014 to 2015?

For either urban and / or rural.

## Ideas from Cleary et al 2016

### Do assemblages at urban and rural bird baths differ across bioregions?

Use
[PERMANOVA](https://en.wikipedia.org/wiki/Permutational_analysis_of_variance),
which can be found in the `vegan` and `InPerm` packages.

### Do assemblages at urban and rural bird baths differ within bioregions?

[Multidimensional Scaling (nMDS) ordination
techniques](http://strata.uga.edu/8370/lecturenotes/multidimensionalScaling.html)
found in library `vegan` and PERMANOVA

### Is species richness different at bird baths in rural areas compared to urban areas?
