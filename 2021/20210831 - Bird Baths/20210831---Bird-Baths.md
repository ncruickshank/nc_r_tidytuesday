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

most_populous_birds
```

    ##  [1] "Noisy Miner"       "Australian Magpie" "Rainbow Lorikeet" 
    ##  [4] "Red Wattlebird"    "Superb Fairy-wren" "Magpie-lark"      
    ##  [7] "Pied Currawong"    "Crimson Rosella"   "Eastern Spinebill"
    ## [10] "Spotted Dove"

## Which birds had the biggest raw change from Urban to Rural?

Looks like there’s a fairly decent breaking point around 300 birds
total.

## Which birds had the biggest raw change from 2014 to 2015?

For either urban and / or rural.

Overall change (ignoring urban / rural and bioregion) shows the
following reductions from 2014

``` r
bird_baths %>%
  filter(!(is.na(survey_year))) %>%
  group_by(bird_type, survey_year) %>%
  dplyr::summarise(
    total = sum(bird_count)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = "survey_year", values_from = "total", names_prefix = "survey_") %>%
  mutate(delta = survey_2015 - survey_2014) %>%
  arrange(delta) %>%
  head(10)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["bird_type"],"name":[1],"type":["chr"],"align":["left"]},{"label":["survey_2014"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["survey_2015"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["delta"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Rainbow Lorikeet","2":"167","3":"68","4":"-99"},{"1":"Pied Currawong","2":"125","3":"32","4":"-93"},{"1":"Satin Bowerbird","2":"92","3":"39","4":"-53"},{"1":"Lewin's Honeyeater","2":"86","3":"47","4":"-39"},{"1":"Noisy Miner","2":"163","3":"129","4":"-34"},{"1":"Eastern Yellow Robin","2":"61","3":"28","4":"-33"},{"1":"Eastern Spinebill","2":"91","3":"62","4":"-29"},{"1":"Spotted Dove","2":"89","3":"62","4":"-27"},{"1":"Little Wattlebird","2":"70","3":"46","4":"-24"},{"1":"Sulphur-crested Cockatoo","2":"51","3":"31","4":"-20"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

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
