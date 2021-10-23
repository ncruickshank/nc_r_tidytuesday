Giant Pumpkins
================
Nick Cruickshank
10/23/2021

-   [Introduction](#introduction)
-   [Data Analyis](#data-analyis)
    -   [Libraries](#libraries)
    -   [Data](#data)
    -   [Tidy](#tidy)
    -   [Visualization](#visualization)
        -   [Questions](#questions)

![](https://images.unsplash.com/photo-1539395369182-84bb72ecf742?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1331&q=80)

# Introduction

Data for this weeks [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-10-19)
comes from
[BigPunmpkins.com](http://www.bigpumpkins.com/ViewArticle.asp?id=132).

> The Great Pumpkin Commonwealth’s (GPC) mission cultivates the hobby of
> growing giant pumpkins throughout the world by establishing standards
> and regulations that ensure quality of fruit, fairness of competition,
> recognition of achievement, fellowship and education for all
> participating growers and weigh-off sites.

> Types: F = “Field Pumpkin”, P = “Giant Pumpkin”, S = “Giant Squash”, W
> = “Giant Watermelon”, L = “Long Gourd” (length in inches, not weight
> in pounds), T = Tomato

More information on how to utilize this dataset (and understand the
community) can be found in the [Great Pumpkin Commonwealth
Handbook](https://gpc1.org/wp-content/uploads/2021/03/GPC-Rules-and-Handbook-2021.pdf).

# Data Analyis

## Libraries

``` r
# libraries
library(readr)
library(tidyverse)
```

## Data

``` r
# data
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')
head(pumpkins)
```

    ## # A tibble: 6 x 14
    ##   id     place weight_lbs grower_name    city    state_prov country gpc_site    
    ##   <chr>  <chr> <chr>      <chr>          <chr>   <chr>      <chr>   <chr>       
    ## 1 2013-F 1     154.50     Ellenbecker, ~ Gleason Wisconsin  United~ Nekoosa Gia~
    ## 2 2013-F 2     146.50     Razo, Steve    New Mi~ Ohio       United~ Ohio Valley~
    ## 3 2013-F 3     145.00     Ellenbecker, ~ Glenson Wisconsin  United~ Mishicot Pu~
    ## 4 2013-F 4     140.80     Martin, Marga~ Combin~ Wisconsin  United~ Cedarburg W~
    ## 5 2013-F 5     139.00     Barlow, John   <NA>    Wisconsin  United~ Stillwater ~
    ## 6 2013-F 5     139.00     Werner, Quinn  Saegar~ Pennsylva~ United~ Ohio Valley~
    ## # ... with 6 more variables: seed_mother <chr>, pollinator_father <chr>,
    ## #   ott <chr>, est_weight <chr>, pct_chart <chr>, variety <chr>

## Tidy

## Visualization

### Questions

Which states have the biggest pumpkins? By type? Over time?

Which competitions have the biggest pumpkins?
