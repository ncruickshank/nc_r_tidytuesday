Emmy Awards
================
Nick Cruickshank
9/25/2021

-   [Introduction](#introduction)
-   [Data Analysis](#data-analysis)

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

# Data Analysis

``` r
# libraries
library(readr)
library(tidyverse)
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
