20211221 - Starbucks
================
Nick Cruickshank
12/22/2021

![](https://stories.starbucks.com/uploads/2020/08/SBX20200824-FallPromo-Feature.jpg)

# Introduction

This week’s [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-12-21)
comes from the Official Starbucks National dataset from the companies
Nutrition Information pdf.

Some inspirational infographics can be found on the
[Behance](https://www.behance.net/gallery/58743971/Starbucks-Menu-Infographic-Design)
website.

# Analysis

``` r
# libraries
library(forcats)
#devtools::install_github("cardiomoon/ggiraphExtra")
library(ggiraphExtra)
library(ggtext)
library(glue)
library(readr)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(shadowtext)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x scales::col_factor() masks readr::col_factor()
    ## x purrr::discard()     masks scales::discard()
    ## x dplyr::filter()      masks stats::filter()
    ## x dplyr::lag()         masks stats::lag()

``` r
# data
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')
```

    ## Rows: 1147 Columns: 15

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (4): product_name, size, trans_fat_g, fiber_g
    ## dbl (11): milk, whip, serv_size_m_l, calories, total_fat_g, saturated_fat_g,...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
starbucks
```

    ## # A tibble: 1,147 x 15
    ##    product_name             size   milk  whip serv_size_m_l calories total_fat_g
    ##    <chr>                    <chr> <dbl> <dbl>         <dbl>    <dbl>       <dbl>
    ##  1 brewed coffee - dark ro~ short     0     0           236        3         0.1
    ##  2 brewed coffee - dark ro~ tall      0     0           354        4         0.1
    ##  3 brewed coffee - dark ro~ gran~     0     0           473        5         0.1
    ##  4 brewed coffee - dark ro~ venti     0     0           591        5         0.1
    ##  5 brewed coffee - decaf p~ short     0     0           236        3         0.1
    ##  6 brewed coffee - decaf p~ tall      0     0           354        4         0.1
    ##  7 brewed coffee - decaf p~ gran~     0     0           473        5         0.1
    ##  8 brewed coffee - decaf p~ venti     0     0           591        5         0.1
    ##  9 brewed coffee - medium ~ short     0     0           236        3         0.1
    ## 10 brewed coffee - medium ~ tall      0     0           354        4         0.1
    ## # ... with 1,137 more rows, and 8 more variables: saturated_fat_g <dbl>,
    ## #   trans_fat_g <chr>, cholesterol_mg <dbl>, sodium_mg <dbl>,
    ## #   total_carbs_g <dbl>, fiber_g <chr>, sugar_g <dbl>, caffeine_mg <dbl>

It looks like most drinks on the menu are duplicated for each
permutation of milk type and presence of whip cream or not. Since I want
to plot each drink to compare them to one another, it would be most
meaningful to pick the same milk+whip cream combination for each drink.
No milk (`milk == 0`) is relatively underrepresented in the data set,
while nonfat milk (`milk == 1`) is most represented. Therefore that will
be used.

``` r
# values

## colors
starbucks_green <- "#0b421a" # use this as background
starbucks_white <- "#fffcfc" # use this when drink uses whip cream
starbucks_yellow <- "#eac784"
starbucks_black <- "#362415"
starbucks_brown <- "#604c4c"
```

``` r
# tidy

venti_coffee <- starbucks %>%
  mutate(product_name = str_to_lower(product_name)) %>%
  filter(
    size == "venti", # size of interest
    # I only care about hot coffee for this exercise
    !str_detect(product_name, "(ice|iced|cold|tea|lemonade|hot chocolate|chai|refreshers)"),
    caffeine_mg >= 10,
    # standardize milk and whip
    milk == 1,
    whip == 0
  )  %>%
  select(product_name, calories, caffeine_mg, total_fat_g, cholesterol_mg, sodium_mg, total_carbs_g, fiber_g, sugar_g)

venti_radar <- venti_coffee %>%
  arrange(desc(calories)) %>%
  head(20) %>%
  mutate(
    order = as.character(ifelse(row_number() < 10, paste0("0", row_number()), row_number())),
    color = starbucks_black,
    group = paste0(glue("<b>"), str_to_title(product_name), "</b><br><br><i>", calories, " Calories<br>", caffeine_mg, "mg Caffeine</i>"),
    fiber_g = as.double(fiber_g)
  ) %>%
  rename(c(
    "Fat" = "total_fat_g",
    "Sugar" = "sugar_g",
    "Cholesterol" = "cholesterol_mg",
    "Sodium" = "sodium_mg",
    "Carbs" = "total_carbs_g",
    "Fiber" = "fiber_g"
  )) %>%
  select(group, color, Sugar, Fat, Cholesterol, Fiber, Carbs, Sodium) 
```

## Visualization

Spider plots of the top drinks for the holiday season (or just hot
drinks).

``` r
venti_radar %>%
  ggRadar(
    rescale = TRUE,
    mapping = aes(colour = color, facet = group, group = group),
    fill = starbucks_black,
    alpha = 0.7
  ) + 
  scale_color_identity() +
  scale_y_discrete(breaks = NULL, expand = c(-1,1)) +
  scale_fill_manual(values = rep(starbucks_black, nrow(venti_radar))) + 
  #facet_wrap(~ group) + 
  labs(
    title = "Nutrition information for <b>STARBUCKS</b> hot coffee beverages<br>",
    caption = "Data Source: <b>Starbucks</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    strip.text = element_textbox(color = starbucks_yellow, size = 13, hjust = 0.5, halign = 0.5, width = unit(1, "npc"), margin = margin(b = 10, t = 10)),
    strip.background = element_blank(),
    plot.background = element_rect(fill = starbucks_green),
    panel.spacing = unit(3, "lines"),
    panel.grid.major.y = element_line(color = starbucks_white, size = 5),
    panel.grid.major.x = element_line(color = starbucks_brown, size = 35),
    axis.text.x = element_shadowtext(color = starbucks_white, hjust = 0.5, size = 12),
    plot.title = element_textbox(color = starbucks_yellow, size = 36, hjust = 0.5),
    plot.caption = element_textbox(color = starbucks_yellow, size = 12, hjust = 0.5)
  )
```

![](Starbucks_files/figure-gfm/Starbucks%20Nutrition%20Radars-1.png)<!-- -->
