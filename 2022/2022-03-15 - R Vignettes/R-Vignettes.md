R Vignettes
================
Nick Cruickshank
3/17/2022

# Introduction

This weeks [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-03-15)
features data on R Vignettes compiled by [Robert
Flight](https://github.com/rmflight/vignette_analysis).

# Analysis

``` r
# libraries
library(cowplot) # for draw_image()
```

    ## Warning: package 'cowplot' was built under R version 4.1.3

``` r
library(ggtext)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:cowplot':
    ## 
    ##     stamp

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(readr)
library(showtext)
```

    ## Loading required package: sysfonts

    ## Loading required package: showtextdb

``` r
library(sysfonts)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v dplyr   1.0.7
    ## v tibble  3.1.6     v stringr 1.4.0
    ## v tidyr   1.1.4     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x lubridate::date()        masks base::date()
    ## x dplyr::filter()          masks stats::filter()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x lubridate::stamp()       masks cowplot::stamp()
    ## x lubridate::union()       masks base::union()

``` r
# data
#bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')
```

    ## Rows: 109408 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (3): package, version, date
    ## dbl (2): rnw, rmd
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tidyverse_packages <- c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble",
                        "stringr", "forcats")
```

``` r
tv <- cran %>%
  filter(package %in% tidyverse_packages) %>%
  mutate(
    date = as.Date(date, "%Y-%m-%d"),
    month = floor_date(date, unit = "month"),
    year = year(date)
  ) %>%
  filter(!(is.na(date)))

tv_months <- tv %>%
  group_by(package, month) %>%
  dplyr::summarise(n = n())
```

    ## `summarise()` has grouped output by 'package'. You can override using the
    ## `.groups` argument.

## Visualize

``` r
# images
ggplot2_image <- "https://d33wubrfki0l68.cloudfront.net/2c6239d311be6d037c251c71c3902792f8c4ddd2/12f67/css/images/hex/ggplot2.png"
dplyr_image <- "https://d33wubrfki0l68.cloudfront.net/621a9c8c5d7b47c4b6d72e8f01f28d14310e8370/193fc/css/images/hex/dplyr.png"
tidyr_image <- "https://d33wubrfki0l68.cloudfront.net/476fa4025501dcec05be08248b32d390dd2337d5/574c6/css/images/hex/tidyr.png"
readr_image <- "https://d33wubrfki0l68.cloudfront.net/c1c91484f898fe9d7d90a570900f1d5cd703fe2e/d7df4/css/images/hex/readr.png"
purrr_image <- "https://d33wubrfki0l68.cloudfront.net/2d0701b616efa7435cd5a94e703baa595a4f9ed0/d41b9/css/images/hex/purrr.png"
tibble_image <- "https://d33wubrfki0l68.cloudfront.net/c477d7eb7fdf2c3d75637cfe19ff4a4d0a107bcf/017d0/css/images/hex/tibble.png"
stringr_image <- "https://d33wubrfki0l68.cloudfront.net/45fd04ad9cdb2159fea08d07dbc11e742d68e4e3/df327/css/images/hex/stringr.png"
forcats_image <- "https://d33wubrfki0l68.cloudfront.net/412a6f14518ab633a94221dda7e16cf22e43a763/91620/css/images/hex/forcats.png"

# colors
ggplot2_color <- "lightskyblue2"
stringr_color <- "forestgreen"
dplyr_color <- "violetred"
tidyr_color <- "springgreen"
readr_color <- "royalblue1"
purrr_color <- "seashell"
tibble_color <- "darkgoldenrod1"
forcats_color <- "lightsalmon"
line_color <- "gold"

# values
min_date <- min(tv_months$month)
max_date <- max(tv_months$month)
max_count <- max(tv_months$n)

# fonts
font_add_google("Kanit")
showtext_auto()
f1 <- "Kanit"
```

``` r
coords <- tibble(
  date = seq(min_date, max_date, "month"),
  month = month(date),
  sin = sin(month) / 2
)

tv_months2 <- tv_months %>%
  rename(c("date" = "month", "size" = "n"))

package_circa <- tv_months %>%
  group_by(package) %>%
  dplyr::summarise(circa = year(min(month)))

pacakge_legend <- tibble(
  package = tidyverse_packages,
) %>%
  left_join(package_circa, by = "package") %>%
  arrange(circa) %>%
  mutate(
    date = seq(min_date + years(1), max_date - years(1), length.out = 8),
    sin = 2.75,
    ylab = 3.5,
    size = 15
  )

mode_package <- tv_months %>%
  mutate(year = year(month)) %>%
  filter(year != 2009) %>%
  count(year, package) %>%
  group_by(year) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    date = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"),
    y = 1.5
  )
```

``` r
# plot
vignette_plot <- coords %>%
  left_join(tv_months2) %>%
  ggplot(aes(date, sin)) + 
  geom_line(color = "gold4", alpha = 0.8) + 
  geom_jitter(aes(color = package, size = size * 2)) + 
  
  # legend
  geom_point(
    data = pacakge_legend,
    aes(date, sin, size = size, color = package)
  ) +
  geom_text(
    data = pacakge_legend,
    aes(date, ylab, label = package, color = package)
  ) +
  geom_text(
    data = pacakge_legend,
    aes(date, y = 2, label = paste0("c. ", circa), color = package),
    fontface = "italic"
  ) +
  
  # winning packages
  annotate("text", x = ymd(20090801), y = 1.5, label = "Most\nVignettes",
           size = 3, fontface = "italic", color = line_color) + 
  annotate("segment", x = min_date, xend = max_date, y = 1.5, yend = 1.5, color = line_color) + 
  geom_line(
    data = mode_package,
    aes(date, y, color = package, group = package),
    size = 2.5
  ) +
  geom_text(
    data = filter(mode_package, year %% 2 == 0),
    aes(x = date, y = 1, label = year),
    color = line_color
  ) +
  
  # scales, themes, etc
  labs(
    title = "R Vignettes",
    subtitle = "Timeline of the Tidyverse: Number (Size) of Package Updates Monthly",
    caption = "<br><br>Data Source: <b>Robert Flight</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  scale_y_continuous(limits = c(-0.5,4)) + 
  scale_size_identity(guide = "none") +
  scale_color_manual(values = c(
    "dplyr" = dplyr_color,
    "ggplot2" = ggplot2_color,
    "readr" = readr_color,
    "tibble" = tibble_color,
    "forcats" = forcats_color,
    "purrr" = purrr_color,
    "stringr" = stringr_color,
    "tidyr" = tidyr_color
  ), guide = "none") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "midnightblue"),
    panel.background = element_rect(color = "midnightblue", fill = "midnightblue"),
    plot.title = element_text(hjust = 0.5, size = 24, color = line_color),
    plot.subtitle = element_text(hjust = 0.5, size = 16, color = line_color),
    plot.caption = element_textbox(hjust = 0.5, size = 10, color = line_color)
  )
```

    ## Joining, by = "date"

``` r
library(ggimage)
```

    ## 
    ## Attaching package: 'ggimage'

    ## The following object is masked from 'package:cowplot':
    ## 
    ##     theme_nothing

``` r
images <- tibble(
  package = tidyverse_packages,
  image = c(
    ggplot2_image, dplyr_image, tidyr_image, readr_image,
    purrr_image, tibble_image, stringr_image, forcats_image
  )
) %>%
  left_join(pacakge_legend) %>%
  select(date, sin, package, image, size )
```

    ## Joining, by = "package"

``` r
vignette_plot + 
  geom_image(
    data = images,
    aes(image = image),
    size = 0.05, asp = 3.2
  )
```

    ## Warning: Removed 57 rows containing missing values (geom_point).

![](R-Vignettes_files/figure-gfm/R%20Vignette%20Timeline-1.png)<!-- -->
