Baby Names
================
Nick Cruickshank
3/21/2022

# Analysis

``` r
# libraries
library(ggtext)
library(readr)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v dplyr   1.0.7
    ## v tibble  3.1.6     v stringr 1.4.0
    ## v tidyr   1.1.4     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sysfonts)
library(showtext)
```

    ## Loading required package: showtextdb

``` r
# data
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')
```

    ## Rows: 1924665 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): sex, name
    ## dbl (3): year, n, prop
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Visualization

Change in length of name length (number of characters) over time.

Lessons Learned: Max char_count by year and sex winds up being a
meaningless (no change) story.

``` r
# values

## fonts
font_add_google("Playfair Display")
showtext_auto()
f1 <- "Playfair Display"

## colors
baby_blue <- "#89cff0"
baby_red <- "#fcd2c8"
background_color <- "#81898E"
text_color <- "#F2FCC8"
```

``` r
# default df for plotting
bn <- babynames %>%
  mutate(char_count = str_length(name)) %>%
  group_by(year, sex, char_count) %>%
  dplyr::summarise(n2 = sum(n)) %>%
  group_by(year, sex) %>%
  dplyr::summarise(
    avg_name_length = weighted.mean(x = char_count, w = n2)
  ) %>%
  ungroup() 
```

    ## `summarise()` has grouped output by 'year', 'sex'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

``` r
# identify the most common baby names at peaks and valleys

## VALLEYS - years with lowest name length average
valleys <- bn %>%
  filter(year >= 1920) %>%
  group_by(sex) %>%
  slice_min(avg_name_length, n = 1)

valley_names <- babynames %>%
  inner_join(valleys) %>%
  mutate(
    char_count = str_length(name),
    lower_limit = floor(avg_name_length),
    upper_limit = ceiling(avg_name_length)
  ) %>%
  filter(
    #char_count >= lower_limit & char_count <= upper_limit
    char_count == lower_limit
  ) %>%
  group_by(sex) %>%
  slice_max(order_by = n, n = 3) %>%
  ungroup() %>%
  select(year, sex, name, n)
```

    ## Joining, by = c("year", "sex")

``` r
valley_girls <- valley_names %>%
  filter(sex == "F") %>%
  pull(name) %>%
  glue::glue_collapse(sep = "\n")

valley_boys <- valley_names %>%
  filter(sex == "M") %>%
  pull(name) %>%
  glue::glue_collapse(sep = "\n")

## PEAKS - years with highest name length average
peaks <- bn %>%
  filter(year >= 1920) %>%
  group_by(sex) %>%
  slice_max(avg_name_length, n = 1)

peaks_names <- babynames %>%
  inner_join(peaks) %>%
  mutate(
    char_count = str_length(name),
    lower_limit = floor(avg_name_length),
    upper_limit = ceiling(avg_name_length),
    limit = ifelse(sex == "F", ceiling(avg_name_length), floor(avg_name_length))
  ) %>%
  filter(
    #char_count >= lower_limit & char_count <= upper_limit
    char_count == limit
  ) %>%
  group_by(sex) %>%
  slice_max(order_by = n, n = 3) %>%
  ungroup() %>%
  select(year, sex, name, n)
```

    ## Joining, by = c("year", "sex")

``` r
peak_girls <- peaks_names %>%
  filter(sex == "F") %>%
  pull(name) %>%
  glue::glue_collapse(sep = "\n")

peak_boys <- peaks_names %>%
  filter(sex == "M") %>%
  pull(name) %>%
  glue::glue_collapse(sep = "\n")
```

``` r
# plot
bn %>%
  ggplot(aes(year, avg_name_length)) + 
  geom_line(aes(color = sex), size = 1.1) + 
  
  # annotations
  ## general plot annotations
  annotate("text", x = 1885, y = 6.35,
           label = "Yearly Weighted Average",
           color = text_color, fontface = "italic", size = 7, hjust = 0) +
  annotate("text", x = 1885, y = 6.25,
           label = "Baby Name Length",
           color = text_color, fontface = "bold", size = 14, hjust = 0) +
  
  ## girls details
  annotate("text", x = max(bn$year) + 2, y = filter(bn, sex == "F", year == max(year))$avg_name_length,
           label = "Girl", 
           color = baby_red, fontface = "italic", size = 7, hjust = 0) +
  annotate("text", x = 1959, y = 5.95, label = valley_girls,
           color = baby_red, size = 4, hjust = 0.5) +
  annotate("text", x = 1989, y = 6.5, label = peak_girls,
           color = baby_red, size = 4, hjust = 0.5) +
  
  ## boys details
  annotate("text", x = max(bn$year) + 2, y = filter(bn, sex == "M", year == max(year))$avg_name_length,
           label = "Boy", 
           color = baby_blue, fontface = "italic", size = 7, hjust = 0) +
  annotate("text", x = 1959, y = 5.5, label = valley_boys,
           color = baby_blue, size = 4, hjust = 0.5) +
  annotate("text", x = 1989, y = 5.9, label = peak_boys,
           color = baby_blue, size = 4, hjust = 0.5) +
  
  # scales, themes, etc
  scale_x_continuous(name = "Year", expand = c(0,0), limits = c(min(bn$year), max(bn$year) + 12)) +
  scale_y_continuous(name = "Number of Characters", expand = c(0,0), limits = c(min(bn$avg_name_length), max(bn$avg_name_length) + 0.2)) +
  scale_color_manual(values = c(
    "F" = baby_red,
    "M" = baby_blue
  ), guide = "none") +
  labs(caption = "<br><br>Data Source: <b>babyname R Package</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday") +
  theme_minimal(base_family = f1) + 
  theme(
    plot.background = element_rect(fill = background_color, color = background_color),
    panel.background = element_rect(fill = background_color, color = background_color),
    
    panel.grid = element_line(color = "grey40", linetype = "longdash"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    axis.text = element_text(color = text_color, size = 12),
    axis.title = element_text(color = text_color, size = 14),
    
    plot.caption = element_textbox(color = text_color, hjust = 0.5)
  )
```

![](Baby-Names_files/figure-gfm/Baby%20Name%20Length%20Over%20Time-1.png)<!-- -->
