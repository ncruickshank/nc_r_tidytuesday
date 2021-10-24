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
        -   [Explroatory Data Analysis](#explroatory-data-analysis)
        -   [States Subplot](#states-subplot)
        -   [Details on Top 10 States](#details-on-top-10-states)

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

Image kudos to [Patrick Donnely](https://unsplash.com/@patrick).

# Data Analyis

## Libraries

``` r
# libraries
library(extrafont)
library(forcats)
library(ggbump)
library(ggridges)
library(ggtext)
library(glue)
library(mapdata)
library(maps)
library(patchwork)
library(readr)
library(sf)
library(tidyverse)
```

## Data

``` r
# data
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')
state_abr <- read_csv("state_abr.csv")
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

Types: F = “Field Pumpkin”, P = “Giant Pumpkin”, S = “Giant Squash”, W =
“Giant Watermelon”, L = “Long Gourd” (length in inches, not weight in
pounds), T = Tomato

``` r
# pre tidying the dataframe
pumpkins2 <- pumpkins %>%
  filter(country == "United States") %>%
  separate(id, into = c("year", "code"), sep = "-") %>%
  mutate(
    type = case_when(
      str_detect(code, "F") ~ "Field Pumpkin",
      str_detect(code, "P") ~ "Giant Pumpkin",
      str_detect(code, "S") ~ "Giant Squash",
      str_detect(code, "W") ~ "Giant Watermelon",
      str_detect(code, "L") ~ "Long Gourd",
      str_detect(code, "T") ~ "Tomato"
    )
  ) %>%
  mutate(
    year = as.numeric(year),
    weight_lbs = parse_number(weight_lbs)
  )
```

## Visualization

### Explroatory Data Analysis

Field pumpkins and giant pumpkins have a **very** different weight and
distribution.

``` r
# what type of gourd has the greatest weight?
weight_orders <- pumpkins2 %>%
  group_by(type) %>%
  dplyr::summarise(mean_weight = mean(weight_lbs), n = n()) %>%
  ungroup() %>%
  mutate(rank = row_number()) %>%
  arrange(desc(mean_weight))

pumpkins2 %>%
  left_join(weight_orders, by = "type") %>%
  ggplot(aes(weight_lbs, fct_reorder(type, rank))) + 
  geom_density_ridges(rel_min_height = 0.01)
```

![](Giant-Pumpkins_files/figure-gfm/gourd%20type%20weight%20distr-1.png)<!-- -->

Have the number of pumpkin submissions been going up or down? Is it
reasonable to plot 2021?

``` r
pumpkins2 %>%
  count(year) %>%
  ggplot(aes(as.character(year), n)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), vjust = 1.2) + 
  geom_text(aes(y = 200, label = year), fontface = "bold", size = 5, color = "white") + 
  theme_void()
```

![](Giant-Pumpkins_files/figure-gfm/year%20counts-1.png)<!-- -->

Looks like 2021 is good to plot!

### States Subplot

``` r
# states tidy
pump_states <- pumpkins2 %>%
  filter(year == 2021, type == "Giant Pumpkin")  %>%
  group_by(state_prov) %>%
  dplyr::summarise(max_weight = max(weight_lbs)) %>%
  ungroup() %>%
  mutate(region = str_to_lower(state_prov))

# generate lat and long for each state
usa <- map_data('usa')
state <- map_data('state') %>%
  left_join(pump_states, by = "region")

# determine which states had the heaviest max weight pumpkin
list_top_states <- pump_states %>%
  arrange(desc(max_weight)) %>%
  mutate(
    rank = row_number(),
    label = paste0("#", rank, " ", str_to_title(region)),
    odd_even = if_else(rank %% 2 == TRUE, "Odd", "Even")
  ) %>%
  head(10)

# evenly distribute ends for the sigmoid lines
ends <- tibble(
  label = pull(list_top_states, label),
  xend = rep.int(-60, 10),
  yend = rev(seq(31, 105, 7.5))
)

# derive starting points as the middle of each of the top 10 states
state_sigmoids <- state %>%
  group_by(region) %>%
  dplyr::summarise(
    x = mean(long),
    y = mean(lat)
  ) %>%
  inner_join(list_top_states, by = "region") %>%
  left_join(ends)
  
# plot the state max weights, sigmoid lines to top 10, and overall plot texts
map_plot <- ggplot() + 
  # plot setup
  ## title
  annotate("text", x = -125, y = 105, label = "Giant Pumpkins in 2021", size = 10, fontface = "bold", color = "darkorange4", hjust = 0) +
  annotate("segment", x = -125, xend = -75, y = 102, yend = 102, color = "darkorange4", size = 2) + 
  ## subtitle
  annotate("text", x = -125, y = 95, label = str_wrap("Top 10 US States with heaviest pumpkins", 30),
           size = 7, fontface = "bold", color = "darkorange3", hjust = 0, lineheight = 0.8) + 
  annotate("text", x = -125, y = 83, label = str_wrap("Distribution of pumpkin weights submitted in that state", 35),
           size = 6, fontface = "bold", color = "darkorange2", hjust = 0, lineheight = 0.8) + 
  annotate("text", x = -125, y = 75, label = str_wrap("Giant Pumpkin Commonwealth Sites where the Top 3 pumpkins can be found", 28),
           size = 6, fontface = "bold", color = "darkorange2", hjust = 0, lineheight = 0.8) + 
  annotate("text", x = -125, y = 65, label = str_wrap("Rearrangement of those pumpkins by weight, regardless of state", 25),
           size = 6, fontface = "bold", color = "darkorange2", hjust = 0, lineheight = 0.8) + 
  ## state dividers
  annotate("segment", x = -60, xend = -50, y = rev(seq(34.25, 94.25, 7.5)), yend = rev(seq(34.25, 94.25, 7.5)), linetype = "dashed") +
  # draw us map
  geom_polygon(data = state, aes(long, lat, fill = max_weight, group = group), color = "darkorange4") + 
  # connect the top 10 states to a ordered list via sigmoid lines
  geom_sigmoid(data = state_sigmoids, aes(x = x, y = y, xend = xend, yend = yend, group = region, color = odd_even), size = 1.05) +
  geom_point(data = state_sigmoids, aes(x, y, color = odd_even)) + 
  geom_text(data = state_sigmoids, aes(x = xend + 1, y = yend, label = label, color = odd_even), hjust = 0, fontface = "bold") + 
  # scales, themes, etc.
  scale_fill_gradient(low = "palegoldenrod", high = "darkorange2", na.value = "grey80") + 
  scale_color_manual(values = c("darkorange4", "darkgoldenrod3"), guide = "none") + 
  scale_x_continuous(limits = c(-125, -45)) +
  scale_y_continuous(limits = c(24, 110)) + 
  coord_fixed(expand = FALSE) + 
  theme_void() + 
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "antiquewhite", color = "antiquewhite")
  )
```

### Details on Top 10 States

``` r
# details on top 10 states

## define yends for second series of sigmoids
overall_yends <- tibble(
  overall_rank = seq(1, 30, 1),
  yend = seq(0.5, 10.36, 0.34)
)

## set up complete list of coordinates, values, and annotationes
pump_slice <- pumpkins2 %>%
  filter(
    state_prov %in% list_top_states$state_prov,
    year == 2021
  ) %>%
  group_by(state_prov) %>%
  # slice for top three heaviest punpkins by state, and rank them
  slice_max(order_by = weight_lbs, n = 3) %>%
  mutate(state_rank = row_number()) %>%
  ungroup() %>%
  left_join(state_abr, by = "state_prov") %>%
  arrange(desc(weight_lbs)) %>%
  mutate(overall_rank = row_number()) %>%
  select(state_prov, state_rank, overall_rank, grower_name, gpc_site, weight_lbs, abr) %>%
  # create labels
  left_join(select(state_sigmoids, state_prov, rank, max_weight), by = "state_prov") %>%
  mutate(
    odd_even = if_else(rank %% 2 == TRUE, "Odd", "Even"),
    xlab = max(max_weight) + 150,
    y = case_when(
      as.double(state_rank) == 1 ~ as.double(rank) - 0.3,
      as.double(state_rank) == 2 ~ as.double(rank),
      as.double(state_rank) == 3 ~ as.double(rank) + 0.3
    ),
    label1 = paste0("#", state_rank, " ", gpc_site),
    x = max(state_sigmoids$max_weight) + 5500,
    xend = x + 5000,
    label2 = paste0("#", overall_rank),
    x_pre_dot = xend + 150,
    x_dot = x_pre_dot + 700,
    x_post_dot = x_dot + 400,
    label3 = paste0(weight_lbs, " lbs")
  ) %>%
  left_join(overall_yends, by = "overall_rank")

## annotation coordinates for titles in the detail plot
annotations <- tibble(
  x = c(300, 2900, 13100),
  xend = c(2200, 8400, 14800),
  y = c(-0.2, -0.2, -0.2),
  yend = c(-0.2, -0.2, -0.2),
  xlab = c(1250, 5650, 13950),
  ylab = c(0,0,0),
  lab = c("Distributions", "GPC Sites of Top 3 Pumpkins", "Overall")
)

## plot everything
detail_plot <- pumpkins2 %>%
  filter(
    state_prov %in% list_top_states$state_prov,
    year == 2021
  ) %>%
  left_join(select(pump_slice, state_prov, rank), by = "state_prov") %>%
  ggplot(aes(weight_lbs, -rank)) + 
  # annotations outlining the plot
  geom_segment(data = annotations, aes(x = x, y = y, xend = xend, yend = yend), color = "darkorange3") + 
  geom_text(data = annotations, aes(x = xlab, y = ylab, label = lab), color = "darkorange3", fontface = "bold") + 
  annotate("segment", x = 0, xend = 9000, y = seq(-9.5, -1.5, 1), yend = seq(-9.5, -1.5, 1), linetype = "dashed") + 
  # by state weight distributions
  geom_boxplot(aes(group = rank), width = 0.25, fill = "darkorange2", color = "darkorange4") + 
  # by state top 3 pumpkins
  geom_text(data = pump_slice, aes(x = xlab, y = -y, label = label1, color = odd_even), hjust = 0, size = 3) + 
  # rearrange for top 30 pumpkins, regardless of state
  ## sigmoids to connect the points
  geom_sigmoid(data = pump_slice, aes(x = x, y = -y, xend = xend, yend = -yend, color = odd_even, group = overall_rank)) + 
  geom_text(data = pump_slice, aes(x = x_pre_dot, y = -yend, label = label2, color = odd_even), size = 3, hjust = 0) +
  geom_point(data = pump_slice, aes(x = x_dot, y = -yend, size = weight_lbs, color = odd_even)) + 
  geom_text(data = pump_slice, aes(x = x_dot, y = -yend, label = abr), size = 3, color = "white") +
  geom_text(data = pump_slice, aes(x = x_post_dot, y = -yend, label = label3, color = odd_even), hjust = 0, size = 3) + 
  # scales, themes, etc.
  scale_color_manual(values = c("darkorange4", "darkgoldenrod3"), guide = "none") + 
  scale_size_continuous(range = c(7,12), guide = "none") +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "antiquewhite", color = "antiquewhite")
  )
```

``` r
# patchwork vis
map_plot + detail_plot + 
  plot_layout(widths = c(1,1.2)) + 
  plot_annotation(
    theme = theme(
      panel.background = element_rect(fill = "antiquewhite"),
      plot.background = element_rect(fill = "antiquewhite")
    )
  )
```

![](Giant-Pumpkins_files/figure-gfm/Biggest%20Pumpkins%20GPC%202021-1.png)<!-- -->
