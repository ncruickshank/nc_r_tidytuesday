Bees
================
Nick Cruickshank
1/15/2022

# Introduction

This week’s [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-11)
features data concerning bee colonies, kudos to [G.
Karamanis](https://github.com/rfordatascience/tidytuesday/issues/404).

> This report provides information on honey bee colonies in terms of
> number of colonies, maximum, lost, percent lost, added, renovated, and
> percent renovated, as well as colonies lost with Colony Collapse
> Disorder symptoms with both over and less than five colonies. The
> report also identifies colony health stressors with five or more
> colonies. The data for operations with honey bee colonies are
> collected from a stratified sample of operations that responded as
> having honey bees on the Bee and Honey Inquiry and from the NASS list
> frame. For operations with five or more colonies, data was collected
> on a quarterly basis; operations with less than five colonies were
> collected with an annual survey.

# Analysis

``` r
# libraries
library(broom) # for fortify
library(here)
```

    ## here() starts at C:/Users/nccru/OneDrive/Documents/GitHub/nc_r_tidytuesday

``` r
library(ggbump) # for geom_sigmoid()
library(ggtext)
library(glue)
library(geojsonio) # for geosjaon_read()
```

    ## Registered S3 method overwritten by 'geojsonsf':
    ##   method        from   
    ##   print.geojson geojson

    ## 
    ## Attaching package: 'geojsonio'

    ## The following object is masked from 'package:base':
    ## 
    ##     pretty

``` r
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
# data
here <- here()

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
```

    ## Rows: 1222 Columns: 10

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): months, state
    ## dbl (8): year, colony_n, colony_max, colony_lost, colony_lost_pct, colony_ad...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')
```

    ## Rows: 7332 Columns: 5

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (3): months, state, stressor
    ## dbl (2): year, stress_pct

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
spdf <- geojson_read(glue("{here}/2022/2022-01-11 - Bees/us_states_hexgrid.geojson"), what = "sp")

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
```

``` r
head(colony)
```

    ## # A tibble: 6 x 10
    ##    year months        state      colony_n colony_max colony_lost colony_lost_pct
    ##   <dbl> <chr>         <chr>         <dbl>      <dbl>       <dbl>           <dbl>
    ## 1  2015 January-March Alabama        7000       7000        1800              26
    ## 2  2015 January-March Arizona       35000      35000        4600              13
    ## 3  2015 January-March Arkansas      13000      14000        1500              11
    ## 4  2015 January-March California  1440000    1690000      255000              15
    ## 5  2015 January-March Colorado       3500      12500        1500              12
    ## 6  2015 January-March Connectic~     3900       3900         870              22
    ## # ... with 3 more variables: colony_added <dbl>, colony_reno <dbl>,
    ## #   colony_reno_pct <dbl>

``` r
head(stressor)
```

    ## # A tibble: 6 x 5
    ##    year months        state   stressor              stress_pct
    ##   <dbl> <chr>         <chr>   <chr>                      <dbl>
    ## 1  2015 January-March Alabama Varroa mites                10  
    ## 2  2015 January-March Alabama Other pests/parasites        5.4
    ## 3  2015 January-March Alabama Disesases                   NA  
    ## 4  2015 January-March Alabama Pesticides                   2.2
    ## 5  2015 January-March Alabama Other                        9.1
    ## 6  2015 January-March Alabama Unknown                      9.4

# Ideas

## Timeline of Stressors

Timeline of stressors over time across all states. Maybe roll states up
into US regions.

## Hexbin Map of US

Maybe show changes in stressor prevalence between 2015 and 2021. Sigmoid
to reference additional metrics on the right.

Here is a [Good
Article](https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html)
describing how to make hexbins in R.

## EDA

``` r
count(colony, months)
```

    ## # A tibble: 4 x 2
    ##   months               n
    ##   <chr>            <int>
    ## 1 April-June         329
    ## 2 January-March      329
    ## 3 July-September     282
    ## 4 October-December   282

``` r
paste0("Min Year: ", min(colony$year), ", Max Year: ", max(colony$year))
```

    ## [1] "Min Year: 2015, Max Year: 2021"

## Hex Bins

### Tidy

``` r
# values
sigmoid_step_size <- 10
```

``` r
# transform spdf `id` into state abb
## create dataframe for mapping
states <- tibble(
  id = append(state.name, "District of Columbia"),
  abb = append(state.abb, "DC")
)

## join together
spdf_fort2 <- spdf_fortified %>%
  select(id, long, lat) %>%
  left_join(states, by = "id")

spdf_centers <- spdf_fort2 %>%
  group_by(abb) %>%
  dplyr::summarise(
    # find centers of hexes for state names
    mid_long = mean(long),
    mid_lat = mean(lat),
    # and slightly above center to start the sigmoid
    sig_long = mid_long,
    sig_lat = mid_lat + 1
  )

## define sigmoid

# define plotting metrics for state hexbins
state_colony_growth <- colony %>%
  filter(state != "United States") %>%
  mutate(
    colony_added = replace_na(colony_added, 0),
    colony_lost = replace_na(colony_lost, 0),
    growth = colony_added - colony_lost
  ) %>%
  group_by(state) %>%
  dplyr::summarise(total_growth = sum(growth, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(c("id" = "state")) %>%
  left_join(states, by = "id") %>%
  mutate(
    abs_growth = abs(total_growth),
    growth_bin = case_when(
      total_growth < -200000 ~ "Major Decline",
      total_growth < -5000 ~ "Decline",
      total_growth < 5000 ~ "Steady",
      total_growth < 200000 ~ "Growth",
      total_growth >= 20000 ~ "Major Growth"
    ),
    growth_bin = factor(growth_bin, levels = c("Major Decline", "Decline", "Steady", "Growth", "Major Growth"))
  )

# join states and spdf_fort2 for plotting purposes
spdf_bees <- spdf_fort2 %>%
  left_join(state_colony_growth, by = c("id", "abb"))

# highlight the greatest movers for sigmoid plotting
highlight_states <- state_colony_growth %>%
  filter(abs_growth >= 100000) %>%
  left_join(spdf_centers, by = "abb") %>%
  arrange(growth_bin) %>%
  mutate(
    sig_xend = -50,
    sig_yend = rev(seq(25, 75, sigmoid_step_size)), # rev because that incidentally lines up with long better
    text_color = case_when(
      growth_bin == "Major Decline" ~ "darkgoldenrod4",
      growth_bin == "Decline" ~ "darkgoldenrod2",
      growth_bin == "Steady" ~ "gold",
      growth_bin == "Growth" ~ "greenyellow",
      growth_bin == "Major Growth" ~ "yellowgreen"
    )
  )
```

``` r
# define sigmoid plots of stressors over time

## define x points for each quarter
stressor_x <- stressor %>%
  rename(c("id" = "state")) %>%
  inner_join(states, by = "id") %>%
  mutate(
    quarter = case_when(
      months == "January-March" ~ paste0(year, "-Q1"),
      months == "April-June" ~ paste0(year, "-Q2"),
      months == "July-September" ~ paste0(year, "-Q3"),
      months == "October-December" ~ paste0(year, "-Q4")
    )
  ) %>%
  #arrange(abb, quarter) %>%
  count(abb, quarter) %>%
  group_by(abb) %>%
  mutate(x = seq(1,52,2)) %>%
  select(-n)

## define the max stress value for scaling purposes
max_stress <- stressor %>%
  filter(state %in% highlight_states$id) %>%
  arrange(desc(stress_pct)) %>%
  head(1) %>%
  pull(stress_pct)

## final tidying for plotting purposes
sig_stressor <- stressor %>%
  rename(c("id" = "state")) %>%
  inner_join(highlight_states, by = "id") %>%
  mutate(
    quarter = case_when(
      months == "January-March" ~ paste0(year, "-Q1"),
      months == "April-June" ~ paste0(year, "-Q2"),
      months == "July-September" ~ paste0(year, "-Q3"),
      months == "October-December" ~ paste0(year, "-Q4")
    )
  ) %>%
  left_join(stressor_x, by = c("abb", "quarter")) %>%
  mutate(
    x_adj = x + sig_xend + 20,
    stress_pct_scaled = (stress_pct / 100) * (sigmoid_step_size - 3),
    y = stress_pct_scaled + sig_yend
  ) %>%
  group_by(abb, quarter) %>%
  filter(stress_pct == max(stress_pct)) %>%
  ungroup() %>%
  mutate(
    stressor_color = case_when(
      stressor == "Other pests/parasites" ~ "grey40",
      stressor == "Pesticides" ~ "cyan",
      stressor == "Unknown" ~ "grey60",
      stressor == "Varroa mites" ~ "magenta"
    )
  )

## define x axis label coordinates
x_lab_coords <- sig_stressor %>%
  distinct(year, quarter, x_adj, sig_yend) %>%
  group_by(year, sig_yend) %>%
  dplyr::summarise(x_lab = mean(x_adj)) %>%
  ungroup() %>%
  mutate(ylab = sig_yend - 1) %>%
  filter(year %% 2 == 0)
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

``` r
## define ax lines for plot
sig_ax_lines <- sig_stressor %>%
  distinct(abb, sig_xend, sig_yend) %>%
  mutate(
    xstart = sig_xend + 20,
    xend = 20, # xstart + 102,
    ystart = sig_yend,
    yend = ystart + (sigmoid_step_size - 3)
  ) %>%
  select(-sig_xend, -sig_yend)
```

### Plot

``` r
ggplot() +
  annotate("text", x = -125, y = 95, label = "Growth & Stress in Bee Colonies", color = "grey90", size = 15, fontface = "bold", hjust = 0) +
  annotate("text", x = -125, y = 88, label = "Growth = Colonies Added - Colonies Lost", color = "grey90", size = 8, hjust = 0) +
  annotate("text", x = -125, y = 83, label = "Stress = Percent of colony affected by stressor", color = "grey90", size = 8, hjust = 0) +
  annotate("text", x = -115, y = 78, label = "Most Common Stressors:", color = "grey90", size = 6, hjust = 0) + 
  annotate("text", x = -105, y = 74, label = "Pesticides", color = "cyan", size = 6, fontface = "italic", hjust = 0) + 
  annotate("text", x = -105, y = 71, label = "Varroa mites", color = "magenta", size = 6, fontface = "italic", hjust = 0) + 
  
  # state polygons
  annotate("text", x = -100, y = 54, label = "Total Growth\n2015 to 2021", color = "grey90", size = 5, fontface = "bold") +
  
  ## plot polygons
  geom_polygon(data = spdf_bees, aes(x = long, y = lat, fill = growth_bin, group = abb), color="grey10") +
  geom_point(data = spdf_centers, aes(x = sig_long, y = sig_lat, group = abb), color = "grey10") + 
  geom_text(data=spdf_centers, aes(x=mid_long, y=mid_lat, label=abb), color = "grey10") + 
  scale_fill_manual(values = c(
    "Major Decline" = "darkgoldenrod4",
    "Decline" = "darkgoldenrod2",
    "Steady" = "gold",
    "Growth" = "greenyellow",
    "Major Growth" = "yellowgreen"
  ), guide = "none") +
  
  # sigmoid to timelines details
  geom_sigmoid(data = highlight_states, aes(x = sig_long, xend = sig_xend, y = sig_lat, yend = sig_yend, group = abb), color = "grey90") +
  geom_richtext(
    data = highlight_states, 
    aes(x = sig_xend + 2, y = sig_yend, label = paste0("<b>", abb, "</b><br><b>", growth_bin, "</b><br>", round(total_growth / 1000), "k Colonies"), color = text_color),
    label.color = NA, fill = NA, label.padding = grid::unit(rep(0,4), "pt"), hjust = 0, size = 3, vjust = 0
  ) +
  
  # sigmoid time plots
  annotate("text", x = mean(x_lab_coords$x_lab), y = 85, label = "Greatest Quarterly\nStressors", color = "grey90", size = 5, fontface = "bold") + 
  
  ## sigmoid axis
  
  ### x axis
  geom_linerange(data = sig_ax_lines, aes(xmin = xstart, xmax = xend, y = ystart), color = "grey90") + 
  geom_text(data = x_lab_coords, aes(x = x_lab, y = ylab, label = year), color = "grey90", size = 2.5) +
  
  ### y axis
  geom_linerange(data = sig_ax_lines, aes(x = xstart, ymin = ystart, ymax = yend), color = "grey90") + 
  geom_text(data = sig_ax_lines, aes(x = xstart - 3, y = ystart, label = "0%"), color = "grey90", size = 2.5) + 
  geom_text(data = sig_ax_lines, aes(x = xstart - 3, y = (ystart + yend) / 2, label = "50%"), color = "grey90", size = 2.5) + 
  geom_text(data = sig_ax_lines, aes(x = xstart - 3, y = yend, label = "100%"), color = "grey90", size = 2.5) + 
  
  ## siqmoid points
  geom_line(data = sig_stressor, aes(x_adj, y, group = abb), color = "grey80") +
  geom_point(data = sig_stressor, aes(x_adj, y, color = stressor_color)) +
  
  scale_color_identity() +
  # general scales, themes, etc
  coord_cartesian(
    xlim = c(min(spdf_fort2$long) - 2, 25), 
    ylim = c(min(spdf_fort2$lat) - 5, 100),
    expand = c(0,0)
  ) +
  labs(caption = "Data Source: <b>USDA</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday") +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "grey10"),
    plot.caption = element_textbox(hjust = 0.5, size = 8, color = "grey90")
  )
```

    ## Warning in if (!expand) {: the condition has length > 1 and only the first
    ## element will be used

    ## Warning in if (!expand) {: the condition has length > 1 and only the first
    ## element will be used

![](Bees-Analysis_files/figure-gfm/Bee%20Growth%20and%20Stress-1.png)<!-- -->
