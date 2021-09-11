20210831 - Bird Baths
================
Nick Cruickshank
9/11/2021

-   [Introduction](#introduction)
-   [Analysis](#analysis)
    -   [Load Libraries](#load-libraries)
    -   [Load Data](#load-data)
    -   [Define Key Phrases and Tidy
        Dataset](#define-key-phrases-and-tidy-dataset)
    -   [Plot](#plot)
        -   [Rural Subplot](#rural-subplot)
        -   [Urban Subplot](#urban-subplot)
        -   [Combine All Plots Together](#combine-all-plots-together)

# Introduction

![](https://images.unsplash.com/photo-1598283549148-4aee3ca4f1d4?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1490&q=80)

This weeks [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-08-31)
comes from [Cleary et al
2016](https://figshare.com/articles/dataset/Avian_Assemblages_at_Bird_Baths_A_Comparison_of_Urban_and_Rural_Bird_Baths_in_Australia/3110284)
with the corresponding article [Avian Assemblages at Bird Baths: A
Comparison of Urban and Rural Bird Baths in
Australia](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0150899#abstract0).

This week I wanted to practice making slope charts in R. This dataset
provided a good opportunity for that, as it is shaped to easily compare
one year (2014) to the next (2015) for both rural and urban bird baths.

Kudos to
[gkaramanis](https://github.com/gkaramanis/tidytuesday/blob/master/2021/2021-week34/lemurs.R)
for inspiring the formatting for this visualization and to [Mark
Timberlake](https://unsplash.com/@mtimber71) for the banner image.

# Analysis

## Load Libraries

``` r
# libraries
library(forcats)
library(ggrepel)
library(ggtext)
library(glue)
library(patchwork)
library(readr)
library(tidyverse)
```

## Load Data

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

## Define Key Phrases and Tidy Dataset

``` r
## colors
increase_color <- "#4851E2"
decrease_color <- "#E24851"
highlight_color <- "#62BD5D"
```

``` r
# create primary df pre-sliced
bird_props <- bird_baths %>%
  filter(!(is.na(survey_year))) %>%
  group_by(survey_year, urban_rural, bird_type) %>%
  dplyr::summarise(total_birds = sum(bird_count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(survey_year, urban_rural) %>%
  # proportion of total bird sightings belonging to that species
  ## grouped by survey year and urban_rural
  mutate(prop = 100 * (total_birds / sum(total_birds))) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("urban_rural", "bird_type"), names_from = "survey_year", values_from = "prop", names_prefix = "p") %>%
  group_by(urban_rural, bird_type) %>%
  mutate(
    color = ifelse(p2014 > p2015, decrease_color, increase_color),
    direction = ifelse(p2014 > p2015, "Decline", "Increase"),
    delta = p2015 - p2014,
    bird_id = cur_group_id()
  ) %>%
  ungroup() %>%
  arrange(delta) %>%
  filter(((p2014 > 0) & (p2015 > 0)))

# add additional context
urban_rural_counts <- bird_baths %>%
  filter(!(is.na(survey_year))) %>%
  group_by(survey_year, urban_rural, bird_type) %>%
  dplyr::summarise(total_birds = sum(bird_count, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("urban_rural", "bird_type"), names_from = "survey_year", names_prefix = "n", values_from = "total_birds")

# create rural df
rural_bird_incline <- bird_props %>%
  filter(urban_rural == "Rural") %>%
  left_join(urban_rural_counts, by = c("urban_rural", "bird_type")) %>%
  slice_max(delta, n = 5)

rural_bird_decline <- bird_props %>%
  filter(urban_rural == "Rural") %>%
  left_join(urban_rural_counts, by = c("urban_rural", "bird_type")) %>%
  slice_min(delta, n = 5)

rural_birds <- bind_rows(rural_bird_incline, rural_bird_decline)

# create urban df
urban_bird_incline <- bird_props %>%
  filter(urban_rural == "Urban") %>%
  left_join(urban_rural_counts, by = c("urban_rural", "bird_type")) %>%
  slice_max(delta, n = 5) %>%
  mutate(bird_type = ifelse(bird_type == "New Holland Honeyeater", "New Holland\nHoneyeater", bird_type))

urban_bird_decline <- bird_props %>%
  filter(urban_rural == "Urban") %>%
  left_join(urban_rural_counts, by = c("urban_rural", "bird_type")) %>%
  slice_min(delta, n = 5) %>%
  mutate(bird_type = ifelse(bird_type == "Sulphur-crested Cockatoo", "Sulphur-crested\nCockatoo", bird_type))

urban_birds <- bind_rows(urban_bird_incline, urban_bird_decline)
```

## Plot

### Rural Subplot

``` r
# rural plot
rural_lollipops <- rural_birds %>%
  ggplot() +
  # plot title
  annotate("text", x = 1.5, y = 8.8, label = "RURAL", color = highlight_color, size = 10, fontface = "bold") + 
  annotate("segment", x = 0.7, xend = 2.3, y = 8.5, yend = 8.5, alpha = 0.2, size = 0.75) + 
  # total count axis
  annotate("segment", x = 1.1, xend = 1.9, y = seq(1, 8, 1), yend = seq(1, 8, 1), alpha = 0.2, size = 0.2) +
  annotate("point", x = 1.5, y = seq(1, 8, 1), size = 16, color = "grey90") +
  annotate("text", x = 1.5, y = seq(1, 8, 1), label = paste0(seq(1, 8, 1), "%"), size = 6, alpha = 0.2, fontface = "bold", color = "grey20") +
  # year annotations
  annotate("text", x = c(0.9, 2.1), y = 8, label = c("2014", "2015"), hjust = c(1,0), fontface = "bold", size = 8, alpha = 0.3, color = "grey20") + 
  # lollipops
  geom_segment(aes(y = p2014, yend = p2015, x = 1, xend = 2, color = color), size = 1.25) + 
  geom_point(aes(y = p2014, x = 1, color = color, size = n2014)) + 
  geom_point(aes(y = p2015, x = 2, color = color, size = n2015)) + 
  # bird labels
  ggrepel::geom_text_repel(
    data = filter(rural_birds, bird_id %% 2 == 0),
    aes(y = p2014, x = 1, label = bird_type, color = color),
    nudge_x = -0.2, hjust = 1, direction = "y", point.padding = 1, segement.size = 0.2, fontface = "italic"
  ) +
  ggrepel::geom_text_repel(
    data = filter(rural_birds, bird_id %% 2 != 0),
    aes(y = p2015, x = 2, label = bird_type, color = color),
    nudge_x = 0.2, hjust = 0, direction = "y", point.padding = 1, segement.size = 0.2, fontface = "italic"
  ) +
  # scales, themes, etc
  scale_color_identity() + 
  scale_size_continuous(range = c(2, 7), limits = c(0, 140), breaks = seq(0, 140, 20), name = "Total birds spotted yearly") + 
  scale_x_continuous(limits = c(0.5, 2.5)) + 
  scale_y_continuous(limits = c(0, 8.8)) +
  theme_void() + 
  theme(
    plot.title = element_textbox(color = highlight_color, hjust = 0.5, size = 24, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(color = "grey20")
  )
```

### Urban Subplot

``` r
# rural plot
urban_lollipops <- urban_birds %>%
  ggplot() +
  # plot title
  annotate("text", x = 1.5, y = 8.8, label = "URBAN", color = highlight_color, size = 10, fontface = "bold") + 
  annotate("segment", x = 0.7, xend = 2.3, y = 8.5, yend = 8.5, alpha = 0.2, size = 0.75) + 
  # total count axis
  annotate("segment", x = 1.1, xend = 1.9, y = seq(1, 8, 1), yend = seq(1, 8, 1), alpha = 0.2, size = 0.2) +
  annotate("point", x = 1.5, y = seq(1, 8, 1), size = 16, color = "grey90") +
  annotate("text", x = 1.5, y = seq(1, 8, 1), label = paste0(seq(1, 8, 1), "%"), size = 6, alpha = 0.2, fontface = "bold", color = "grey20") +
  # year annotations
  annotate("text", x = c(0.9, 2.1), y = 8, label = c("2014", "2015"), hjust = c(1,0), fontface = "bold", size = 8, alpha = 0.3, color = "grey20") + 
  # lollipops
  geom_segment(aes(y = p2014, yend = p2015, x = 1, xend = 2, color = color), size = 1.25) + 
  geom_point(aes(y = p2014, x = 1, color = color, size = n2014)) + 
  geom_point(aes(y = p2015, x = 2, color = color, size = n2015)) + 
  # bird labels
  ggrepel::geom_text_repel(
    data = filter(urban_birds, bird_id %% 2 == 0),
    aes(y = p2014, x = 1, label = bird_type, color = color),
    nudge_x = -0.2, hjust = 1, direction = "y", point.padding = 1, segement.size = 0.2, fontface = "italic"
  ) +
  ggrepel::geom_text_repel(
    data = filter(urban_birds, bird_id %% 2 != 0),
    aes(y = p2015, x = 2, label = bird_type, color = color),
    nudge_x = 0.2, hjust = 0, direction = "y", point.padding = 1, segement.size = 0.2, fontface = "italic"
  ) +
  # scales, themes, etc
  scale_color_identity() + 
  scale_size_continuous(range = c(2, 7), limits = c(0, 140), breaks = seq(0, 140, 20), name = "Total birds spotted yearly") + 
  scale_x_continuous(limits = c(0.5, 2.5)) + 
  scale_y_continuous(limits = c(0, 8.8)) +
  theme_void() + 
  theme(
    plot.title = element_textbox(color = highlight_color, hjust = 0.5, size = 24, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(color = "grey20")
  )
```

### Combine All Plots Together

``` r
# both plots, side by side
(urban_lollipops + guides(size = guide_legend(nrow = 1))) + (rural_lollipops + guides(size = "none")) +
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(
    title = glue("Bird species with the most pronounced <i style = 'color:{highlight_color}'>change (<i style='color:{increase_color}'>increase</i> or <i style = 'color:{decrease_color}'>decrease</i>) in percent representation</i> of total yearly Australian bird bath sightings"),
    caption = "Data Source: <b>Cleary et al 2016</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday",
    theme = theme(
      plot.title = element_textbox(color = "grey20", size = 24, face = "bold", hjust = 0.5, halign = 0.5, width = unit(1, "npc"), margin = margin(b = 15)),
      plot.caption = element_textbox(hjust = 0.5, size = 10, color = "grey20"),
      plot.background = element_rect(fill = "grey96")
    )
  ) & theme(legend.position = "bottom")
```

![](20210831---Bird-Baths_files/figure-gfm/Bird%20Bath%20Slope%20Plot-1.png)<!-- -->
