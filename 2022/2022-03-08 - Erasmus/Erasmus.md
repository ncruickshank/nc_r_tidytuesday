Erasmus
================
Nick Cruickshank
3/13/2022

![](https://static.s123-cdn-static-d.com/uploads/4608029/normal_6075b777dc56f.png)

# Introduction

This weeks [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-03-08)
features data sourced from
[Data.Europa](https://data.europa.eu/data/datasets?locale=en&catalog=eac&query=erasmus&page=1&sort=issued%2Bdesc,%20relevance%2Bdesc,%20title.en%2Basc).
The following information is provided about Erasmus.

> Erasmus students are those that take advantage of the Erasmus exchange
> program, a well supported and organised scheme that has been in
> operation since the late 1980’s. It allows for students to study at
> universities in the EU member states for set periods of time. Erasmus
> students study a wide variety of subjects but most use the program for
> advancing their language skills with a view to working in the
> international sphere, and it is advised that anyone interested seeks
> information on the Erasmus scheme online.
>
> The European Credit Transfer System means that academic credits you
> earn in your course while abroad will count towards your
> qualification.

> Similar mobility periods are aggregated where possible (same
> sending/receiving organisation, same status regrading fewer oppts,
> gender, age, …) in order to reduce file size. Mobility periods started
> in 2020 and 2021 will be published once they are finalised.

# Analysis

``` r
# libraries
library(glue)
```

    ## Warning: package 'glue' was built under R version 4.1.3

``` r
library(ggtext)
library(ggsankey)
library(forcats)
library(here)
```

    ## here() starts at C:/Users/nccru/OneDrive/Documents/GitHub/nc_r_tidytuesday

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

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# data
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')
```

    ## Rows: 164635 Columns: 24
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (21): project_reference, academic_year, mobility_start_month, mobility_e...
    ## dbl  (3): mobility_duration, participant_age, participants
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
here <- here()
eu_codes <- read_csv(glue("{here}/2022/2022-03-08 - Erasmus/EU Codes.csv"))
```

    ## Rows: 28 Columns: 4
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (3): Country, Alpha-2, Alpha-3
    ## dbl (1): Code
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
erasmus
```

    ## # A tibble: 164,635 x 24
    ##    project_reference        academic_year mobility_start_month mobility_end_mon~
    ##    <chr>                    <chr>         <chr>                <chr>            
    ##  1 2014-1-AT02-KA347-000139 2014-2015     2014-11              2014-11          
    ##  2 2014-1-AT02-KA347-000139 2014-2015     2014-11              2014-11          
    ##  3 2014-1-AT02-KA347-000139 2014-2015     2014-11              2014-11          
    ##  4 2014-1-AT02-KA347-000139 2014-2015     2014-11              2014-11          
    ##  5 2014-1-AT02-KA347-000139 2014-2015     2014-11              2014-11          
    ##  6 2014-1-AT02-KA347-000139 2014-2015     2014-12              2014-12          
    ##  7 2014-1-AT02-KA347-000139 2014-2015     2014-12              2014-12          
    ##  8 2014-1-AT02-KA347-000139 2014-2015     2014-12              2014-12          
    ##  9 2014-1-AT02-KA347-000139 2014-2015     2014-12              2014-12          
    ## 10 2014-1-AT02-KA347-000139 2014-2015     2014-12              2014-12          
    ## # ... with 164,625 more rows, and 20 more variables: mobility_duration <dbl>,
    ## #   activity_mob <chr>, field_of_education <chr>,
    ## #   participant_nationality <chr>, education_level <chr>,
    ## #   participant_gender <chr>, participant_profile <chr>, special_needs <chr>,
    ## #   fewer_opportunities <chr>, group_leader <chr>, participant_age <dbl>,
    ## #   sending_country_code <chr>, sending_city <chr>, sending_organization <chr>,
    ## #   sending_organisation_erasmus_code <chr>, receiving_country_code <chr>, ...

**Observation:** The final field `participants` does *not* match with
the data structure of the rest of the data frame. Every other column
indicates that one row = one learner.

``` r
# values

## fonts
font_add_google("Cormorant") 
showtext_auto()
f1 <- "Cormorant"
```

## Exploratory Data Analysis

Should most likely focus on “National youth meetings”, as that accounts
for most of the data.

``` r
erasmus %>%
  count(activity_mob, sort = TRUE)
```

    ## # A tibble: 3 x 2
    ##   activity_mob                               n
    ##   <chr>                                  <int>
    ## 1 National youth meetings               139800
    ## 2 Transnational youth meetings           24834
    ## 3 Youth Exchanges - Programme Countries      1

Education Level is a useless field

``` r
erasmus %>%
  count(education_level, sort = TRUE)
```

    ## # A tibble: 1 x 2
    ##   education_level        n
    ##   <chr>              <int>
    ## 1 ??? - ? Unknown ? 164635

``` r
erasmus %>%
  count(field_of_education)
```

    ## # A tibble: 1 x 2
    ##   field_of_education      n
    ##   <chr>               <int>
    ## 1 ? Unknown ?        164635

Participant Profile is a useless field. Everybody is a learner.

``` r
erasmus %>%
  count(participant_profile)
```

    ## # A tibble: 1 x 2
    ##   participant_profile      n
    ##   <chr>                <int>
    ## 1 Learner             164635

There is actually a surprising amount of distribution here.

``` r
erasmus %>%
  count(mobility_duration)
```

    ## # A tibble: 48 x 2
    ##    mobility_duration     n
    ##                <dbl> <int>
    ##  1                 1 88142
    ##  2                 2 20942
    ##  3                 3 25343
    ##  4                 4 10547
    ##  5                 5  8177
    ##  6                 6  4364
    ##  7                 7  3761
    ##  8                 8  1763
    ##  9                 9   433
    ## 10                10   608
    ## # ... with 38 more rows

I wonder which countries have the greatest lag time in mobility
duration. Looks like Palestine and Israel are a cut above the rest,
interesting. Although honestly anything above 6 might be considered
noteworthy.

``` r
erasmus_agg <- erasmus %>%
  filter(activity_mob != "Youth Exchanges - Programme Countries") %>%
  group_by(sending_country_code, activity_mob) %>%
  dplyr::summarise(mean_duration = mean(mobility_duration)) %>%
  ungroup() %>%
  arrange(desc(mean_duration)) 
```

    ## `summarise()` has grouped output by 'sending_country_code'. You can override
    ## using the `.groups` argument.

``` r
flagged_countries <- erasmus_agg %>%
  filter(mean_duration >= 6 & mean_duration <= 10) %>%
  pull(sending_country_code)

flagged_countries_high <- erasmus_agg %>%
  filter(mean_duration >= 6 & mean_duration <= 10) %>%
  head(5) %>%
  pull(sending_country_code)

erasmus_agg %>%
  filter(mean_duration <= 10) %>%
  ggplot(aes(mean_duration, fct_reorder(sending_country_code, mean_duration))) +
  geom_vline(xintercept = 6, color = "red") +
  geom_bar(aes(fill = activity_mob), position = "dodge", stat = "identity") +
  #geom_linerange(aes(xmin = 0, xmax = mean_duration, y = fct_reorder(sending_country_code, mean_duration), color = activity_mob)) + 
  #geom_point(aes(color = activity_mob)) + 
  geom_text(
    data = erasmus_agg %>%
      filter(mean_duration <= 10) %>%
      group_by(sending_country_code) %>%
      slice_max(order_by = mean_duration, n = 1),
    aes(label = sending_country_code, color = activity_mob), 
    hjust = -1
  ) + 
  scale_y_discrete(labels = NULL, name = "Country") +
  scale_x_continuous(name = "Average Mobility Duration", limits = c(0, 8)) + 
  scale_color_discrete(name = NULL, guide = "none") +
  labs(
    title = "Average Mobility Duration (Days) By Country in Erasmus",
    subtitle = "Palestine (PS) and Iserael (IL) appear to be much higher than the rest, with any country above 6 days being noteworthy",
    fill = "Activity Mob"
  ) +
  theme_minimal(base_family = f1) + 
  theme(
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_textbox_simple(margin = margin(b = 10))
  )
```

![](Erasmus_files/figure-gfm/Average%20Mobility%20Period%20Per%20Country-1.png)<!-- -->

Further analysis reveals that this is entirely from the “Transnational
youth meetings” activity mobs. How interesting! This suggests to me that
Erasmus mobility is hampered by the fact that these students are coming
from outside the EU.

## Visualization

Sankey Diagram of sending country to receiving country within countries
with the highest mobility duration. Where do learners from these
countries like to go?

``` r
# define the most popular erasmus destinations
top_receivers <- erasmus %>%
  group_by(receiving_country_code) %>%
  dplyr::summarise(sum = sum(participants)) %>%
  ungroup() %>%
  arrange(desc(sum)) %>%
  head(3) %>%
  pull(receiving_country_code)

# define top senders when not amongst the top receivers
key_senders <- erasmus %>%
  filter(
    receiving_country_code %in% top_receivers,
    !(sending_country_code %in% top_receivers)
  ) %>%
  group_by(sending_country_code) %>%
  dplyr::summarise(sum = sum(participants)) %>%
  ungroup() %>%
  arrange(desc(sum)) %>%
  head(5) %>%
  pull(sending_country_code)

# tidy eu codes for joining
eu_node <- eu_codes %>%
  rename(c("node" = "Alpha-2", "sender" = "Country")) %>%
  select(node, sender)

eu_next_node <- eu_codes %>%
  rename(c("next_node" = "Alpha-2", "receiver" = "Country")) %>%
  select(next_node, receiver)

# create the sankey data frame
erasmus_sankey <- erasmus %>%
  filter(
    sending_country_code %in% key_senders,
    receiving_country_code %in% top_receivers
  ) %>%
  rename(c("1" = "sending_country_code", "2" = "receiving_country_code")) %>%
  make_long("1", "2") %>%
  mutate(x = as.numeric(x), next_x = as.numeric(next_x)) %>%
  left_join(eu_node, by = "node") %>%
  left_join(eu_next_node, by = "next_node")

# count the number of students per country
es_agg <- erasmus_sankey %>%
  filter(!is.na(node)) %>%
  group_by(x, node) %>%
  summarise(n = n()) 
```

    ## `summarise()` has grouped output by 'x'. You can override using the `.groups`
    ## argument.

``` r
# join back to sankey df
erasmus_sankey2 <- erasmus_sankey %>%
  left_join(es_agg, by = c("x", "node"))
```

``` r
# plot
erasmus_sankey2 %>%
  ggplot(aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = factor(node)
  )) + 
  geom_sankey(
    flow.alpha = 0.6,
    node.color = "grey30"
  ) + 
  annotate("text", x = 1, y = 325, label = "Sender", family = f1, size = 8, fontface = "bold") +
  annotate("segment", x = 0.9, xend = 1.1, y = 302, yend = 302) +
  annotate("text", x = 2, y = 325, label = "Receiver", family = f1, size = 8, fontface = "bold") +
  annotate("segment", x = 1.9, xend = 2.1, y = 302, yend = 302) +
  geom_sankey_text(
    data = filter(erasmus_sankey2, x == 1), 
    aes(x = x - 0.08, label = sender), 
    size = 5, hjust = 1, family = f1, fontface = "bold"
  ) +
  geom_sankey_text(
    data = filter(erasmus_sankey2, x == 2), 
    aes(x = x + 0.08, label = sender), 
    size = 5, hjust = 0, family = f1, fontface = "bold"
  ) +
  geom_sankey_text(aes(label = n), size = 5, vjust = 1, family = f1) +
  labs(
    title = "EU Erasmus Foreign Exchange Programs",
    subtitle = "Countries that send the most learners to the most popular destinations\n",
    caption = "<br>Top Receivers excluded from consideration for Top Senders. Each of these countries exchanged<br>a large enough volume of learners with each other to minimize all other senders.<br><br>Data Source: <b>Data Europa</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  scale_x_continuous(limits = c(0.6,2.4), expand = c(0,0)) +
  scale_y_continuous(limits = c(-300, 350), expand = c(0,0)) + 
  scale_fill_brewer(palette = "Accent") +
  theme_void(base_family = f1) + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 24, face = "italic"),
    plot.caption = element_textbox(hjust = 0.5, halign = 0.5, size = 12)
  )
```

![](Erasmus_files/figure-gfm/Erasmus%20Popular%20Destinations-1.png)<!-- -->
