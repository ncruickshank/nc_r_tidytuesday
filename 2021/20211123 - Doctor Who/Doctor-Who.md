Doctor Who
================
Nick Cruickshank
11/26/2021

![](https://ichef.bbci.co.uk/images/ic/1200x675/p08hrycq.jpg)

# Introduction

This week’s [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-11-23)
features data from the [‘datardis’
package](https://github.com/KittJonathan/datardis/tree/main/data). The
package is accompanied by a [brief blog
post](https://randomics.netlify.app/posts/2021-11-16-datardis/).

## Regeneration Timeline

[This post](https://www.timetoast.com/timelines/dr-who-regeneration)
provides a nice breakdown of doctors 9 through 11. Does not include 12
or 13 though.

[This BBC Article](https://www.bbc.co.uk/newsround/25004050) nicely
breaks down the descriptions of each of the Revival doctors.

# Analysis

## Import Libraries and Data

``` r
# libraries
library(forcats)
library(ggbreak)
library(ggtext)
library(glue)
library(lubridate)
library(readr)
library(tidyverse)
```

``` r
# data
directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')
```

``` r
head(directors)
```

    ## # A tibble: 6 x 2
    ##   story_number director   
    ##   <chr>        <chr>      
    ## 1 157          Keith Boak 
    ## 2 158          Euros Lyn  
    ## 3 159          Euros Lyn  
    ## 4 160a         Keith Boak 
    ## 5 160b         Keith Boak 
    ## 6 161          Joe Ahearne

``` r
head(episodes)
```

    ## # A tibble: 6 x 12
    ##   era     season_number serial_title story_number episode_number episode_title  
    ##   <chr>           <dbl> <chr>        <chr>                 <dbl> <chr>          
    ## 1 revived             1 <NA>         157                       1 Rose           
    ## 2 revived             1 <NA>         158                       2 The End of the~
    ## 3 revived             1 <NA>         159                       3 The Unquiet De~
    ## 4 revived             1 <NA>         160a                      4 Aliens of Lond~
    ## 5 revived             1 <NA>         160b                      5 World War Three
    ## 6 revived             1 <NA>         161                       6 Dalek          
    ## # ... with 6 more variables: type <chr>, first_aired <date>,
    ## #   production_code <chr>, uk_viewers <dbl>, rating <dbl>, duration <dbl>

``` r
head(writers)
```

    ## # A tibble: 6 x 2
    ##   story_number writer          
    ##   <chr>        <chr>           
    ## 1 157          Russell T Davies
    ## 2 158          Russell T Davies
    ## 3 159          Mark Gatiss     
    ## 4 160a         Russell T Davies
    ## 5 160b         Russell T Davies
    ## 6 161          Robert Shearman

``` r
head(imdb)
```

    ## # A tibble: 6 x 6
    ##   season ep_num air_date     rating rating_n desc                               
    ##    <dbl>  <dbl> <chr>         <dbl>    <dbl> <chr>                              
    ## 1      1      1 17 Mar. 2006    7.5     8301 When ordinary shop-worker Rose Tyl~
    ## 2      1      2 17 Mar. 2006    7.5     7279 The Doctor takes Rose to the year ~
    ## 3      1      3 24 Mar. 2006    7.5     6804 The Doctor has great expectations ~
    ## 4      1      4 31 Mar. 2006    7       6543 The Doctor returns Rose to her own~
    ## 5      1      5 7 Apr. 2006     7       6337 The Slitheen have infiltrated Parl~
    ## 6      1      6 14 Apr. 2006    8.7     7211 The TARDIS is drawn to an alien mu~

## Tidying

[Info on Charles
Dickens](https://en.wikipedia.org/wiki/Charles_Dickens). Born in 1812,
died in 1870. Major success began in 1836.

[The London Blitz](https://en.wikipedia.org/wiki/The_Blitz) occurred
from 1940-09-07 to 1941-05-11.

[Queen Victoria](https://en.wikipedia.org/wiki/Queen_Victoria) was born
on 1819-05-24 and died on 1901-01-22. She was coronated on 1837-06-20.

[ELizabeth II](https://en.wikipedia.org/wiki/Elizabeth_II) is the
current queen of englad. She was born on 1926-04-21 and is still alive.
She was coronated on 1953-06-02.

[William Shakespeare](https://en.wikipedia.org/wiki/William_Shakespeare)
is a famous playwright (apparently). He was born on ??? (unavailable),
baptized on 1564-04-26, and died on 1616-04-23. His career really began
between 1585 and 1592.

[Winston Churchill](https://en.wikipedia.org/wiki/Winston_Churchill) was
a prime minister of the UK. Born on 1874-11-30 and died on 1965-01-24.
He was in office from 1951-10-26 to 1955-04-05.

``` r
# tidy

## prepare imdb for joining
imdb2 <- imdb %>%
  rename(c(
    "season_number" = "season",
    "episode_number" = "ep_num",
    "first_aired" = "air_date",
    "imdb_rating" = "rating"
  )) %>%
  ## this wound up being too messy to worry about
  ## especially given it will just be joined anyway
  #mutate(first_aired = as.Date(first_aired, format = "%d %b. %Y"))
  select(-first_aired)

# regex used for extracting most years
travel_year_regex <- "([0-9] (m|b)illion|[0-9]{2,3},[0-9]{3}|[0-9]{1,4}(th|nd|st) (century|millenium)|[0-9]{3,4}|(ad|bc) [0-9]{2,4})"

who <- episodes %>%
  left_join(directors, by = "story_number") %>%
  left_join(writers, by = "story_number") %>%
  left_join(imdb2, by = c("season_number", "episode_number")) %>%
  # only 'revived' era included
  select(-era) %>%
  # define which doctor was in
  mutate(
    doctor_num = case_when(
      first_aired >= ymd(20050301) & first_aired <= ymd(20050618) ~ 9,
      first_aired >= ymd(20050619) & first_aired <= ymd(20091225) ~ 10,
      first_aired >= ymd(20091226) & first_aired <= ymd(20131124) ~ 11,
      first_aired >= ymd(20131125) & first_aired <= ymd(20170901) ~ 12,
      first_aired >= ymd(20170902) ~ 13
    ),
    doctor = case_when(
      doctor_num == 9 ~ "Christopher Eccleston",
      doctor_num == 10 ~ "David Tennant",
      doctor_num == 11 ~ "Matt Smith",
      doctor_num == 12 ~ "Peter Capaldi",
      doctor_num == 13 ~ "Jodie Whittaker"
    )
  ) %>%
  # extracting dates from episode descriptions
  mutate(
    tag = ifelse(str_detect(str_to_lower(desc), "(minutes|hours)"), "Yes", "No"),
    travel_year = str_extract(str_to_lower(desc), travel_year_regex),
    # some desc's reference historical figures or events
    # this can also be used to year references
    history_tag = case_when(
      str_detect(desc, "Charles Dickens") ~ ymd(18360101), 
      str_detect(desc, "London Blitz") ~ ymd(19410511),
      str_detect(desc, "Queen Victoria") ~ ymd(18370620),
      str_detect(desc, "Elizabeth II") ~ ymd(19530602),
      str_detect(desc, "William Shakespeare") ~ ymd(15900101),
      str_detect(desc, "Churchill") ~ ymd(19511026),
    ),
    travel_year2 = case_when(
      !(is.na(history_tag)) ~ year(history_tag),
      str_detect(travel_year, "billion") ~ as.double(str_trim(str_remove(travel_year, "billion"))) * 1000000000,
      str_detect(travel_year, ",") ~ as.double(str_remove(travel_year, ",")),
      str_detect(travel_year, "century") ~ (as.double(str_extract(travel_year, "[0-9]{1,3}")) * 100) - 100,
      str_detect(travel_year, "ad") ~ as.double(str_extract(travel_year, "[0-9]{1,3}")),
      TRUE ~ as.double(travel_year)
    )
  ) %>%
  select(-travel_year, -history_tag, -tag)
```

## Which Doctors Travelled the Farthest in Time?

``` r
paste0("Data set runs from ",  min(who$first_aired),  " to ",  max(who$first_aired), ".")
```

    ## [1] "Data set runs from 2005-03-26 to 2021-12-05."

``` r
count(who, type)
```

    ## # A tibble: 2 x 2
    ##   type        n
    ##   <chr>   <int>
    ## 1 episode   164
    ## 2 special    21

``` r
count(who, director, sort = TRUE) %>%
  head(10)
```

    ## # A tibble: 10 x 2
    ##    director               n
    ##    <chr>              <int>
    ##  1 Graeme Harper         13
    ##  2 Euros Lyn             11
    ##  3 Douglas Mackinnon      9
    ##  4 Jamie Magnus Stone     9
    ##  5 James Strong           8
    ##  6 Rachel Talalay         7
    ##  7 Charles Palmer         6
    ##  8 Daniel Nettheim        6
    ##  9 James Hawes            5
    ## 10 Joe Ahearne            5

``` r
count(who, writer, sort = TRUE) %>%
  head(10)
```

    ## # A tibble: 10 x 2
    ##    writer               n
    ##    <chr>            <int>
    ##  1 Steven Moffat       48
    ##  2 Russell T Davies    31
    ##  3 Chris Chibnall      26
    ##  4 Mark Gatiss          9
    ##  5 Toby Whithouse       7
    ##  6 Gareth Roberts       6
    ##  7 Helen Raynor         4
    ##  8 Jamie Mathieson      4
    ##  9 Peter Harness        4
    ## 10 Matthew Graham       3

Turns out that Doctor 9 is not really worth plotting. His travels were
so far into the future that it makes every other doctors travels look
like nothing. It’s worth describing that with numbers and words, but not
showing it.

``` r
who_time <- who %>%
  arrange(season_number, episode_number) %>%
  mutate(cum_ep = row_number()) %>%
  filter(!is.na(travel_year2) & travel_year2 != 0) %>%
  mutate(
    air_year = year(first_aired),
    travel = travel_year2 - air_year,
    direction = ifelse(travel < 0, "Past", "Future")
  )

doc_stats <- who_time %>%
  group_by(doctor, doctor_num) %>%
  dplyr::summarise(
    min_year = min(travel + air_year),
    mean_abs_travel = mean(abs(travel)),
    max_year = max(travel + air_year),
    future_prop = sum(direction == "Future") / n(),
    past_prop = sum(direction == "Past") / n()
  ) %>%
  ungroup() %>%
  arrange(desc(mean_abs_travel)) %>%
  mutate(rank = row_number())
```

``` r
# plot colors
space_color <- "#17003C"
tardis_color <- "#3C91FF" # not literally, but close enough
detail_color <- "#F7FFAD" # kind of like the text color on the tardis
future_color <- "#91FF3C"
past_color <- "#FF3C91"

# values
doc9_string <- who_time %>%
  filter(doctor_num == 9) %>%
  arrange(desc(travel)) %>%
  head(2) %>%
  mutate(string = paste0(round(travel / 100000, 2), " hundred thousand years into the ", str_to_lower(direction), " in s", season_number, "e", episode_number)) %>%
  pull(string) %>%
  glue_collapse(sep = ",", last = ", & ")

doc9eps <- nrow(filter(who, doctor_num == 9))

all_doc_strings <-who_time %>%
  left_join(doc_stats, by = c("doctor", "doctor_num")) %>%
  mutate(abs_travel = abs(travel)) %>%
  group_by(doctor, doctor_num, rank) %>%
  arrange(desc(travel)) %>%
  mutate(travel_rank = row_number()) %>%
  filter(travel_rank == min(travel_rank) | travel_rank == max(travel_rank)) %>%
  #slice_max(abs_travel, n = 2) %>%
  ungroup() %>%
  mutate(
    string = paste0("s", season_number, "e", episode_number, " - ", abs_travel, "y into the ", str_to_lower(direction)),
    string2 = paste0(abs_travel, "y in s", season_number, "e", episode_number)
  ) %>%
  select(doctor, doctor_num, rank, travel, string, string2)
```

``` r
who_time %>%
  left_join(doc_stats, by = c("doctor", "doctor_num")) %>%
  mutate(plot_doctor = str_replace(doctor, " ", "\n")) %>% 
  # the travel to year 5 billion is such an extreme outlier it isn't worth plotting
  filter(doctor_num != 9) %>%
  ggplot(aes(rank, travel)) + 
  
  # set up the plot
  ## x axis
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = tardis_color, size = 1.5) +
  ## y axis
  annotate("segment", x = 3.5, xend = 3.5, y = -2000, yend = 3000, linetype = "dashed", color = tardis_color, size = 1.5) + 
  annotate("point", x = 3.5,  y = seq(-2000, -1000, 1000), size = 13, shape = 25, fill = tardis_color, color = past_color) +
  annotate("text", x = 3.5, y = seq(-2000, -1000, 1000), label = paste0(seq(-2, -1, 1), "k")) +
  annotate("point", x = 3.5, y = 0, size = 10, shape = 23, fill = tardis_color, color = detail_color) + 
  annotate("text", x = 3.5, y = 0, label = "0") + 
  annotate("point", x = 3.5, y = seq(1000, 3000, 1000), size = 13, shape = 24, fill = tardis_color, color = future_color) + 
  annotate("text", x = 3.5, y = seq(1000, 3000, 1000), label = paste0(seq(1,3,1), "k")) + 
  geom_text(aes(x = rank, y = 3500, label = plot_doctor), color = detail_color, size = 5, face = "bold") + 
  annotate("segment", x = seq(2,5,1), xend = seq(2,5,1), y = -2000, yend = 3000, color = detail_color, linetype = "dashed") +
  
  # plot travels
  ## doctor averages
  geom_linerange(
    data = filter(doc_stats, doctor_num != 9),
    aes(xmin = rank - 0.2, xmax = rank + 0.2, y = mean_abs_travel),
    color = detail_color, linetype = "longdash"
  ) +
  geom_point(
    data = filter(doc_stats, doctor_num != 9), 
    aes(x = rank, y = mean_abs_travel),
    shape = 23, fill = detail_color, size = 6
  ) + 
  geom_text(
    data = filter(doc_stats, doctor_num != 9, rank %% 2 == 0),
    aes(x = rank - 0.15, y = mean_abs_travel + 200, label = paste0(round(mean_abs_travel), "y")),
    color = detail_color, fontface = "bold"
  ) + 
  geom_text(
    data = filter(doc_stats, doctor_num != 9, rank %% 2 != 0),
    aes(x = rank + 0.15, y = mean_abs_travel + 200, label = paste0(round(mean_abs_travel), "y")),
    color = detail_color, fontface = "bold"
  ) + 
  ## each episode distribution
  geom_point(
    aes(fill = direction, shape = direction), 
    color = space_color, size = 5, alpha = 0.75
  ) + 
  geom_text(
    data = filter(all_doc_strings, doctor_num != 9, rank %% 2 != 0),
    aes(x = rank - 0.08, y = travel, label = string2),
    color = detail_color, hjust = 1, size = 3
  ) +
  geom_text(
    data = filter(all_doc_strings, doctor_num != 9, rank %% 2 == 0),
    aes(x = rank + 0.08, y = travel, label = string2),
    color = detail_color, hjust = 0, size = 3
  ) +
  
  # scales, themes, etc
  ## title
  annotate("richtext", x = 3.5, y = 5500, label = "DOCTOR WHO", 
           size = 14, fontface = "bold", color = tardis_color,
           fill = NA, label.color = NA) +
  ## subtitle 
  annotate("text", x = 3.5, y = 5000, label = str_wrap("Which doctor travelled the greatest average absolute distance in time?", 35),
           size = 8, fontface = "bold", lineheight = 0.8, color = detail_color) +
  annotate("text", x = 3.5, y = 4500, label = str_wrap("Travel years extracted from episode descriptions and compared to the episode air date.", 55),
           size = 6, fontface = "italic", lineheight = 0.8, color = tardis_color) +
  annotate("text", x = 3.5, y = 4050, label = str_wrap(glue("Christopher Eccleston was technically the greatest traveller by a very wide margin. In his {doc9eps} epsiodes, Eccleston travelled {doc9_string}."), 80),
           size = 4, fontface = "italic", lineheight = 0.8, color = detail_color) +
  labs(
    caption = "Data Source: <b>datardis</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  scale_shape_manual(values = c(
    "Past" = 25,
    "Future" = 24
  ), guide = "none") + 
  scale_fill_manual(values = c(
    "Past" = past_color,
    "Future" = future_color
  ), guide = "none") + 
  #scale_y_reverse() + 
  theme_void() + 
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = space_color, color = space_color),
    panel.background = element_rect(fill = space_color, color = space_color),
    plot.caption = element_textbox(color = tardis_color, hjust = 0.5)
  )
```

![](Doctor-Who_files/figure-gfm/Doctor%20Time%20Travels-1.png)<!-- -->
