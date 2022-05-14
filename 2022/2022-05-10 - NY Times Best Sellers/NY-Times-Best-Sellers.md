NY Times Best Sellers
================
Nick Cruickshank
5/12/2022

# Analysis

``` r
# library
library(glue)
```

    ## Warning: package 'glue' was built under R version 4.1.3

``` r
library(ggtext)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
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
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x lubridate::date()        masks base::date()
    ## x dplyr::filter()          masks stats::filter()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x lubridate::union()       masks base::union()

``` r
# data
#nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')
```

    ## Rows: 60386 Columns: 6
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "\t"
    ## chr  (2): title, author
    ## dbl  (3): year, rank, title_id
    ## date (1): week
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Visual

Pick an author, track their books over time (week over week) to see the
rising and falling in rank for each book

``` r
nyt_full %>%
  group_by(author, title) %>%
  summarise(
    weeks = n()
  ) %>%
  ungroup() %>%
  count(author, sort = TRUE) %>%
  head(10)
```

    ## `summarise()` has grouped output by 'author'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 10 x 2
    ##    author                 n
    ##    <chr>              <int>
    ##  1 Danielle Steel       116
    ##  2 Stuart Woods          63
    ##  3 Stephen King          54
    ##  4 Robert B. Parker      47
    ##  5 John Sandford         44
    ##  6 David Baldacci        42
    ##  7 Dean Koontz           40
    ##  8 Mary Higgins Clark    40
    ##  9 Sandra Brown          40
    ## 10 Nora Roberts          38

### Tracking the Works of Stephen King

For books which showed up for at least 10 or more weeks.

``` r
# define books which show up for at least 10 weeks
sk_books <- nyt_full %>%
  filter(author == "Stephen King") %>%
  count(title, sort = TRUE) %>%
  filter(n >= 10) %>%
  pull(title)

# define stephen king data frame
sk <- nyt_full %>%
  filter(
    author == "Stephen King",
    title %in% sk_books
  )

# define how many books showed up on NYT Best Seller List each week SK Showed Up
total_books <- nyt_full %>%
  filter(week %in% sk$week) %>%
  group_by(week) %>%
  summarise(total_books = max(rank))

# join total books back to sk to get relative rank
change_sensitivity <- 2
sk <- sk %>%
  left_join(total_books, by = "week") %>%
  mutate(ranks_from_bottom = total_books - rank) %>%
  group_by(title) %>%
  mutate(
    rank_change = rank - lag(rank),
    rank_change = replace_na(rank_change, 0),
    rank_change_shape = case_when(
      rank_change <= -change_sensitivity ~ "Decrease",
      rank_change < change_sensitivity ~ "No Change",
      rank_change >= change_sensitivity ~ "Increase"
    )
  ) %>%
  ungroup()

# define aggregate duration for each book
sk_title_agg <- sk %>%
  group_by(title) %>%
  summarise(
    start_week = min(week),
    end_week = max(week),
    best_rank = min(rank)
  ) %>%
  ungroup() %>%
  mutate(
    duration = as.numeric(end_week - start_week),
    start_year = year(start_week),
    yday_start = yday(start_week),
    end_year = year(end_week),
    yday_end = yday(end_week),
    #title = str_to_title(title)
  )

# define beginning of peak period for each book
# set up date windows
sk <- sk %>%
  left_join(sk_title_agg, by = "title") %>%
  mutate(
    peak_week = ifelse(rank == best_rank, "Yes", "No"),
    day_of_year = yday(week)
  )

# set up year jittering
distinct_year_books <- sk %>%
  distinct(title, year) 

distinct_year_books_n <- distinct_year_books %>%
  count(year) %>%
  rename(c("books_active_that_year" = "n"))

sk_year_jitter <- distinct_year_books %>%
  group_by(year) %>%
  mutate(book_number = row_number()) %>%
  left_join(distinct_year_books_n, by = "year") %>%
  mutate(
    jitter = case_when(
      books_active_that_year == 1 ~ 0,
      books_active_that_year == 2 ~ 0.25,
      books_active_that_year == 3 ~ 0.33,
      books_active_that_year == 4 ~ 0.2
    ),
    year_jitter = case_when(
      # 1 book per year
      (books_active_that_year == 1 & book_number == 1) ~ year,
      
      # 2 books per year
      (books_active_that_year == 2 & book_number == 1) ~ year - jitter,
      (books_active_that_year == 2 & book_number == 2) ~ year + jitter,
      
      # 3 books per year
      (books_active_that_year == 3 & book_number == 1) ~ year - jitter,
      (books_active_that_year == 3 & book_number == 2) ~ year,
      (books_active_that_year == 3 & book_number == 3) ~ year + jitter,
      
      # 4 books per year
      (books_active_that_year == 4 & book_number == 1) ~ year - 2 * jitter,
      (books_active_that_year == 4 & book_number == 2) ~ year - jitter,
      (books_active_that_year == 4 & book_number == 3) ~ year + jitter,
      (books_active_that_year == 4 & book_number == 4) ~ year + 2 *jitter,
    )
  ) %>%
  select(year, title, year_jitter)

# set up weekly points
sk_weeks <- sk %>%
  left_join(sk_year_jitter, by = c("title", "year")) %>%
  mutate(
    week_color = case_when(
      week == start_week ~ "First",
      rank == 1 ~ "Peak - Rank 1",
      peak_week == "Yes" ~ "Peak - Not Rank 1",
      ranks_from_bottom == 0 ~ "Bottom of List",
      week == end_week ~ "Final",
      TRUE ~ "Regular"
    ),
    week_color = factor(week_color, levels = c("First", "Peak - Rank 1", "Peak - Not Rank 1", "Bottom of List", "Regular", "Final"))
  )

# set up title durations
sk_title_segments <- sk %>%
  group_by(title, year) %>%
  summarise(
    year_min_week = min(week),
    year_max_week = max(week)
  ) %>%
  ungroup() %>%
  mutate(
    yday_min_week = yday(year_min_week),
    yday_max_week = yday(year_max_week)
  ) %>%
  left_join(sk_year_jitter, by = c("title", "year")) %>%
  select(-year_min_week, -year_max_week)
```

    ## `summarise()` has grouped output by 'title'. You can override using the
    ## `.groups` argument.

``` r
sk_weeks_per_book <- nyt_full %>%
  filter(author == "Stephen King") %>%
  count(title)

# refresh year jitter onto title agg
sk_title_agg <- sk_title_agg %>%
  mutate(title = str_to_upper(title)) %>%
  left_join(rename(sk_year_jitter, c("start_year" = "year", "start_jitter" = "year_jitter")), by = c("title", "start_year")) %>%
  left_join(rename(sk_year_jitter, c("end_year" = "year", "end_jitter" = "year_jitter")), by = c("title", "end_year")) %>%
  mutate(title = str_to_title(title))
```

Good annotations - The Stand saw a resurgence from 90 - 91.

``` r
# values

## season starts
spring_start <- yday(ymd(20220321))
summer_start <- yday(ymd(20220621))
fall_start <- yday(ymd(20220921))
winter_start <- yday(ymd(20221221))

## mid season
mid_winter <- (0 + spring_start) / 2
mid_spring <- (spring_start + summer_start) / 2
mid_summer <- (summer_start + fall_start) / 2
mid_fall <- (fall_start + winter_start) / 2

## fonts
font_add_google("Playfair Display")
showtext_auto()
f1 <- "Playfair Display"

## colors
first_week_color <- "#374CD6"
peak_rank1_color <- "#4EEEEE"
peak_nrank1_color <- "#34E03B"
regular_week_color <- "#9A9995"
bottom_rank_color <- "#EE4E4E"
final_week_color <- "#D6C137"
```

``` r
ggplot() +
  
  # plot annotations
  
  ## set up x and y panel grids
  annotate(
    "segment", 
    x = 0, xend = 365, 
    y = seq(min(sk_weeks$year), max(sk_weeks$year), 1), 
    yend = seq(min(sk_weeks$year), max(sk_weeks$year), 1),
    linetype = "dashed", color = "grey80"
  ) +
  annotate("text", x = mid_winter, y = 1977.5, label = "Winter", fontface = "bold", size = 6, family = f1) +
  annotate("text", x = mid_spring, y = 1977.5, label = "Spring", fontface = "bold", size = 6, family = f1) +
  annotate("text", x = mid_summer, y = 1977.5, label = "Summer", fontface = "bold", size = 6, family = f1) +
  annotate("text", x = mid_fall,   y = 1977.5, label = "Fall", fontface = "bold", size = 6, family = f1) +
  geom_text(
    data = sk_title_agg,
    aes(x = yday_start - 3, y = start_jitter, label = paste0("(", start_year, ") ", title)),
    size = 3, fontface = "italic", hjust = 1
  ) + 
  geom_text(
    data = filter(sk_title_agg, end_year != start_year),
    aes(x = yday_end + 3, y = end_jitter, label = paste0(title, " (", round(duration / 7), "w from start)")),
    size = 3, fontface = "italic", hjust = 0 
  ) + 
  geom_text(
    data = filter(sk_title_agg, end_year == start_year),
    aes(x = yday_end + 3, y = end_jitter, label = paste0("(", round(duration / 7), "w from start)")),
    size = 3, fontface = "italic", hjust = 0 
  ) +
  
  # plot the Stephen King Titles
  geom_segment(
    data = sk_title_segments,
    aes(x = yday_min_week, xend = yday_max_week, 
        y = year_jitter, yend = year_jitter)
  ) +
  geom_point(
    data = sk_weeks,
    aes(day_of_year, year_jitter, fill = week_color, shape = rank_change_shape),
    size = 3, color = "grey10"
  ) + 
  
  # scales, themes, etc,
  labs(
    title = "Stephen King's Performance on the New York Times Best Seller Lists",
    subtitle = glue("<br>This prolific author has been on the New York Times (NYT) Best Seller List consistently <b> from 1979 until 2020</b> (<i>except for a brief lull from 2005 to 2010</i>) . During that time, he has had <b>{nrow(sk_weeks_per_book)} books show up on the list</b>, with {length(sk_books)} of them appearing for at least 10 weeks (plotted here). Most years he had between 1 and 3 unique books (with a minimum of 10 weeks duration) on the List, and <b>in 1987 he had 4 books on the NYT Best Seller List!</b><br><br>Each dot represents a week that a book was present on the NYT Best Seller List, from that books <b style='color:{first_week_color}'>first week</b> until its <b style='color:{final_week_color}'>final week</b> for each book. The intervening weeks are highlighted if they were <b style='color:{peak_rank1_color}'>highest ranked on the List</b>, or <b style='color:{bottom_rank_color}'>lowest ranked on the List</b> that week. Each week where the book was <b style='color:{peak_nrank1_color}'>at it's peak</b> is also highlighted.<br>"),
    caption = "<br><br>Data Source: <b>Post 45</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  scale_x_continuous(
    name = "Day of Year", 
    breaks = c(spring_start, summer_start, fall_start, winter_start),
    minor_breaks = NULL,
    limits = c(-50, 375),
    expand = c(0,0)
  ) +
  scale_y_reverse(
    name = "Year",
    breaks = seq(1980, 2020, 5),
    minor_breaks = NULL,
    limits = c(2021, 1977),
    expand = c(0,0)
  ) + 
  scale_fill_manual(values = c(
    "Final" = final_week_color,
    "First" = first_week_color,
    "Peak - Rank 1" = peak_rank1_color,
    "Peak - Not Rank 1" = peak_nrank1_color,
    "Bottom of List" = bottom_rank_color,
    "Regular" = regular_week_color
  ), guide = "none") + 
  scale_shape_manual(values = c(
    "Decrease" = 25,
    "No Change" = 21,
    "Increase" = 24
  ), name = glue("Change From Previous Week (Sensitivity = {change_sensitivity})")) +
  theme_void(base_family = f1) + 
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey80", linetype = "longdash"),
    plot.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_textbox(hjust = 0.5, halign = 0.5, size = 12, width = unit(1, "npc"), margin = margin(b = 10), lineheight = 1.4),
    plot.caption = element_textbox(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_blank()
  )
```

![](NY-Times-Best-Sellers_files/figure-gfm/Stephen%20King%20Best%20Seller%20Timeline-1.png)<!-- -->
