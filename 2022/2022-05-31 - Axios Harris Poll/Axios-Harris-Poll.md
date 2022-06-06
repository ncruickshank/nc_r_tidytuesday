Axios Harris Poll
================
Nick Cruickshank
2022-06-05

# Libraries and Data

``` r
# libraries
library(ggbump)
library(ggtext)
library(sysfonts)
library(showtext)
```

    ## Loading required package: showtextdb

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# data
poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
```

    ## Rows: 500 Columns: 8
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): company, industry
    ## dbl (6): 2022_rank, 2022_rq, change, year, rank, rq
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')
```

# Tidy Data

``` r
# top industries in 2022
top_industries <- poll %>%
  group_by(industry) %>%
  summarise(current_rq = mean(`2022_rq`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(current_rq)) %>%
  head(10) %>%
  mutate(
    current_rank = rank(desc(current_rq)),
    rank_cat = case_when(
      current_rank == 1 ~ "No. 1",
      current_rank == 2 ~ "No. 2",
      current_rank == 9 ~ "No. 9",
      current_rank == 10 ~ "No. 10",
      TRUE ~ "Other"
    )
  ) 
```

``` r
# transform shape of poll

## retrieve 2022 data and rename
poll_2022 <- poll %>%
  select(company, industry, `2022_rank`, `2022_rq`) %>%
  rename(c(
    "overall_rank" = "2022_rank",
    "rq" = "2022_rq"
  )) %>%
  mutate(year = 2022) %>%
  distinct(company, industry, overall_rank, rq, year)

## retrieve not-2022 data and rename
poll_no_2022 <- poll %>%
  select(company, industry, year, rank, rq) %>%
  rename(c("overall_rank" = "rank"))

## join all back together
poll_long <- rbind(poll_2022, poll_no_2022)
```

# Visualizaiton

Also include a graph along side which details the best companies (as of
2022) by industry.

``` r
# fonts
f1 <- "Playfair Display"
font_add_google(f1)
showtext_auto()
```

``` r
# transform poll for industries
poll_ranks <- poll_long %>%
  group_by(industry, year) %>%
  summarise(
    companies = n_distinct(company),
    mean_rq = mean(rq, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank = rank(desc(mean_rq))) %>%
  ungroup() %>%
  arrange(year, rank) %>%
  filter(industry %in% top_industries$industry) %>%
  filter(!is.na(mean_rq)) %>% 
  left_join(top_industries, by = "industry")
```

    ## `summarise()` has grouped output by 'industry'. You can override using the
    ## `.groups` argument.

``` r
# get beginning and ending rq for each industry
industry_labels <- poll_ranks %>%
  group_by(industry) %>%
  mutate(
    min_year = min(year),
    max_year = max(year)
  ) %>%
  ungroup() %>%
  filter(year == min_year | year == max_year) %>%
  select(industry, year, min_year, max_year, rank) %>%
  mutate(period = ifelse(year == min_year, "Begin", "End")) %>%
  left_join(top_industries, by = "industry")

top_cos_2022 <- poll %>%
  filter(industry %in% top_industries$industry) %>%
  distinct(company, industry, `2022_rank`, `2022_rq`) %>%
  group_by(industry) %>%
  mutate(industry_rank = rank(desc(`2022_rq`))) %>%
  ungroup() %>%
  filter(industry_rank == 1) %>%
  select(company, industry)
  
top_cos_2017 <- poll %>%
  filter(industry %in% top_industries$industry, year == 2017) %>%
  group_by(industry) %>%
  mutate(max_rq = max(rq, na.rm = TRUE)) %>%
  arrange(industry, desc(rq)) %>%
  ungroup() %>%
  filter(rq == max_rq) %>%
  select(company, industry)
```

    ## Warning in max(rq, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf

``` r
# plot
ggplot() + 
  geom_bump(
    data = poll_ranks,
    aes(year, rank, color = current_rank, group = industry),
    size = 1.5
  ) + 
  # lefthand labels
  geom_label(
    data = filter(industry_labels, period == "Begin"),
    aes(x = min_year, y = rank, label = industry, color = current_rank),
    hjust = 1
  ) + 
  geom_text(
    data = industry_labels %>%
      left_join(top_cos_2017, by = "industry") %>%
      filter(year == min(year)),
    aes(x = min_year - 2, y = rank, label = company),
    color = "grey10", hjust = 1, size = 3, fontface = "italic"
  ) +
  # righthand labels
  geom_label(
    data = filter(industry_labels, period == "End"),
    aes(x = max_year, y = rank, label = industry, color = current_rank),
    hjust = 0
  ) + 
  geom_text(
    data = industry_labels %>%
      left_join(top_cos_2022, by = "industry") %>%
      filter(year == max(year)),
    aes(x = max_year + 2, y = rank, label = company),
    color = "grey10", hjust = 0, size = 3, fontface = "italic"
  ) +
  
  # labels, scales, themes, etc
  labs(
    title = "Axios-Harris Poll",
    subtitle = "Changes in Ranking for Industries with the Highest Reputation as of 2022",
    caption = "<br><br>Data Source: <b>Axios Harris Poll</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  scale_x_continuous(
    limits = c(min(poll_long$year, na.rm = TRUE) - 4, max(poll_long$year, na.rm = TRUE) + 4), 
    expand = c(0,0), breaks = seq(2017,2021,2), name = "Year"
  ) + 
  scale_y_reverse(
    breaks = seq(1, 20, 5), name = "Rank"
  ) + 
  scale_color_gradient2(
    low="springgreen", mid="grey75", high="dodgerblue", 
    midpoint=5.5
  ) +
  theme_minimal(base_family = f1) + 
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.caption = element_textbox(hjust = 0.5, size = 10)
  )
```

![](Axios-Harris-Poll_files/figure-gfm/Axios%20Harris%20Industry%20Rankings-1.png)<!-- -->
