Gender Pay Gap
================
Nick Cruickshank
2022-07-02

# Load Libraries and Data

``` r
# libraries
library(ggtext)
library(glue)
```

    ## Warning: package 'glue' was built under R version 4.1.3

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
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

SIC codes were found on the [UK Government
website](https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic).

``` r
# data
paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv') %>%
  janitor::clean_names()
```

    ## Rows: 48711 Columns: 27
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr   (9): employer_name, address, post_code, company_number, sic_codes, com...
    ## dbl  (15): employer_id, diff_mean_hourly_percent, diff_median_hourly_percent...
    ## lgl   (1): submitted_after_the_deadline
    ## dttm  (2): due_date, date_submitted
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# SIC Codes
sic_codes <- read_csv("SIC07_CH_condensed_list_en.csv") %>%
  janitor::clean_names()
```

    ## Rows: 731 Columns: 2
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): SIC Code, Description
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
paygap %>%
  head()
```

    ## # A tibble: 6 x 27
    ##   employer_name           employer_id address post_code company_number sic_codes
    ##   <chr>                         <dbl> <chr>   <chr>     <chr>          <chr>    
    ## 1 Bryanston School, Inco~         676 Bryans~ DT11 0PX  00226143       85310    
    ## 2 RED BAND CHEMICAL COMP~       16879 19 Smi~ EH6 8NU   SC016876       47730    
    ## 3 123 EMPLOYEES LTD             17677 34 Rou~ LS7 1AB   10530651       78300    
    ## 4 1610 LIMITED                    682 Trinit~ TA6 3JA   06727055       93110    
    ## 5 1879 EVENTS MANAGEMENT~       17101 The Su~ SR5 1SU   07743495       56210:70~
    ## 6 1LIFE MANAGEMENT SOLUT~         687 Ldh Ho~ PE27 4AA  02566586       93110:93~
    ## # ... with 21 more variables: diff_mean_hourly_percent <dbl>,
    ## #   diff_median_hourly_percent <dbl>, diff_mean_bonus_percent <dbl>,
    ## #   diff_median_bonus_percent <dbl>, male_bonus_percent <dbl>,
    ## #   female_bonus_percent <dbl>, male_lower_quartile <dbl>,
    ## #   female_lower_quartile <dbl>, male_lower_middle_quartile <dbl>,
    ## #   female_lower_middle_quartile <dbl>, male_upper_middle_quartile <dbl>,
    ## #   female_upper_middle_quartile <dbl>, male_top_quartile <dbl>, ...

# Exploratory Data Analysis

What is the range of data collection?

``` r
paste0("Min Data Submitted: ", min(paygap$date_submitted))
```

    ## [1] "Min Data Submitted: 2017-04-10 09:14:59"

``` r
paste0("Max Data Submitted: ", max(paygap$date_submitted))
```

    ## [1] "Max Data Submitted: 2022-06-27 08:31:39"

Are there any employers who are represented more than once? If there are
many who show up more than once, we may need to aggregate.

``` r
paygap %>%
  count(current_name, sort = TRUE)
```

    ## # A tibble: 12,745 x 2
    ##    current_name                           n
    ##    <chr>                              <int>
    ##  1 ABACUS EMPLOYMENT SERVICES LIMITED     6
    ##  2 Allerdale Borough Council              6
    ##  3 ARRK EUROPE LIMITED                    6
    ##  4 Bank of England                        6
    ##  5 BE MODERN LIMITED                      6
    ##  6 BIRTENSHAW                             6
    ##  7 BLACK SWAN INTERNATIONAL LIMITED       6
    ##  8 BOWDRAPER LIMITED                      6
    ##  9 BRANCASTER CARE HOMES LIMITED          6
    ## 10 BRISTOL LABORATORIES LIMITED           6
    ## # ... with 12,735 more rows

How many different employer sizes are there? If there’s a lot, we might
be able to linear regression. If there’s not, the amount of modeling we
can do is limited.

``` r
paygap %>%
  count(employer_size)
```

    ## # A tibble: 7 x 2
    ##   employer_size      n
    ##   <chr>          <int>
    ## 1 1000 to 4999   10383
    ## 2 20,000 or more   303
    ## 3 250 to 499     21235
    ## 4 500 to 999     11899
    ## 5 5000 to 19,999  2233
    ## 6 Less than 250   1903
    ## 7 Not Provided     755

How many different SIC codes are represented?

``` r
paygap %>%
  separate_rows(sic_codes, sep = ":") %>%
  filter(sic_codes != 1, !is.na(sic_codes)) %>%
  count(sic_codes, sort = TRUE) %>%
  pull(sic_codes) %>%
  length()
```

    ## [1] 637

## Tidy Data

Facet by employer size. What are the industries with the largest average
pay gap?

``` r
# tidy tuesday
pg <- paygap %>%
  filter(!is.na(sic_codes)) %>%
  
  # aggregate by employer to collapse all years where data was collected
  group_by(current_name, employer_size, sic_codes) %>%
  summarise(percent_gap = mean(diff_mean_hourly_percent, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # separate SIC codes into discrete rows and join in context
  separate_rows(sic_codes, sep = ":") %>% 
  rename(c("sic_code" = "sic_codes")) %>%
  inner_join(sic_codes, by = "sic_code") %>%
  rename(c("sector" = "description")) %>%
  
  # aggregate by sector
  group_by(sic_code, sector, employer_size) %>%
  summarise(
    n_companies = n(),
    mean_pct_gap = mean(percent_gap, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # final reshaping, filtering and sorting
  filter(
    employer_size != "Not Provided",
    n_companies >= 3 # has to have at least n companies represented in aggregattion
  ) %>%
  mutate(
    employer_size = factor(
      employer_size, 
      levels = c("Less than 250", "250 to 499", "500 to 999", "1000 to 4999", "5000 to 19,999", "20,000 or more")
      )
  ) %>%
  arrange(sector, employer_size, mean_pct_gap)
```

    ## `summarise()` has grouped output by 'current_name', 'employer_size'. You can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'sic_code', 'sector'. You can override using the `.groups` argument.

``` r
# further transformation

## top 10 industries where MALES get paid more
pgm <- pg %>%
  group_by(employer_size) %>%
  slice_max(order_by = mean_pct_gap, n = 10, with_ties = FALSE) %>%
  mutate(
    gender_rank = row_number(),
    gender = "Male"
  )

## top 10 industries where MALES get paid more
pgf <- pg %>%
  filter(mean_pct_gap < 0) %>%
  group_by(employer_size) %>%
  slice_min(order_by = mean_pct_gap, n = 10, with_ties = FALSE) %>%
  mutate(
    gender_rank = row_number(),
    gender = "Female"
  )

pg2 <- rbind(pgm, pgf) %>%
  mutate(abs_gap = abs(mean_pct_gap)) %>%
  group_by(employer_size) %>%
  mutate(overall_rank = rank(-abs_gap, ties = "last"))
```

Based on the row counts in the tibble below, we are going to need to
slice the top or bottom n from each employer size.

``` r
pg %>%
  count(employer_size, sort = TRUE)
```

    ## # A tibble: 6 x 2
    ##   employer_size      n
    ##   <fct>          <int>
    ## 1 250 to 499       409
    ## 2 500 to 999       316
    ## 3 1000 to 4999     258
    ## 4 Less than 250    120
    ## 5 5000 to 19,999    73
    ## 6 20,000 or more    14

# Visualize

``` r
# values

## font
f1 <- "EB Garamond"
font_add_google(f1)
showtext_auto()

## colors
male_color <- "#4169E1"
female_color <- "#DC143C"
```

``` r
for (i in unique(pg2$employer_size)) {
  pg_plot_df <- pg2 %>%
    filter(employer_size == i) 
  
  pg_plot <- pg_plot_df %>%
    ggplot(aes(abs_gap, overall_rank)) + 
    
    # points
    geom_segment(aes(x = 0, xend = abs_gap, y = overall_rank, yend = overall_rank, color = gender)) + 
    geom_point(aes(color = gender), size = 2) + 
    
    # annotations
    geom_text(
      aes(x = 0, y = overall_rank - 0.3, label = paste0(sector, " (n = ", n_companies, ")")), 
      hjust = 0, size = 3, color = "grey20"
    ) + 
    geom_text(
      aes(x = abs_gap + 1, y = overall_rank, label = paste0(round(abs_gap, 1), "%"), color = gender), 
      hjust = 0, size = 4
    ) + 
    labs(
      title = glue("Employer Size: {i}"),
      subtitle = glue("Sectors in the UK with the largest pay gap<br>in favor of <b style='color:{male_color}'>men</b> and <b style='color:{female_color}'>women</b>"),
      caption = "<br><br>Data Source: <b>UK Gender Pay Gap Services</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
    ) +
    annotate("text", x = (max(pg_plot_df$abs_gap) + 2.5) / 2, y = 0, label = "Mean % Difference in Pay", fontface = "italic", family = f1, size = 4) +
    
    # scales, themes, etc
    scale_x_continuous(limits = c(0, max(pg_plot_df$abs_gap) + 2.5)) +
    scale_y_reverse(limits = c(20, 0)) + 
    scale_color_manual(values = c(
      "Male" = male_color,
      "Female" = female_color
    )) +
    theme_void(base_family = f1) + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_textbox(hjust = 0.5, halign = 0.5, size = 16),
      plot.caption = element_textbox(hjust = 0.5, size = 8)
    )
  
  print(pg_plot)
}
```

![](Gender-Pay-Gap_files/figure-gfm/Gender%20Gap%20by%20Industry%20Size-1.png)<!-- -->![](Gender-Pay-Gap_files/figure-gfm/Gender%20Gap%20by%20Industry%20Size-2.png)<!-- -->![](Gender-Pay-Gap_files/figure-gfm/Gender%20Gap%20by%20Industry%20Size-3.png)<!-- -->![](Gender-Pay-Gap_files/figure-gfm/Gender%20Gap%20by%20Industry%20Size-4.png)<!-- -->![](Gender-Pay-Gap_files/figure-gfm/Gender%20Gap%20by%20Industry%20Size-5.png)<!-- -->![](Gender-Pay-Gap_files/figure-gfm/Gender%20Gap%20by%20Industry%20Size-6.png)<!-- -->
