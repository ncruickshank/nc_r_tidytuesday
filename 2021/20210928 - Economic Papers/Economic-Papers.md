Economic Papers
================
Nick Cruickshank
10/2/2021

-   [Introduction](#introduction)
-   [Data Analysis](#data-analysis)
    -   [Libraries](#libraries)
    -   [Data](#data)
    -   [Visualization](#visualization)

![From corporate finance
institute](https://cdn.corporatefinanceinstitute.com/assets/national-bureau-of-economic-research-nber1.jpg)

# Introduction

This weeks [Tidy
Tuesday](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-09-28)
project focuses on data from the [National Bureau of Economic
Research](https://www2.nber.org/RePEc/nbr/nberwo/) and the [`nberwp`
package by Ben Davies](https://github.com/bldavies/nberwp).

> New research by NBER affiliates, circulated for discussion and
> comment. The NBER distributes more than 1,200 working papers each
> year. These papers have not been peer reviewed. Papers issued more
> than 18 months ago are open access. More recent papers are available
> without charge to affiliates of subscribing academic institutions,
> employees of NBER Corporate Associates, government employees in the
> US, journalists, and residents of low-income countries.

# Data Analysis

## Libraries

``` r
# libraries
library(forcats)
library(ggtext)
library(readr)
library(shadowtext)
library(tidytext)
library(tidyverse)
```

## Data

``` r
# data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')
```

``` r
head(papers)
```

    ## # A tibble: 6 x 4
    ##   paper  year month title                                                       
    ##   <chr> <dbl> <dbl> <chr>                                                       
    ## 1 w0001  1973     6 Education, Information, and Efficiency                      
    ## 2 w0002  1973     6 Hospital Utilization: An Analysis of SMSA Differences in Ho~
    ## 3 w0003  1973     6 Error Components Regression Models and Their Applications   
    ## 4 w0004  1973     7 Human Capital Life Cycle of Earnings Models: A Specific Sol~
    ## 5 w0005  1973     7 A Life Cycle Family Model                                   
    ## 6 w0006  1973     7 A Review of Cyclical Indicators for the United States: Prel~

``` r
head(authors)
```

    ## # A tibble: 6 x 4
    ##   author  name             user_nber        user_repec
    ##   <chr>   <chr>            <chr>            <chr>     
    ## 1 w0001.1 Finis Welch      finis_welch      <NA>      
    ## 2 w0002.1 Barry R Chiswick barry_chiswick   pch425    
    ## 3 w0003.1 Swarnjit S Arora swarnjit_arora   <NA>      
    ## 4 w0004.1 Lee A Lillard    <NA>             pli669    
    ## 5 w0005.1 James P Smith    james_smith      psm28     
    ## 6 w0006.1 Victor Zarnowitz victor_zarnowitz <NA>

``` r
head(programs)
```

    ## # A tibble: 6 x 3
    ##   program program_desc                        program_category
    ##   <chr>   <chr>                               <chr>           
    ## 1 AG      Economics of Aging                  Micro           
    ## 2 AP      Asset Pricing                       Finance         
    ## 3 CF      Corporate Finance                   Finance         
    ## 4 CH      Children                            Micro           
    ## 5 DAE     Development of the American Economy Micro           
    ## 6 DEV     Development Economics               Micro

``` r
head(paper_authors)
```

    ## # A tibble: 6 x 2
    ##   paper author 
    ##   <chr> <chr>  
    ## 1 w0001 w0001.1
    ## 2 w0002 w0002.1
    ## 3 w0003 w0003.1
    ## 4 w0004 w0004.1
    ## 5 w0005 w0005.1
    ## 6 w0006 w0006.1

``` r
head(paper_programs)
```

    ## # A tibble: 6 x 2
    ##   paper program
    ##   <chr> <chr>  
    ## 1 w0074 EFG    
    ## 2 w0087 IFM    
    ## 3 w0087 ITI    
    ## 4 w0107 PE     
    ## 5 w0116 PE     
    ## 6 w0117 LS

## Visualization

``` r
# tidy

## join tables
papers_tidy <- papers %>%
  left_join(paper_programs, by = "paper") %>%
  left_join(programs, by = "program") %>%
  filter(
    !(is.na(program))
  )

## generate counts by year and program
program_years <- papers_tidy %>%
  count(program, program_desc, year)

## determine which term appeared most frequently in titles
## by year and programp
word_decades <- papers_tidy %>%
  mutate(decade = 10 * floor(year / 10)) %>%
  unnest_tokens(word, title) %>%
  count(program, program_desc, decade, word) %>%
  anti_join(stop_words) %>%
  group_by(program_desc) %>%
  bind_tf_idf(word, decade, n) %>%
  ungroup() %>%
  group_by(program, program_desc, decade) %>%
  slice_max(n = 1, order_by = tf_idf, with_ties = FALSE) %>%
  ungroup() %>%
  rename(c("word_count" = "n", "year" = "decade"))

## identify which programs are most popular
program_ranks <- papers_tidy %>%
  count(program, program_desc, sort = TRUE) %>%
  mutate(rank = row_number()) %>%
  select(-n)

## bind tidied datasets together
df <- program_years %>%
  left_join(word_decades, by = c("program", "program_desc", "year")) %>%
  left_join(program_ranks, by = c("program", "program_desc")) %>%
  rename(c("paper_count" = "n")) %>%
  mutate(word = str_to_title(word))
```

``` r
# plot
df %>%
  ggplot(aes(year, fct_reorder(str_wrap(program_desc, 10), rank, .desc = TRUE), fill = paper_count)) + 
  geom_tile(aes(height = 0.4), color = "grey20") +
  geom_text(aes(label = word), vjust = -1.5) +
  scale_x_continuous(name = NULL) +
  scale_y_discrete(name = NULL) +
  scale_fill_viridis_c(option = "magma") +
  guides(fill = guide_colorbar(
    title = "Number of Papers Published Annually",
    title.theme = element_text(size = 14, face = "italic", hjust = 0.5),
    title.position = "top",
    barwidth = 35,
    barheight = 0.5,
    label.position = "bottom",
    frame.colour = "grey20"
  )) +
  labs(
    title = "National Bureau of Economic Research",
    subtitle = str_wrap("Most distinctive term (from TF-IDF) within each program by decade", 80),
    fill = "Number of Yearly Papers",
    caption = "Data Source: <b>National Bureau of Economic Research</b> |  Visualization: <b>N. Cruickshank</b> | #TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_textbox(size = 10, hjust = 0.5),
    axis.text.y = element_text(hjust = 0.5, size = 10, color = "black", face = "bold"),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_line(color = "grey20"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "grey96")
  )
```

![](Economic-Papers_files/figure-gfm/NBER%20Paper%20Counts-1.png)<!-- -->
