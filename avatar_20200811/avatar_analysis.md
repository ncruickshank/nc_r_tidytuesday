Avatar: The Last Airbender Analysis
================
Nick Cruickshank
9/24/2020

# Load Information

``` r
# load libraries
library(glue)
library(readr)
library(tidyverse)
library(shadowtext)
```

``` r
# load data
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')
```

``` r
# create ep list
eps <- avatar %>%
  distinct(book, book_num, chapter, chapter_num, writer, director, imdb_rating) %>%
  #mutate(writer = str_remove(writer, "<U+200E>")) %>%
  separate(writer, into = c("writer1","writer2","writer3","writer4","writer5",
                            "writer6","writer7","writer8","writer9","writer10"),
           sep = ", ")
# for each character, create a column with percent of spoke lines

books <- unique(avatar$book)
```

# Describe DF

``` r
avatar %>% head()
```

    ## # A tibble: 6 x 11
    ##      id book  book_num chapter chapter_num character full_text character_words
    ##   <dbl> <chr>    <dbl> <chr>         <dbl> <chr>     <chr>     <chr>          
    ## 1     1 Water        1 The Bo~           1 Katara    "Water. ~ Water. Earth. ~
    ## 2     2 Water        1 The Bo~           1 Scene De~ "As the ~ <NA>           
    ## 3     3 Water        1 The Bo~           1 Sokka     "It's no~ It's not getti~
    ## 4     4 Water        1 The Bo~           1 Scene De~ "The sho~ <NA>           
    ## 5     5 Water        1 The Bo~           1 Katara    "[Happil~ Sokka, look!   
    ## 6     6 Water        1 The Bo~           1 Sokka     "[Close-~ Sshh! Katara, ~
    ## # ... with 3 more variables: writer <chr>, director <chr>, imdb_rating <dbl>

# Exploratory Analysis

## Episode Breakdown by IMDB Rating

``` r
# worst episode
worst_ep <- eps %>%
  arrange(imdb_rating) %>%
  select(chapter, imdb_rating) %>%
  head(1)

worst_chap <- worst_ep$chapter
worst_chap_rating <- worst_ep$imdb_rating

# best episode
best_ep <- eps %>%
  arrange(desc(imdb_rating)) %>%
  select(chapter, imdb_rating) %>%
  head(1)

best_chap <- best_ep$chapter
best_chap_rating <- best_ep$imdb_rating
```

``` r
# create a graph visualizing the episodes by imbd_rating
eps %>%
  ggplot(aes(book_num, chapter_num)) + 
  geom_tile(aes(fill = imdb_rating), color = "black") + 
  scale_fill_viridis_c(option = "plasma") + 
  #geom_text(aes(label = chapter), size = 2.5, color = "white") + 
  geom_shadowtext(aes(label = chapter), size = 2.5) + 
  labs(
    title = "Avatar Episode IMDb Ratings",
    subtitle = glue("Best episode was '{best_chap}' with a rating of {best_chap_rating}\n
                    Worst episode was '{worst_chap}' with a rating of {worst_chap_rating}"),
    x = "Book",
    y = "Chapter",
    fill = "IMBd Rating"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2,0.96),
    legend.direction = "horizontal",
    legend.box.background = element_rect(),
    panel.grid = element_blank()
  )
```

![](avatar_analysis_files/figure-gfm/episode%20breakdown-1.png)<!-- -->
