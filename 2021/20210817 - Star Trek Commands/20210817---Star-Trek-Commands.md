20210817 - Star Trek Commands
================
Nick Cruickshank
8/19/2021

-   [NCruickshank Tweet](#ncruickshank-tweet)
    -   [Who had the most interactions with the
        computer?](#who-had-the-most-interactions-with-the-computer)
    -   [Sentiment Analysis by Star Trek Character and Interaction
        Type](#sentiment-analysis-by-star-trek-character-and-interaction-type)
        -   [Facetted boxplots for each character by interaction
            type](#facetted-boxplots-for-each-character-by-interaction-type)
-   [Other Tweets](#other-tweets)

``` r
# libraries
library(ggtext)
library(readr)
library(sentimentr)
library(tidytext)
library(tidyverse)
```

``` r
# data
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')
```

# NCruickshank Tweet

``` r
computer
```

    ## # A tibble: 2,214 x 14
    ##     name char           line  direction type  pri_type domain sub_domain nv_resp
    ##    <dbl> <chr>          <chr> <chr>     <chr> <chr>    <chr>  <chr>      <lgl>  
    ##  1   102 Tasha          Batt~ The door~ Stat~ Stateme~ IoT    Turbolift  TRUE   
    ##  2   102 Beverly        Show~ The scre~ Comm~ Command  InfoS~ <NA>       TRUE   
    ##  3   102 Beverly        Yes ~ The scre~ Stat~ Stateme~ IoT    <NA>       TRUE   
    ##  4   102 Picard         And ~ MAIN VIE~ Wake~ Question InfoS~ <NA>       FALSE  
    ##  5   102 Picard         And ~ MAIN VIE~ Ques~ Question InfoS~ <NA>       FALSE  
    ##  6   102 Picard         And ~ MAIN VIE~ Conv~ Question InfoS~ <NA>       FALSE  
    ##  7   102 Young Ensign   You ~ At the t~ Comm~ Command  InfoS~ Locate     FALSE  
    ##  8   102 Computer Voice Lieu~ <NA>      Resp~ Response InfoS~ Locate     FALSE  
    ##  9   102 Computer Voice This~ <NA>      Info  Alert    InfoS~ <NA>       FALSE  
    ## 10   102 Computer Voice This~ <NA>      Alert Alert    InfoS~ <NA>       FALSE  
    ## # ... with 2,204 more rows, and 5 more variables: interaction <chr>,
    ## #   char_type <chr>, is_fed <lgl>, error <lgl>, value_id <dbl>

## Who had the most interactions with the computer?

``` r
computer %>%
  filter(!(str_detect(char, "(Computer|Com Panel)"))) %>%
  count(char, sort = TRUE) %>%
  head(10)
```

    ## # A tibble: 10 x 2
    ##    char         n
    ##    <chr>    <int>
    ##  1 Geordi     320
    ##  2 Picard     266
    ##  3 Data       235
    ##  4 Riker      150
    ##  5 Beverly    121
    ##  6 Troi        74
    ##  7 Worf        66
    ##  8 Barclay     41
    ##  9 Wesley      30
    ## 10 K'Ehleyr    20

## Sentiment Analysis by Star Trek Character and Interaction Type

What was the average sentiment value for these metrics?

``` r
char_sent <- computer %>%
  # I care about human to computer interactions here
  filter(!(str_detect(char, "(Computer|Com Panel)"))) %>%
  select(char, pri_type, line) %>%
  mutate(observation = row_number()) %>%
  get_sentences() %$%
  sentiment_by(line, by = list(char, pri_type, observation)) %>%
  unite("char_pri", char:pri_type, remove = FALSE)

top_characters <- head(count(char_sent, char, sort = TRUE), 5)$char

char_sent2 <- char_sent %>%
  filter(char %in% top_characters) %>%
  group_by(char, pri_type) %>%
  summarise(
    observations = n(),
    avg_word_count = mean(word_count),
    avg_line_sentiment = mean(ave_sentiment)
  ) %>%
  ungroup() %>%
  mutate(
    sign = ifelse(avg_line_sentiment < 0, "Negative", "Positive"),
    abs_sentiment = abs(avg_line_sentiment),
    char = factor(char, levels = top_characters)
  )

char_sent2 %>%
  ggplot(aes(char, pri_type)) + 
  geom_point(aes(size = abs_sentiment, color = sign)) + 
  scale_color_manual(values = c("Red", "Green")) + 
  guides(size = "none", color = "none") +
  labs(
    title = "Sentiment Analysis of Star Trek Characters by Interaction Type",
    subtitle = "Size represents absolute sentiment while color represents direction (<b style='color:red'>negative</b> or <b style='color:green'>positive</b>)",
    x = "Character",
    y = "Interaction Type"
  ) + 
  theme_minimal() + 
  theme(
    plot.subtitle = element_textbox()
  )
```

![](20210817---Star-Trek-Commands_files/figure-gfm/Star%20Trek%20Sentiment%20by%20Character%20and%20Command%20Type-1.png)<!-- -->

### Facetted boxplots for each character by interaction type

# Other Tweets

``` r
# @neuroandstats Analysis
## https://twitter.com/neuroandstats/status/1427733648086814725
summarized_computer <- computer %>%
  summarise(char_type, line, pri_type)

summarized_computer$observation <- 1:nrow(summarized_computer)

avg_sentiment <- summarized_computer %>%
  get_sentences() %$% # weird that we're not piping here, but ok
  sentiment_by(line, by = list(char_type, pri_type, observation)) %>%
  unite("char_pri", char_type:pri_type, remove = FALSE)

neuroandstats_plot <- avg_sentiment %>%
  ggplot(aes(ave_sentiment, reorder(char_pri, ave_sentiment))) + 
  geom_jitter(alpha = 0.3, color = "black") + 
  geom_point() + 
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 24, fill = "red") + 
  labs(
    title = "Sentiment analysis of Star Trek speech interactions",
    x = "Average Sentiment Value",
    y = "Type of Dialog",
    caption = "Tidy Tuesday week 34 \ndata courtesy of SpeechInteraction.org"
  )
```
