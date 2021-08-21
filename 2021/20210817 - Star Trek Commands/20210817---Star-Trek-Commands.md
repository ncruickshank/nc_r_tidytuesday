20210817 - Star Trek Commands
================
Nick Cruickshank
8/21/2021

-   [Text Analysis of Interactions with the
    Computer](#text-analysis-of-interactions-with-the-computer)
    -   [Sentiment Analysis by Star Trek Character and Interaction
        Type](#sentiment-analysis-by-star-trek-character-and-interaction-type)
    -   [TF-IDF Analysis by Top
        Character](#tf-idf-analysis-by-top-character)
    -   [Combine Plots](#combine-plots)
-   [Tweets of Inspiration](#tweets-of-inspiration)

``` r
# libraries
library(here)
library(glue)
library(ggtext)
library(jpeg)
library(patchwork)
library(png)
library(readr)
library(sentimentr)
library(tidytext)
library(tidyverse)
```

``` r
# data
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')
```

# Text Analysis of Interactions with the Computer

For this week’s [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-08-17)
I wanted to perform various text analysis tasks on lines spoken by each
of the main cast members to the computer. Here, the “primary cast” is
defined as the characters who had the greatest number of interactions
with the computer. Interestingly, interactions with the computer
slightly shift the rankings of most prominent character. Specifically,
[Geordi La Forge](https://en.wikipedia.org/wiki/Geordi_La_Forge) is not
the main character of [Star Trek: The Next
Generation](https://en.wikipedia.org/wiki/Star_Trek:_The_Next_Generation),
but he does have the most interactions with the computer by a
comfortable margin.

``` r
# character interactions with the computer
computer %>%
  filter(!(str_detect(char, "(Computer|Com Panel)"))) %>%
  count(char, sort = TRUE) %>%
  filter(n > 20) %>%
  ggplot(aes(fct_reorder(char, n), n)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), hjust = 1) +
  coord_flip() + 
  labs(title = "Which Star Trek TNG Characters Interacted with the Computer the Most?")
```

![](20210817---Star-Trek-Commands_files/figure-gfm/Star%20Trek%20TNG%20Char-Comp%20Interaction%20Counts-1.png)<!-- -->

For the following analysis I chose to focus on the Top 5 characters by
interactions with the computer. This decision was made in part because
there was a fairly steep drop-off in interactions with the computer
after [Beverly Crusher](https://en.wikipedia.org/wiki/Beverly_Crusher),
which would make [Term Frequency - Inverse Document Frequency
(TF-IDF)](https://en.wikipedia.org/wiki/Tf%E2%80%93idf) difficult.

## Sentiment Analysis by Star Trek Character and Interaction Type

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

char_ranks <- computer %>%
  filter(char %in% top_characters) %>%
  count(char, sort = TRUE) %>%
  mutate(rank = row_number())

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
```

``` r
# plot

## x-axis labels
labels <- c(
  "Geordi" = glue("<img src='{here('2021', '20210817 - Star Trek Commands', 'images', 'geordi.jpg')}' width='80'/><br>**Geordi**"),
  "Picard" = glue("<img src='{here('2021', '20210817 - Star Trek Commands', 'images', 'picard.jpg')}' width='80'/><br>**Picard**"),
  "Data" = glue("<img src='{here('2021', '20210817 - Star Trek Commands', 'images', 'data.jpg')}' width='80'/><br>**Data**"),
  "Riker" = glue("<img src='{here('2021', '20210817 - Star Trek Commands', 'images', 'riker.jpg')}' width='80'/><br>**Riker**"),
  "Beverly" = glue("<img src='{here('2021', '20210817 - Star Trek Commands', 'images', 'beverly.jpg')}' width='80'/><br>**Beverly**")
)

## plot the sentiment values
sent_plot <- char_sent2 %>%
  left_join(char_ranks, by = "char") %>%
  ggplot(aes(fct_reorder(char, rank), pri_type)) + 
  geom_point(aes(size = abs_sentiment, color = sign)) + 
  geom_text(aes(label = round(avg_line_sentiment, 3), color = sign), vjust = 1.75) +
  scale_color_manual(values = c("Red", "Green")) + 
  scale_x_discrete(name = NULL, labels = labels) +
  guides(size = "none", color = "none") +
  labs(y = "") + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#1D1135", color = "#1D1135"),
    panel.background = element_rect(fill = "#1D1135", color = "#1D1135"),
    panel.border = element_blank(),
    axis.text.y = element_text(color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "longdash"),
    axis.text.x = element_markdown(color = "white", size = 15)
  )
```

## TF-IDF Analysis by Top Character

``` r
extra_stop_words <- c("computer", "picard", "beverly", "crusher")

# one word phrases
trek_words <- computer %>%
  filter(char %in% top_characters) %>% # only inclue chars in graphic
  unnest_tokens(word, line) %>% # tokenize lines into one word per row
  anti_join(stop_words, by = "word") %>% # remove stop words
  filter(!(word %in% extra_stop_words)) %>% # remove extra stop words
  select(char, word)

# perform tf-idf on tokens
char_tf_idf <- trek_words %>%
  add_count(word) %>% # count how often each work appears
  filter(n >= 5) %>% # remove infrequently used tokens
  count(word, char) %>% # create a column counting each token by char
  bind_tf_idf(word, char, n) %>% # create TF-IDF scores
  arrange(desc(tf_idf))

# prep for plotting
top_char_tf_idf <- char_tf_idf %>%
  left_join(char_ranks, by = "char") %>%
  group_by(rank, char) %>%
  top_n(5, tf_idf) %>%
  mutate(word_rank = rank(- tf_idf, ties = "random")) %>%
  ungroup() %>%
  filter(word_rank <= 5) 

tf_idf_plot <- top_char_tf_idf %>%
  ggplot(aes("", -word_rank)) + 
  geom_text(aes(label = word, size = tf_idf), color = "white", fontface = "bold") + 
  geom_text(aes(label = paste0("(", round(tf_idf, 4), ")"), size = 0.8 * tf_idf), vjust = 2, color = "white") +
  guides(size = "none") +
  scale_y_discrete(breaks = c(1:6)) +
  facet_wrap(~ factor(char, levels = top_characters), ncol = 5) + 
  labs(y = "", x = "") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#1D1135", color = "#1D1135"),
    panel.background = element_rect(fill = "#1D1135", color = "#1D1135"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_textbox(color = "white"),
    plot.subtitle = element_textbox(color = "white", width = unit(1, "npc"), margin = margin(b = 15)),
    strip.text = element_blank()
  )
```

## Combine Plots

``` r
# combine plots
sent_plot /
  tf_idf_plot + 
  plot_annotation(
    tag_levels = 'A',
    title = "<b style='color:#8AE6FF'>Star Trek: The Next Generation</b><br><i style='color:#8750F5'>Interactions with the Computer</i>",
    subtitle = "<br><b>(A) Sentiment Analysis by Character and Interaction Type:</b> Average sentiment value of lines spoken by each character by interaction type. Size represents absolute sentiment. Color represents sign (<b style='color:red'>negative</b> or <b style='color:green'>positive</b>). Exact sentiment value shown below the points.
    <br><br>
    <b>(B)TF-IDF Analysis by Character:</b> Top 5 words which were most unique to that character compared to the other primary characters as determined by term frequency-inverse document frequency (TF-IDF) analysis. Terms are ranked from highest to lowest TF-IDF score, with the exact value provided below the word.
    <br><br>
    <i style='color:#8750F5'> Characters arranged from left to right by the number of interactions with the computer.</i>",
    caption = "Source: <b>speechinteraction.org</b> | Visualization: <b>NCruickshank (@shank4494)</b> | #TidyTuesday",
    theme = theme(
      plot.title = element_textbox(color = "white", size = 28, hjust = 0.5, halign = 0.5),
      plot.subtitle = element_textbox(color = "white", width = unit(1, "npc"), margin = margin(b = 15), halign = 0.5),
      plot.caption = element_textbox(color = "#8750F5", size = 10, hjust = 0.5),
      plot.background = element_rect(fill = "#1D1135", color = "#1D1135"),
      panel.border = element_blank()
    )
  ) &
  theme(plot.tag = element_text(color = "white"))
```

![](20210817---Star-Trek-Commands_files/figure-gfm/Star%20Trek%20Character%20Computer%20Interactions-1.png)<!-- -->

# Tweets of Inspiration

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
    caption = "Tidy Tuesday week 34 \ndata courtesy of SpeechInteraction.org\nVisualization inspired by code from @neuroandstats"
  )

neuroandstats_plot
```

![](20210817---Star-Trek-Commands_files/figure-gfm/neuroandstats%20plot-1.png)<!-- -->
