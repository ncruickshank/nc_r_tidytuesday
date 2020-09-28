Avatar: The Last Airbender Analysis
================
Nick Cruickshank
9/24/2020

## Load Information

``` r
# load libraries
library(extrafont)
library(forcats)
library(glue)
library(ngram)
library(readr)
library(shadowtext)
library(tidyverse)
library(tvthemes)
loadfonts(device = "win")
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

## Describe DF

``` r
knitr::kable(avatar %>% head())
```

|    id | book     |   book\_num | chapter                  |   chapter\_num | character           | full\_text                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | character\_words                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | writer                                                                                  | director    | imdb\_rating |
| ----: | :------- | ----------: | :----------------------- | -------------: | :------------------ | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------- | :---------- | -----------: |
|     1 | Water    |           1 | The Boy in the Iceberg   |              1 | Katara              | Water. Earth. Fire. Air. My grandmother used to tell me stories about the old days: a time of peace when the Avatar kept balance between the Water Tribes, Earth Kingdom, Fire Nation and Air Nomads. But that all changed when the Fire Nation attacked. Only the Avatar mastered all four elements; only he could stop the ruthless firebenders. But when the world needed him most, he vanished. A hundred years have passed, and the Fire Nation is nearing victory in the war. Two years ago, my father and the men of my tribe journeyed to the Earth Kingdom to help fight against the Fire Nation, leaving me and my brother to look after our tribe. Some people believe that the Avatar was never reborn into the Air Nomads and that the cycle is broken, but I haven’t lost hope. I still believe that, somehow, the Avatar will return to save the world.                                                                                                                                                                                                                                                                | Water. Earth. Fire. Air. My grandmother used to tell me stories about the old days: a time of peace when the Avatar kept balance between the Water Tribes, Earth Kingdom, Fire Nation and Air Nomads. But that all changed when the Fire Nation attacked. Only the Avatar mastered all four elements; only he could stop the ruthless firebenders. But when the world needed him most, he vanished. A hundred years have passed, and the Fire Nation is nearing victory in the war. Two years ago, my father and the men of my tribe journeyed to the Earth Kingdom to help fight against the Fire Nation, leaving me and my brother to look after our tribe. Some people believe that the Avatar was never reborn into the Air Nomads and that the cycle is broken, but I haven’t lost hope. I still believe that, somehow, the Avatar will return to save the world. | ‎Michael Dante DiMartino, Bryan Konietzko, Aaron Ehasz, Peter Goldfinger, Josh Stolberg | Dave Filoni |          8.1 |
|     2 | Water    |           1 | The Boy in the Iceberg   |              1 | Scene Description   | As the title card fades, the scene opens onto a shot of an icy sea before panning slowly to the left, revealing more towering icebergs drifting in the water as the shot rotates, moving over a large snow-covered area where the untouched blanket of snow is broken by two tracks of footsteps. The shot fades to another shot of the sea and of icebergs in blue water contrasted by the paleness of the sky beyond. As the shot once again pans to the left and rotates likewise, a small canoe comes into view, its motion through the water indicated by the wake left behind it. The shot zooms slightly on the canoe before cutting down to it, providing a side angle of the canoe and of the walls of ice rising on either side of the vessel. Two people, a boy and a girl, are sitting in the boat; the boy holds a spear at the ready, while the girl simply stares into the water on the other side of the boat. The shot cuts to an overhead view, revealing that a fish is swimming close to the surface right in front of the boy, who is focused on it, following its every movement. Cut to a frontal view of him. | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | ‎Michael Dante DiMartino, Bryan Konietzko, Aaron Ehasz, Peter Goldfinger, Josh Stolberg | Dave Filoni |          8.1 |
|     3 | Water    |           1 | The Boy in the Iceberg   |              1 | Sokka               | It’s not getting away from me this time. \[Close-up of the boy as he grins confidently over his shoulder in the direction of the girl.\] Watch and learn, Katara. This is how you catch a fish.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | It’s not getting away from me this time. Watch and learn, Katara. This is how you catch a fish.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | ‎Michael Dante DiMartino, Bryan Konietzko, Aaron Ehasz, Peter Goldfinger, Josh Stolberg | Dave Filoni |          8.1 |
|     4 | Water    |           1 | The Boy in the Iceberg   |              1 | Scene Description   | The shot pans quickly from the boy to Katara, who seems indifferent to his claim and turns back to her side of the boat again. Her expression changes to surprise; as the shot jumps behind her, looking down into the water over her shoulder, another fish quickly swims by her, close to where she sits. She shoots a quick glance toward her brother, removing the glove from her left hand. She stretches her arm out in the direction of the fish, taking a deep breath. Her look is a mixture of concentration and apprehension as she starts making a wavy motion with her wrist, moving her hand up and down.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                         |             |              |
| The s | hot wide | ns out; Sok | ka, still focused wholly | on the fish in | front of him, is co | mpletely unaware of what is happening behind his back. As Katara continues to move her hand, the water in front of Katara, just in front of the shot, starts to ripple. Suddenly, a bubble of water rises up from the ocean containing the newly trapped fish. NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | ‎Michael Dante DiMartino, Bryan Konietzko, Aaron Ehasz, Peter Goldfinger, Josh Stolberg Dave Filoni 8.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |                                                                                         |             |              |
|     5 | Water    |           1 | The Boy in the Iceberg   |              1 | Katara              | \[Happily surprised.\] Sokka, look\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Sokka, look\!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | ‎Michael Dante DiMartino, Bryan Konietzko, Aaron Ehasz, Peter Goldfinger, Josh Stolberg | Dave Filoni |          8.1 |
|     6 | Water    |           1 | The Boy in the Iceberg   |              1 | Sokka               | \[Close-up of Sokka; whispering.\] Sshh\! Katara, you’re going to scare it away. \[A look of bliss adorns his face. He licks his lips and wiggles his fingers, not taking his eyes off the fish.\] Mmmm … I can already smell it cookin’.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | Sshh\! Katara, you’re going to scare it away. Mmmm … I can already smell it cookin’.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | ‎Michael Dante DiMartino, Bryan Konietzko, Aaron Ehasz, Peter Goldfinger, Josh Stolberg | Dave Filoni |          8.1 |

## Exploratory Analysis

### Episode Breakdown by IMDB Rating

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
  geom_shadowtext(aes(label = chapter), size = 2.5) + 
  labs(
    title = "Avatar Episode IMDb Ratings",
    subtitle = glue("Best episode was '{best_chap}' with a rating of {best_chap_rating}\n
                    Worst episode was '{worst_chap}' with a rating of {worst_chap_rating}"),
    x = "Book",
    y = "Chapter",
    fill = "IMBd Rating"
  ) +
  theme_avatar(title.font = "Herculanum",
               text.font = "Herculanum") + 
  theme(
    legend.position = c(0.2,0.96),
    legend.direction = "horizontal",
    legend.box.background = element_rect(),
    panel.grid = element_blank()
  )
```

![](avatar_analysis_files/figure-gfm/episode%20breakdown-1.png)<!-- -->

### Director and Writer Analysis

Which directors or writers were associated with the most succesful
episodes of Avatar?

### “Cabbages” Trending

Track the running joke for the “MY CABBAGES” joke.

### Zuko Transformation Analysis

## Machine Learning: Predict IMDb score based on characters

For each line of each episode, create a column which counts the number
of words (excluding stop words?) that line contains.

``` r
# define strings to trim from dataset.
intro1 <- "Water. Earth. Fire. Air. My grandmother used to tell me"
intro2 <- "Long ago, the four nations lived together in harmony."

character_words <- avatar %>%
  filter(!(grepl(c(intro1,intro2), character_words)),
         character != "Scene Description") %>%
  group_by(book, book_num, chapter, chapter_num, character, writer, director, imdb_rating) %>%
  dplyr::summarise(
    words = wordcount(character_words)
  )
  #mutate(words = wordcount(character_words)) #doesn't work right, appears to work within a summarise
```

``` r
main_characters <- c("Aang", "Azula", "Iroh", "Katara", "Ozai", "Sokka", "Toph", "Zuko")

#font_import(pattern = "h", paths = "C:\\Windows\\Fonts\\")

character_words %>%
  filter(character %in% main_characters) %>%
  group_by(book, character) %>%
  dplyr::summarise(
    total_words = sum(words)
  ) %>%
  ggplot(aes(fct_reorder(character, total_words, .desc = TRUE), total_words)) + 
  geom_bar(aes(fill = book), stat = "identity", color = "black") + 
  scale_fill_manual(values = c(
    "Earth" = "tan4",
    "Fire" = "firebrick",
    "Water" = "royalblue2"
  )) +
  labs(
    title = "Avatar: Word Count By Character",
    x = "Character",
    y = "Total Words"
  ) +
  theme_avatar(title.font = "Herculanum",
               text.font = "Herculanum")
```

![](avatar_analysis_files/figure-gfm/word%20count%20distribution-1.png)<!-- -->

Pivot the dataframe wider, so each character gets a column whose values
are the number of words they had in the episode.

Apply the same logic for each writer and director, but instead the
column is boolean (i.e. If writer one is in s1e1, than the column value
is 1 for that episode, else 0).

Train-test split the resulting dataframe, with IMDb rating as the
y-value.

Assess numerous different models for accuracy.
