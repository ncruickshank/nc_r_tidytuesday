Spotify Phish Analysis
================
Nick Cruickshank
1/8/2022

-   [Introduction](#introduction)
    -   [Documentation](#documentation)
-   [Analysis](#analysis)
-   [Visualization](#visualization)
    -   [Timeline of Phish Songs](#timeline-of-phish-songs)
    -   [Average Audio Feature Metrics for Phish
        Songs](#average-audio-feature-metrics-for-phish-songs)

![](https://cdn.wallpapersafari.com/32/67/pE9vy8.png)

# Introduction

This week’s [Tidy Tuesday
Project](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-04)
is technically a “Bring your own data” week. As such, I chose to get
some practice connecting to data sources through the use of APIs. I also
chose this oppurtunity to play around some more with
[Spotify](https://developer.spotify.com/) data. Namely, for one of my
favorite bands, [**Phish**](https://en.wikipedia.org/wiki/Phish)!

## Documentation

[Spotify Developer Data
Dictionary](https://developer.spotify.com/documentation/web-api/reference/#/operations/get-audio-features)

# Analysis

``` r
# libraries
library(forcats)
library(ggtext)
library(glue)
library(here)
```

    ## here() starts at C:/Users/nccru/OneDrive/Documents/GitHub/nc_r_tidytuesday

``` r
library(patchwork)
library(readr)
library(showtext)
```

    ## Loading required package: sysfonts

    ## Loading required package: showtextdb

``` r
library(spotifyr)
library(systemfonts)
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
here <- here()

## authenticate api
## retrieve these keys from https://developer.spotify.com/dashboard/
client_id = read_file(glue("{here}/spotify_client_id.txt"))
Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
client_secret = read_file(glue("{here}/spotify_client_secret.txt"))
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

### use client_id and client_secret to get an access_token
access_token <- get_spotify_access_token()

## download the relevant data set
spotify <- get_artist_audio_features('phish')

## preliminary tidy
phish_studio_albums <- read_csv(glue("{here}/2022/2022-01-04 - Bring Your Own Data/phish_studio_albums.csv"))
```

    ## Rows: 18 Columns: 2

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): Album Name, Release Date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
phish <- spotify %>%
  select(artist_name, album_name, album_release_year, track_name, disc_number, track_number,
         danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo, 
         time_signature, duration_ms, key_name, mode_name, key_mode) %>%
  mutate(
    track_name = str_to_title(track_name),
    track_name_clean = str_remove(track_name, " - (Live|Bonus Track)"),
    track_name_clean = str_remove(track_name_clean, " At Madison Square Garden, New Year's Eve [0-9]{4}"),
    track_name_clean = str_remove(track_name_clean, " (At|In) (Hampton|Brooklyn)(, [0-9]{4})?"),
    live_tag = if_else(album_name %in% phish_studio_albums$`Album Name`, 0, 1)
  )
```

# Visualization

**Limitations:** Likely missing a few data points due to songs names
with a syntax of \[Song Name\] + \[Live at Venue\].

``` r
# average metrics per song
# first need to roll up to one row per song (collapse all live versions)

## emulate song list
song_titles <- phish %>%
  count(track_name_clean, sort = TRUE) %>%
  # more or less removes any funky naming
  filter(n > 1) %>%
  pull(track_name_clean)

phish_songs <- phish %>%
  filter(track_name_clean %in% song_titles) %>%
  group_by(track_name_clean) %>%
  dplyr::summarise(
    occurrences = n(),
    duration_ms = mean(duration_ms),
    min_year = min(album_release_year),
    mean_year = round(mean(album_release_year)),
    max_year = max(album_release_year),
    danceability = mean(danceability),
    energy = mean(energy),
    loudness = mean(loudness),
    speechiness = mean(speechiness),
    acousticness = mean(acousticness),
    instrumentalness = mean(instrumentalness),
    liveness = mean(liveness),
    live_tag = mean(live_tag),
    valence = mean(valence),
    tempo = mean(tempo)
  )
```

``` r
# values

## colors
phish_light_blue <- "#A8C4D0"
phish_blue <- "#176282"
phish_red <- "#c33833"
phish_dark_red <- "#7A2320"

## fonts
fonts <- system_fonts()

georgia_path <- filter(fonts, name == "Georgia")$path
font_add("Georgia", georgia_path)
showtext_auto()

f1 <- "Georgia"
```

Use the idea of phish donuts as points on a scatter plot.

## Timeline of Phish Songs

``` r
top_phish_songs <- phish %>%
  count(track_name_clean, sort = TRUE) %>%
  head(100) %>%
  pull(track_name_clean)

phish_songs %>%
  filter(track_name_clean %in% top_phish_songs) %>%
  mutate(track = paste0(track_name_clean, " (", occurrences, ")")) %>%
  ggplot() +
  geom_linerange(aes(xmin = min_year, xmax = max_year, y = fct_reorder(track, mean_year, .desc = TRUE)), color = phish_dark_red) +
  geom_point(
    aes(mean_year, fct_reorder(track, mean_year, .desc = TRUE)), 
    color = phish_red, fill = phish_blue, shape = 21, size = 2
  ) +
  scale_y_discrete(name = NULL) +
  scale_x_continuous(name = NULL) +
  labs(
    title = "<b>Timeline of the Top 100 Played Phish Songs</b>",
    subtitle = "Mean year amongst all occurrences (in parenthesis) and range"
  ) + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = phish_blue),
    axis.text = element_text(color = phish_light_blue, family = f1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = phish_light_blue, linetype = "dashed"),
    panel.grid.minor.x = element_blank(),
    plot.title = element_textbox(color = phish_light_blue, family = f1, hjust = 0.5, size = 18),
    plot.subtitle = element_text(color = phish_light_blue, family = f1, hjust = 0.5, size = 10, face = "italic")
  )
```

![](Phish-Spotify_files/figure-gfm/Phish%20Song%20Timeline-1.png)<!-- -->

## Average Audio Feature Metrics for Phish Songs

Weight by frequency of play.

``` r
phish_features <- phish_songs %>%
  uncount(occurrences) %>%
  select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, live_tag, valence) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  dplyr::summarise(value = mean(value)) %>%
  # remove unwanted features
  filter(name != "liveness") %>%
  mutate(
    # rename liveness
    name = if_else(name == "live_tag", "liveness", name),
    # normalize loudness (typically between -60 and 0)
    value = if_else(name == "loudness", (60 + value) / 60, value),
    # define negative space, as all metrics are on a 0 to 1 scale
    zz_empty = 1 - value
  )
```

``` r
# def donut function
plot_phish_donuts <- function(df, feature, hsize = 4, subtitle) {
  donut_df <- df %>%
    filter(name == feature) %>%
    pivot_longer(cols = c("value", "zz_empty"), names_to = "cat") 
  
  feature_value <- round(filter(donut_df, cat == "value")$value, 2)
  feature_title <- str_to_title(feature)
  
  donut_df %>%
    ggplot(aes(x = hsize, y = value, fill = cat)) + 
    geom_col() + 
    scale_fill_manual(values = c(phish_red, phish_dark_red)) + 
    coord_polar(theta = "y") + 
    xlim(c(0.2, hsize + 0.5)) + 
    labs(
      title = feature_title,
      subtitle = str_wrap(subtitle, 80)
    ) + 
    annotate("text", x = 0.5, y = 0, label = feature_value, color = phish_light_blue, size = 10, family = f1) + 
    theme_void() + 
    theme(
      plot.background = element_rect(fill = phish_blue, color = phish_blue),
      #panel.background = element_rect(fill = phish_blue),
      plot.title = element_text(color = phish_light_blue, hjust = 0.5, size = 20, face = "bold", family = f1),
      plot.subtitle = element_text(color = phish_light_blue, hjust = 0.5, size = 10, face = "italic", family = f1),
      legend.position = "none"
    )
}
```

``` r
# create sub plots for patching together

## acousticness
ac_sub <- "Confidence song is acoustic (0 to 1)"
ac_plot <- plot_phish_donuts(phish_features, "acousticness", subtitle = ac_sub)

## danceability
da_sub <- "Suitability for dancing (0 to 1)"
da_plot <- plot_phish_donuts(phish_features, "danceability", subtitle = da_sub)

## energy
en_sub <- "Perceptual measure of intensity and activity (0 to 1)"
en_plot <- plot_phish_donuts(phish_features, "energy", subtitle = en_sub)

## instrumentalness
in_sub <- "Likelihood of no vocals (0 to 1)"
in_plot <- plot_phish_donuts(phish_features, "instrumentalness", subtitle = in_sub)

## liveness
li_sub <- "Proportion of tracks played live (0 to 1)"
li_plot <- plot_phish_donuts(phish_features, "liveness", subtitle = li_sub)

## loudness
lo_sub <- "Average track loudness (normalized from -60dB to 0dB)"
lo_plot <- plot_phish_donuts(phish_features, "loudness", subtitle = lo_sub)

## speechiness
sp_sub <- "How much spoken word there is (0 to 1)"
sp_plot <- plot_phish_donuts(phish_features, "speechiness", subtitle = sp_sub)

## valence
va_sub <- "Positiveness conveyed by track (0 to 1)"
va_plot <- plot_phish_donuts(phish_features, "valence", subtitle = va_sub)
```

``` r
# patchwork donuts together
ac_plot + da_plot + en_plot + in_plot + li_plot + lo_plot + sp_plot + va_plot + 
  plot_layout(ncol = 2) + 
  plot_annotation(
    title = "Sharing in the Groove",
    subtitle = "Average audio features of <b>PHISH Songs</b> on Spotify<br>",
    theme = theme(
      plot.background = element_rect(fill = phish_blue),
      panel.background = element_rect(fill = phish_blue),
      plot.title = element_text(color = phish_light_blue, hjust = 0.5, size = 35, face = "bold", family = f1),
      plot.subtitle = element_textbox(color = phish_light_blue, hjust = 0.5, size = 20, face = "italic", family = f1)
    )
  )
```

![](Phish-Spotify_files/figure-gfm/Phish%20Spotify%20Features-1.png)<!-- -->
