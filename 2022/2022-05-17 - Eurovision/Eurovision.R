# Eurovision

# libraries
library(showtext)
library(sysfonts)
library(tidyverse)

# data
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

# values
f1 <- "Shadows Into Light"
font_add_google(f1)
f2 <- "IBM Plex Sans"
font_add_google(f2)
showtext_auto()


# visualization
## IDEA ONE
### how did the release of Will Ferrell's movie impact Iceland's performance at
### at Eurovision?
### Likely most seen in televoting audience
## IDEA TWO
### which countries vote the most for iceland?


# who votes for iceland?
vis <- votes %>%
  filter(
    year >= 2000,
    to_country == "Iceland",
    points > 0,
    is.na(duplicate),
    jury_or_televoting == "J"
  )

## define who most commonly votes for iceland
is_top_voters <- vis %>%
  group_by(from_country) %>%
  summarise(
    mean_points = mean(points),
    n = n()
  ) %>%
  ungroup() %>%
  arrange(desc(mean_points)) %>%
  filter(n >= 3) %>% # they had to have voted for iceland at least a few times
  head(5) %>%
  pull(from_country)

## update vis
vis2 <- vis %>%
  mutate(
    from_country2 = ifelse(from_country %in% is_top_voters, from_country, "Other"),
    from_country2 = factor(from_country2, levels = append(is_top_voters, "Other"))
  ) %>%
  group_by(year, from_country2) %>%
  summarise(points = sum(points))

# find out what was happening during spike years
is_top_performances <- eurovision %>%
  filter(
    artist_country == "Iceland",
    year == 2009 | year == 2021
  ) %>%
  mutate(
    section = str_replace(section, "-", " "),
    section = str_to_title(section),
    label = paste0(
      "Event: ", event, 
      "\nSection: ", section, 
      "\nArtist:", artist, 
      "\nSong: ", song, 
      "\nRank: ", rank_ordinal, 
      "\nWinner: ", str_to_title(winner))
  ) %>%
  pull(label)

## plot
vis2 %>%
  ggplot(aes(year, points)) + 
  # annotations
  ## title
  annotate(
    "text", x = 2000, y = 475, label = "Iceland at the Eurovision Song Contest",
    hjust = 0, family = f1, size = 12, fontface = "bold", color = "Yellow"
  ) + 
  ## subtitle
  annotate(
    "text", x = 2000, y = 410, label = "Legend shows top counties (orderred top to bottom)\nwho vote for Iceland",
    hjust = 0, family = f2, size = 6, lineheight = 0.8, color = "grey70"
  ) +
  ## top performances
  ### 2009
  annotate(
    "segment", x = 2007.5, xend = 2007.5, y = 225, yend = 350,
    color = "grey70"
  ) +
  annotate(
    "segment", x = 2007.5, xend = 2008.25, y = 350, yend = 390,
    color = "grey70"
  ) +
  annotate(
    "text", x = 2004.5, y = 250, label = is_top_performances[[3]],
    hjust = 0, family = f2, size = 2, lineheight = 0.8, color = "grey70"
  ) +
  annotate(
    "text", x = 2004.5, y = 325, label = is_top_performances[[4]],
    hjust = 0, family = f2, size = 2, lineheight = 0.8, color = "grey70"
  ) +
  
  # actual points accrued
  geom_bar(aes(fill = from_country2), color = "grey50", stat = "identity") + 
  scale_x_continuous(name = "Year", expand = c(0,0)) + 
  scale_y_continuous(name = "Total Points from Jury") +
  scale_fill_manual(values = c(
    "The Netherlands" = "red",
    "Malta" = "blue",
    "Andorra" = "purple",
    "Finland" = "pink",
    "Serbia" = "yellow",
    "Other" = "grey80"
  ), name = NULL) + 
  theme_minimal(base_family = f2) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey30"),
    panel.background = element_rect(fill = "grey15", color = "grey15"),
    plot.background = element_rect(fill = "grey15", color = "grey15"),
    legend.text = element_text(size = 12, color = "grey50"),
    axis.text = element_text(color = "grey50"),
    axis.title = element_text(color = "grey50")
  )
