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

## who votes for iceland?
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

## update original df
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
eurovision_iceland_plot <- vis2 %>%
  ggplot(aes(year, points)) + 
  # annotations
  ## title
  annotate(
    "text", x = 2000, y = 460, label = "Iceland at the Eurovision Song Contest",
    hjust = 0, family = f1, size = 12, fontface = "bold", color = "Yellow"
  ) + 
  ## subtitle
  annotate(
    "text", x = 2000, y = 410, label = "Legend shows top counties (ordered top to bottom) who vote for Iceland",
    hjust = 0, family = f2, size = 5, lineheight = 0.8, color = "grey70"
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
  ### 2021
  annotate(
    "segment", x = 2019.5, xend = 2019.5, y = 175, yend = 325,
    color = "grey70"
  ) +
  annotate(
    "segment", x = 2019.5, xend = 2020.25, y = 325, yend = 335,
    color = "grey70"
  ) +
  annotate(
    "text", x = 2016, y = 200, label = is_top_performances[[1]],
    hjust = 0, family = f2, size = 2, lineheight = 0.8, color = "grey70"
  ) +
  annotate(
    "text", x = 2016, y = 275, label = is_top_performances[[2]],
    hjust = 0, family = f2, size = 2, lineheight = 0.8, color = "grey70"
  ) +
  # will ferrell movie
  annotate(
    "segment", x = 2020, xend = 2020, y = 0, yend = 360,
    linetype = "dashed", color = "yellow"
  ) +
  annotate(
    "segment", x = 2019, xend = 2020, y = 370, yend = 360,
    linetype = "dashed", color = "yellow"
  ) +
  annotate(
    "text", x = 2018.5, y = 375, label = "Eurovision: Fire Saga",
    color = "yellow", family = f1, size = 5, hjust = 1
  ) +
  annotate(
    "text", x = 2018.5, y = 360, label = "Released on Netflix in 2020",
    color = "grey70", family = f1, size = 3, hjust = 1
  ) +
  
  labs(caption = "Data Source: Eurovision || Visualization: N. Cruickshank || #TidyTuesday") +
  
  # actual points accrued
  geom_bar(aes(fill = from_country2), color = "grey50", stat = "identity") + 
  scale_x_continuous(name = "Year", expand = c(0,0)) + 
  scale_y_continuous(name = "Total Points from Jury") +
  scale_fill_manual(values = c(
    "The Netherlands" = "navyblue",
    "Malta" = "indianred1",
    "Andorra" = "yellow",
    "Finland" = "lightskyblue",
    "Serbia" = "darkgoldenrod",
    "Other" = "grey70"
  ), name = NULL) + 
  theme_minimal(base_family = f2) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey30"),
    panel.background = element_rect(fill = "grey15", color = "grey15"),
    plot.background = element_rect(fill = "grey15", color = "grey15"),
    legend.text = element_text(size = 12, color = "grey70"),
    axis.text = element_text(color = "grey50"),
    axis.title = element_text(color = "grey50"),
    plot.caption = element_text(family = f2, color = "grey70", size = 10, hjust = 0.1)
  )

eurovision_iceland_plot

# dim 1000 x 550
#ggsave("Iceland at Eurovision.png", plot = eurovision_iceland_plot, width = 10, height = 5.5, dpi = 500)
