Tuskegee Airmen
================
Nick Cruickshank
2/13/2022

![](https://www.history.com/.image/ar_16:9%2Cc_fill%2Ccs_srgb%2Cfl_progressive%2Cq_auto:good%2Cw_1200/MTc4MzcyODE1NDc1NDUxNDk2/tuskegee-airmen-gettyimages-469556647.jpg)

# Introduction

This week’s [Tidy Tuesday]() data visualization project comes from the
[Tuskegee Airmen
Challenge](https://github.com/lang1023/Tuskegee-Airman-Challenge/blob/main/Tuskegee%20Airmen%20Challenge.xlsx)
as part of the [Verterans Advocacy Tableau
Group](https://usergroups.tableau.com/airmenchallegekickoff).

Some background information on the Tuskegee Airmen:  
\>The Tuskegee Airmen /tʌsˈkiːɡiː/\[1\] were a group of primarily
African American military pilots (fighter and bomber) and airmen who
fought in World War II. They formed the 332d Expeditionary Operations
Group and the 477th Bombardment Group of the United States Army Air
Forces.

> The Tuskegee Airmen were the first African-American military aviators
> in the United States Armed Forces. During World War II, black
> Americans in many U.S. states were still subject to the Jim Crow laws
> and the American military was racially segregated, as was much of the
> federal government. The Tuskegee Airmen were subjected to
> discrimination, both within and outside of the army.

> Before the Tuskegee Airmen, no African-American had been a U.S.
> military pilot. In 1917, African-American men had tried to become
> aerial observers but were rejected.\[6\] African-American Eugene
> Bullard served in the French air service during World War I because he
> was not allowed to serve in an American unit. Instead, Bullard
> returned to infantry duty with the French.\[7\]

> The racially motivated rejections of World War I African-American
> recruits sparked more than two decades of advocacy by
> African-Americans who wished to enlist and train as military aviators.

> Because of the restrictive nature of selection policies, the situation
> did not seem promising for African-Americans, since in 1940 the U.S.
> Census Bureau reported there were only 124 African-American pilots in
> the nation.\[10\] The exclusionary policies failed dramatically when
> the Air Corps received an abundance of applications from men who
> qualified, even under the restrictive requirements. Many of the
> applicants had already participated in the Civilian Pilot Training
> Program, unveiled in late December 1938 (CPTP). Tuskegee University
> had participated since 1939.

# Analysis

``` r
# libraries
library(ggtext)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(readr)
library(showtext)
```

    ## Loading required package: sysfonts

    ## Loading required package: showtextdb

``` r
library(sysfonts)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v dplyr   1.0.7
    ## v tibble  3.1.6     v stringr 1.4.0
    ## v tidyr   1.1.4     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x lubridate::date()        masks base::date()
    ## x dplyr::filter()          masks stats::filter()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x lubridate::union()       masks base::union()

``` r
# data
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')
```

    ## Rows: 1006 Columns: 16

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (13): name, last_name, first_name, rank_at_graduation, class, graduated...
    ## dbl   (1): number_of_aerial_victory_credits
    ## dttm  (2): graduation_date, reported_lost_date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
knitr::kable(head(airmen))
```

| name                  | last_name | first_name    | graduation_date | rank_at_graduation | class     | graduated_from | pilot_type    | military_hometown_of_record | state | aerial_victory_credits | number_of_aerial_victory_credits | reported_lost | reported_lost_date | reported_lost_location | web_profile                                     |
|:----------------------|:----------|:--------------|:----------------|:-------------------|:----------|:---------------|:--------------|:----------------------------|:------|:-----------------------|---------------------------------:|:--------------|:-------------------|:-----------------------|:------------------------------------------------|
| Adams, John H., Jr.   | Adams     | John H., Jr.  | 1945-04-15      | 2nd Lt             | SE-45-B   | TAAF           | Single engine | Kansas City                 | KS    | NA                     |                                0 | NA            | NA                 | NA                     | <https://cafriseabove.org/john-h-adams-jr/>     |
| Adams, Paul           | Adams     | Paul          | 1943-04-29      | 2nd Lt             | SE-43-D   | TAAF           | Single engine | Greenville                  | SC    | NA                     |                                0 | NA            | NA                 | NA                     | <https://cafriseabove.org/paul-adams/>          |
| Adkins, Rutherford H. | Adkins    | Rutherford H. | 1944-10-16      | 2nd Lt             | SE-44-I-1 | TAAF           | Single engine | Alexandria                  | VA    | NA                     |                                0 | NA            | NA                 | NA                     | <https://cafriseabove.org/rutherford-h-adkins/> |
| Adkins, Winston A.    | Adkins    | Winston A.    | 1944-02-08      | 2nd Lt             | TE-44-B   | TAAF           | Twin engine   | Chicago                     | IL    | NA                     |                                0 | NA            | NA                 | NA                     | NA                                              |
| Alexander, Halbert L. | Alexander | Halbert L.    | 1944-11-20      | 2nd Lt             | SE-44-I   | TAAF           | Single engine | Georgetown                  | IL    | NA                     |                                0 | NA            | NA                 | NA                     | <https://cafriseabove.org/halbert-l-alexander/> |
| Alexander, Harvey R.  | Alexander | Harvey R.     | 1944-04-15      | 2nd Lt             | TE-44-D   | TAAF           | Twin engine   | Georgetown                  | IL    | NA                     |                                0 | NA            | NA                 | NA                     | <https://cafriseabove.org/harvey-r-alexander/>  |

# Visualization

x-axis equals day-month, y-axis equals year. Points are pilot graduation
dates, color is pilot type, size is number of graduates.

``` r
# values

## fonts
font_add_google(family = "Righteous", name = "righteous")
showtext_auto()
f1 <- "Righteous"
```

``` r
airmen_dates <- airmen %>%
  # I only care about single or twin engine pilots, they are the majority anyway
  filter(pilot_type %in% c("Single engine", "Twin engine")) %>%
  mutate(
    # define timeframes
    year = as.double(format(graduation_date, "%Y")),
    day_of_year = yday(graduation_date)
  ) %>%
  count(year, day_of_year, pilot_type) 

# create legends for context

## all days
date_legend <- tibble(
  day = seq.Date(from = as.Date("1941-01-01"), to = as.Date("1941-12-31"), by = 1),
  year = as.double(format(day, "%Y")),
  day_of_year = yday(day),
  quarter = paste0("Q", quarter(day)),
  month = format(day, "%b")
)

## months specifically
months_legend <- date_legend %>%
  group_by(year, month) %>%
  dplyr::summarise(
    max = max(day_of_year),
    day_of_year = quantile(day_of_year, 0.5)
  ) %>%
  ungroup() %>%
  arrange(day_of_year)
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

``` r
## quarters specifically
quarter_legend <- date_legend %>%
  group_by(quarter) %>%
  dplyr::summarise(
    max = max(day_of_year),
    day_of_year = quantile(day_of_year, 0.5)
  ) %>%
  ungroup() %>%
  mutate(year = 1940)
```

``` r
ggplot() + 
  # quarter legend
  geom_text(
    data = quarter_legend,
    aes(x = day_of_year, y = year, label = quarter),
    family = f1
  ) +
  geom_vline(
    data = filter(quarter_legend, quarter != "Q4"),
    aes(xintercept = max),
    color = "tan3"
  ) + 
  # month legends
  geom_text(
    data = months_legend,
    aes(x = day_of_year, y = year, label = month),
    family = f1
  ) + 
  # plot graduation class size
  geom_point(
    data = airmen_dates, 
    aes(day_of_year, year, size = n, color = pilot_type)
  ) + 
  # scales, themes, etc
  scale_y_reverse(name = NULL) + 
  scale_x_continuous(breaks = months_legend$max, name = NULL) + 
  scale_size_continuous(name = "Graduation\nClass Size") + 
  scale_color_manual(name = "Pilot\nType", values = c(
    "Single engine" = "lightskyblue1",
    "Twin engine" = "skyblue3"
  )) +
  labs(
    title = "Tuskegee Airmen Graduation Classes",
    caption = "Data Source: Tuskegee Airmen Challenge |  Visualization: N. Cruickshank | #TidyTuesday"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(family = f1, hjust = 0.5),
    legend.text = element_text(family = f1),
    legend.key = element_rect(fill = "papayawhip"),
    legend.background = element_rect(fill = "papayawhip"),
    plot.background = element_rect(fill = "papayawhip"),
    panel.background = element_rect(fill = "papayawhip"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = f1),
    axis.ticks.x = element_blank(),
    plot.title = element_text(family = f1, hjust = 0.5, size = 16),
    plot.caption = element_text(family = f1, hjust = 0.5, size = 8)
  )
```

![](Tuskegee-Airmen_files/figure-gfm/Tuskegee%20Graduations-1.png)<!-- -->
