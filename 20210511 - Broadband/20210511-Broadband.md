20210511 - Broadband
================
Nick Cruickshank
6/20/2021

``` r
# libraries
library(cowplot) # in this case the script only uses this library for theme_cowplot()
library(glue)
library(patchwork) # better than cowplot for this project
library(readr)
library(rworldmap)
library(tidycensus)
library(tidyverse)
library(tigris)
library(usmap)
library(zipcodeR)
```

``` r
# data
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv') 
broadband <- broadband %>%
  janitor::clean_names()
broadband_zip <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband_zip.csv')
broadband_zip <- broadband_zip %>%
  janitor::clean_names()
```

``` r
# functions
RMSE <- function(error) {sqrt(mean(error^2))}
```

# Introduction

![Broabd Image from
NATaT](http://www.natat.org/wp-content/uploads/2016/04/broadband.jpg)

This weeks analysis comes from the [R Tidy Tuesday
Community 2021-05-11](https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-05-11)
project, which featured data provided by
[Microsoft](https://github.com/microsoft/USBroadbandUsagePercentages)
and [The
Verge](https://www.theverge.com/22418074/broadband-gap-america-map-county-microsoft-data)
on broadband data by country in America.

> If broadband access was a problem before 2020, the pandemic turned it
> into a crisis. As everyday businesses moved online, city council
> meetings or court proceedings became near-inaccessible to anyone whose
> connection couldn’t support a Zoom call. Some school districts started
> providing Wi-Fi hotspots to students without a reliable home
> connection. In other districts, kids set up in McDonald’s parking lots
> just to get a reliable enough signal to do their homework. After years
> of slowly widening, the broadband gap became impossible to ignore.

# Tidy Data

``` r
latlong <- geocode_zip(broadband_zip$postal_code)

## broadband
bd <- broadband %>%
  rename(c(
    "state" = "st",
    "fips" = "county_id"
    )) %>%
  mutate(
    group = 1,
    broadband_category = case_when(
      broadband_usage <= 0.1 ~ "<= 10%",
      broadband_usage < 0.9 ~ "> 10% and < 90%",
      broadband_usage >= 0.9 ~ ">= 90%"
    )
  )

## broadband zip
bz <- broadband_zip %>%
  rename(c(
    "zipcode" = "postal_code",
    "state" = "st",
    "fips" = "county_id"
    )) %>%
  mutate(
    zipcode = as.character(zipcode),
    group = 1,
    broadband_category = case_when(
      broadband_usage <= 0.1 ~ "<= 10%",
      #broadband_usage < 0.9 ~ NULL,
      broadband_usage >= 0.9 ~ ">= 90%"
    )
    ) %>%
  left_join(latlong)
```

# Broadband Distribution by Country

For the primary data source, I decided to do a slight twist on [The
Verge’s](https://www.theverge.com/22418074/broadband-gap-america-map-county-microsoft-data)
plot. I elected to highlight counties with less than 10% broadband usage
*and* counties with greater than 90% broadband usage. While including
the “\>= 90%” group provides an additional layer to the graph, it does
not suggest any interesting patterns. There are very few counties that
fall into this group, and those that do appear to be scattered rather
than clustered.

``` r
bd_map <- bd %>%
  select(fips, broadband_category)

broadband_map <- plot_usmap(regions = "counties", data = bd_map, values = "broadband_category", color = "navy", size = 0.1) + 
  scale_fill_viridis_d() + 
  labs(
    title = "Broadband Internet Access in America by County (as of October 2020)",
    fill = "Percent of people per county using broadband speed internet"
  ) + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightblue1"),
    plot.background = element_rect(fill = "lightblue1")
    )

broadband_map
```

![](20210511-Broadband_files/figure-gfm/broadband_choropleth-1.png)<!-- -->

# Comparing Broadband Access to Census Data

Next I decided to briefly explore what features may help predict county
broadband usage. Some quick research led to an [article by Reddick et
al](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7480260/) which
suggests that features such as **population size** and **socio-economic
status** (among others) may contribute to broadband access. These
features are readily available through [census
data](https://data.census.gov/cedsci/).

``` r
key <- as.character(read.delim("census_api_key.txt", header = FALSE)$V1)
census_api_key(key)
#v19 <- load_variables(2019, "acs5", cache = TRUE)
```

## Population by County

``` r
census_pop <- get_acs(geography = "county",
                  variables = c(total_pop = "B01003_001"),
                  year = 2019) %>%
  janitor::clean_names()

pop_map <- census_pop %>%
  rename(c(
    "fips" = "geoid",
    "total_pop" = "estimate"
  )) %>%
  mutate(
    pop_category = case_when(
      total_pop < 250000 ~ "0 - 250k",
      total_pop < 500000 ~ "250k - 500k",
      total_pop < 750000 ~ "500k - 750k",
      total_pop < 1000000 ~ "750k - 1000k",
      total_pop >= 1000000 ~ "More Than 1000k+",
    )
  ) %>%
  select(fips, pop_category)
```

### Choropleth

``` r
pop_map <- plot_usmap(regions = "counties", data = pop_map, values = "pop_category", color = "navy", size = 0.1) + 
  scale_fill_viridis_d() + 
  labs(
    title = "Estimated population per county in 2019",
    fill = "Binned Population Size"
  ) + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightblue3"),
    plot.background = element_rect(fill = "lightblue3"),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

pop_map
```

![](20210511-Broadband_files/figure-gfm/population_choropleth-1.png)<!-- -->

### Linear Model to Predict Broadband Usage by County Population

``` r
census_pop_join <- census_pop %>%
  rename(c(
    "fips" = "geoid",
    "total_pop" = "estimate"
    ))  %>%
  mutate(fips = as.double(fips)) %>%
  select(fips, name, total_pop)

max_pop <- max(census_pop_join$total_pop)

bd_pop <- bd %>% 
  left_join(census_pop_join) %>%
  mutate(
    broadband_usage = as.double(broadband_usage),
    scale_pop = round(total_pop / max_pop, 4)
  ) %>%
  filter(!(is.na(scale_pop)))

# make the linear model
bd_pop_model <- lm(broadband_usage ~ scale_pop, data = bd_pop)

## get equation
bd_pop_model_intercept <- as.double(coef(bd_pop_model)["(Intercept)"])
bd_pop_model_weight <- as.double(coef(bd_pop_model)["scale_pop"])
bd_pop_model_rsquared <- summary(bd_pop_model)$r.squared
bd_pop_model_equation <- paste0("broadband_usage = ", round(bd_pop_model_intercept, 2), " + ", round(bd_pop_model_weight, 2), "scaled_pop",
                                "\n(R Squared = ", round(bd_pop_model_rsquared, 2), ")")
bd_pop_model_equation_plot <- data.frame(scale_pop = 0.75, broadband_usage = 0.125, label = bd_pop_model_equation)

## evaluate model fit
bd_pop_model_rmse <- round(RMSE(bd_pop_model$residuals),2)
mean_broadband_usage <- mean(bd_pop$broadband_usage, na.rm = TRUE)
bd_pop_scatter_index <- round(RMSE(bd_pop_model$residuals) / mean_broadband_usage,2)

## plot the relationship
bd_pop_plot <- bd_pop %>%
  ggplot(aes(scale_pop, broadband_usage)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "yellow") +
  geom_label(data = bd_pop_model_equation_plot, aes(label = label), alpha = 0.5) + 
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  labs(
    title = "County Population x County Broadband Usage",
    subtitle = glue("Good positive linear correlation with an RMSE of {bd_pop_model_rmse} and a scatter index of {bd_pop_scatter_index}"),
    x = "Scaled Population\n(County Population / Max of Country Population Sizes)",
    y = "Broadband Usage"
  ) + 
  theme_cowplot() + 
  theme(
    plot.background = element_rect(fill = "lightblue3", color = "black"),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

bd_pop_plot
```

![](20210511-Broadband_files/figure-gfm/lm_bd_pop-1.png)<!-- -->

``` r
summary(bd_pop_model)
```

    ## 
    ## Call:
    ## lm(formula = broadband_usage ~ scale_pop, data = bd_pop)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8722 -0.1381 -0.0257  0.1092  0.7442 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.255838   0.003286   77.86   <2e-16 ***
    ## scale_pop   2.266334   0.095382   23.76   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1756 on 3132 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.1527, Adjusted R-squared:  0.1525 
    ## F-statistic: 564.6 on 1 and 3132 DF,  p-value: < 2.2e-16

## Average Income By County

``` r
census_income <- get_acs(geograph = "county",
                         variables = c(median_income = "B19013_001"),
                         year = 2019) %>%
  janitor::clean_names()

income_map <- census_income %>%
  rename(c(
    "fips" = "geoid",
    "median_income" = "estimate"
  )) %>%
  mutate(
    median_income_d = case_when(
      median_income < 20000 ~ "$0 - $20k",
      median_income < 40000 ~ "$20k - $40k",
      median_income < 60000 ~ "$40k - $60k",
      median_income < 80000 ~ "$60k - $80k",
      median_income < 100000 ~ "$80k - $100k",
      median_income >= 100000 ~ "More Than $100k"
    )
  ) %>%
  select(fips, median_income_d)
```

### Choropleth

``` r
income_map <- plot_usmap(regions = "county", data = income_map, values = "median_income_d", color = "navy", size = 0.1) + 
  scale_fill_viridis_d() + 
  labs(
    title = "Median household annual income per county in 2019",
    fill = "Binned Median Household Income"
  ) + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightblue3"),
    plot.background = element_rect(fill = "lightblue3"),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

income_map
```

![](20210511-Broadband_files/figure-gfm/median_income_choropleth-1.png)<!-- -->

### Linear Model to Predict Broadband Usage by County Population

``` r
census_income_join <- census_income %>%
  rename(c(
    "fips" = "geoid", 
    "median_income" = "estimate"
  )) %>%
  mutate(fips = as.double(fips)) %>%
  select(fips, median_income)

max_income = max(census_income$estimate)

bd_pop_income <- bd_pop %>%
  left_join(census_income_join) %>%
  mutate(scale_income = round(median_income / max_income, 4)) %>%
  filter(!(is.na(scale_income)))

# create the linear model
bd_income_model <- lm(formula = broadband_usage ~ scale_income, data = bd_pop_income)

## get the equation
bd_income_model_intercept <- as.double(coef(bd_income_model)["(Intercept)"])
bd_income_model_weight <- as.double(coef(bd_income_model)["scale_income"])
bd_income_model_rsquared <- summary(bd_income_model)$r.squared
bd_income_model_equation <- paste0("broadband_usage = ", round(bd_income_model_intercept, 2), " + ", round(bd_income_model_weight, 2), "scale_income",
                                   "\n(R Squared = ", round(bd_income_model_rsquared, 2), ")")
bd_income_model_equation_plot <- data.frame(scale_income = 0.75, broadband_usage = 0.125, label = bd_income_model_equation)

## evaluate the model fit
bd_income_model_rmse = round(RMSE(bd_income_model$residuals), 2)
bd_income_scatter_index <- round(RMSE(bd_income_model$residuals) / mean_broadband_usage, 2)

bd_income_plot <- bd_pop_income %>%
  ggplot(aes(scale_income, broadband_usage)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "yellow") + 
  geom_label(data = bd_income_model_equation_plot, aes(label = label), alpha = 0.75) + 
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  labs(
    title = "Median Household Income x County Broadband Usage",
    subtitle = glue("Stronger linear relationship with an RMSE of {bd_income_model_rmse} and a scatter index of {bd_income_scatter_index}"),
    x = "Scaled Median Household Income\n(Median Household Income / Max of Median Household Incomes)",
    y = "Broadband Usage"
  ) + 
  theme_cowplot() + 
  theme(
    plot.background = element_rect("lightblue3", color = "black"),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

bd_income_plot
```

![](20210511-Broadband_files/figure-gfm/lm_bd_income-1.png)<!-- -->

``` r
summary(bd_income_model)
```

    ## 
    ## Call:
    ## lm(formula = broadband_usage ~ scale_income, data = bd_pop_income)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.55129 -0.10375 -0.01027  0.10029  0.68666 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.18053    0.01029  -17.55   <2e-16 ***
    ## scale_income  1.22307    0.02647   46.21   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1471 on 3132 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.4054, Adjusted R-squared:  0.4052 
    ## F-statistic:  2135 on 1 and 3132 DF,  p-value: < 2.2e-16

## Linear Model to Predict Broadband Usage by County Population AND Median Household Income

``` r
bd_all_model <- lm(formula = broadband_usage ~ scale_pop + scale_income, data = bd_pop_income)

## get model equation
bd_all_model_intercept <- as.double(coef(bd_all_model)["(Intercept)"])
bd_all_model_pop_weight <- as.double(coef(bd_all_model)["scale_pop"])
bd_all_model_income_weight <- as.double(coef(bd_all_model)["scale_income"])


## evaluate the model
bd_all_model_rsquared <- summary(bd_all_model)$r.squared
bd_all_model_rmse <- round(RMSE(bd_all_model$residuals), 2)
bd_all_model_scatter_index <- round(RMSE(bd_all_model$residuals) / mean_broadband_usage, 2)

## knit equation together
bd_all_model_equation <- paste0("[Broadband Usage] = ", round(bd_all_model_intercept, 2), " + ", 
                                round(bd_all_model_pop_weight, 2), "[Scaled Population] + ", 
                                round(bd_all_model_income_weight, 2), "[Scaled Median Income]",
                                " (R Squared = ", round(bd_all_model_rsquared, 2), 
                                ", RMSE = ", bd_all_model_rmse, 
                                ", Scatter Index = ", bd_all_model_scatter_index,")")

summary(bd_all_model)
```

    ## 
    ## Call:
    ## lm(formula = broadband_usage ~ scale_pop + scale_income, data = bd_pop_income)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.09103 -0.09700 -0.00977  0.09150  0.70205 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.14840    0.01001  -14.82   <2e-16 ***
    ## scale_pop     1.36327    0.07939   17.17   <2e-16 ***
    ## scale_income  1.10030    0.02630   41.84   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1406 on 3131 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.4566, Adjusted R-squared:  0.4562 
    ## F-statistic:  1315 on 2 and 3131 DF,  p-value: < 2.2e-16

### Plot all data together

``` r
overall_plot <- ((pop_map + bd_pop_plot + 
  plot_layout(ncol = 1, heights = c(3,1))) | 
  broadband_map |
  (income_map + bd_income_plot + 
  plot_layout(ncol = 1, heights = c(3,1)))) +
  plot_layout(widths = c(1,2,1)) + 
  plot_annotation(
    title = "Broadband Internet Access in America by County: Comparisons to Population and Median Household Income",
    subtitle = glue("Using both county population and median household income as features, the linear model used to predict broadband internet usage is as follows:\n{bd_all_model_equation}"),
    caption = "Source: The Verge (https://www.theverge.com/22418074/broadband-gap-america-map-county-microsoft-data) and USA 2019 Census Data (https://data.census.gov/cedsci/)",
    theme = theme(
      plot.title = element_text(size = 30),
      plot.subtitle = element_text(size = 20),
      plot.caption = element_text(size = 16)
        )
  )

overall_plot
```

![](20210511-Broadband_files/figure-gfm/Broadband%20Access%20Compared%20to%20Population%20and%20Income-1.png)<!-- -->
