20210518 - Ask a Manager Survey
================
Nick Cruickshank
5/18/2021

``` r
# Load Libraries
library(forcats)
library(tidytuesdayR)
library(tidyverse)
```

``` r
# Get the Data
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')
```

    The salary survey a few weeks ago got a huge response — 24,000+ people shared their salaries and other info, which is a lot of raw data to sift through. Reader Elisabeth Engl kindly took the raw data and analyzed some of the trends in it and here’s what she found. (She asked me to note that she did this as a fun project to share some insights from the survey, rather than as a paid engagement.)
    
    This data does not reflect the general population; it reflects Ask a Manager readers who self-selected to respond, which is a very different group (as you can see just from the demographic breakdown below, which is very white and very female).

# Tidy Data Set

``` r
# filter for just America
survey$other_monetary_comp[is.na(survey$other_monetary_comp)] <- 0
sv <- survey %>%
  filter(currency == "USD") %>%
  mutate(
    # add additional salary columns
    other_monetary_comp = as.numeric(other_monetary_comp),
    total_annual_earnings = annual_salary + other_monetary_comp,
    bonus_proportion = other_monetary_comp / total_annual_earnings,
    # tidy industries
    industry = str_to_lower(industry)
    )
  
colnames(sv)
```

    ##  [1] "timestamp"                               
    ##  [2] "how_old_are_you"                         
    ##  [3] "industry"                                
    ##  [4] "job_title"                               
    ##  [5] "additional_context_on_job_title"         
    ##  [6] "annual_salary"                           
    ##  [7] "other_monetary_comp"                     
    ##  [8] "currency"                                
    ##  [9] "currency_other"                          
    ## [10] "additional_context_on_income"            
    ## [11] "country"                                 
    ## [12] "state"                                   
    ## [13] "city"                                    
    ## [14] "overall_years_of_professional_experience"
    ## [15] "years_of_experience_in_field"            
    ## [16] "highest_level_of_education_completed"    
    ## [17] "gender"                                  
    ## [18] "race"                                    
    ## [19] "total_annual_earnings"                   
    ## [20] "bonus_proportion"

# Exploratory Analysis

## Group by industry

Looks like the survey must have allowed for open response in the
industry field. I could manually group tidy the industries into to cut
out all the noise through use of a `case_when()` statement within a
`mutate()` clause (i.e. when `str_detect(industry, "academ(ia|ic|y)") ==
TRUE ~ "academia"`). However, that would require a lot of time to
carefully parse through the 800+ unique `industry` entries, and would
still likely be subject to my own biases. It’s likely easier (and more
meaningful) simply find a reasonable cutoff point to `filter()` the data
set by.

The following graph plots the top 50 industries by number of survey
responses. The number 50 is chosen somewhat arbitrarily, but serves to
show where a plausible cutoff point would be for trimming
difficult-to-use entries. From this graph, it looks like `filtering` for
industries with \> 50 responses should be appropriate.

``` r
sv %>%
  filter(!(is.na(industry))) %>%
  group_by(industry) %>%
  dplyr::summarise(responses = n()) %>%
  arrange(desc(responses)) %>%
  head(50) %>%
  ggplot(aes(fct_reorder(industry, responses), responses)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = responses), hjust = -1) +
  coord_flip() + 
  labs(
    title = "Survey responses by industry",
    x = ""
  )
```

![](20210518---Ask-a-Manager-Survey_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# create industries rollup df for plotting
industries <- sv %>%
  filter(
    total_annual_earnings < 100000000 # boxplot revealed a clear outlier here
  ) %>%
  group_by(industry) %>%
  dplyr::summarise(
    responses = n(),
    mean_salary = mean(total_annual_earnings),
    sd_salary = sd(total_annual_earnings),
    min_salary = min(total_annual_earnings),
    max_salary = max(total_annual_earnings),
    mean_proportional_bonus = mean(bonus_proportion, na.rm = TRUE)
  ) %>%
  filter(
    responses > 50 # to cut out the noisy free response entries
  )

industries_list <- unique(industries$industry)
```

### Which industries have the highest income?

``` r
industries %>%
  ggplot(aes(fct_reorder(industry, mean_salary), mean_salary)) + 
  geom_bar(stat = "identity", color = "darkolivegreen", fill = "darkolivegreen1") + 
  geom_text(aes(label = paste0("$", round(mean_salary/1000), "k")), hjust = 1, color = "darkolivegreen") +
  coord_flip() + 
  labs(
    title = "Mean annual income by industry",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )
```

![](20210518---Ask-a-Manager-Survey_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Which industries have the highest proporional bonuses?

The phrase “bonus” is used here as a shorthand for “other monetary
compensation”, as there is little difference for most industries.
However, there are some clear interesting trends here. “Sales” making
the top of the list is no surprise as comission is built into their
employment contracts. A similar principle stands for “retail” and
“property or construction” (i.e. real estate).

``` r
industries %>%
  ggplot(aes(fct_reorder(industry, mean_proportional_bonus), mean_proportional_bonus)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = paste0(round(100*mean_proportional_bonus, 2), "%")), hjust = -0.5) +
  coord_flip() + 
  labs(
    title = "Mean proportional bonus by industry",
    x = ""
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )
```

![](20210518---Ask-a-Manager-Survey_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## What factors drive salary?

### How does age correlate with salary?

### How does years of experience relate to salary?

### How does level of education relate to salary?

### Do men or women earn more money?

### Do white people make more money than non-white people?

# Machine Learning: Predict Salary