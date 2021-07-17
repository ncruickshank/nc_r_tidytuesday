# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# %% libraries

import pandas as pd
import matplotlib.pyplot as plt

# %% data

netflix_titles = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

# %% tidy

netflix_titles['date_added'] = pd.to_datetime(netflix_titles['date_added'])
netflix_titles['year_added'] = netflix_titles['date_added'].dt.year
netflix_titles['month_added'] = netflix_titles['date_added'].dt.month
netflix_titles['round_month'] = netflix_titles['date_added'] + pd.offsets.MonthBegin(-1)

# %% EDA: Movies and TV Shows added to netflix over time

fig, axs = plt.subplots(2, sharex = True)
fig.suptitle('Netflix Titles Over Time')
fig.set_figheight(6)
fig.set_figwidth(12)

## Movies

### reshape netflix_titles
netflix_months_movies = netflix_titles[netflix_titles['type'] == 'Movie'].groupby(
    ['round_month']).size().reset_index(name = 'count').sort_values(by = 'round_month')

### average over each quarter
netflix_months_movies['round_quarter'] = netflix_months_movies['round_month'] + pd.offsets.QuarterBegin(-1)
nf_movies_qrt_avg = netflix_months_movies.groupby('round_quarter')['count'].mean().reset_index(name = 'mean')

### plot the movie subplot
axs[0].plot(netflix_months_movies['round_month'], netflix_months_movies['count'],
            linestyle = 'dashdot', label = 'Number added each month')
axs[0].plot(nf_movies_qrt_avg['round_quarter'], nf_movies_qrt_avg['mean'],
            linewidth = 3, label = 'Average added each quarter')
axs[0].set_title('Movies')
axs[0].legend()

## TV Shows

### reshape netflx_titles
netflix_months_tv = netflix_titles[netflix_titles['type'] == 'TV Show'].groupby(
    ['round_month']).size().reset_index(name = 'count').sort_values(by = 'round_month')

### average over each quarter
netflix_months_tv['round_quarter'] = netflix_months_tv['round_month'] + pd.offsets.QuarterBegin(-1)
nf_tv_qrt_avg = netflix_months_tv.groupby('round_quarter')['count'].mean().reset_index(name = 'mean')

### plot the tv subplot
axs[1].plot(netflix_months_tv['round_month'], netflix_months_tv['count'],
            linestyle = 'dashdot', label = 'Number added each month')
axs[1].plot(nf_tv_qrt_avg['round_quarter'], nf_tv_qrt_avg['mean'],
            linewidth = 3, label = 'Average added each quarter')
axs[1].set_title('TV Shows')

# %% EDA: Most Represented Categories

## exploded dataframe
genres = netflix_titles
genres = genres.assign(listed_in = genres.listed_in.str.split(', ')).explode('listed_in')

## plot

fig, (ax1, ax2) = plt.subplots(1, 2, constrained_layout = True)
fig.suptitle('Top 10 Genres with the Most Titles Added to Netflix')
fig.set_figheight(6)
fig.set_figwidth(12)

### movies

#### reshape
movie_genres = genres[genres['type'] == 'Movie']
movie_genres_counts = movie_genres.groupby('listed_in').size(
    ).reset_index(name = 'count').sort_values(by = 'count', ascending = True).head(10)

#### plot
ax1.barh(movie_genres_counts['listed_in'], movie_genres_counts['count'])
ax1.set_title('Movies')

### tv shows

#### reshape
tv_genres = genres[genres['type'] == 'TV Show']
tv_genres_counts = tv_genres.groupby('listed_in').size(
    ).reset_index(name = 'count').sort_values(by = 'count', ascending = True).head(10)

#### plot
ax2.barh(tv_genres_counts['listed_in'], tv_genres_counts['count'], color = 'red')
ax2.set_title('TV Shows')