library(tidyverse)
library(spotifyr)
library(compmus)
source('spotify.R')

CTO <- 
  get_tidy_audio_analysis('6lSxM9BKcEZBSDKl2VODsF') %>% 
  select(segments) %>% unnest(segments) %>% 
  select(start, duration, pitches)
CT5 <- 
  get_tidy_audio_analysis('5c6gzvWGw9EZ7qMxfH9HVy') %>% 
  select(segments) %>% unnest(segments) %>% 
  select(start, duration, pitches)

plotje <- compmus_long_distance(
  CTO %>% mutate(pitches = map(pitches, compmus_normalise, 'chebyshev')),
  CT5 %>% mutate(pitches = map(pitches, compmus_normalise, 'chebyshev')),
  feature = pitches,
  method = 'euclidean') %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2, 
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d)) + 
  geom_tile() +
  scale_fill_continuous(type = 'viridis', guide = 'none') +
  labs(x = 'Original', y = 'Take 5') +
  theme_minimal()

plotje
