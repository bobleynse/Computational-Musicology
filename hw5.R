library(tidyverse)
library(plotly)
library(spotifyr)
library(stringr)
library(compmus)
library(scales)

source('spotify.R')

# import albums
the_red_album <- get_playlist_audio_features('bobleynse', '0ghyYvT2B8me30mShW8pUB')
the_blue_album <- get_playlist_audio_features('bobleynse', '0UfoFrm53Q2G7kuQMjchvs')

# merge datasets
total1 <- rbind(the_red_album, the_blue_album)
total1$track.album.name <- str_sub(total1$track.album.name, 1, str_length(total1$track.album.name)-13)
total1$track.name <- str_sub(total1$track.name, 1, str_length(total1$track.name)-18)



##############################
# import albums
the_red_album <- get_playlist_audio_features('bobleynse', '0ghyYvT2B8me30mShW8pUB')
the_blue_album <- get_playlist_audio_features('bobleynse', '0UfoFrm53Q2G7kuQMjchvs')

# merge datasets
total <- rbind(the_red_album, the_blue_album)
total$track.album.name <- str_sub(total$track.album.name, 1, str_length(total$track.album.name)-13)
total$track.name <- str_sub(total$track.name, 1, str_length(total$track.name)-18)

means <- total %>%
  group_by(track.album.release_date) %>%
  summarize(mean_valence = mean(valence), mean_energy = mean(energy), 
            release_date = first(track.album.release_date), 
            playlist_name = first(playlist_name),
            album_name = first(track.album.name))

means$release_date <- as.Date(means$release_date)

ff <- (
  ggplot(means, aes(x=release_date, group=0.5, text=paste("Album:", album_name, "\nRelease:", release_date, "\nMean valence:", mean_valence, "\nMean energy:", mean_energy))) + 
    geom_line(aes(y=mean_valence, col='Mean valence'), col='green') +
    geom_line(aes(y=mean_energy, col='Mean energy'), col='orange') +
    ylim(0,1) +
    scale_x_date(labels = date_format("%d-%m-%Y")) +
    geom_text(aes(y=((mean_valence+mean_energy)/2)-0.2, x=release_date, label=album_name), size = 4) +
    geom_segment(aes(x=release_date, xend=release_date, y=((mean_valence+mean_energy)/2)-0.2, yend=mean_valence), size=2, alpha=0.1, linetype="dotted", col=c('red', 'red', 'red', 'red', 'red', 'red', 'red', 'blue', 'blue', 'blue', 'blue', 'blue', 'blue')) +
    xlab("Date") +
    ylab("Value")
)

ggplotly(ff, tooltip = c("text"))

##########################


BB <- 
  get_tidy_audio_analysis('6lSxM9BKcEZBSDKl2VODsF') %>% 
  # change these 3 to adjust the timestamps (beats, bars, sections)
  compmus_align(sections, segments) %>% 
  select(sections) %>% unnest(sections) %>% 
  mutate(
    pitches = 
      map(segments, 
          compmus_summarise, pitches, 
          method = 'mean', norm = 'manhattan'))

BB %>% 
  compmus_match_pitch_template(key_templates, 'euclidean', 'manhattan') %>% 
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)) +
  geom_tile() +
  scale_fill_viridis_c(option = 'A', guide = 'none') +
  theme_minimal() +
  labs(x = 'Time (s)', y = '')
