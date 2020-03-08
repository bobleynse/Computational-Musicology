library(tidyverse)
library(plotly)
library(spotifyr)
library(stringr)
library(compmus)
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
              track.album.release_date = first(track.album.release_date), 
              playlist_name = first(playlist_name))


ff <- (ggplot(means, aes(x=track.album.release_date, group=1))
  + geom_line(aes(y=mean_valence))
  + geom_line(aes(y=mean_energy)))

ggplotly(ff)

