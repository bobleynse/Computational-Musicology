# Install/update tidyverse and spotifyr (one time)

# Load libraries (every time)

library(tidyverse)
library(spotifyr)

# Set Spotify access variables (every time)

Sys.setenv(SPOTIFY_CLIENT_ID = '63189d0ffa0d469ab97dfbe02cbcb356')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '54e78dfb2ee24d3281756fde0417d7f9')

# Work with spotifyr. Note that playlists also require a username.

juditha <-
  get_track_audio_features(
    c('2M5b9YLAgFroqWzeaZf86e', '3DBKc4ioGnMQLlbGQcFDIO')
  )

alla <- get_album_tracks('7oI0E3DdTbD85rhMg19GSU')
gilberto <- get_artist_audio_features('gilberto gil')
disney <- get_playlist_audio_features('128899670', '5NtjgKz4doejP5HJtKXFcS')

# Summarise key patterns

gilberto %>% summarise(M = mean(danceability), SD = sd(danceability))
disney %>% summarise(M = mean(danceability), SD = sd(danceability))

# Questions to answer during the lab
#
# 1. Is 'Let It Go' typical or atypical of Disney hits, with respect to the
#    features in the Spotify API?
# 2. What are the similarities and differences between the 'This is Cazwell'
#    and 'This is Eminem' playlists on Spotify? (Be warned that the lyrics are
#    explicit, like much hip hop, but you don't need to listen.)