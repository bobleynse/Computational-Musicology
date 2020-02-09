# Load libraries

library(tidyverse)
library(spotifyr)

# Set Spotify access variables
Sys.setenv(SPOTIFY_CLIENT_ID = '63189d0ffa0d469ab97dfbe02cbcb356')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '54e78dfb2ee24d3281756fde0417d7f9')

# import albums
the_red_album <- get_playlist_audio_features('bobleynse', '0ghyYvT2B8me30mShW8pUB')
the_blue_album <- get_playlist_audio_features('bobleynse', '0UfoFrm53Q2G7kuQMjchvs')

# get means
red_M <- the_red_album %>% summarize(dan=mean(danceability), 
                                     ene=mean(energy), 
                                     lou=mean(loudness), 
                                     spe=mean(speechiness), 
                                     aco=mean(acousticness), 
                                     ins=mean(instrumentalness), 
                                     liv=mean(liveness), 
                                     val=mean(valence), 
                                     tem=mean(tempo))

blue_M <- the_blue_album %>% summarize(dan=mean(danceability), 
                                       ene=mean(energy), 
                                       lou=mean(loudness), 
                                       spe=mean(speechiness), 
                                       aco=mean(acousticness), 
                                       ins=mean(instrumentalness), 
                                       liv=mean(liveness), 
                                       val=mean(valence), 
                                       tem=mean(tempo))

# get standard deviation
red_SD <- the_red_album %>% summarize(dan=sd(danceability), 
                                     ene=sd(energy), 
                                     lou=sd(loudness), 
                                     spe=sd(speechiness), 
                                     aco=sd(acousticness), 
                                     ins=sd(instrumentalness), 
                                     liv=sd(liveness), 
                                     val=sd(valence), 
                                     tem=sd(tempo))

blue_SD <- the_blue_album %>% summarize(dan=sd(danceability), 
                                       ene=sd(energy), 
                                       lou=sd(loudness), 
                                       spe=sd(speechiness), 
                                       aco=sd(acousticness), 
                                       ins=sd(instrumentalness), 
                                       liv=sd(liveness), 
                                       val=sd(valence), 
                                       tem=sd(tempo))

val_the_red_album <- the_red_album %>% select(valence)
val_the_blue_album <- the_blue_album%>% select(valence)

ggplot(val_the_red_album, aes(x=valence)) + geom_histogram(bins=10) + ggtitle("Valence of the Beatles in 1962-1966")
ggplot(val_the_blue_album, aes(x=valence)) + geom_histogram(bins=10) + ggtitle("Valence of the Beatles in 1967-1970")

