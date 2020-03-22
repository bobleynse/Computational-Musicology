# Load libraries

library(tidyverse)
library(spotifyr)
library(plotly)

# Set Spotify access variables
Sys.setenv(SPOTIFY_CLIENT_ID = '63189d0ffa0d469ab97dfbe02cbcb356')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '54e78dfb2ee24d3281756fde0417d7f9')

# import albums
the_red_album <- get_playlist_audio_features('bobleynse', '0ghyYvT2B8me30mShW8pUB')
the_blue_album <- get_playlist_audio_features('bobleynse', '0UfoFrm53Q2G7kuQMjchvs')

# merge datasets
total1 <- rbind(the_red_album, the_blue_album)
total1$track.album.name <- str_sub(total1$track.album.name, 1, str_length(total1$track.album.name)-13)
total1$track.name <- str_sub(total1$track.name, 1, str_length(total1$track.name)-18)

# get means
M_dif <- total1 %>%
  group_by(playlist_name) %>%
  arrange(desc(playlist_name)) %>%
    summarize(dan=mean(danceability), 
              ene=mean(energy), 
              lou=mean(loudness),
              spe=mean(speechiness), 
              aco=mean(acousticness), 
              ins=mean(instrumentalness), 
              liv=mean(liveness), 
              val=mean(valence), 
              tem=mean(tempo),
              dur=mean(track.duration_ms),
              dansd=sd(danceability), 
              enesd=sd(energy), 
              lousd=sd(loudness), 
              spesd=sd(speechiness), 
              acosd=sd(acousticness), 
              inssd=sd(instrumentalness), 
              livsd=sd(liveness), 
              valsd=sd(valence), 
              temsd=sd(tempo),
              dursd=mean(track.duration_ms)
              )

M <- rbind(tail(M_dif, 1), head(M_dif, 1))

MF <- M %>% mutate(year=c('1963-1966', '1967-1970'))

plot <- (
  ggplot(MF) 
  + geom_point(aes(x=year, y=dan, size=dansd, text=paste("<b>Danceability</b>\nValue:", dan, "\nSD:", dansd), alpha=0.5), col='green') 
  + geom_line(aes(x=year, y=dan, group=1, alpha=0.5), col='green')
  + geom_text(aes(x=0.7, y=first(dan), label='Danceability'), col='green')
  
  + geom_point(aes(x=year, y=ene, size=enesd, text=paste("<b>Energy</b>\nValue:", ene, "\nSD:", enesd), alpha=0.5), col='red') 
  + geom_line(aes(x=year, y=ene, group=1, alpha=0.5), col='red')
  + geom_text(aes(x=0.7, y=first(ene), label='Energy'), col='red')
  
  + geom_point(aes(x=year, y=spe, size=spesd, text=paste("<b>Speechiness</b>\nValue:", spe, "\nSD:", spesd), alpha=0.5), col='blue') 
  + geom_line(aes(x=year, y=spe, group=1, alpha=0.5), col='blue')
  + geom_text(aes(x=0.7, y=first(spe), label='Speechiness'), col='blue')
  
  + geom_point(aes(x=year, y=aco, size=acosd, text=paste("<b>Acousticness</b>\nValue:", aco, "\nSD:", acosd), alpha=0.5), col='pink') 
  + geom_line(aes(x=year, y=aco, group=1, alpha=0.5), col='blue')
  + geom_text(aes(x=0.7, y=first(aco), label='Acousticness'), col='blue')
  
  + geom_point(aes(x=year, y=ins, size=inssd, text=paste("<b>Instrumentalness</b>\nValue:", ins, "\nSD:", inssd), alpha=0.5), col='orange') 
  + geom_line(aes(x=year, y=ins, group=1, alpha=0.5), col='blue')
  + geom_text(aes(x=0.7, y=first(ins), label='Instrumentalness'), col='blue')
)

ggplotly(plot, tooltip = c("text"))

M_dif <- total1 %>%
  group_by(playlist_name) %>%
  group_by(mean(danceability), mean(energy), mean(loudness), mean(speechiness), mean(acousticness), mean(instrumentalness), mean(liveness), mean(valence), mean(tempo),mean(track.duration_ms),first(playlist_name)) %>%
  summarise()

M_dif$ID <- seq.int(nrow(M_dif))

ggplot(data = M_dif, aes(x = playlist_name, y = ene)) + geom_line(aes(x = ene)) + geom_point()

ggplot(data = M_dif, aes(x = playlist, y = ene, group = Party)) +
  geom_line(aes(color = Party, alpha = 1), size = 2) +
  geom_point(aes(color = Party, alpha = 1), size = 4)


# get standard deviation
SD_red <- the_red_album %>% summarize(dan=sd(danceability), 
                                     ene=sd(energy), 
                                     lou=sd(loudness), 
                                     spe=sd(speechiness), 
                                     aco=sd(acousticness), 
                                     ins=sd(instrumentalness), 
                                     liv=sd(liveness), 
                                     val=sd(valence), 
                                     tem=sd(tempo),
                                     album=first(playlist_name),
                                     dur=mean(track.duration_ms))

SD_blue <- the_blue_album %>% summarize(dan=sd(danceability), 
                                       ene=sd(energy), 
                                       lou=sd(loudness), 
                                       spe=sd(speechiness), 
                                       aco=sd(acousticness), 
                                       ins=sd(instrumentalness), 
                                       liv=sd(liveness), 
                                       val=sd(valence), 
                                       tem=sd(tempo),
                                       album=first(playlist_name))

val_the_red_album <- the_red_album %>% select(valence)
val_the_blue_album <- the_blue_album%>% select(valence)

ggplot(val_the_red_album, aes(x=valence)) + geom_histogram(bins=10) + ggtitle("Valence of the Beatles in 1962-1966")
ggplot(val_the_blue_album, aes(x=valence)) + geom_histogram(bins=10) + ggtitle("Valence of the Beatles in 1967-1970")




M_dif <- total1 %>%
  group_by(playlist_name) %>%
    summarize(dan=mean(danceability), 
              ene=mean(energy), 
              lou=mean(loudness), 
              spe=mean(speechiness), 
              aco=mean(acousticness), 
              ins=mean(instrumentalness), 
              liv=mean(liveness), 
              val=mean(valence), 
              tem=mean(tempo),
              dur=mean(track.duration_ms),
              playlist=first(playlist_name))


