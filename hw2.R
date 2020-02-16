# Load libraries

library(tidyverse)
library(spotifyr)
library(stringr)


# Set Spotify access variables
Sys.setenv(SPOTIFY_CLIENT_ID = '63189d0ffa0d469ab97dfbe02cbcb356')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '54e78dfb2ee24d3281756fde0417d7f9')

# import albums
the_red_album <- get_playlist_audio_features('bobleynse', '0ghyYvT2B8me30mShW8pUB')
the_blue_album <- get_playlist_audio_features('bobleynse', '0UfoFrm53Q2G7kuQMjchvs')

# concat songs
total <- rbind(the_red_album, the_blue_album)

# plot
ggplot(total, aes(x=energy, y=valence, col=track.album.name)) + geom_point() + facet_wrap(facet="track.album.name")

ggplot(total, aes(x=0, y=valence, col=track.album.name)) + geom_point() + facet_wrap(facet="track.album.name")
ggplot(total, aes(x=energy, y=valence, col=track.album.release_date)) + geom_point() + facet_wrap(facet="track.album.release_date", nrow=1, ncol=13)
ggplot(total, aes(x=0, y=valence, col=track.album.release_date)) + geom_point() + facet_wrap(facet="track.album.release_date", nrow=1, ncol=13)
ggplot(total, aes(x=0, y=valence, col=playlist_name)) + geom_boxplot() + facet_wrap(facet="track.album.release_date", nrow=1, ncol=13)
ggplot(total, aes(x=energy, y=valence, col=playlist_name)) + geom_point() + facet_wrap(facet="track.album.release_date", nrow=1, ncol=13)

ggplot(total, aes(x=energy, y=valence, col=playlist_name, size=(track.duration_ms/1000)/60)) + geom_point(alpha=0.4) + facet_wrap(facet="track.album.release_date", nrow=1, ncol=13, shrink = TRUE, labeller=labeller(sex = labels)) + scale_color_manual(values= c("#bf0d00", "#03b5fc")) + scale_x_continuous(breaks=c(0, 0.5, 1)) + theme(legend.position="bottom") + labs(x="Energy", y="Valence", size="Duration in min", col="")

ggplot(total, aes(x=0, y=valence, col=playlist_name, size=(track.duration_ms/1000)/60)) + geom_boxplot(alpha=0.4) + facet_wrap(facet="track.album.release_date", nrow=1, ncol=13, shrink = TRUE, labeller=track.album.name) + scale_color_manual(values= c("#bf0d00", "#03b5fc")) + scale_x_continuous(breaks=c(0, 0.5, 1)) + theme(legend.position="bottom") + labs(x="Energy", y="Valence", size="Duration in min", col="")



print(total$track.album.name)
print(str_sub(total$track.album.name, 1, str_length(total$track.album.name)-13))

total <- rbind(the_red_album, the_blue_album)
total <- unite(total, "name", track.album.release_date, track.album.name, sep="\n")
total$name <- str_sub(total$name, 1, str_length(total$name)-13)
ggplot(total, aes(x=0, y=valence, col=playlist_name, size=(track.duration_ms/1000)/60)) + geom_boxplot(alpha=0.4) + facet_wrap(facet="name", nrow=1, ncol=13, shrink = TRUE) + scale_color_manual(values= c("#bf0d00", "#03b5fc")) + scale_x_continuous(breaks=c(0, 0.5, 1)) + theme(legend.position="bottom") + labs(x="Energy", y="Valence", size="Duration in min", col="")


ggplot(total, aes(x=energy, y=valence, col=playlist_name, size=(track.duration_ms/1000)/60)) + geom_point(alpha=0.4) + facet_wrap(facet="name", nrow=1, ncol=13, shrink = TRUE) + scale_color_manual(values= c("#bf0d00", "#03b5fc")) + scale_x_continuous(breaks=c(0, 0.5, 1)) + theme(legend.position="bottom") + labs(x="Energy", y="Valence", size="Duration in min", col="")

