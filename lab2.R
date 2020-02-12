# Load libraries (every time)

library(tidyverse)
library(spotifyr)

# Set Spotify access variables (every time)

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR_CLIENT_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR_CLIENT_SECRET')

# Download Grammy and Edison award playlists (pop) for 2019

grammy <- get_playlist_audio_features('digster.fm', '4kQovkgBZTd8h2HCM3fF31')
edison <- get_playlist_audio_features('spotify', '37i9dQZF1DX8mnKbIkppDf')

# Combine data sets with a labelling variable

awards <-
  grammy %>% mutate(playlist = "Grammys") %>%
  bind_rows(edison %>% mutate(playlist = "Edisons"))

# Start with histogram or bar for showing one variable.

grammy %>% ggplot(aes(x = energy)) + geom_histogram(binwidth = 0.1)

# Start with faceting or boxplot/violin to split by group.

awards %>%
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~ playlist)
awards %>%
  ggplot(aes(x = playlist, y = energy)) +
  geom_boxplot()
awards %>%
  ggplot(aes(x = playlist, y = energy)) +
  geom_violin()

# Start with point/jitter to compare two continuous variables.

grammy %>% ggplot(aes(x = valence, y = energy)) + geom_point() + geom_smooth()

# Good visualisations have many more components.

award_labels <-
  tibble(
    label = c("By the Bright of Night", "Immaterial"),
    playlist = c("Edisons", "Grammys"),
    valence = c(0.151, 0.828),
    energy = c(0.119, 0.717),
  )

awards %>%                       # Start with awards.
  ggplot(                      # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = factor(mode)
    )
  ) +
  geom_point() +               # Scatter plot.
  geom_rug(size = 0.1) +       # Add 'fringes' to show data distribution.
  geom_text(                   # Add text labels from above.
    aes(
      x = valence,
      y = energy,
      label = label),
    colour = "black",        # Override colour (not mode here).
    size = 3,                # Override size (not loudness here).
    data = award_labels,     # Specify the data source for labels.
    hjust = "left",          # Align left side of label with the point.
    vjust = "bottom",        # Align bottom of label with the point.
    nudge_x = -0.05,         # Nudge the label slightly left.
    nudge_y = 0.02           # Nudge the label slightly up.
  ) +
  facet_wrap(~ playlist) +     # Separate charts per playlist.
  scale_x_continuous(          # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),  # Use grid-lines for quadrants only.
    minor_breaks = NULL      # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(          # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(         # Use the Color Brewer to choose a palette.
    type = "qual",           # Qualitative set.
    palette = "Paired"       # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(       # Fine-tune the sizes of each point.
    trans = "exp",           # Use an exp transformation to emphasise loud.
    guide = "none"           # Remove the legend for size.
  ) +
  theme_light() +              # Use a simpler them.
  labs(                        # Make the titles nice.
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )

# Use the ggplot cheat sheet and documentation to make your own!
#
# Working in pairs, can you find a difference between the Eurovision songs from
# 2019 and the pop charts for your country or countries of origin?