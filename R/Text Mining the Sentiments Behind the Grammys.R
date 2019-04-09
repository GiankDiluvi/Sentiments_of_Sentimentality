### ### ### ### ### ### ### ###
# Text Mining the Sentiments  #
#     Behind the Grammys      #
#                             #
# Author: Gian Carlo Diluvi   #
#               2019          #
### ### ### ### ### ### ### ###


# Preamble ####
library(tidyverse)
library(readr)
library(tidytext)
library(genius)
library(geniusR)
library(scales)
library(easyGgplot2)
library(ggrepel)
library(wordcloud)
library(BMS)
library(beepr)
theme_set(theme_classic())

# Set colors
colores <- hue_pal()(6)

# Set seed for reproducibility
set.seed(100295)


# Data wrangling / genius ####

# Import Grammy winners and nominees information
dataset <- readr::read_csv("Grammy Awards data.csv") %>% # Add decade
    dplyr::mutate(Decade = ifelse(year < 2000, 
                                  paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                                  ifelse(year < 2010, "00s", "10s")),
                  Decade = factor(Decade, levels = c("60s",
                                                     "70s",
                                                     "80s",
                                                     "90s",
                                                     "00s",
                                                     "10s")))

# Import lyrics
lyrics <- tibble(track_title = character(),
                 track_n = integer(),
                 lyric = character(),
                 line = integer(),
                 artist = character(),
                 album = character(),
                 year = double(),
                 winner = integer())


# As of February 17, 2019 there is an issue with album no. 24, Paul Simon's Graceland.
# GeniusR crashes when trying to download its lyrics, so they are added manually afterwards
for(i in c(1:23, 25:nrow(dataset))){
    
    # Monitor progress
    print(paste0(i, ".- ",
                 dataset$album[i], " by ",
                 dataset$artist[i]))
    
    aux <- geniusR::genius_album(artist = dataset$artist[i],
                                 album = dataset$album[i]) %>% 
        mutate(artist = dataset$artist[i],
               album = dataset$album[i],
               year = dataset$year[i],
               winner = dataset$winner[i])
    
    lyrics <- bind_rows(lyrics, aux)
}
beepr::beep(2)

# Add decade and winner to lyrics dataset
lyrics <- lyrics %>% 
    dplyr::mutate(Decade = ifelse(year < 2000, 
                                  paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                                  ifelse(year < 2010, "00s", "10s")),
                  Decade = factor(Decade, levels = c("60s",
                                                     "70s",
                                                     "80s",
                                                     "90s",
                                                     "00s",
                                                     "10s")))


# Get Paul Simon's Graceland lyrics manually and add them to the lyrics dataset

# Get songs and exclude You Can Call Me Al (demo version), whose
# lyrics are missing from Genius.com as of Feb. 9, 2019
ps_graceland_songs <- genius::genius_tracklist("Paul Simon", "Graceland") %>% 
    dplyr::filter(track_n != 15) %>% 
    dplyr::select(track_title)

# Get song lyrics one by one and merge
ps_graceland <- tibble(track_title = character(),
                       track_n = integer(),
                       lyric = character(),
                       line = integer(),
                       album = character(),
                       year = double(),
                       Decade = character(),
                       winner = integer())

for(i in 1:nrow(ps_graceland_songs)){
    aux_ps <- genius::genius_lyrics(artist = "Paul Simon",
                                    song = ps_graceland_songs$track_title[i],
                                    info = "all") %>% 
        dplyr::mutate(Decade = "80s",
                      album = "Graceland",
                      artist = "Paul Simon",
                      year = 1987,
                      winner = 1L,
                      track_n = i) %>% 
        dplyr::select(track_title,
                      track_n,
                      lyric,
                      line,
                      artist,
                      album,
                      year,
                      winner,
                      Decade)
    ps_graceland <- bind_rows(ps_graceland, aux_ps)
}

# Convert Decade to factor
ps_graceland <- ps_graceland %>% 
    dplyr::mutate(Decade = factor(Decade, levels = c("60s",
                                                     "70s",
                                                     "80s",
                                                     "90s",
                                                     "00s",
                                                     "10s")))

# Add to new lyrics database in order to safekep lyrics
lyrics_2 <- bind_rows(lyrics, ps_graceland)

# SONG ANALYSIS ####

# Compute different number of songs
num.songs <- lyrics_2 %>% 
    dplyr::filter(winner == 1) %>% 
    dplyr::distinct(track_title) %>% 
    dplyr::count() %>% 
    dplyr::pull()

# Compute different number of albums
num.albums <- lyrics_2 %>% 
    dplyr::filter(winner == 1) %>% 
    dplyr::distinct(album) %>% 
    dplyr::count() %>% 
    dplyr::pull()

# Compute average songs per album
num.songs / num.albums

# Compute total number of words
num.words <- lyrics_2 %>%
    dplyr::filter(winner == 1) %>% 
    tidytext::unnest_tokens(word, lyric) %>% 
    dplyr::count() %>% 
    dplyr::pull()

# Compute average words per song
num.words / num.songs


# Compute average number of albums, songs, and words per decade
Albums_Songs_Words <- lyrics_2 %>% 
    dplyr::filter(winner == 1) %>% 
    dplyr::group_by(Decade) %>% 
    dplyr::summarise(Songs = n_distinct(track_title)) %>% 
    dplyr::left_join(
        lyrics_2 %>% 
            dplyr::filter(winner == 1) %>% 
            dplyr::group_by(Decade) %>% 
            dplyr::summarise(Albums = n_distinct(album))
    ) %>% 
    dplyr::left_join(
        lyrics_2 %>% 
            dplyr::filter(winner == 1) %>% 
            tidytext::unnest_tokens(word, lyric) %>% 
            dplyr::group_by(Decade) %>% 
            dplyr::summarise(Words = n())
    ) %>% 
    dplyr::mutate(SpA = Songs / Albums,
                  WpS = Words / Songs)



# Boxplot of different number of words per song
boxplot_WpS <- lyrics_2 %>% 
    dplyr::filter(winner == 1) %>% 
    tidytext::unnest_tokens(word, lyric) %>% 
    dplyr::mutate(ID = paste0(track_title, "-",
                              track_n, "-",
                              album)) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::mutate(WpS = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(Decade, ID) %>% 
    dplyr::summarise(WpS = mean(WpS)) %>%
    ggplot(aes(x = Decade, y = WpS)) +
    geom_boxplot(aes(color = Decade)) +
    scale_color_manual(values = c(colores[3:6], colores[1:2])) +
    labs(x = "Decade",
         y = "Words per song") +
    theme(text = element_text(size = 16),
          legend.position = "none")

ggsave("Plots/Boxplot_WpS.png")
ggsave("Plots/Boxplot_WpS.pdf")



# Violin plot of different number of words per song
violin_WpS <- lyrics_2 %>% 
    dplyr::filter(winner == 1) %>% 
    tidytext::unnest_tokens(word, lyric) %>% 
    dplyr::mutate(ID = paste0(track_title, "-",
                              track_n, "-",
                              album)) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::mutate(WpS = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(Decade, ID) %>% 
    dplyr::summarise(WpS = mean(WpS)) %>%
    ggplot(aes(x = Decade, y = WpS)) +
    geom_violin(aes(fill = Decade),
                alpha = 0.2) +
    geom_jitter(aes(color = Decade),
                width = 0.1) +
    scale_color_manual(values = c(colores[3:6], colores[1:2])) +
    scale_fill_manual(values = c(colores[3:6], colores[1:2])) +
    labs(x = "Decade",
         y = "Words per song") +
    theme(text = element_text(size = 16),
          legend.position = "none")

ggsave("Plots/Violin_WpS.png")
ggsave("Plots/Violin_WpS.pdf")



# FRECUENCY ANALYSIS ####

# Common words overall
graph.common.words <- lyrics_2 %>% 
    dplyr::filter(winner == 1) %>% 
    unnest_tokens(word, lyric) %>% 
    anti_join(stop_words) %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 278) %>% 
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n)) +
    geom_col(fill = "skyblue") +
    labs(x = NULL,
         y = "Number of occurrences") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text = element_text(size = 16),
          text = element_text(size = 16)) +
    coord_flip()

ggsave("Plots/graph_common_words.png")
ggsave("Plots/graph_common_words.pdf")


# Common words by decade
graph_decade_total <- lyrics_2 %>% 
    dplyr::filter(winner == 1) %>% 
    unnest_tokens(word, lyric) %>%
    anti_join(stop_words) %>% 
    mutate(Decade = ifelse(year < 2000, 
                           paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                           ifelse(year < 2010, "00s", "10s"))) %>% 
    group_by(Decade) %>% 
    count(word, sort = TRUE) %>% 
    arrange(Decade, desc(n)) %>% 
    top_n(10, n) %>%
    dplyr::ungroup() %>% 
    mutate(word = reorder(word, n)) %>% 
    dplyr::mutate(Decade = factor(Decade,
                                  levels = c("60s", "70s", "80s", "90s", "00s", "10s"))) %>% 
    dplyr::arrange(Decade, n) %>%  
    dplyr::mutate(order = row_number())

graph_decade_total <- ggplot(graph_decade_total,
                             aes(order, n, fill = Decade)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(x = NULL,
         y = "Number of occurrences") +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, 500)) +
    scale_fill_manual(values = c(colores[3:6], colores[1:2])) +
    scale_x_continuous(
        breaks = graph_decade_total$order,
        labels = graph_decade_total$word,
        expand = c(0,0)
    ) +
    coord_flip() +
    facet_wrap(~ Decade, scales = "free_y") +
    guides(fill=FALSE)


ggsave("Plots/graph_common_words_decade_FW.png")
ggsave("Plots/graph_common_words_decade_FW.pdf")




# NET SENTIMENT ANALYSIS ####

# Save sentiments in new data set
albums.sentiments <- lyrics_2 %>% 
    unnest_tokens(word, lyric) %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(album, year, winner, Decade, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative)



# Net sentiment per year
graph.albums.sentiment <- albums.sentiments %>%
    dplyr::filter(winner == 1) %>% 
    ggplot(aes(x = year, y = sentiment, fill = Decade)) +
    geom_col() +
    labs(x = "Year",
         y = "Net Sentiment") +
    scale_x_continuous(breaks = seq(from = 1960, to = 2020, by = 10),
                       labels = seq(from = 1960, to = 2020, by = 10)) +
    theme(text = element_text(size = 16)) +
    scale_fill_manual(breaks = c("60s", "70s", "80s", "90s", "00s", "10s"),
                      values = c(colores[3:6], colores[1:2]))

ggsave("Plots/graph_annual_sentiment.png")
ggsave("Plots/graph_annual_sentiment.pdf")




# Net sentiment average by decade
graph.albums.mean.sentiment.decade <- albums.sentiments %>% 
    dplyr::filter(winner == 1) %>% 
    group_by(Decade) %>% 
    summarise(AvgSentiment = mean(sentiment)) %>% 
    bind_cols(tibble(n = 1:6)) %>% 
    ggplot(aes(n, AvgSentiment, fill = Decade)) +
    geom_col() +
    labs(x = "Decade",
         y = "Average Net Sentiment") +
    scale_x_continuous(breaks = 1:6,
                       labels = c("60s", "70s", "80s", "90s", "00s", "10s")) +
    scale_y_continuous(breaks = seq(from = 0, to = 80, by = 20),
                       labels = seq(from = 0, to = 80, by = 20),
                       limits = c(0, 80)) +
    theme(text = element_text(size = 16)) +
    scale_fill_manual(breaks = c("60s", "70s", "80s", "90s", "00s", "10s"),
                      values = c(colores[3:6], colores[1:2]))

ggsave("Plots/graph_decade_mean_sentiment.png")
ggsave("Plots/graph_decade_mean_sentiment.pdf")



# LINEAR TREND ####


# Compute average net sentiments and other descriptive statistics
albums.sentiments %>% 
    select(sentiment) %>% 
    summary


# Graph net sentiment trend
sentiments_trend <- albums.sentiments %>% 
    dplyr::filter(winner == 1) %>% 
    ggplot(aes(year, sentiment)) +
    geom_point(aes(color = Decade),
               size = 3) +
    geom_line(size = 0.1,
              color = "grey60") +
    geom_smooth(method = "lm",
                color = "red",
                se = FALSE) +
    labs(x = "Year",
         y = "Net Sentiment") +
    theme(text = element_text(size = 16)) +
    scale_color_manual(values = c(colores[3:6], colores[1:2]))

ggsave("Plots/graph_sentiment_linear_trend.png")
ggsave("Plots/graph_sentiment_linear_trend.pdf")

# Adjust model
model <- lm(sentiment ~ year, data = albums.sentiments %>% 
                dplyr::filter(winner == 1))
summary(model)
plot(model)
acf(model$residuals)


# AND WHAT ABOUT THE LOSERS? ####

# Define numero function
numero <- function(x, digits = 0){
    paste0(round(x, digits))
}

# Add sentiment to original dataset
dataset <- dataset %>% 
    dplyr::left_join(albums.sentiments %>% 
                         dplyr::select(album, year, sentiment),
                     by = c("album" = "album",
                            "year" = "year"))

# Create histogram highlighting outliers

# Labels information
data_labels <- tibble(x = c(-632, -346, -250),
                      y = c(25, 20, 15),
                      lab = c("The Marshall Mathers LP by Eminem",
                              "The Eminem Show by Eminem",
                              "Recovery by Eminem"),
                      Winner = factor("Nominated to the Grammy but did not win",
                                      levels = c("Won Grammy Award for Album of the Year", 
                                                 "Nominated to the Grammy but did not win")))
data_curves <- tibble(x = c(-678, -405, -273),
                      xend = c(-632, -346, -250),
                      y = c(2, 2, 2),
                      yend = c(23, 18, 13),
                      Winner = factor("Nominated to the Grammy but did not win",
                                      levels = c("Won Grammy Award for Album of the Year", 
                                                 "Nominated to the Grammy but did not win")))


net_sentiment_histogram <- dataset %>%
    dplyr::mutate(Winner = ifelse(winner==1, "Won Grammy Award for Album of the Year",
                                  "Nominated to the Grammy but did not win"),
                  Winner = factor(Winner, levels = c("Won Grammy Award for Album of the Year", 
                                                     "Nominated to the Grammy but did not win"))) %>% 
    ggplot(aes(x = sentiment, fill = Winner)) +
    geom_histogram(bins = 50,
                   color = "white") +
    facet_wrap(.~Winner,
               nrow = 2,
               scales = "free_y") +
    labs(x = "Net Sentiment",
         y = "Number of albums",
         fill = "Won the\n Grammy?") +
    scale_fill_manual(values = c("green3", "orange2"),
                      breaks = c("No", "Yes")) +
    scale_y_continuous(labels = numero) +
    theme(text = element_text(size = 16),
          strip.background = element_blank(),
          panel.spacing = unit(1, "lines")) +
    geom_label(data = data_labels,
               aes(x = x, y = y, label = lab),
               hjust = 0, 
               vjust = 0.5, 
               colour = "#555555", 
               fill = "white", 
               label.size = NA, 
               size = 3) +
    geom_curve(data = data_curves,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "#555555", 
               curvature = -0.2,
               size=0.25)


ggsave("Plots/graph_net_sentiment_histogram.png")
ggsave("Plots/graph_net_sentiment_histogram.pdf")


# APPENDIX ####

# 2019 nominees Analysis ###
nominees_2019 <- tibble(artist = c("Cardi B",
                                   "Brandi Carlile",
                                   "Drake",
                                   "Her",
                                   "Post Malone",
                                   "Janelle Monae",
                                   "Kacey Musgraves",
                                   "Kendrick Lamar The Weeknd and SZA"),
                        album = c("Invasion of Privacy",
                                  "By The Way I Forgive You",
                                  "Scorpion",
                                  "H E R",
                                  "Beerbongs Bentleys",
                                  "Dirty Computer",
                                  "Golden Hour",
                                  "Black Panther The Album Music from and Inspired By"),
                        year = rep(2019, 8))



# Import lyrics
lyrics_nominees <- tibble(track_title = character(),
                          track_n = integer(),
                          lyric = character(),
                          line = integer(),
                          album = character(),
                          year = double())



# Save available lyrics in the lyrics tibble for further analyses
for(i in 1:nrow(nominees_2019)){
    aux <- geniusR::genius_album(artist = nominees_2019$artist[i],
                                 album = nominees_2019$album[i]) %>% 
        mutate(album = nominees_2019$album[i],
               year = nominees_2019$year[i])
    
    lyrics_nominees <- bind_rows(lyrics_nominees, aux)
}


# Get albums sentiments
nominees.sentiments <- lyrics_nominees %>% 
    unnest_tokens(word, lyric) %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(album, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative)


# Add to nominees data set
nominees_2019 <- nominees_2019 %>% 
    dplyr::left_join(nominees.sentiments) %>%
    dplyr::select(artist, album, year, sentiment) %>% 
    dplyr::mutate(winner = 0)



# Create labels info
data_labels_trend <- tibble(x = 2020,
                            y = c(-64, -46, -32, 91, -9, 40, 18, -89),
                            lab = c("Invasion of Privacy \nby Cardi B",
                                    "By The Way I Forgive \nYou by Brandi Carlile",
                                    "Scorpion by Drake",
                                    "HER by HER",
                                    "Beerbongs & Bentleys \nby Post Malone",
                                    "Dirty Computer by \nJanelle Monae",
                                    "Golden Hour by \nKacey Musgraves",
                                    "Black Panther \nSoundtrack"))


# Add to trend plot
trend_2019 <- sentiments_trend +
    xlim(1960, 2030) +
    geom_point(data = nominees_2019 %>% 
                   dplyr::mutate(Decade = "10s"),
               aes(x = year, y = sentiment, color = Decade),
               size = 3) +
    geom_label(data = data_labels_trend,
               aes(x = x, y = y, label = lab),
               hjust = 0, 
               vjust = 0.5, 
               colour = "#555555", 
               fill = "white", 
               label.size = NA, 
               size = 2)


ggsave("Plots/graph_2019_trend.png")
ggsave("Plots/graph_2019_trend.pdf")