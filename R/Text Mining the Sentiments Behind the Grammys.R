### ### ### ### ### ### ### ###
# Text Mining the Sentiments  #
#     Behind the Grammys      #
#                             #
# Author: Gian Carlo Diluvi   #
#               2018          #
### ### ### ### ### ### ### ###


# Preamble ####
library(tidyverse)
library(readr)
library(tidytext)
library(geniusR)
library(scales)
library(easyGgplot2)
library(ggrepel)
library(wordcloud)
library(BMS)
library(beepr)
theme_set(theme_classic())


# Set parameters
colores <- hue_pal()(6)


# Set seed for reproducibility
set.seed(100295)


# IMPORT LYRICS ####
# Define year, artists, albums and sex
year <- c(1959:1964, 1966:2018)

artists <- c("Henry Mancini",
             "Frank Sinatra",
             "Bob Newhart",
             "Judy Garland",
             "Vaughn Meader",
             "Barbra Streisand",
             "Frank Sinatra",
             "Frank Sinatra",
             "The Beatles",
             "Glen Campbell",
             "Blood, Sweat & Tears",
             "Simon & Garfunkel",
             "Carole King",
             "George Harrison",
             "Stevie Wonder",
             "Stevie Wonder",
             "Paul Simon",
             "Stevie Wonder",
             "Fleetwood Mac",
             "Bee Gees",
             "Billy Joel",
             "Christopher Cross",
             "John Lennon",
             "Toto",
             "Michael Jackson",
             "Lionel Richie",
             "Phil Collins",
             "Paul Simon",
             "U2",
             "George Michael",
             "Bonnie Raitt",
             "Quincy Jones",
             "Natalie Cole",
             "Eric Clapton",
             "Whitney Houston",
             "Tony Bennett",
             "Alanis Morissette",
             "Celine Dion",
             "Bob Dylan",
             "Lauryn Hill",
             "Santana",
             "Steely Dan",
             "Various Artists",
             "Norah Jones",
             "OutKast",
             "Ray Charles",
             "U2",
             "Dixie Chicks",
             "Herbie Hancock",
             "Robert Plant & Alison Krauss",
             "Taylor Swift",
             "Arcade Fire",
             "Adele",
             "Mumford & Sons",
             "Daft Punk",
             "Beck",
             "Taylor Swift",
             "Adele",
             "Bruno Mars")

albums <- c("The Music from Peter Gunn",
            "Come Dance With Me!",
            "The Button-Down Mind of Bob Newhart",
            "Judy at Carnegie Hall",
            "The First Family",
            "The Barbra Streisand Album",
            "September of My Years",
            "A Man and His Music",
            "Sgt. Pepper's Lonely Hearts Club Band",
            "By the Time I Get to Phoenix",
            "Blood, Sweat Tears",
            "Bridge over Troubled Water",
            "Tapestry",
            "The Concert for Bangladesh",
            "Innervisions",
            "Fulfillingness' First Finale",
            "Still Crazy After All These Years",
            "Songs in the Key of Life",
            "Rumours",
            "Saturday Night Fever: The Original Movie Sound Track",
            "52nd Street",
            "Christopher Cross",
            "Double Fantasy",
            "Toto IV",
            "Thriller",
            "Can't Slow Down",
            "No Jacket Required",
            "Graceland",
            "The Joshua Tree",
            "Faith",
            "Nick of Time",
            "Back on the Block",
            "Unforgettable... with Love",
            "Unplugged",
            "The Bodyguard: Original Soundtrack Album",
            "MTV Unplugged",
            "Jagged Little Pill",
            "Falling into You",
            "Time Out of Mind",
            "The Miseducation of Lauryn Hill",
            "Supernatural",
            "Two Against Nature",
            "O Brother, Where Art Thou? (Music from the Motion Picture)",
            "Come Away with Me",
            "Speakerboxxx/The Love Below",
            "Genius Loves Company",
            "How to Dismantle an Atomic Bomb",
            "Taking the Long Way",
            "River: The Joni Letters",
            "Raising Sand",
            "Fearless",
            "The Suburbs",
            "21",
            "Babel",
            "Random Access Memories",
            "Morning Phase",
            "1989",
            "25",
            "24K Magic")





# Import lyrics
lyrics <- tibble(track_title = character(),
                 track_n = integer(),
                 lyric = character(),
                 line = integer(),
                 album = character(),
                 year = double())

# Only some albums' lyrics are available in genius
available <- c(2, 6:59)

# Save available lyrics in the lyrics tibble for further analyses
for(i in available){
  aux <- geniusR::genius_album(artist = artists[i],
                               album = albums[i]) %>% 
    mutate(album = albums[i],
           year = year[i])
  
  lyrics <- bind_rows(lyrics, aux)
  #beepr::beep(2)
}
#beepr::beep(8)


# SONG ANALYSIS ####
# Compute different number of songs
lyrics %>% 
  dplyr::distinct(track_title) %>% 
  dplyr::count()

# Compute different number of albums
lyrics %>% 
  dplyr::distinct(album) %>% 
  dplyr::count()
746/55

# Compute total number of words
lyrics %>%
  tidytext::unnest_tokens(word, lyric)
179381/747
  


# Compute average number of albums, songs, and words per decade
Decades <- lyrics %>% 
  dplyr::mutate(Decade = ifelse(year < 2000, 
                                paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                                ifelse(year < 2010, "00s", "10s"))) 

Albums_Songs_Words <- Decades %>% 
  dplyr::group_by(Decade) %>% 
  dplyr::summarise(Songs = n_distinct(track_title)) %>% 
  dplyr::left_join(
    Decades %>% 
      dplyr::group_by(Decade) %>% 
      dplyr::summarise(Albums = n_distinct(album))
  ) %>% 
  dplyr::left_join(
    Decades %>% 
      tidytext::unnest_tokens(word, lyric) %>% 
      dplyr::group_by(Decade) %>% 
      dplyr::summarise(Words = n())
  ) %>% 
  dplyr::mutate(SpA = Songs / Albums,
                WpS = Words / Songs)


# Boxplot of different number of words per song
boxplot_WpS <- Decades %>% 
  tidytext::unnest_tokens(word, lyric) %>% 
  dplyr::mutate(ID = paste0(track_title, "-",
                            track_n, "-",
                            album),
                Decade = factor(Decade,
                                levels = c("60s", "70s", "80s", "90s", "00s", "10s"))) %>% 
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


# Same boxplot but marking outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

won <- tibble(Album = albums,
              Artist = artists)

boxplot_WpS_outliers <- Decades %>% 
  dplyr::left_join(won, by = c("album" = "Album")) %>% 
  tidytext::unnest_tokens(word, lyric) %>% 
  dplyr::mutate(ID = paste0(track_title, "-",
                            track_n, "-",
                            album),
                Decade = factor(Decade,
                                levels = c("60s", "70s", "80s", "90s", "00s", "10s")),
                info = paste0(track_title, " - ", Artist)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(WpS = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Decade, ID, info) %>% 
  dplyr::summarise(WpS = mean(WpS)) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(Decade) %>% 
  dplyr::mutate(outlier = ifelse(is_outlier(WpS) == 1,
                                 info, 
                                 as.numeric(NA))) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = Decade, y = WpS)) +
  geom_boxplot(aes(color = Decade)) +
  scale_color_manual(values = c(colores[3:6], colores[1:2])) +
  labs(x = "Decade",
       y = "Words per song") +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  geom_text(aes(label = outlier), 
            na.rm = TRUE,
            check_overlap = TRUE,
            size = 2,
            angle = 15)

ggsave("Plots/Boxplot_WpS_outliers.png")
ggsave("Plots/Boxplot_WpS_outliers.pdf")


# Now with ggrepel

boxplot_WpS_outliers_ggrepel <- Decades %>% 
  dplyr::left_join(won, by = c("album" = "Album")) %>% 
  tidytext::unnest_tokens(word, lyric) %>% 
  dplyr::mutate(ID = paste0(track_title, "-",
                            track_n, "-",
                            album),
                Decade = factor(Decade,
                                levels = c("60s", "70s", "80s", "90s", "00s", "10s")),
                info = paste0(track_title, " - ", Artist)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(WpS = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Decade, ID, info) %>% 
  dplyr::summarise(WpS = mean(WpS)) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(Decade) %>% 
  dplyr::mutate(outlier = ifelse(is_outlier(WpS) == 1,
                                 info, 
                                 as.numeric(NA))) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = Decade, y = WpS)) +
  geom_boxplot(aes(color = Decade)) +
  scale_color_manual(values = c(colores[3:6], colores[1:2])) +
  labs(x = "Decade",
       y = "Words per song") +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  geom_text_repel(aes(label = outlier), 
                  na.rm = TRUE,
                  size = 2)

ggsave("Plots/Boxplot_WpS_outliers_ggrepel.png")
ggsave("Plots/Boxplot_WpS_outliers_ggrepel.pdf")






# FRECUENCY ANALYSIS ####

# Common words overall
graph.common.words <- lyrics %>% 
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




# Average songs per album by decade
Decades %>% 
  group_by(Decade) %>%
  distinct(album) %>%
  count() %>%
  rename(n2 = n) %>% 
  inner_join(Decades %>%
               group_by(Decade) %>%
               distinct(track_title) %>%
               count()) %>% 
  mutate(avg = n/n2)

# Average words per album and song per decade
Decades %>% 
  unnest_tokens(word, lyric) %>%
  mutate(conc=paste(track_title, album)) %>%
  group_by(Decade) %>%
  summarise(words=n()) %>% 
  inner_join(Decades %>%
               group_by(Decade) %>%
               distinct(album) %>% 
               count %>% 
               rename(albums=n)) %>% 
  inner_join(Decades %>% 
               group_by(Decade) %>%
               mutate(conc=paste(track_title, album)) %>% 
               distinct(conc) %>% 
               count %>% 
               rename(songs=n)) %>% 
  mutate(words.album = words / albums,
         words.song = words / songs)


# Common words graph by decade
graph.common.words.decade <- lyrics %>% 
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>% 
  mutate(Decade = ifelse(year < 2000, 
                         paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                         ifelse(year < 2010, "00s", "10s"))) %>% 
  group_by(Decade) %>% 
  count(word, sort = TRUE) %>% 
  arrange(Decade, desc(n)) %>% 
  top_n(10, n) %>%
  mutate(word = reorder(word, n))


# 60s
graph1 <- graph.common.words.decade %>% 
  filter(Decade == "60s") %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = colores[3]) +
  labs(x = NULL,
       y = NULL,
       title = "60s") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 500)) +
  coord_flip() +
  guides(fill=FALSE)


# 70s
graph2 <- graph.common.words.decade %>% 
  filter(Decade == "70s") %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = colores[4]) +
  labs(x = NULL,
       y = NULL,
       title = "70s") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 500)) +
  coord_flip()
guides(fill=FALSE)


# 80s
graph3 <- graph.common.words.decade %>% 
  filter(Decade == "80s") %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = colores[5]) +
  labs(x = NULL,
       y = NULL,
       title = "80s") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 500)) +
  coord_flip() +
  guides(fill=FALSE)


#90s
graph4 <- graph.common.words.decade %>% 
  filter(Decade == "90s") %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = colores[6]) +
  labs(x = NULL,
       y = NULL,
       title = "90s") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 500)) +
  coord_flip() +
  guides(fill=FALSE)


# 00s
graph5 <- graph.common.words.decade %>% 
  filter(Decade == "00s") %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = colores[1]) +
  labs(x = NULL,
       y = NULL,
       title = "00s") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 500)) +
  coord_flip() +
  guides(fill=FALSE)


# 10s
graph6 <- graph.common.words.decade %>% 
  filter(Decade == "10s") %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = colores[2]) +
  labs(x = NULL,
       y = NULL,
       title = "10s") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 500)) +
  coord_flip() +
  guides(fill=FALSE)



# Generate graph
ggsave("Plots/graph_common_words_decade.png",
       ggplot2.multiplot(graph1, graph2, graph3,
                         graph4, graph5, graph6,
                         cols = 3))
ggsave("Plots/graph_common_words_decade.pdf",
       ggplot2.multiplot(graph1, graph2, graph3,
                         graph4, graph5, graph6,
                         cols = 3))


# Now do same plot with Facet Wrap

graph_decade_total <- graph.common.words.decade %>% 
  dplyr::ungroup() %>% 
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





# Word cloud

png(filename="Plots/Word_Cloud.png")
par(mar = rep(0, 4))
word_cloud <- lyrics %>% 
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>% 
  mutate(Decade = ifelse(year < 2000, 
                         paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                         ifelse(year < 2010, "00s", "10s")),
         Decade = factor(Decade,
                         levels = c("60s", "70s", "80s", "90s", "00s", "10s"))) %>% 
  group_by(Decade) %>% 
  count(word, sort = TRUE) %>% 
  arrange(Decade, desc(n)) %>% 
  top_n(100, n) %>% 
  dplyr::filter(!(Decade %in% c("60s", "70s"))) %>% 
  dplyr::group_by(Decade) %>% 
  dplyr::mutate(prop = n / sum(n)) %>% 
  reshape2::acast(word ~ Decade, value.var = "prop", fill = 0) %>% 
  comparison.cloud(max.words = 200,
                   scale = c(2, 0.25),
                   title.size = 2,
                   title.bg.colors = "white",
                   colors = colores[c(5, 6, 1, 2)])
dev.off()










# NET SENTIMENT ANALYSIS ####

albums.sentiments <- lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(year, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative,
         mean_sentiment = sentiment - mean(sentiment),
         Decade = ifelse(year < 2000, 
                         paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                         ifelse(year < 2010, "00s", "10s")))

# Positive sentiment per year
graph.albums.positive.sentiment <- albums.sentiments %>%
  ggplot(aes(x = year, y = positive, fill = Decade)) +
  geom_col() +
  scale_fill_discrete(breaks = c("60s", "70s", "80s", "90s", "00s", "10s")) +
  labs(x = "Year",
       y = "Positive sentiment") +
  scale_x_continuous(breaks = seq(from = 1960, to = 2020, by = 10),
                     labels = seq(from = 1960, to = 2020, by = 10)) +
  theme(text = element_text(size = 16))


# Negative sentiment per year
graph.albums.negative.sentiment <- albums.sentiments %>%
  ggplot(aes(x = year, y = negative, fill = Decade)) +
  geom_col() +
  scale_fill_discrete(breaks = c("60s", "70s", "80s", "90s", "00s", "10s")) +
  labs(x = "Year",
       y = "Negative sentiment") +
  scale_x_continuous(breaks = seq(from = 1960, to = 2020, by = 10),
                     labels = seq(from = 1960, to = 2020, by = 10)) +
  theme(text = element_text(size = 16))



# Net sentiment per year
graph.albums.sentiment <- albums.sentiments %>%
  ggplot(aes(x = year, y = sentiment, fill = Decade)) +
  geom_col() +
  scale_fill_discrete(breaks = c("60s", "70s", "80s", "90s", "00s", "10s")) +
  labs(x = "Year",
       y = "Net Sentiment") +
  scale_x_continuous(breaks = seq(from = 1960, to = 2020, by = 10),
                     labels = seq(from = 1960, to = 2020, by = 10)) +
  theme(text = element_text(size = 16))

ggsave("Plots/graph_annual_sentiment.png")
ggsave("Plots/graph_annual_sentiment.pdf")


# Net mean sentiment per year
graph.albums.mean.sentiment <- albums.sentiments %>%
  ggplot(aes(x = year, y = mean_sentiment, fill = Decade)) +
  geom_col() +
  scale_fill_discrete(breaks = c("60s", "70s", "80s", "90s", "00s", "10s")) +
  labs(x = "Year",
       y = "Net Sentiment") +
  scale_x_continuous(breaks = seq(from = 1960, to = 2020, by = 10),
                     labels = seq(from = 1960, to = 2020, by = 10)) +
  theme(text = element_text(size = 16))

ggsave("Plots/graph_annual_mean_sentiment.png")
ggsave("Plots/graph_annual_mean_sentiment.pdf")


# Net sentiment average by decade
graph.albums.mean.sentiment.decade <- albums.sentiments %>% 
  group_by(Decade) %>% 
  summarise(AvgSentiment = mean(sentiment)) %>% 
  arrange(factor(Decade, levels = c("60s", "70s", "80s", "90s", "00s", "10s"))) %>% 
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
  scale_fill_discrete(breaks = c("60s", "70s", "80s", "90s", "00s", "10s")) +
  theme(text = element_text(size = 16))

ggsave("Plots/graph_decade_mean_sentiment.png")
ggsave("Plots/graph_decade_mean_sentiment.pdf")



# PREDICTING THE UNPREDICTABLE ####


# Compute average net sentiments and other descriptive statistics
albums.sentiments %>% 
  select(sentiment) %>% 
  summary


# Graph net sentiment trend
sentiments_trend <- albums.sentiments %>% 
  ggplot(aes(year, sentiment)) +
  geom_point(aes(color = Decade),
             size = 3) +
  geom_line() +
  geom_smooth(method = "lm",
              color = "red",
              se = FALSE) +
  scale_color_discrete(breaks = c("60s", "70s", "80s", "90s", "00s", "10s")) +
  labs(x = "Year",
       y = "Net Sentiment") +
  theme(text = element_text(size = 16))

ggsave("Plots/graph_sentiment_linear_trend.png")
ggsave("Plots/graph_sentiment_linear_trend.pdf")

# Adjust model
model <- lm(albums.sentiments$sentiment ~ albums.sentiments$year)
summary(model)
plot(model)
acf(model$residuals)





# IMPORT NOT-WINNERS ####


# Artists and albums ####
artists_nw <- c("Andy Williams",
                "Al Hirt",
                "The Singing Nun",
                "Al Hirt",
                "The Beatles",
                "Richard Rodgers",
                "The Beatles",
                "Bobbie Gentry",
                "Simon and Garfunkel",
                "The Beatles",
                "Crosby, Stills, Nash And Young",
                "George Harrison",
                "Harry Nilsson",
                "Bette Midler",
                "Wings",
                "Linda Ronstadt",
                "George Benson",
                "James Taylor",
                "The Rolling Stones",
                "Donna Summer",
                "Pink Floyd",
                "Al Jarreau",
                "Paul McCartney",
                "The Police",
                "Tina Turner",
                "Sting",
                "Janet Jackson",
                "Michael Jackson",
                "Steve Winwood",
                "Don Henley",
                "Mariah Carey",
                "Amy Grant",
                "Annie Lennox",
                "Billy Joel",
                "Eric Clapton",
                "Joan Osborne",
                "Fugees",
                "Paul McCartney",
                "Garbage",
                "Dixie Chicks",
                "Beck",
                "Outkast",
                "Bruce Springsteen",
                "Justin Timberlake",
                "Usher",
                "Gwen Stefani",
                "Red Hot Chili Peppers",
                "Amy Winehouse",
                "Ne Yo",
                "Lady Gaga",
                "Lady Gaga",
                "Rihanna",
                "Fun",
                "Kendrick Lamar",
                "Beyonce",
                "Chris Stapleton",
                "Drake",
                "Kendrick Lamar")

albums_nw <- c("Days of Wine and Roses and Other TV Requests",
               "Honey in the Horn",
               "The Singing Nun",
               "Cotton Candy",
               "Help",
               "My Name is Barbra",
               "The Sound of Music Original Soundtrack Recording",
               "Revolver",
               "Ode to Billie Joe",
               "Bookends",
               "Abbey Road",
               "Deja Vu",
               "All Things Must Pass",
               "Nilsson Schmilsson",
               "The Divine Miss M",
               "Band on the Run",
               "Heart Like a Wheel",
               "Breezin",
               "JT",
               "Some Girls",
               "Bad Girls",
               "The Wall",
               "Breakin Away",
               "Tug of War",
               "Synchronicity",
               "Private Dancer",
               "The Dream of the Blue Turtles",
               "Control",
               "Bad",
               "Roll with It",
               "The End of the Innocence",
               "Mariah Carey",
               "Heart in Motion",
               "Diva",
               "River of Dreams",
               "From the Cradle",
               "Relish",
               "The Score",
               "Flaming Pie",
               "Version 2 0",
               "Fly",
               "Midnite Vultures",
               "Stankonia",
               "The Rising",
               "Justified",
               "Confessions",
               "Love. Angel. Music. Baby.",
               "Stadium Arcadium",
               "Back to Black",
               "Year of the Gentleman",
               "The Fame",
               "The Fame Monster",
               "Loud",
               "Some Nights",
               "good kid, m.A.A.d city",
               "Beyonce",
               "Traveller",
               "Views",
               "Damn")





# Import lyrics from not winners ####
lyrics_nw <- tibble(track_title = character(),
                    track_n = integer(),
                    lyric = character(),
                    line = integer(),
                    album = character(),
                    year = double())


# Import not winners
nw <- readr::read_csv("not_winners.csv")

# Save available lyrics in the lyrics tibble for further analyses
for(i in 1:length(nw$Year)){
  
  print(paste0(as.character(i), " - ", nw$Album[i], " by ", nw$Artist[i]))
  
  aux <- geniusR::genius_album(artist = nw$Artist[i],
                               album = nw$Album[i]) %>% 
    mutate(album = nw$Album[i],
           year = nw$Year[i])
  
  #beepr::beep(2)
  
  lyrics_nw <- bind_rows(lyrics_nw, aux)
}
beepr::beep(2)


# Compute net sentiment of songs
albums.sentiments_nw <- lyrics_nw %>% 
  unnest_tokens(word, lyric) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(album, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative,
         mean_sentiment = sentiment - mean(sentiment))


#         Decade = ifelse(year < 2000, 
#                         paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
#                         ifelse(year < 2010, "00s", "10s")))


# LOGISTIC REGRESSION ANALYSIS ####

# Create data set
dataset <- tibble(artist = c(artists[available],
                             albums.sentiments_nw %>% 
                               left_join(nw, by = c("album" = "Album")) %>% 
                               dplyr::select(Artist) %>% 
                               pull),
                  album = c(albums[available],
                            albums.sentiments_nw$album),
                  year = c(year[available],
                           albums.sentiments_nw %>% 
                             left_join(nw, by = c("album" = "Album")) %>% 
                             dplyr::select(Year) %>% 
                             pull),
                  sentiment = c(albums.sentiments$sentiment,
                                albums.sentiments_nw$sentiment),
                  winner = c(rep(1, nrow(albums.sentiments)), 
                             rep(0, nrow(albums.sentiments_nw))))



# Fit logistic regression
logistic <- glm(winner ~ sentiment,
                family = binomial(link = "logit"),
                data = dataset)

# Determine 0 and 100 values
b0 <- as.double(logistic$coefficients[1])
b1 <- as.double(logistic$coefficients[2])

p0 <- 1 / (1 + exp(-b0))
p100 <- 1 / (1 + exp(-b0 - 100*b1))



# Graph logistic regressions

# intercept <- logistic$coefficients[1]
# sentiment <- logistic$coefficients[2]
# 
# xx <- seq(from=-700, to=700, by=1)
# yy <- rep(0, length(xx))
# for(i in 1:length(yy)){
#   eta <- intercept + xx[i]*sentiment
#   yy[i] <- exp(eta) / (1 + exp(eta))
# }
# 
# plot(xx, yy)

# Determine lowest net sentiments of non-winners
albums.sentiments_nw %>% 
  arrange(sentiment) %>% 
  left_join(nw, by = c("album" = "Album"))


graph.logistic.regression <- dataset %>% 
  dplyr::mutate(Winner = ifelse(winner==1, "Yes", "No"),
                album_by_artist = paste0(album,
                                         " by ",
                                         artist),
                outlier = ifelse(sentiment < -250,
                                 album_by_artist,
                                 NA)) %>% 
  ggplot() +
  geom_point(aes(x = sentiment,
                 y = winner,
                 color = Winner)) +
  stat_smooth(aes(x = sentiment,
                  y = winner),
              method = "glm",
              method.args = list(family="binomial"), 
              se = FALSE,
              color = "red") +
  labs(x = "Net Sentiment",
       y = "Estimated probability of winning",
       color = "Won the\n Grammy?") +
  scale_color_manual(values = c("orange2", "green3"),
                       breaks = c("Yes", "No")) +
  theme(text = element_text(size = 16)) +
  geom_label(aes(x = -575, y = 0.47, label = "The Marshall Mathers LP by Eminem"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             size = 4) +
  geom_curve(aes(x = -625, y = 0.01, xend = -575, yend = 0.45), 
             colour = "#555555", 
             curvature = -0.2,
             size=0.25) +
  geom_label(aes(x = -346, y = 0.37, label = "The Eminem Show by Eminem"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             size = 4) +
  geom_curve(aes(x = -396, y = 0.01, xend = -346, yend = 0.35), 
             colour = "#555555", 
             curvature = -0.2,
             size=0.25) +
  geom_label(aes(x = -232, y = 0.27, label = "Recovery by Eminem"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             size = 4) +
  geom_curve(aes(x = -282, y = 0.01, xend = -232, yend = 0.25), 
             colour = "#555555", 
             curvature = -0.2,
             size=0.25)
  
  
  
  
  #geom_text_repel(aes(x = sentiment,
  #                    y = winner,
  #                    label = outlier), 
  #                na.rm = TRUE,
  #                size = 3,
  #                nudge_y = 0.4,
  #                angle = 15)
#xseq = seq(-300, 300, length=2000)

ggsave("Plots/graph_logistic_regression.png")
ggsave("Plots/graph_logistic_regression.pdf")




# Appendix ####

# Define porcentaje function
porcentaje <- function(x, digits = 1){
  paste0(round(100*x, digits), "%")
}

# Define numero function
numero <- function(x, digits = 0){
  paste0(round(x, digits))
}



# Net sentiment histogram
data_labels <- tibble(x = c(-575, -346, -232),
                      y = c(25, 20, 15),
                      lab = c("The Marshall Mathers LP by Eminem",
                              "The Eminem Show by Eminem",
                              "Recovery by Eminem"),
                      Winner = factor("Nominated to the Grammy but did not win",
                                      levels = c("Won Grammy Award for Album of the Year", 
                                                 "Nominated to the Grammy but did not win")))
data_curves <- tibble(x = c(-623, -403, -275),
                      xend = c(-575, -346, -232),
                      y = c(2, 2, 2),
                      yend = c(23, 18, 13),
                      Winner = factor("Nominated to the Grammy but did not win",
                                      levels = c("Won Grammy Award for Album of the Year", 
                                                 "Nominated to the Grammy but did not win")))

data_labels_positive <- tibble(x = 100,
                              y = 20,
                              lab = "Acoustic Soul by India Arie",
                              Winner = factor("Nominated to the Grammy but did not win",
                                              levels = c("Won Grammy Award for Album of the Year", 
                                                         "Nominated to the Grammy but did not win")))
data_curves_positive <- tibble(x = 270,
                               xend = 310,
                               y = 2,
                               yend = 18,
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


  # geom_label(data = data_labels_positive,
  #            aes(x = x, y = y, label = lab),
  #            hjust = 0, 
  #            vjust = 0.5, 
  #            colour = "#555555", 
  #            fill = "white", 
  #            label.size = NA, 
  #            size = 3) +
  # geom_curve(data = data_curves_positive,
  #            aes(x = x, y = y, xend = xend, yend = yend),
  #            colour = "#555555", 
  #            curvature = 0.2,
  #            size=0.25)

ggsave("Plots/graph_net_sentiment_histogram.png")
ggsave("Plots/graph_net_sentiment_histogram.pdf")



# 2019 nominees Analysis ####
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


### ### ###

# Histogram w/bbplot, BBC's package for data viz

library(bbplot)
bbc_histogram <- dataset %>%
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
  labs(title = "Why so serious?",
       subtitle = "Histograms of Grammy nominees and winners' net sentiment",
       x = "Net Sentiment",
       y = "Number of albums",
       fill = "Won the\n Grammy?") +
  scale_fill_manual(values = c("green3", "orange2"),
                    breaks = c("No", "Yes")) +
  scale_y_continuous(labels = numero) +
  geom_label(data = data_labels,
             aes(x = x, y = y, label = lab),
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             size = 3.5) +
  geom_curve(data = data_curves,
             aes(x = x, y = y, xend = xend, yend = yend),
             colour = "#555555", 
             curvature = -0.2,
             size=0.25) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() +
  theme(strip.background = element_blank())


finalise_plot(plot_name = bbc_histogram,
              source = "Sources: Genius, RGenius, tidytext. Grammy Awards from 1960 to 2018.",
              save_filepath = "Plots/BBC_histogram.png")