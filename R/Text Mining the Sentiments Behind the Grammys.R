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
dataset <- tibble(sentiment = c(albums.sentiments$sentiment,
                                albums.sentiments_nw$sentiment),
                  winner = c(rep(1, 55), rep(0, 209)))

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
  dplyr::mutate(Winner = ifelse(winner==1, "Yes", "No")) %>% 
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
  theme(text = element_text(size = 16))
#xseq = seq(-300, 300, length=2000)

ggsave("Plots/graph_logistic_regression.png")
ggsave("Plots/graph_logistic_regression.pdf")