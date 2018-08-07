### ### ### ### ### ### ### ###
# Text Mining the Sentiments  #
#     Behind the Grammys      #
#                             #
# Author: Gian Carlo Diluvi   #
#               2018          #
### ### ### ### ### ### ### ###


# Preamble ####
library(tidyverse)
library(tidytext)
library(geniusR)
library(scales)
library(easyGgplot2)
library(BMS)
theme_set(theme_classic())


# Set parameters
colores <- hue_pal()(6)

rden <- function(n, den)
{
  diffs <- diff(den$x)
  # Making sure we have equal increments
  stopifnot(all(abs(diff(den$x) - mean(diff(den$x))) < 1e-9))
  total <- sum(den$y)
  den$y <- den$y / total
  ydistr <- cumsum(den$y)
  yunif <- runif(n)
  indices <- sapply(yunif, function(y) min(which(ydistr > y)))
  x <- den$x[indices]
  
  return(x)
}


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
             "George Harrison & Friends",
             "Stevie Wonder",
             "Stevie Wonder",
             "Paul Simon",
             "Stevie Wonder",
             "Fleetwod Mac",
             "Bee Gees",
             "Billy Joel",
             "Christopher Cross",
             "John Lennon & Yoko Ono",
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

sex <- c("M",
         "M",
         "M",
         "W",
         "M",
         "W",
         "M",
         "M",
         "M",
         "M",
         "M",
         "M",
         "W",
         "M",
         "M",
         "M",
         "M",
         "M",
         "M",
         "M",
         "M",
         "M",
         "Mix",
         "Mix",
         "M",
         "M",
         "M",
         "M",
         "M",
         "M",
         "W",
         "M",
         "W",
         "M",
         "W",
         "M",
         "W",
         "W",
         "M",
         "W",
         "M",
         "M",
         "Mix",
         "W",
         "M",
         "M",
         "M",
         "W",
         "M",
         "Mix",
         "W",
         "Mix",
         "W",
         "M",
         "M",
         "M",
         "W",
         "W",
         "M")

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
            "Blood, Sweat & Tears",
            "Bridge over Troubled Water",
            "Tapestry",
            "The Concert for Bangladesh",
            "Innervisions",
            "Fulfillingness' First Finale",
            "Still Crazy After All These Years",
            "Songs in the Key of Life",
            "Rumours",
            "Saturday Night Fever: The Original Movie Soundtrack",
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
            "O Brother, Where Art Thou? soundtrack",
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
                 year = double(),
                 sex = character())

# Only some albums' lyrics are available in genius
available <- c(2, 6:10, 12, 13, 15:18, 21, 22, 24:34, 36:42, 44:59)

# Save available lyrics in the lyrics tibble for further analyses
for(i in available){
  aux <- geniusR::genius_album(artist = artists[i],
                               album = albums[i]) %>% 
    mutate(album = albums[i],
           year = year[i],
           sex = sex[i])
  
  lyrics <- bind_rows(lyrics, aux)
}



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


Decades <- lyrics %>% 
  mutate(Decade = ifelse(year < 2000, 
                         paste(as.character(10*(floor( (year - 1900) / 10) )), "s", sep = ""),
                         ifelse(year < 2010, "00s", "10s"))) 

# Average songs per album by decade
Decades %>% group_by(Decade) %>% distinct(album) %>% count() %>% rename(n2 = n) %>% inner_join(Decades %>% group_by(Decade) %>% distinct(track_title) %>% count()) %>% mutate(avg = n/n2)

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
                     limits = c(0, 400)) +
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
                     limits = c(0, 400)) +
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
                     limits = c(0, 400)) +
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
                     limits = c(0, 400)) +
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
                     limits = c(0, 400)) +
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
                     limits = c(0, 400)) +
  coord_flip() +
  guides(fill=FALSE)



# Generate graph
ggsave("Plots/graph_common_words_decade.png",
       ggplot2.multiplot(graph1, graph2, graph3,
                         graph4, graph5, graph6,
                         cols = 3))



# NET SENTIMENT ANALYSIS
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
                     limits = c(0, 65)) +
  scale_fill_discrete(breaks = c("60s", "70s", "80s", "90s", "00s", "10s")) +
  theme(text = element_text(size = 16))

ggsave("Plots/graph_decade_mean_sentiment.png")


# BAYESIAN ANALYSIS ####
# Save density object
density.albums <- albums.sentiments %>% 
  select(sentiment) %>% 
  pull %>% 
  density()

# Obtain y-values for MH Algorithm
p.grammy <- dnorm(density.albums$x, mean = 0, sd = 10000)
dens.y <- approxfun(x = density.albums$x,
                    y = p.grammy * density.albums$y,
                    rule = 2)




# Metropolis-Hastings algorithm to sample
# from posterior distribution

N <- 1000   # Sample size
B <- 1000   # Number of iterations pero sample unit
X <- NULL   # Initialize sample


for(i in 1:N){
  
  x <- NULL
  x <- rnorm(n = 1, mean = 0, sd = 1000)   # Initial guess
  
  for(j in 2:B){
    
    y <- rnorm(n = 1, mean = 0, sd = 1000)  # From changes PDF
    # Hastings quotient
    r <- dens.y(y) / dens.y(x[j-1])
    r <- min(1, r)                          # To make it a probability
    
    xnew <- ifelse(runif(1) < r, y, x[j-1]) # Accept or reject new point
    x <- c(x, xnew)
    
  }
  
  # Take last number to be an observation of
  # the posterior distribution
  X <- c(X, x[B])
}





# Likelihood density graph
density.graph <- albums.sentiments %>%
  ggplot(aes(sentiment)) + 
  geom_density(fill = "skyblue") +
  labs(x = "Sentiment",
       y = "Density")

ggsave("Plots/graph_sentiment_density.png")



# Posterior density graph
posterior.density.graph <- tibble(Density = X) %>%
  ggplot(aes(Density)) + 
  geom_density(fill = "skyblue") +
  labs(x = "Sentiment",
       y = "Density")

ggsave("Plots/graph_sentiment_posterior_density.png")


ggplot2.multiplot(density.graph, posterior.density.graph)


# Graph all together
graph.together <- albums.sentiments %>% 
  select(Sentiment = sentiment) %>% 
  mutate(Density = "P(Sentiment | Grammy)") %>% 
  bind_rows(tibble(Sentiment = X,
                   Density = rep("P(Grammy | Sentiment)", length(X)))) %>% 
  bind_rows(tibble(Sentiment = rnorm(n = 10000, mean = 0, sd = 1000),
                   Density = rep("P(Grammy)", 10000))) %>% 
  ggplot(aes(x = Sentiment, fill = Density)) +
  geom_density(alpha = 0.25) +
  xlim(-150, 250) +
  scale_fill_discrete(breaks=c("P(Grammy)", "P(Sentiment | Grammy)", "P(Grammy | Sentiment)")) +
  labs(y = "Density") +
  theme(text = element_text(size = 16))

ggsave("Plots/graph_densities_together.png", graph.together)


# Only posterior
graph.posterior <- tibble(Sentiment = X) %>% 
  ggplot(aes(Sentiment)) +
  geom_density(fill = hue_pal()(3)[1], alpha = 0.25) +
  labs(y = "Density") +
  theme(text = element_text(size = 16)) +
  xlim(-150, 250)

ggsave("Plots/graph_posterior_density.png")



# MUSE ANALYSIS
# Import two latest Muse songs
dig.down <- genius_lyrics(artist = "Muse", song = "Dig Down")
thought.contagion <- genius_lyrics(artist = "Muse", song = "Thought Contagion")

# Compute net sentiment per song
muse <- bind_rows(dig.down, thought.contagion) %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(track_title, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Function with posterior density values
#posterior <- approxfun(density(X)$y, 
#                       rule = 2)
posterior <- ecdf(X)

# Probabilty of Muse winning the Grammy
posterior(2) - posterior(-5)





# More albums
second.law <- genius_album(artist = "Muse", album = "The 2nd Law") %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(track_title, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)


resistance <- genius_album(artist = "Muse", album = "The Resistance") %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(track_title, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)


drones <- genius_album(artist = "Muse", album = "Drones") %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(track_title, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)