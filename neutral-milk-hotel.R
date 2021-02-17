library(spotifyr)
library(tidyverse)
library(knitr)
library(ggjoy)
library(plyr)
library(httr)
library(rvest)
library(stringr)
library(ggthemes)
library(tidytext)
library(ggridges)
library(wesanderson)
library(yarrr)
library(kableExtra)
library(radarchart)
library(textdata)
library(scales)
library(RColorBrewer)
library(highcharter)
library(wordcloud)

# using my personal API authentication details
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxx')


# pulling audio features for NMH's entire catalogue on Spotify
nmh_data <- get_artist_audio_features('neutral milk hotel', include_groups = "album")
nmh_data <- nmh_data[!duplicated(nmh_data$track_name),]
nmh_data <- subset(nmh_data, track_name!="A Baby For Pree")
str(nmh_data)

# Similar to with Spotify, I first used the search API call to get the artist_id.
token <- 'xxx'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q='
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('neutral milk hotel')
genius_artists

# loop through the contents of the songs endpoint (the limit is 50 per page), 
# pulling down each result (a list containing the url of the tracks’ lyrics) 
# until the next_page parameter was null.

baseURL <- 'https://api.genius.com/artists/'
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

filtered_track_lyric_urls <- c()
filtered_track_lyric_titles <- c()
index <- c()


for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$primary_artist$name == "Neutral Milk Hotel") {
    filtered_track_lyric_urls <- append(filtered_track_lyric_urls, track_lyric_urls[[i]]$url)
    filtered_track_lyric_titles <- append(filtered_track_lyric_titles, track_lyric_urls[[i]]$title)
    
    index <- append(index, i)
  }
}

filtered_track_lyric_titles[106] <- "You've Passed"
filtered_track_lyric_titles[101] <- "Where You'll Find Me Now"
filtered_track_lyric_titles[67] <- "Pree-Sisters Swallowing a Donkey's Eye"
filtered_track_lyric_titles[29] <- "Everything Is…"
filtered_track_lyric_titles[76] <- "Snow Song, Pt. 1"
filtered_track_lyric_titles[87] <- "King of Carrot Flowers Pt. 1"
filtered_track_lyric_titles[88] <- "King of Carrot Flowers Pts. 2 & 3"
filtered_track_lyric_titles[97] <- "Two-Headed Boy Pt. 2"


nmh_lyrics <- data.frame(filtered_track_lyric_urls, filtered_track_lyric_titles)
nmh_lyrics <- nmh_lyrics[filtered_track_lyric_titles %in% nmh_data$track_name, ]

nmh_lyrics$filtered_track_lyric_urls <- as.character(nmh_lyrics$filtered_track_lyric_urls)
nmh_lyrics$filtered_track_lyric_titles <- as.character(nmh_lyrics$filtered_track_lyric_titles)


# Webscraping lyrics using rvest 
lyric_text <- rep(NA, 25)
for (i in 1:25) {
  lyric_text[i] <- read_html(nmh_lyrics$filtered_track_lyric_urls[i]) %>% 
    html_nodes(".lyrics p") %>% 
    html_text()
}


# Cleaning and standardizing lyrics
for (i in 1:25) {
  lyric_text[i] <- gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text[i])
  lyric_text[i] <- gsub("\n", " ", lyric_text[i])
  lyric_text[i] <- gsub("\\[.*?\\]", " ", lyric_text[i])
  lyric_text[i] <- tolower(lyric_text[i])
  lyric_text[i] <- gsub("[ [:punct:] ]", " ", lyric_text[i])
  lyric_text[i] <- gsub(" {2,}", " ", lyric_text[i])
}

genius_data <- data.frame(track_name = nmh_lyrics$filtered_track_lyric_titles, lyrics = lyric_text)
genius_data$track_name <- as.character(genius_data$track_name)
genius_data$lyrics <- as.character(genius_data$lyrics)


# joining Spotify and Genius data
spotify_genius <- full_join(genius_data, nmh_data, by = "track_name")


ordered_albums <- factor(spotify_genius$album_name)
ordered_albums <- factor(ordered_albums,levels(ordered_albums)[c(2,1)])
spotify_genius$ordered_albums <- ordered_albums

options("digits" = 3)

# valence ridge plot (I used fig.height = 6, fig.width = 6 in an rmd)
spotify_genius %>% ggplot(aes(x = valence, y = ordered_albums, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "maroon3") + 
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  theme(legend.position = "none")


# table: top 10 songs by valence
spotify_genius %>% 
  select(track_name, album_name, valence) %>% 
  top_n(10) %>% 
  arrange(-valence) %>% 
  kable(col.names = c("Track", "Album", "Valence")) %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "deeppink")

# sonic score graph
pirateplot(danceability + energy ~ album_release_year, spotify_genius,
           xlab = "Album", ylab = "Jumpiness Score",
           theme = 1) 
legend("bottomright", c("1: On Avery Island", "2: In the Aeroplane Over the Sea"), bty = "n") 


# OAI sonic scores
spotify_genius %>% 
  mutate(sonic_score = danceability + energy) %>% 
  select(album_name, track_name, sonic_score) %>% 
  arrange(desc(sonic_score)) %>% 
  filter(album_name == "On Avery Island") %>% 
  kable(col.names = c("Album", "Track", "Jumpiness Score")) %>% 
  kable_styling(full_width = F, position = "left")


# songs by energy
spotify_genius %>% 
  summarise(track_name, energy) %>% 
  arrange(desc(`energy`)) %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1, background = "seashell", color = "#b39db2")

# tokenized and cleaned datasets of lyrics for textual analysis
nmh_word <- spotify_genius %>% unnest_tokens(word, lyrics)
better_lyrics <- nmh_word %>% anti_join(rbind(stop_words[1], "dee", "uh", "yeah", "hey", "baby", "ooh", "wanna", "gonna", "ah", "ahh", "ha", "la", "mmm", "whoa", "haa"))
better_lyrics$word[better_lyrics$word == "don" | better_lyrics$word == "didn"] <- NA
better_lyrics$word[better_lyrics$word == "ain"] <- NA
better_lyrics$word[better_lyrics$word == "isn"] <- NA
better_lyrics$word[better_lyrics$word == "usin"] <- "using"
better_lyrics$word[better_lyrics$word == "wouldn"] <- "wouldn't"
better_lyrics$word[better_lyrics$word == "couldn"] <- "couldn't"
better_lyrics$word[better_lyrics$word == "shouldn"] <- "shouldn't"
better_lyrics$word[better_lyrics$word == "won"] <- "won't"
better_lyrics$word[better_lyrics$word == "ve" | better_lyrics$word == "ll"] <- NA
better_lyrics <- na.omit(better_lyrics)

# wordcloud: all
word_count <- better_lyrics %>%
  dplyr::count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count$word, freq = word_count$n,
          random.order=FALSE, 
          colors= c(wes_palettes$Moonrise3[c(1:35,90)], wes_palettes$IsleofDogs2[1]))

# wordcloud: ITAOTS
word_count_rep <- better_lyrics %>%
  filter(album_name == "In the Aeroplane Over the Sea") %>% 
  dplyr::count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count_rep$word, freq = word_count_rep$n,
          random.order=FALSE, 
          colors= c(wes_palettes$BottleRocket2))


# joining the tokenized, tidied lyric dataset with sentiment lexicons
bing <- get_sentiments("bing") %>% 
  mutate(lexicon = "bing", 
         words_in_lexicon = n_distinct(word))    

nrc <- get_sentiments("nrc") %>% 
  mutate(lexicon = "nrc", 
         words_in_lexicon = n_distinct(word))

new_sentiments <- bind_rows(sentiments, bing, nrc)

joy_words <- new_sentiments %>%
  filter(lexicon == 'nrc', sentiment == 'joy') %>%
  select(word) %>%
  mutate(joy = T)

sent_df <- better_lyrics %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(joy_words, by = 'word') %>%
  group_by(track_name) %>%
  dplyr::summarise(pct_joy = round(sum(joy, na.rm = T) / n(), 4),
                   word_count = n()) %>%
  ungroup

sent_df %>%
  select(pct_joy, track_name) %>%
  arrange(desc(pct_joy)) %>%
  head(10) %>%
  kable(col.names = c("Pct. Joy", "Track")) %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "palevioletred")

nrc_nmh <- better_lyrics %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

afinn_nmh <- better_lyrics %>% 
  inner_join(get_sentiments("afinn"))

bing_nmh <- better_lyrics %>% 
  inner_join(get_sentiments("bing"))

# sentiment scores using AFINN
dim <- afinn_nmh %>% 
  dplyr::count(album_name)
afinn_nmh %>%
  group_by(album_name) %>% 
  summarise(sum(value)) %>% 
  ggplot(aes(x = album_name, y = sum(value))) +
  geom_bar(stat = "identity") +
  ylim(-200,200) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("palevioletred", "violetred3")) +
  theme(legend.position="none")

# OAI pyramid plot
pyr_avery <- bing_nmh %>%
  unique() %>% 
  group_by(track_name, sentiment, album_name) %>%
  dplyr::count(track_name, sentiment) %>%
  filter(album_name == "On Avery Island")

for(i in 1:14) {
  if(pyr_avery$sentiment[i] == "negative")
    pyr_avery$n[i] <- -pyr_avery$n[i]
}

pyr_avery %>% 
  ggplot(aes(x = track_name, y = n, fill = sentiment)) + 
  geom_bar(subset = .(sentiment == "positive"), stat = "identity") + 
  geom_bar(subset = .(sentiment == "negative"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-20, 20, 5)) +
  coord_flip() +
  theme_fivethirtyeight() +
  ylim(-20,10) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("palevioletred", "olivedrab3")) +
  theme(legend.position="none")


# all-album radar chart
sentiment_nrc <- nrc_nmh %>%
  group_by(album_name, sentiment) %>%
  dplyr::count(album_name, sentiment) %>% 
  select(album_name, sentiment, sentiment_total = n)

album_nrc <- nrc_nmh %>%
  dplyr::count(album_name) %>% 
  select(album_name, album_total = n)

radar_chart <- sentiment_nrc %>% 
  inner_join(album_nrc, by = "album_name") %>% 
  mutate(percent = round((sentiment_total/album_total * 100), 3)) %>% 
  select(-sentiment_total, -album_total) %>%
  spread(album_name, percent)

chartJSRadar(radar_chart, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 25,
             colMatrix = matrix(c(0, 255, 255, 255, 185, 15, 139, 0, 139, 
                                  255, 0, 0, 201, 167, 198, 0, 0, 0), byrow = F, nrow = 3))

# by joy

track_df <- better_lyrics %>%
  left_join(sent_df, by = 'track_name') %>%
  mutate_at(c('pct_joy', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>%
  mutate(lyrical_density = word_count / duration_ms * 1000,
         joy_index = round(rescale(1 - (valence + (pct_joy * (1 + lyrical_density))) / 2, to = c(1, 100)), 2))


track_df %>%
  select(joy_index, track_name) %>%
  arrange(desc(joy_index)) %>%
  unique() %>%
  head(10) %>%
  kable(col.names = c("Joy Index", "Track")) %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "palevioletred")

plot_df <- track_df %>%
  rowwise %>%
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                          '<img src=', album_images, ' height="50" style="float:left;margin-right:5px">',
                          '<b>Album:</b> ', album_name,
                          '<br><b>Track:</b> ', track_name)) %>%
  ungroup

avg_line <- plot_df %>%
  dplyr::group_by(album_release_year, album_name, album_images) %>%
  dplyr::summarise(avg = mean(joy_index)) %>%
  ungroup %>%
  dplyr::transmute(x = as.numeric(as.factor(album_release_year)),
                   y = avg,
                   tooltip = paste0('<a style = "margin-right:55px">',
                                    '<img src=', album_images, ' height="50" style="float:left;margin-right:5px">',
                                    '<b>Album:</b> ', album_name,
                                    '<br><b>Average Joy Index:</b> ', round(avg, 2),
                                    '</a>'))
plot_track_df <- plot_df %>%
  mutate(tooltip = paste0(tooltip, '<br><b>Joy Index:</b> ', joy_index, '</a>'),
         album_number = as.numeric(as.factor(album_release_year))) %>%
  ungroup

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = joy_index, group = album_name)) %>%
  hc_add_series(data = avg_line, type = 'line', name = 'Album Averages') %>%
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
  hc_colors(c(sample(brewer.pal(n_distinct(track_df$album_name), 'Paired')), 'black')) %>%
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>%
  hc_yAxis(max = 100, title = list(text = 'Joy Index')) %>%
  hc_title(text = 'Data Driven Musical Analysis') %>%
  hc_subtitle(text = 'Neutral Milk Hotel song happiness by album') %>%
  hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[3]]$name <- 'Album Averages'
album_chart
