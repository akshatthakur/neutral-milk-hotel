--- 
title: "NeutRal Milk Hotel"
author: "Akshat Thakur"
description: "This is a personal project."
---

Neutral Milk Hotel is one of the most surreal and enigmatic artists in the history of music.
I am, myself, a huge fan of the band, and thought it'd be a cool project to analyze the intricacies of their music using data science. 

This will be a data-driven process to analyze and visualize different aspects of Neutral Milk Hotel's discography.

## About Neutral Milk Hotel

> Neutral Milk Hotel was an American rock band formed in Ruston, Louisiana, by musician Jeff Mangum. The band's music featured a deliberately low-quality sound, influenced by indie rock and psychedelic folk. Mangum was the band's lyricist, and wrote surreal and opaque songs that covered a wide range of topics, including love, spirituality, nostalgia, and loneliness. He and the other band members played a variety of instruments, including nontraditional rock instruments like the singing saw, zanzithophone, and uilleann pipes.

(from Wikipedia)

## Installing and loading a bunch of libraries

```r
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
library(wordcloud)
library(highcharter)
```

## Scraping, Tidying and Standardizing

### Getting Data
The **Web API from Spotify** provides some really interesting audio stats for all songs in their catalogue. Some of the most interesting metrics available on Spotify are:

>**Valence**
A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).

>**Energy**
Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.

>**Danceability**
Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.

So that's what part of what I'll use from the data I'll get from the Spotify API.

**Genius**, the world's most popular (and aesthetically pleasing) lyrics website, too, has a great API that can be used to pull song data. 

### Spotify API
As well documented as the Spotify API is, it's a very complicated and long drawn process to get all the songs for any given artist, and includes a lot of tedious steps like identifying the particular "uri" from a huge list of "uri"s. Fortunately, an R genius called RCharlie designed the spotifyr package, which simplifies the process of grabbing all the information for any artist on Spotify. 


```r
devtools::install_github('charlie86/spotifyr')
library(spotifyr)
```

Use your API authentication details.




```r
Sys.setenv(SPOTIFY_CLIENT_ID = 'your id here')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your secret here')
```

I can now pull audio features for Neutral Milk Hotel. 
I'm filtering out duplicates. 


```r
nmh_data <- get_artist_audio_features('neutral milk hotel', include_groups = "album")
nmh_data <- nmh_data[!duplicated(nmh_data$track_name),]
nmh_data <- subset(nmh_data, track_name!="A Baby For Pree")
str(nmh_data)
```

```
## 'data.frame':	25 obs. of  39 variables:
##  $ artist_name                 : chr  "Neutral Milk Hotel" "Neutral Milk Hotel" "Neutral Milk Hotel" "Neutral Milk Hotel" ...
##  $ artist_id                   : chr  "2ooIqOf4X2uz4mMptXCtie" "2ooIqOf4X2uz4mMptXCtie" "2ooIqOf4X2uz4mMptXCtie" "2ooIqOf4X2uz4mMptXCtie" ...
##  $ album_id                    : chr  "5COXoP5kj2DWfCDg0vxi4F" "5COXoP5kj2DWfCDg0vxi4F" "5COXoP5kj2DWfCDg0vxi4F" "5COXoP5kj2DWfCDg0vxi4F" ...
##  $ album_type                  : chr  "album" "album" "album" "album" ...
##  $ album_images                :List of 25
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00001e02fed522e78ecdc9fba4a0cc07" "https://i.scdn.co/image/ab67616d00004851fed522e78ecdc9fba4a0cc07"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b2737e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d00001e027e15c61a6a86ef61c9ecc655" "https://i.scdn.co/image/ab67616d000048517e15c61a6a86ef61c9ecc655"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273538aa3a9a6d6059254bba72b" "https://i.scdn.co/image/ab67616d00001e02538aa3a9a6d6059254bba72b" "https://i.scdn.co/image/ab67616d00004851538aa3a9a6d6059254bba72b"
##   .. ..$ width : int  640 300 64
##   ..$ :'data.frame':	3 obs. of  3 variables:
##   .. ..$ height: int  640 300 64
##   .. ..$ url   : chr  "https://i.scdn.co/image/ab67616d0000b273538aa3a9a6d6059254bba72b" "https://i.scdn.co/image/ab67616d00001e02538aa3a9a6d6059254bba72b" "https://i.scdn.co/image/ab67616d00004851538aa3a9a6d6059254bba72b"
##   .. ..$ width : int  640 300 64
##  $ album_release_date          : chr  "1998-02-20" "1998-02-20" "1998-02-20" "1998-02-20" ...
##  $ album_release_year          : num  1998 1998 1998 1998 1998 ...
##  $ album_release_date_precision: chr  "day" "day" "day" "day" ...
##  $ danceability                : num  0.419 0.221 0.275 0.342 0.284 0.0617 0.192 0.366 0.192 0.0733 ...
##  $ energy                      : num  0.519 0.826 0.714 0.613 0.683 0.887 0.377 0.357 0.943 0.873 ...
##  $ key                         : int  5 5 7 7 2 7 7 4 4 4 ...
##  $ loudness                    : num  -6.47 -5.39 -5.12 -4.08 -4.91 ...
##  $ mode                        : int  1 1 1 1 0 1 1 0 1 1 ...
##  $ speechiness                 : num  0.0334 0.126 0.0328 0.0311 0.0303 0.0636 0.0352 0.0342 0.0826 0.119 ...
##  $ acousticness                : num  0.11 0.0514 0.395 0.705 0.0557 0.0399 0.285 0.744 0.0245 0.00189 ...
##  $ instrumentalness            : num  8.89e-01 0.00 0.00 0.00 8.55e-01 1.57e-04 8.13e-01 1.97e-05 7.61e-06 3.86e-01 ...
##  $ liveness                    : num  0.409 0.135 0.115 0.14 0.529 0.619 0.173 0.11 0.135 0.13 ...
##  $ valence                     : num  0.343 0.116 0.242 0.267 0.308 0.48 0.0976 0.117 0.223 0.11 ...
##  $ tempo                       : num  94 130.6 94.1 145.8 109.8 ...
##  $ track_id                    : chr  "17Nowmq4iF2rkbd1rAe1Vt" "6dFlPqbcAjpvDKp8voczqX" "4OciRObYGzPzlU40U7YRc8" "19Ov4l8mtvCT1iEUKks4aM" ...
##  $ analysis_url                : chr  "https://api.spotify.com/v1/audio-analysis/17Nowmq4iF2rkbd1rAe1Vt" "https://api.spotify.com/v1/audio-analysis/6dFlPqbcAjpvDKp8voczqX" "https://api.spotify.com/v1/audio-analysis/4OciRObYGzPzlU40U7YRc8" "https://api.spotify.com/v1/audio-analysis/19Ov4l8mtvCT1iEUKks4aM" ...
##  $ time_signature              : int  4 4 3 4 3 4 4 3 4 4 ...
##  $ artists                     :List of 25
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##   ..$ :'data.frame':	1 obs. of  6 variables:
##   .. ..$ href                 : chr "https://api.spotify.com/v1/artists/2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ id                   : chr "2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ name                 : chr "Neutral Milk Hotel"
##   .. ..$ type                 : chr "artist"
##   .. ..$ uri                  : chr "spotify:artist:2ooIqOf4X2uz4mMptXCtie"
##   .. ..$ external_urls.spotify: chr "https://open.spotify.com/artist/2ooIqOf4X2uz4mMptXCtie"
##  $ available_markets           :List of 25
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AE" "AR" "AU" "BH" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##   ..$ : chr  "AD" "AE" "AL" "AR" ...
##  $ disc_number                 : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ duration_ms                 : int  120426 186280 202346 266106 113146 192533 117306 498146 248573 136026 ...
##  $ explicit                    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ track_href                  : chr  "https://api.spotify.com/v1/tracks/17Nowmq4iF2rkbd1rAe1Vt" "https://api.spotify.com/v1/tracks/6dFlPqbcAjpvDKp8voczqX" "https://api.spotify.com/v1/tracks/4OciRObYGzPzlU40U7YRc8" "https://api.spotify.com/v1/tracks/19Ov4l8mtvCT1iEUKks4aM" ...
##  $ is_local                    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ track_name                  : chr  "King of Carrot Flowers Pt. 1" "King of Carrot Flowers Pts. 2 & 3" "In the Aeroplane Over the Sea" "Two-Headed Boy" ...
##  $ track_preview_url           : chr  "https://p.scdn.co/mp3-preview/819c776142451ea143b5f6fc8a8ed8dad6318da8?cid=22fe4d31b88044ceb0f550d7f9c5e04e" "https://p.scdn.co/mp3-preview/51dca6df0755318b3b8c6f532caf8726fe2f7f50?cid=22fe4d31b88044ceb0f550d7f9c5e04e" "https://p.scdn.co/mp3-preview/23f5b39b7eb6b13081ec4a8705405e1c75fbd836?cid=22fe4d31b88044ceb0f550d7f9c5e04e" "https://p.scdn.co/mp3-preview/d4e4930b5e3c21bfe9fbec6af8643ce5e0670c91?cid=22fe4d31b88044ceb0f550d7f9c5e04e" ...
##  $ track_number                : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ type                        : chr  "track" "track" "track" "track" ...
##  $ track_uri                   : chr  "spotify:track:17Nowmq4iF2rkbd1rAe1Vt" "spotify:track:6dFlPqbcAjpvDKp8voczqX" "spotify:track:4OciRObYGzPzlU40U7YRc8" "spotify:track:19Ov4l8mtvCT1iEUKks4aM" ...
##  $ external_urls.spotify       : chr  "https://open.spotify.com/track/17Nowmq4iF2rkbd1rAe1Vt" "https://open.spotify.com/track/6dFlPqbcAjpvDKp8voczqX" "https://open.spotify.com/track/4OciRObYGzPzlU40U7YRc8" "https://open.spotify.com/track/19Ov4l8mtvCT1iEUKks4aM" ...
##  $ album_name                  : chr  "In the Aeroplane Over the Sea" "In the Aeroplane Over the Sea" "In the Aeroplane Over the Sea" "In the Aeroplane Over the Sea" ...
##  $ key_name                    : chr  "F" "F" "G" "G" ...
##  $ mode_name                   : chr  "major" "major" "major" "major" ...
##  $ key_mode                    : chr  "F major" "F major" "G major" "G major" ...
```

### Genius API

I didn't have to use any specific package to do this. I just used the search API call from Genius to get the artist ID for Neutral Milk Hotel.


```r
token <- 'your token here'
```




```r
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
```

```
## # A tibble: 1 x 2
##   artist_id artist_name       
##       <int> <chr>             
## 1     16004 Neutral Milk Hotel
```

Then, I looped through the contents of the songs. We pull down a list with the url of the lyrics for each song.


```r
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
```

Then, I filtered to get urls for all the songs that have Neautral Milk Hotel as the primary artist.


```r
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
```

Time to fix some inconsistencies between the track names on Spotify and Genius respectively.


```r
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
```

Then, I used rvest to webscrape lyrics from the urls.


```r
lyric_text <- rep(NA, 25)
for (i in 1:25) {
  lyric_text[i] <- read_html(nmh_lyrics$filtered_track_lyric_urls[i]) %>% 
    html_nodes(".lyrics p") %>% 
    html_text()
}
```

And then, I cleaned and standardized all the lyrics.


```r
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
```

### Joining Spotify and Genius Data


```r
spotify_genius <- full_join(genius_data, nmh_data, by = "track_name")
```

I also ordered the albums, with their names as factors.


```r
ordered_albums <- factor(spotify_genius$album_name)
ordered_albums <- factor(ordered_albums,levels(ordered_albums)[c(2,1)])
spotify_genius$ordered_albums <- ordered_albums
```

The oven is now preheated to 350, and we can finally get into the good stuff, hehehe.


## Musical Analysis and Visualization
**Based on Spotify Data alone.** 

### Ridge Plot of Valence

```r
spotify_genius %>% ggplot(aes(x = valence, y = ordered_albums, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "maroon1") + 
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  theme(legend.position = "none")
```

```
## Picking joint bandwidth of 0.0865
```

<img src="neutral-milk-hotel_final_files/figure-html/unnamed-chunk-16-1.png" width="672" />

### Top 10 Songs (by Valence)
These are, essentially, as per the secrets in Spotify's vaults, the happiest Neutral Milk Hotel songs.


```r
spotify_genius %>% 
  select(track_name, album_name, valence) %>% 
  top_n(10) %>% 
  arrange(-valence) %>% 
  kable(col.names = c("Track", "Album", "Valence")) %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "deeppink")
```

```
## Selecting by valence
```

<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Track </th>
   <th style="text-align:left;"> Album </th>
   <th style="text-align:right;"> Valence </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> Everything Is… </td>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> On Avery Island </td>
   <td style="text-align:right;color: deeppink !important;background-color: azure !important;"> 0.793 </td>
  </tr>
  <tr>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> Song Against Sex </td>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> On Avery Island </td>
   <td style="text-align:right;color: deeppink !important;background-color: azure !important;"> 0.694 </td>
  </tr>
  <tr>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> Holland, 1945 </td>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> In the Aeroplane Over the Sea </td>
   <td style="text-align:right;color: deeppink !important;background-color: azure !important;"> 0.480 </td>
  </tr>
  <tr>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> King of Carrot Flowers Pt. 1 </td>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> In the Aeroplane Over the Sea </td>
   <td style="text-align:right;color: deeppink !important;background-color: azure !important;"> 0.343 </td>
  </tr>
  <tr>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> Naomi </td>
   <td style="text-align:left;color: deeppink !important;background-color: azure !important;"> On Avery Island </td>
   <td style="text-align:right;color: deeppink !important;background-color: azure !important;"> 0.316 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Avery Island / April 1st </td>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:right;"> 0.310 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gardenhead / Leave Me Alone </td>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:right;"> 0.310 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Fool </td>
   <td style="text-align:left;"> In the Aeroplane Over the Sea </td>
   <td style="text-align:right;"> 0.308 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Two-Headed Boy </td>
   <td style="text-align:left;"> In the Aeroplane Over the Sea </td>
   <td style="text-align:right;"> 0.267 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> In the Aeroplane Over the Sea </td>
   <td style="text-align:left;"> In the Aeroplane Over the Sea </td>
   <td style="text-align:right;"> 0.242 </td>
  </tr>
</tbody>
</table>

### Jumpiness Score Graph
Jumpiness Score, i.e. a measure of how much a song makes you jump, a metric devised by your truly, is the sum of a song's danceability and energy. 

I'm making a pirateplot using the yarrr package. 


```r
pirateplot(danceability + energy ~ album_release_year, spotify_genius,
           xlab = "Album", ylab = "Jumpiness Score",
           theme = 1) 
legend("bottomright", c("1: On Avery Island", "2: In the Aeroplane Over the Sea"), bty = "n", cex = 0.6) 
```

<img src="neutral-milk-hotel_final_files/figure-html/unnamed-chunk-18-1.png" width="672" />

#### Top 10 Songs (by Jumpiness)

```r
spotify_genius %>% 
  mutate(sonic_score = danceability + energy) %>% 
  select(album_name, track_name, sonic_score) %>% 
  arrange(desc(sonic_score)) %>% 
  filter(album_name == "On Avery Island") %>% 
  kable(col.names = c("Album", "Track", "Jumpiness Score")) %>% 
  kable_styling(full_width = F, position = "left")
```

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Album </th>
   <th style="text-align:left;"> Track </th>
   <th style="text-align:right;"> Jumpiness Score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Everything Is… </td>
   <td style="text-align:right;"> 1.0940 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Gardenhead / Leave Me Alone </td>
   <td style="text-align:right;"> 1.0390 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Song Against Sex </td>
   <td style="text-align:right;"> 0.9590 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> You've Passed </td>
   <td style="text-align:right;"> 0.8380 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Snow Song, Pt. 1 </td>
   <td style="text-align:right;"> 0.7210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> A Baby for Pree </td>
   <td style="text-align:right;"> 0.7070 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Where You'll Find Me Now </td>
   <td style="text-align:right;"> 0.6860 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Someone Is Waiting </td>
   <td style="text-align:right;"> 0.6430 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Naomi </td>
   <td style="text-align:right;"> 0.6030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Marching Theme </td>
   <td style="text-align:right;"> 0.5430 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> April 8th </td>
   <td style="text-align:right;"> 0.5150 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Three Peaches </td>
   <td style="text-align:right;"> 0.3906 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Pree-Sisters Swallowing a Donkey's Eye </td>
   <td style="text-align:right;"> 0.3104 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> On Avery Island </td>
   <td style="text-align:left;"> Avery Island / April 1st </td>
   <td style="text-align:right;"> 0.1810 </td>
  </tr>
</tbody>
</table>

## Lyrical Analysis and Visualization
**Based on Genius Lyrics alone.**

I have tokenized and cleaned the datasets of track lyrics for more accurate analysis.


```r
nmh_word <- spotify_genius %>% unnest_tokens(word, lyrics)
better_lyrics <- nmh_word %>% anti_join(rbind(stop_words[1], "dee", "uh", "yeah", "hey", "baby", "ooh", "wanna", "gonna", "ah", "ahh", "ha", "la", "mmm", "whoa", "haa"))
```

```
## Joining, by = "word"
```

```r
better_lyrics$word[better_lyrics$word == "don" | better_lyrics$word == "didn"] <- NA
better_lyrics$word[better_lyrics$word == "ain"] <- NA
better_lyrics$word[better_lyrics$word == "isn"] <- NA
better_lyrics$word[better_lyrics$word == "shouldn"] <- "shouldn't"
better_lyrics$word[better_lyrics$word == "won"] <- "won't"
better_lyrics$word[better_lyrics$word == "wouldn"] <- "wouldn't"
better_lyrics$word[better_lyrics$word == "couldn"] <- "couldn't"
better_lyrics$word[better_lyrics$word == "ve" | better_lyrics$word == "ll"] <- NA
better_lyrics <- na.omit(better_lyrics)
```

### Neutral Milk Hotel: wordclouds

Here's a wordcloud built from the entire set of Neutral Milk Hotel lyrics.

```r
word_count <- better_lyrics %>%
  dplyr::count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count$word, freq = word_count$n,
          random.order=FALSE, 
          colors= c(wes_palettes$Moonrise3[c(1:35,90)], wes_palettes$IsleofDogs2[1]))
```

<img src="neutral-milk-hotel_final_files/figure-html/unnamed-chunk-21-1.png" width="672" />

Here's a wordcloud built from songs in Neutral Milk Hotel's most popular album, In the Aeroplane Over the Sea:


```r
word_count_rep <- better_lyrics %>%
  filter(album_name == "In the Aeroplane Over the Sea") %>% 
  dplyr::count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count_rep$word, freq = word_count_rep$n,
          random.order=FALSE, 
          colors= c(wes_palettes$BottleRocket2))
```

<img src="neutral-milk-hotel_final_files/figure-html/unnamed-chunk-22-1.png" width="672" />

### Sentiment Analysis
I used the super cool tidytext library and three general-purpose lexicons - 

AFINN from Finn Årup Nielsen,

bing from Bing Liu and collaborators, and

nrc from Saif Mohammad and Peter Turney.

>All three of these lexicons are based on unigrams, i.e., single words. These lexicons contain many English words and the words are assigned scores for positive/negative sentiment, and also possibly emotions like joy, anger, sadness, and so forth. The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. The bing lexicon categorizes words in a binary fashion into positive and negative categories. The AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment.

(from <https://www.tidytextmining.com/sentiment.html>)

I am joining the sentiment lexicons with the lyrics datasets I previously tidied up and tokenized. 

```r
bing <- get_sentiments("bing") %>% 
  mutate(lexicon = "bing", 
         words_in_lexicon = n_distinct(word))    

nrc <- get_sentiments("nrc") %>% 
  mutate(lexicon = "nrc", 
         words_in_lexicon = n_distinct(word))

new_sentiments <- bind_rows(sentiments, bing, nrc)
```

To play around with the NRC lexicon, for starters, I tried to quantify the happiness of Neutral Milk Hotel songs. For this, I found out the percentage of "happy" words in each song, non-inclusive of extraneous words like articles, conjunctions, pronouns, etc.


```r
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
```

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:right;"> Pct. Joy </th>
   <th style="text-align:left;"> Track </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 0.2564 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> Three Peaches </td>
  </tr>
  <tr>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 0.2267 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> In the Aeroplane Over the Sea </td>
  </tr>
  <tr>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 0.1754 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> Naomi </td>
  </tr>
  <tr>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 0.1700 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> Two-Headed Boy Pt. 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 0.1538 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> King of Carrot Flowers Pts. 2 &amp; 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.1455 </td>
   <td style="text-align:left;"> Everything Is… </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.1449 </td>
   <td style="text-align:left;"> Oh Comely </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.1299 </td>
   <td style="text-align:left;"> Holland, 1945 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.1250 </td>
   <td style="text-align:left;"> Snow Song, Pt. 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.0962 </td>
   <td style="text-align:left;"> Communist Daughter </td>
  </tr>
</tbody>
</table>

By the percentage of happy words, “Three Peaches” wins, with over 25% of its lyrics containing happy words.


Let's find out sentiment scores using bing now.

I made a pyramid plot for On Avery Island. This is for positive v.s. negative sentiments.

```r
bing_nmh <- better_lyrics %>% 
  inner_join(get_sentiments("bing"))
```

```
## Joining, by = "word"
```

```r
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
```

```
## Warning: Ignoring unknown parameters: subset

## Warning: Ignoring unknown parameters: subset
```

```
## Scale for 'y' is already present. Adding another scale for 'y', which will
## replace the existing scale.
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

<img src="neutral-milk-hotel_final_files/figure-html/unnamed-chunk-25-1.png" width="672" />

#### Sentiment Radar

I used chart.js to create a radar chart for all Neutral Milk Hotel songs' sentiment analysis via the NRC lexicon. Doesn't it look really pretty?

```r
nrc_nmh <- better_lyrics %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))
```

```
## Joining, by = "word"
```

```r
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
```

```{=html}
<canvas id="htmlwidget-ca3d9031354e015ba285" class="chartJSRadar html-widget" width="672" height="480"></canvas>
<script type="application/json" data-for="htmlwidget-ca3d9031354e015ba285">{"x":{"data":{"labels":["anger","anticipation","disgust","fear","joy","sadness","surprise","trust"],"datasets":[{"label":"In the Aeroplane Over the Sea","data":[3.725,17.479,6.59,8.023,24.642,11.175,9.169,19.198],"backgroundColor":"rgba(0,255,255,0.1)","borderColor":"rgba(0,255,255,0.8)","pointBackgroundColor":"rgba(0,255,255,0.8)","pointBorderColor":"#fff","pointHoverBackgroundColor":"#fff","pointHoverBorderColor":"rgba(0,255,255,0.8)"},{"label":"On Avery Island","data":[9.756,14.634,8.537,10.976,20.732,13.11,6.707,15.549],"backgroundColor":"rgba(255,185,15,0.1)","borderColor":"rgba(255,185,15,0.8)","pointBackgroundColor":"rgba(255,185,15,0.8)","pointBorderColor":"#fff","pointHoverBackgroundColor":"#fff","pointHoverBorderColor":"rgba(255,185,15,0.8)"}]},"options":{"responsive":true,"title":{"display":false,"text":null},"scale":{"ticks":{"max":25,"min":0},"pointLabels":{"fontSize":18}},"tooltips":{"enabled":true,"mode":"label"},"legend":{"display":true}}},"evals":[],"jsHooks":[]}</script>
```

## Visualizing Neutral Milk Hotel using both Spotify Data and Lyrical Content

Now, for the best thing of all.

##### Lyrical Density
To combine both musical properties (as determined by Spotify) and lyrical sentiment, I used the concept of Lyrical Density.
>The number of lyrics per song over the track length.

(As defined by Myles Harrison)

This is to calculate how significant lyrical content is for each track. 

$lyricalDensity = \frac{wordCount}{duration}$

##### Joy Index
I will attempt to determine the Joy Index, the average of valence and percentage of happy words for every song, weighted by the lyrical density.
Mathematically, $joyIndex = \frac{valence + (joyPercentage * (1 + lyricalDensity))}{2}$

I have also rescaled the Joy Index to fit within 1 and 100, with the saddest song at 1 and the happiest song at 100.

(Note that Spotify notes track duration in microseconds.)


```r
track_df <- better_lyrics %>%
  left_join(sent_df, by = 'track_name') %>%
  mutate_at(c('pct_joy', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>%
  mutate(lyrical_density = word_count / duration_ms * 1000,
         joy_index = round(rescale(valence + (pct_joy * (1 + lyrical_density)) / 2, to = c(1, 100)), 2))
```

```
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## Please use a list of either functions or lambdas: 
## 
##   # Simple named list: 
##   list(mean = mean, median = median)
## 
##   # Auto named with `tibble::lst()`: 
##   tibble::lst(mean, median)
## 
##   # Using lambdas
##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
```

##### Moment of Truth 
Let's do this!
 

```r
track_df %>%
  select(joy_index, track_name) %>%
  arrange(desc(joy_index)) %>%
  unique() %>%
  head(10) %>%
  kable(col.names = c("Joy Index", "Track")) %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "palevioletred")
```

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Joy Index </th>
   <th style="text-align:left;"> Track </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> 1 </td>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 100.00 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> Everything Is… </td>
  </tr>
  <tr>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> 112 </td>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 82.79 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> Song Against Sex </td>
  </tr>
  <tr>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> 248 </td>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 61.54 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> Holland, 1945 </td>
  </tr>
  <tr>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> 326 </td>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 44.11 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> Naomi </td>
  </tr>
  <tr>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> 383 </td>
   <td style="text-align:right;color: palevioletred !important;background-color: azure !important;"> 41.40 </td>
   <td style="text-align:left;color: palevioletred !important;background-color: azure !important;"> In the Aeroplane Over the Sea </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 458 </td>
   <td style="text-align:right;"> 38.10 </td>
   <td style="text-align:left;"> Gardenhead / Leave Me Alone </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 527 </td>
   <td style="text-align:right;"> 37.18 </td>
   <td style="text-align:left;"> King of Carrot Flowers Pt. 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 563 </td>
   <td style="text-align:right;"> 31.09 </td>
   <td style="text-align:left;"> Two-Headed Boy </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 662 </td>
   <td style="text-align:right;"> 31.02 </td>
   <td style="text-align:left;"> The Fool </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 664 </td>
   <td style="text-align:right;"> 23.83 </td>
   <td style="text-align:left;"> Two-Headed Boy Pt. 2 </td>
  </tr>
</tbody>
</table>

So, turns out Everything Is... is the happiest Neutral Milk Hotel song. This definitely makes sense, seeing how it has the highest Valence, and the 6th highest Joy Percentage. I gave it a listen to confirm this, and it does seem to be the case that Everything Is... is pretty much the happiest song on their discography (which just now occurs to me, is rather dismal, overall).

To see how happy each song on the discography is, I found the average Joy Index for both albums, and plotted every song by album (in chronological order) using highcharter.


```r
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
```

```
## `summarise()` has grouped output by 'album_release_year', 'album_name'. You can override using the `.groups` argument.
```

```r
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
```

```
## Warning in brewer.pal(n_distinct(track_df$album_name), "Paired"): minimal value for n is 3, returning requested palette with 3 different levels
```

```r
album_chart$x$hc_opts$series[[3]]$name <- 'Album Averages'
album_chart
```

```{=html}
<div id="htmlwidget-3b067e7fc2501fd08eb0" style="width:100%;height:500px;" class="highchart html-widget"></div>
```