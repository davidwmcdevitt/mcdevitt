library(shiny)
library(tidyverse)
library(spotifyr)
library(tidytext)
library(extrafont)
library(dplyr)
library(knitr)
library(shinybusy)
library(rsconnect)
library(rvest)

Sys.setenv(SPOTIFY_CLIENT_ID = "0e6eb928dff94031aac094c8c65181f4")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "e452221800314c08a5d9e7739b276779")

access_token <- get_spotify_access_token()

ui = basicPage(
  h1("Hello! This app will scrape Pitchfork reviews for every album in one of your Spotify playlists (that it can find) and give you a Pitchfork rating of your playlist! Go ahead and try it!", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  h2("Give it a try! Go to one of your playlists, Click the '...' button, go to share, and click 'Copy Link to Playlist.'", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  h3("Note: Unfortunately, Spotify generated playlists such as Spotify Wrapped or Blend can't be shared by URL, so the app won't be able to find those.", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  textInput("url", "Enter a Spotify playlist URL"),
  actionButton("goButton", "Go!", class = "btn-success"),
  verbatimTextOutput("verb")
)
server = function(input, output) {
  observeEvent(input$goButton, {
    
    show_modal_spinner() # show the modal window
    
    
    Sys.setenv(SPOTIFY_CLIENT_ID = "0e6eb928dff94031aac094c8c65181f4")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "e452221800314c08a5d9e7739b276779")
    
    access_token <- get_spotify_access_token()
    
    url <- input$url
    uri <- substring(url,regexpr("playlist/", url)+9)
    uri <-substring(uri, 1,regexpr("=",uri)-4)
    
    
    temp <- get_playlist(uri)
    
    playlists <- get_playlist_tracks(uri)
    
    len <- ceiling(temp$tracks$total / 100)
    
    if (nrow(playlists) > 100) {
      for (l in 2:len) {
        offset <- l*100 - 100 
        temp <- get_playlist_tracks(uri, offset = offset)
        playlists <- rbind(playlists, temp)
      }
    }
    
    tracks <- subset(playlists, select = c("track.name","track.artists","track.album.name"))
    tracks$artist <- NA
    
    z <- nrow(tracks)
    
    artists <- c()
    
    for (i in 1:z) {
      temp <- tracks[[2]][[i]]$name[1]
      artists <- c(artists, temp)
    }
    
    tracks$artist <- artists
    tracks$track.artists <- NULL
    
    tracks <- rename(tracks, name= track.name, album = track.album.name)
    
    # Clean and Guess URL
    
    tracks$artist <- tolower(tracks$artist)
    tracks$artist <- gsub("/", "-",tracks$artist)
    tracks$artist <- gsub(":", "",tracks$artist)
    tracks$artist <- gsub("&", "and",tracks$artist)
    tracks$artist <- gsub("-", " ",tracks$artist)
    tracks$artist <- gsub("[[:punct:]]", "",tracks$artist)
    tracks$artist <- gsub("'", "",tracks$artist)
    tracks$artist <- gsub(" ", "-",tracks$artist)
    
    tracks$album <- tolower(tracks$album)
    tracks$album <- gsub("/", "-",tracks$album)
    tracks$album <- gsub(":", "",tracks$album)
    tracks$album <- gsub("&", "and",tracks$album)
    tracks$album <- gsub("-", " ",tracks$album)
    tracks$album <- gsub("[[:punct:]]", "",tracks$album)
    tracks$album <- gsub("'", "",tracks$album)
    tracks$album <- gsub(" ", "-",tracks$album)
    
    tracks$url_guess <- paste("https://pitchfork.com/reviews/albums/",tracks$artist,"-",tracks$album, sep = "")
    
    url_list <- tracks$url_guess
    
    content <- data.frame()
    
    for (x in url_list) {
      text <- paste(x)
      url <- text
      
      try(webpage <- read_html(url), silent = TRUE)
      
      if (exists("webpage") != FALSE ) {
        review_score_html <- html_nodes(webpage, '.score')
        review_genre_html <- html_nodes(webpage, '.genre-list__item')
        review_text_html  <- html_nodes(webpage, 'p')
        
        if(length(review_genre_html) == 0){
          review_genre <- "No Genre"
        }
        
        if(length(review_genre_html) >= 1){
          review_genre <- html_text(review_genre_html)
        }
        
        review_score <- html_text(review_score_html)
        review_text <- html_text(review_text_html)
        temp <- list()
        for (i in 1:length(review_text)) {
          temp <- paste(temp, review_text[i])   
        }
        review_text <- temp
        
        url_string <- x
        
        review_content <- data.frame(url_string, review_genre, review_score, review_text)
        content <- rbind(content, review_content)
        remove(webpage)
      }
      
    } 
    content <- unique(content)
    mean <- round(mean(as.numeric(content$review_score)),digits = 1)
    text <- paste("Pitchfork gives this playlist a ", mean, ". ", nrow(content),"/",nrow(playlists), " reviews found.",sep = "")
    
    
    remove_modal_spinner() # remove it when done
    
  output$verb <- renderText({ text })
  })
}
shinyApp(ui, server)
