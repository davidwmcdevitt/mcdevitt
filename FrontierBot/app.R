library(shiny)
library(tidyverse)
library(spotifyr)
library(tidytext)
library(extrafont)
library(dplyr)
library(knitr)
library(shinybusy)

Sys.setenv(SPOTIFY_CLIENT_ID = "0e6eb928dff94031aac094c8c65181f4")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "e452221800314c08a5d9e7739b276779")

access_token <- get_spotify_access_token()
uri <- '3q3jgSxIHYzqF1CshvPeQG' # R Input
# uri <- '51OHUnNDstw3VFPRwCcY8r' #Rchive

temp <- get_playlist(uri)
len <- ceiling(temp$tracks$total / 100)

list <- data.frame(track.name = character(), track.album.name = character(),LKey = character(), LMode = character(), tempo = numeric())

for (l in 1:len) {
  offset <- l*100 - 100 
  playlists <- get_playlist_tracks(uri, offset = offset)
  
  emulate <- get_track_audio_features(playlists$track.id, authorization = get_spotify_access_token())
  
  names(emulate)[names(emulate) == 'id'] <- 'track.id'
  
  merge <- merge(playlists,emulate,by="track.id")
  
  test <- mutate(merge, LKey = ifelse(key == "0","C",ifelse(key == "1", "Db",ifelse(key == "2", "D", ifelse(key == "3", "Eb", ifelse(key == "4", "E", ifelse(key == "5", "F", ifelse(key == "6", "Gb", ifelse(key == "7", "G",ifelse(key == "8", "Ab",ifelse(key == "9", "A",ifelse(key == "10", "Bb",ifelse(key == "11", "B","Unknown")))))))))))))
  test <- mutate(test, LMode = ifelse(mode == "0", "Minor", "Major"))
  test <- subset(test, select = c("track.name","track.album.name","LKey", "LMode", "tempo"))
  
  test$LKey[test$Lkey == "C" & test$LMode == "Minor"] <- "Eb"
  test$LKey[test$Lkey == "Db" & test$LMode == "Minor"] <- "E"
  test$LKey[test$Lkey == "D" & test$LMode == "Minor"] <- "F"
  test$LKey[test$Lkey == "Eb" & test$LMode == "Minor"] <- "Gb"
  test$LKey[test$Lkey == "E" & test$LMode == "Minor"] <- "G"
  test$LKey[test$Lkey == "F" & test$LMode == "Minor"] <- "Ab"
  test$LKey[test$Lkey == "Gb" & test$LMode == "Minor"] <- "A"
  test$LKey[test$Lkey == "G" & test$LMode == "Minor"] <- "Bb"
  test$LKey[test$Lkey == "Ab" & test$LMode == "Minor"] <- "B"
  test$LKey[test$Lkey == "A" & test$LMode == "Minor"] <- "C"
  test$LKey[test$Lkey == "Bb" & test$LMode == "Minor"] <- "Db"
  test$LKey[test$Lkey == "B" & test$LMode == "Minor"] <- "D"
  
  test$LMode[test$LMode == "Minor"] <- "Major"
  list <- rbind(list, test)
}


keys <- c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B")
modes <- c("Major","Minor")



ui <- fluidPage(
  h1("Hello! This app will call Spotify's API to return the key and tempo of every song in a given Spotify playlist.", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  h2("Give it a try! Go to one of your playlists, Click the '...' button, go to share, and click 'Copy Link to Playlist.'", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  h3("Note: Unfortunately, Spotify generated playlists such as Spotify Wrapped or Blend can't be shared by URL, so the app won't be able to find those.", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  h4("All keys are converted to their major equivalents.", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  textInput("url", "Playlist URL", ""),
  selectInput('key', 'Key', keys),
  selectInput('mode', 'Mode', modes),
  numericInput('tempo', 'Tempo', 120,
               min = 0, max = 9999),
  actionButton("goButton", "Go!", class = "btn-success"),
  actionButton("goButton2", "Give Me Everything", class = "btn-all"),
  tableOutput("first")
)

server <- function(input, output, session) {
  
  Sys.setenv(SPOTIFY_CLIENT_ID = "0e6eb928dff94031aac094c8c65181f4")
  Sys.setenv(SPOTIFY_CLIENT_SECRET = "e452221800314c08a5d9e7739b276779")
  
  access_token <- get_spotify_access_token()
  uri <- '3q3jgSxIHYzqF1CshvPeQG' # R Input
  
  temp <- get_playlist(uri)
  len <- ceiling(temp$tracks$total / 100)
  
  list <- data.frame(track.name = character(), track.album.name = character(),LKey = character(), LMode = character(), tempo = numeric())
  
  for (l in 1:len) {
    offset <- l*100 - 100 
    playlists <- get_playlist_tracks(uri, offset = offset)
    
    emulate <- get_track_audio_features(playlists$track.id, authorization = get_spotify_access_token())
    
    names(emulate)[names(emulate) == 'id'] <- 'track.id'
    
    merge <- merge(playlists,emulate,by="track.id")
    
    test <- mutate(merge, LKey = ifelse(key == "0","C",ifelse(key == "1", "Db",ifelse(key == "2", "D", ifelse(key == "3", "Eb", ifelse(key == "4", "E", ifelse(key == "5", "F", ifelse(key == "6", "Gb", ifelse(key == "7", "G",ifelse(key == "8", "Ab",ifelse(key == "9", "A",ifelse(key == "10", "Bb",ifelse(key == "11", "B","Unknown")))))))))))))
    test <- mutate(test, LMode = ifelse(mode == "0", "Minor", "Major"))
    test <- subset(test, select = c("track.name","track.album.name","LKey", "LMode", "tempo"))
    
    test$LKey[test$Lkey == "C" & test$LMode == "Minor"] <- "Eb"
    test$LKey[test$Lkey == "Db" & test$LMode == "Minor"] <- "E"
    test$LKey[test$Lkey == "D" & test$LMode == "Minor"] <- "F"
    test$LKey[test$Lkey == "Eb" & test$LMode == "Minor"] <- "Gb"
    test$LKey[test$Lkey == "E" & test$LMode == "Minor"] <- "G"
    test$LKey[test$Lkey == "F" & test$LMode == "Minor"] <- "Ab"
    test$LKey[test$Lkey == "Gb" & test$LMode == "Minor"] <- "A"
    test$LKey[test$Lkey == "G" & test$LMode == "Minor"] <- "Bb"
    test$LKey[test$Lkey == "Ab" & test$LMode == "Minor"] <- "B"
    test$LKey[test$Lkey == "A" & test$LMode == "Minor"] <- "C"
    test$LKey[test$Lkey == "Bb" & test$LMode == "Minor"] <- "Db"
    test$LKey[test$Lkey == "B" & test$LMode == "Minor"] <- "D"
    
    test$LMode[test$LMode == "Minor"] <- "Major"
    list <- rbind(list, test)
  }
  
  tempo <- 120
  key <- "C"
  
  view <- subset(list, list$LKey == key)
  view$dist <- abs(view$tempo - tempo)
  view <- view[order(view$dist),]
  
  tableData = reactiveValues(d1 = as.data.frame(view))
  
  observeEvent(input$goButton, {
    show_modal_spinner() # show the modal window
    
    
    Sys.setenv(SPOTIFY_CLIENT_ID = "0e6eb928dff94031aac094c8c65181f4")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "e452221800314c08a5d9e7739b276779")
    
    access_token <- get_spotify_access_token()
    
    url <- input$url
    uri <- substring(url,regexpr("playlist/", url)+9)
    uri <-substring(uri, 1,regexpr("=",uri)-4)
    
    temp <- get_playlist(uri)
    len <- ceiling(temp$tracks$total / 100)
    
    list <- data.frame(track.name = character(), track.album.name = character(),LKey = character(), LMode = character(), tempo = numeric())
    
    for (l in 1:len) {
      offset <- l*100 - 100 
      playlists <- get_playlist_tracks(uri, offset = offset)
      
      emulate <- get_track_audio_features(playlists$track.id, authorization = get_spotify_access_token())
      
      names(emulate)[names(emulate) == 'id'] <- 'track.id'
      
      merge <- merge(playlists,emulate,by="track.id")
      
      test <- mutate(merge, LKey = ifelse(key == "0","C",ifelse(key == "1", "Db",ifelse(key == "2", "D", ifelse(key == "3", "Eb", ifelse(key == "4", "E", ifelse(key == "5", "F", ifelse(key == "6", "Gb", ifelse(key == "7", "G",ifelse(key == "8", "Ab",ifelse(key == "9", "A",ifelse(key == "10", "Bb",ifelse(key == "11", "B","Unknown")))))))))))))
      test <- mutate(test, LMode = ifelse(mode == "0", "Minor", "Major"))
      test <- subset(test, select = c("track.name","track.album.name","LKey", "LMode", "tempo"))
      
      test$LKey[test$Lkey == "C" & test$LMode == "Minor"] <- "Eb"
      test$LKey[test$Lkey == "Db" & test$LMode == "Minor"] <- "E"
      test$LKey[test$Lkey == "D" & test$LMode == "Minor"] <- "F"
      test$LKey[test$Lkey == "Eb" & test$LMode == "Minor"] <- "Gb"
      test$LKey[test$Lkey == "E" & test$LMode == "Minor"] <- "G"
      test$LKey[test$Lkey == "F" & test$LMode == "Minor"] <- "Ab"
      test$LKey[test$Lkey == "Gb" & test$LMode == "Minor"] <- "A"
      test$LKey[test$Lkey == "G" & test$LMode == "Minor"] <- "Bb"
      test$LKey[test$Lkey == "Ab" & test$LMode == "Minor"] <- "B"
      test$LKey[test$Lkey == "A" & test$LMode == "Minor"] <- "C"
      test$LKey[test$Lkey == "Bb" & test$LMode == "Minor"] <- "Db"
      test$LKey[test$Lkey == "B" & test$LMode == "Minor"] <- "D"
      
      test$LMode[test$LMode == "Minor"] <- "Major"
      list <- rbind(list, test)
    }
    
    tempo <- input$tempo
    key <- input$key
    
    view <- subset(list, list$LKey == key)
    view$dist <- abs(view$tempo - tempo)
    view <- view[order(view$dist),]
    
    tableData$d1 = view
    remove_modal_spinner() # remove it when done
    
  })
  
  
  observeEvent(input$goButton2, {
    show_modal_spinner() 
    
    
    Sys.setenv(SPOTIFY_CLIENT_ID = "0e6eb928dff94031aac094c8c65181f4")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "e452221800314c08a5d9e7739b276779")
    
    access_token <- get_spotify_access_token()
    
    url <- input$url
    uri <- substring(url,regexpr("playlist/", url)+9)
    uri <-substring(uri, 1,regexpr("=",uri)-4)
    
    temp <- get_playlist(uri)
    len <- ceiling(temp$tracks$total / 100)
    
    list <- data.frame(track.name = character(), track.album.name = character(),LKey = character(), LMode = character(), tempo = numeric())
    
    for (l in 1:len) {
      offset <- l*100 - 100 
      playlists <- get_playlist_tracks(uri, offset = offset)
      
      emulate <- get_track_audio_features(playlists$track.id, authorization = get_spotify_access_token())
      
      names(emulate)[names(emulate) == 'id'] <- 'track.id'
      
      merge <- merge(playlists,emulate,by="track.id")
      
      test <- mutate(merge, LKey = ifelse(key == "0","C",ifelse(key == "1", "Db",ifelse(key == "2", "D", ifelse(key == "3", "Eb", ifelse(key == "4", "E", ifelse(key == "5", "F", ifelse(key == "6", "Gb", ifelse(key == "7", "G",ifelse(key == "8", "Ab",ifelse(key == "9", "A",ifelse(key == "10", "Bb",ifelse(key == "11", "B","Unknown")))))))))))))
      test <- mutate(test, LMode = ifelse(mode == "0", "Minor", "Major"))
      test <- subset(test, select = c("track.name","track.album.name","LKey", "LMode", "tempo"))
      
      test$LKey[test$Lkey == "C" & test$LMode == "Minor"] <- "Eb"
      test$LKey[test$Lkey == "Db" & test$LMode == "Minor"] <- "E"
      test$LKey[test$Lkey == "D" & test$LMode == "Minor"] <- "F"
      test$LKey[test$Lkey == "Eb" & test$LMode == "Minor"] <- "Gb"
      test$LKey[test$Lkey == "E" & test$LMode == "Minor"] <- "G"
      test$LKey[test$Lkey == "F" & test$LMode == "Minor"] <- "Ab"
      test$LKey[test$Lkey == "Gb" & test$LMode == "Minor"] <- "A"
      test$LKey[test$Lkey == "G" & test$LMode == "Minor"] <- "Bb"
      test$LKey[test$Lkey == "Ab" & test$LMode == "Minor"] <- "B"
      test$LKey[test$Lkey == "A" & test$LMode == "Minor"] <- "C"
      test$LKey[test$Lkey == "Bb" & test$LMode == "Minor"] <- "Db"
      test$LKey[test$Lkey == "B" & test$LMode == "Minor"] <- "D"
      
      test$LMode[test$LMode == "Minor"] <- "Major"
      list <- rbind(list, test)
    }
    
    tableData$d1 = list
    remove_modal_spinner() 
  })
  
  test = reactive({
    d1 = as.data.frame(view)
  })
  
  output$first=renderTable({
    tableData$d1
  })
  
  observe({print(test())})        
  observe({print(tableData$d1)})  
  
  observe({print(is.data.frame(test()))
    print(is.data.frame(tableData$d1))
    
  })
}

shinyApp(ui, server)