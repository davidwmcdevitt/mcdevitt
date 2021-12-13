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
  textInput("url", "Playlist URL", ""),
  selectInput('key', 'Key', keys),
  selectInput('mode', 'Mode', modes),
  numericInput('tempo', 'Tempo', 120,
               min = 0, max = 9999),
  actionButton("goButton", "Go!", class = "btn-success"),
  tableOutput("first")
)

server <- function(input, output, session) {
  
  # t1 = (as.data.frame(forecast %>% filter(Date==Sys.Date()-21) %>% group_by(Resort,Date) %>% summarise(`Powder Total` = sum(Snow))))
  # t1=t1[order(t1$`Powder Total`,decreasing=TRUE),][1:5,]
  
  # output$first = renderTable({
  #   t1[1,]
  #   })
  
  # tempo <- 120
  # key <- "F"
  # 
  # view <- subset(list, list$LKey == key)
  # view$dist <- abs(view$tempo - tempo)
  # view <- view[order(view$dist),]
  
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
  
  test = reactive({
    d1 = as.data.frame(view)
    # names(d1)=c("col1","col2")
    # d1$col1=input$num
    # d1$col2=input$num+1
  })
  
  output$first=renderTable({
    tableData$d1
  })
  
  observe({print(test())})        # check console output
  observe({print(tableData$d1)})  # check console output
  
  observe({print(is.data.frame(test()))
    print(is.data.frame(tableData$d1))
    
  })
}

shinyApp(ui, server)