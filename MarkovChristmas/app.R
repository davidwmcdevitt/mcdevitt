library(rvest)
library(markovchain)
library(shiny)
library(shinybusy)
library(rsconnect)
library(stringr)


ui = basicPage(
  h1("Generate a Markov Christmas Carol", align="left", style = "font-family: 'Times', serif; font-weight: 5px; font-size: 5; line-height: 1; color: #404040;"),
  actionButton("goButton", "Ho Ho Ho!", class = "btn-success"),
  h2(verbatimTextOutput("verb1"),
  verbatimTextOutput("verb2"),
  verbatimTextOutput("verb3"),
  verbatimTextOutput("verb4"), align="center")
)
server = function(input, output) {
  

  xmas <-read_html("https://www.41051.com/xmaslyrics/")
  
  links <- xmas %>% html_nodes("a") %>% html_attr("href")
  links <- links[4:42]
  
  lyrics <- c()
  
  for (carol in links) {
    url <- paste("https://www.41051.com/xmaslyrics/",carol, sep = "")
    try(temp <- read_html(url), silent =TRUE)
    if (exists("temp") == TRUE ) {
      temp <- html_text(temp %>% html_nodes(".bodytext"))
      temp <- gsub("\r", " ",temp, fixed = TRUE)
      temp <- gsub("\n", " ",temp, fixed = TRUE)
      temp <- gsub("\t", " ",temp, fixed = TRUE)
      temp <- gsub("(adsbygoogle = windowadsbygoogle || [])push({});", " ",temp, fixed = TRUE)
      temp <- gsub("(adsbygoogle = window.adsbygoogle || []).push({});", " ",temp, fixed = TRUE)
      lyrics <- c(lyrics, temp)
      remove(temp)
    }
  }
  
  lyrics_clean <- lyrics %>% 
    strsplit(" ") %>% 
    unlist() 
  
  lyrics_clean <- lyrics_clean[lapply(lyrics_clean,nchar)>0]
  
  fit_markov <- markovchainFit(lyrics_clean)
  
  
  create_me <- function(num = 5, first_word = "i", n = 2) {
    
    
    for (i in 1:num) {
      
      set.seed(i+5)
      
      markovchainSequence(n = n, # generate 2 additional random words
                          markovchain = fit_markov$estimate,
                          t0 = tolower(first_word), include.t0 = T) %>%
        # joint words
        paste(collapse = " ") %>% # join generated words with space
        # create proper sentence form
        str_replace_all(pattern = " ,", replacement = ",") %>%
        str_replace_all(pattern = " [.]", replacement = ".") %>%
        str_replace_all(pattern = " [!]", replacement = "!") %>%
        str_replace_all(pattern = "[\"]", replacement = "") %>%
        str_to_sentence() %>% # start every sentences with capitalization
        print()
      
      
    }
    
  }
  
  num <- floor(runif(4,1,5))
  words <- c("Reindeer","Jolly", "Snow", "Jingle","The", "Bell", "Red","Angel","Nose","Tree","Sleigh","White","Ho","Love","True","Presents","Man","Star")
  words <- c(sample(words,4, replace = FALSE))
  
  observeEvent(input$goButton, {
    show_modal_spinner()
    text1 <- capture.output(create_me(num = 1, first_word = words[1] , n = 16))
    output$verb1 <-renderText({ text1 }) 
    text2 <- capture.output(create_me(num = 1, first_word = words[2] , n = 16))
    output$verb2 <-renderText({ text2 }) 
    text3 <- capture.output(create_me(num = 1, first_word = words[3] , n = 16))
    output$verb3 <-renderText({ text3 }) 
    text4 <- capture.output(create_me(num = 1, first_word = words[4] , n = 16))
    output$verb4 <-renderText({ text4 }) 
    remove_modal_spinner() 
  })
}
shinyApp(ui, server)

