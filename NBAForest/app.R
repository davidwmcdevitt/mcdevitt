library(readxl)
library(dplyr)
library(tidyverse)
library(randomForest)
library(nbastatR)
library(R.utils)
library(shiny)
library(rsconnect)
library(shinybusy)


set.seed(9999)
# Pull box scores
# 
# nba_box <- data.frame()
# 
# for (i in 22100001:22100420) {
#   traditional <- try(withTimeout(nbastatR::box_scores(game_ids = i,box = c('traditional','four factors'), join_data  =TRUE),timeout = 60, onTimeout = "silent"),silent = TRUE)
#   advanced <- try(withTimeout(nbastatR::box_scores(game_ids = i,box = c('advanced'), join_data  =TRUE),timeout = 60, onTimeout = "silent"),silent = TRUE)
#   
#   temp <- try(data.frame(traditional[[2]][[2]]), silent = TRUE)
#   temp <- try(merge(temp, advanced[[2]][[2]], by = c("idGame", "slugTeam", "idTeam",'teamName',"cityTeam","minExact")), silent = TRUE)
#   
#   nba_box <- try(rbind(nba_box, temp), silent = TRUE)
#   print(i)
# }
teams<-c("BKN","MIL","GSW","LAL","CHA","IND","CHI","DET","BOS","NYK","TOR","WAS","CLE","MEM","HOU","MIN","NOP","PHI","ORL","SAS","OKC","UTA","DEN","PHX","POR","SAC","ATL","DAL","MIA","LAC")

ui <- fluidPage(
  selectInput('home_team', 'Home Team', teams),
  selectInput('away_team', 'Away Team', teams),
  actionButton("goButton", "Go!", class = "btn-success"),
  verbatimTextOutput("text")
)

server <- function(input, output, session) {
  
  nba_box <<- read_csv("./www/nba_box.csv")
  
  trainset <- subset(nba_box, select = c('idGame', 'slugTeam','ortgE', 'drtgE','pctEFG','rateFTA','rateFTAOpponent','pctTOVTeam','pctAST','pctTREB','pctOREB','pctEFGOpponent','pctTOVOpponent','pctOREBOpponent','plusminus'))
  
  teams <- unique(nba_box$slugTeam)
  
  trainset <- trainset %>% 
    group_by(idGame) %>% 
    mutate(id = row_number())
  
  away_teams <- subset(trainset, trainset$id == 2)
  home_teams <- subset(trainset, trainset$id == 1)
  
  
  home_teams <- home_teams %>%
    rename(
      home = slugTeam,
      ortgE_home = ortgE,
      drtgE_home = drtgE,
      pctEFG_home = pctEFG,
      rateFTA_home = rateFTA,
      rateFTAOpponent_home = rateFTAOpponent,
      pctTOVTeam_home = pctTOVTeam,
      pctAST_home = pctAST,
      pctTREB_home = pctTREB,
      pctOREB_home = pctOREB,
      pctEFGOpponent_home = pctEFGOpponent,
      pctTOVOpponent_home = pctTOVOpponent,
      pctOREBOpponent_home = pctOREBOpponent)
  
  
  away_teams <- away_teams %>%
    rename(
      away = slugTeam,
      ortgE_away = ortgE,
      drtgE_away = drtgE,
      pctEFG_away = pctEFG,
      rateFTA_away = rateFTA,
      rateFTAOpponent_away = rateFTAOpponent,
      pctTOVTeam_away = pctTOVTeam,
      pctAST_away = pctAST,
      pctTREB_away = pctTREB,
      pctOREB_away = pctOREB,
      pctEFGOpponent_away = pctEFGOpponent,
      pctTOVOpponent_away = pctTOVOpponent,
      pctOREBOpponent_away = pctOREBOpponent)
  
  home_teams$home_win <- 0
  home_teams$home_win[home_teams$plusminus >0] <-1
  
  home_teams$plusminus <- NULL
  away_teams$plusminus <- NULL
  home_teams$id <- NULL
  away_teams$id <- NULL
  
  
  trainset <- merge(home_teams, away_teams, by ="idGame")
  trainset$idGame <- NULL
  trainset$home <- NULL
  trainset$away <- NULL
  
  # Train Model
  model <- randomForest(as.factor(home_win) ~ ., data = trainset, ntree = 2000, mtry = 4, importance = TRUE)
  
  
  observeEvent(input$goButton, {
    show_modal_spinner() # show the modal window
        
    #Simulate Games
    home_team <- input$home_team
    away_team <- input$away_team
    
    away_temp <- subset(away_teams, away_teams$away == away_team)
    home_temp <- subset(home_teams, home_teams$home == home_team)
    
    home_temp <- home_temp %>% 
      group_by(home)%>%
      summarize(
        ortgE_home_mean = mean(ortgE_home),
        drtgE_home_mean = mean(drtgE_home),
        pctEFG_home_mean = mean(pctEFG_home),
        rateFTA_home_mean = mean(rateFTA_home),
        rateFTAOpponent_home_mean = mean(rateFTAOpponent_home),
        pctTOVTeam_home_mean = mean(pctTOVTeam_home),
        pctAST_home_mean = mean(pctAST_home),
        pctTREB_home_mean = mean(pctTREB_home),
        pctOREB_home_mean = mean(pctOREB_home),
        pctEFGOpponent_home_mean = mean(pctEFGOpponent_home),
        pctTOVOpponent_home_mean = mean(pctTOVOpponent_home),
        pctOREBOpponent_home_mean = mean(pctOREBOpponent_home),
        ortgE_home_sd = sd(ortgE_home),
        drtgE_home_sd = sd(drtgE_home),
        pctEFG_home_sd = sd(pctEFG_home),
        rateFTA_home_sd = sd(rateFTA_home),
        rateFTAOpponent_home_sd = sd(rateFTAOpponent_home),
        pctTOVTeam_home_sd = sd(pctTOVTeam_home),
        pctAST_home_sd = sd(pctAST_home),
        pctTREB_home_sd = sd(pctTREB_home),
        pctOREB_home_sd = sd(pctOREB_home),
        pctEFGOpponent_home_sd = sd(pctEFGOpponent_home),
        pctTOVOpponent_home_sd = sd(pctTOVOpponent_home),
        pctOREBOpponent_home_sd = sd(pctOREBOpponent_home))
    
    
    away_temp <- away_temp %>% 
      group_by(away)%>%
      summarize(
        ortgE_away_mean = mean(ortgE_away),
        drtgE_away_mean = mean(drtgE_away),
        pctEFG_away_mean = mean(pctEFG_away),
        rateFTA_away_mean = mean(rateFTA_away),
        rateFTAOpponent_away_mean = mean(rateFTAOpponent_away),
        pctTOVTeam_away_mean = mean(pctTOVTeam_away),
        pctAST_away_mean = mean(pctAST_away),
        pctTREB_away_mean = mean(pctTREB_away),
        pctOREB_away_mean = mean(pctOREB_away),
        pctEFGOpponent_away_mean = mean(pctEFGOpponent_away),
        pctTOVOpponent_away_mean = mean(pctTOVOpponent_away),
        pctOREBOpponent_away_mean = mean(pctOREBOpponent_away),
        ortgE_away_sd = sd(ortgE_away),
        drtgE_away_sd = sd(drtgE_away),
        pctEFG_away_sd = sd(pctEFG_away),
        rateFTA_away_sd = sd(rateFTA_away),
        rateFTAOpponent_away_sd = sd(rateFTAOpponent_away),
        pctTOVTeam_away_sd = sd(pctTOVTeam_away),
        pctAST_away_sd = sd(pctAST_away),
        pctTREB_away_sd = sd(pctTREB_away),
        pctOREB_away_sd = sd(pctOREB_away),
        pctEFGOpponent_away_sd = sd(pctEFGOpponent_away),
        pctTOVOpponent_away_sd = sd(pctTOVOpponent_away),
        pctOREBOpponent_away_sd = sd(pctOREBOpponent_away))
    
    
    simulate <- data.frame(ortgE_home = rnorm(1000, home_temp$ortgE_home_mean, home_temp$ortgE_home_sd),
                           drtgE_home = rnorm(1000, home_temp$drtgE_home_mean, home_temp$drtgE_home_sd),
                           pctEFG_home = rnorm(1000, home_temp$pctEFG_home_mean, home_temp$pctEFG_home_sd),
                           rateFTA_home = rnorm(1000, home_temp$rateFTA_home_mean, home_temp$rateFTA_home_sd),
                           rateFTAOpponent_home = rnorm(1000, home_temp$rateFTAOpponent_home_mean, home_temp$rateFTAOpponent_home_sd),
                           pctTOVTeam_home = rnorm(1000, home_temp$pctTOVTeam_home_mean, home_temp$pctTOVTeam_home_sd),
                           pctAST_home = rnorm(1000, home_temp$pctAST_home_mean, home_temp$pctAST_home_sd),
                           pctTREB_home = rnorm(1000, home_temp$pctTREB_home_mean, home_temp$pctTREB_home_sd),
                           pctOREB_home = rnorm(1000, home_temp$pctOREB_home_mean, home_temp$pctOREB_home_sd),
                           pctEFGOpponent_home = rnorm(1000, home_temp$pctEFGOpponent_home_mean, home_temp$pctEFGOpponent_home_sd),
                           pctTOVOpponent_home = rnorm(1000, home_temp$pctTOVOpponent_home_mean, home_temp$pctTOVOpponent_home_sd),
                           pctOREBOpponent_home = rnorm(1000, home_temp$pctOREBOpponent_home_mean, home_temp$pctOREBOpponent_home_sd),
                           ortgE_away = rnorm(1000, away_temp$ortgE_away_mean, away_temp$ortgE_away_sd),
                           drtgE_away = rnorm(1000, away_temp$drtgE_away_mean, away_temp$drtgE_away_sd),
                           pctEFG_away = rnorm(1000, away_temp$pctEFG_away_mean, away_temp$pctEFG_away_sd),
                           rateFTA_away = rnorm(1000, away_temp$rateFTA_away_mean, away_temp$rateFTA_away_sd),
                           rateFTAOpponent_away = rnorm(1000, away_temp$rateFTAOpponent_away_mean, away_temp$rateFTAOpponent_away_sd),
                           pctTOVTeam_away = rnorm(1000, away_temp$pctTOVTeam_away_mean, away_temp$pctTOVTeam_away_sd),
                           pctAST_away = rnorm(1000, away_temp$pctAST_away_mean, away_temp$pctAST_away_sd),
                           pctTREB_away = rnorm(1000, away_temp$pctTREB_away_mean, away_temp$pctTREB_away_sd),
                           pctOREB_away = rnorm(1000, away_temp$pctOREB_away_mean, away_temp$pctOREB_away_sd),
                           pctEFGOpponent_away = rnorm(1000, away_temp$pctEFGOpponent_away_mean, away_temp$pctEFGOpponent_away_sd),
                           pctTOVOpponent_away = rnorm(1000, away_temp$pctTOVOpponent_away_mean, away_temp$pctTOVOpponent_away_sd),
                           pctOREBOpponent_away = rnorm(1000, away_temp$pctOREBOpponent_away_mean, away_temp$pctOREBOpponent_away_sd))
    
    simulate$home_win <- predict(model,newdata = simulate, type = "class")
    
    # table(predTrain, trainset_games$homewin)
    
    outcome <- as.matrix(table(simulate$home_win))
    winPct <- 100*(outcome[2,1] / 1000)
    
    output$text <- renderText(paste(home_team, " wins ", winPct, "% of matchups.", sep=""))
    remove_modal_spinner() # remove it when done
    
  })
}    
     

shinyApp(ui, server)  