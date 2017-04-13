#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

bigramTable <- readRDS("data/bigrams.rds") %>% ungroup()
trigramTable <- readRDS("data/trigrams.rds") %>% ungroup()
quadgramTable <- readRDS("data/quadgrams.rds") %>% ungroup()



shinyServer(function(input, output) {
  
  backoffPred <- function(f){
    three = tail(unlist(strsplit(f," ")),3)
    leftover <- 1
    
    pred = data.frame()
    quadtab <- quadgramTable %>% filter(first == paste(three, collapse = " "))
    
    if (nrow(quadtab) > 0) {
      # print("quadgram hit!")
      pred <- quadtab %>% mutate(prob = freq * discount / sum(quadtab$freq)) %>% select(last, prob)
      leftover <-  1 - sum(pred$prob)
      if (leftover < 0.1)
        return(pred %>% arrange(desc(prob)) %>% head(5))
    }
    
    
    tritab <- trigramTable %>% filter(first == paste(tail(three,2), collapse = " ")) %>% anti_join(quadtab, by = "last")
    
    if (nrow(tritab) > 0) {
      # print("trigram hit!")
      pred <- rbind(pred, tritab %>% mutate(prob = leftover * freq * discount / sum(tritab$freq)) %>% select(last, prob))
      leftover <-  1 - sum(pred$prob)
      if (leftover < 0.1)
        return(pred %>% arrange(desc(prob)) %>% head(5))
    }
    
    bitab <- bigramTable %>% filter(first == tail(three, 1)) %>% anti_join(quadtab, by = "last") %>% anti_join(tritab, by = "last")
    
    if (nrow(bitab) > 0) {
      # print("bigram hit!")
      pred <- rbind(pred, bitab %>% mutate(prob = leftover * freq * discount / sum(bitab$freq)) %>% select(last, prob))
    }
    
    return(pred %>% arrange(desc(prob)) %>% head(5))
  }
  
  mypredtext <- eventReactive(
    input$goButton,{
      validate(
        need(input$intext != "", "Please type something!")
      )
      
      paste(backoffPred(tolower(input$intext))$last, collapse = ", ")
      
    }
  )
  
  output$predText <- renderText({
    mypredtext()
  })
  
})
