ft <- readRDS("ft.Rda")
source("nextWords.R")
library(shiny)
library(tm)
library(SnowballC)
library(data.table)
library(dplyr)
library(slam)
library(reshape2)
library(scales)
library(stringi)
library(stringr)
library(jsonlite)
library(httr)

aText <- "Type to get a search result"
aLink <- "http://www.google.com"

shinyServer(function(input, output) {
    aText <- reactive({ifelse(input$suggestInput==T,paste0(input$textHistory," ",nextWords(ft,input$textHistory)$term[1]),input$textHistory)})
    jsonIMG <- reactive({fromJSON(paste0("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=",gsub(" ","+",aText())))})
    output$googleLink <- renderUI({
        aLink <- paste0("https://www.google.com/search?q=",gsub(" ","+",aText()))
        a(aText(),id='google',href=aLink)
    })
    output$duckLink <- renderUI({
        aLink <- paste0("https://duckduckgo.com/?q=",gsub(" ","+",aText()))
        a(aText(),id='google',href=aLink)
    })
    output$ecoLink <- renderUI({
        aLink <- paste0("https://www.ecosia.org/search?q=",gsub(" ","+",aText()))
        a(aText(),id='google',href=aLink)
    })
    output$giphyLink <- renderUI({
        IMG <- jsonIMG()$data$image_url
        img(id='giphyGIF',style='display:block;',src=IMG)
    })
})
