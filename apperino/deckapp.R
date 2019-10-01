#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(RCurl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(leaflet)
library(stringr)
library(rvest)
library(data.table)
library(shiny)
library(stats)

jdd1<-read_csv("jdd_new.csv")
decks<-jdd1%>%
  group_by(deckID, colors, baseColors, splashes, sphere, archetype, powered)%>%
  summarise(count = n())%>%
  select(-count)
jdd1s<-jdd1
jdd1$color<-tolower(jdd1$color)
jdd1$text<-tolower(jdd1$text)
jdd1<-jdd1%>%
  filter(!str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))
color_vector <- c("white", "blue", "black", "red", "green")
none_color_vector <- c("white", "blue", "black", "red", "green", "none")
sphere_vector <- c('midrange', 'ramp', 'control', 'combo', 'aggro')
arch_vector <- c('natural_order', 'twin', 'reanimator', 'turns', 'opposition', 
                 'storm', 'cheat', 'wildfire', 'artifacts', 'none')
type_vector <- unique(jdd1$types)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Deck Stats"),
  br(),
  textInput(inputId = "sdecks",
    label = "Search by Deck ID",
    value = "234"),
  br(),
  textOutput(outputId = "numdecks"),
  br(),
  textOutput(outputId = "numcards"),
  br(),
  fluidRow(
    column(6,
           h4("Cards"),
           tableOutput(outputId = "stb")),
    column(6,
           h4("Deck Charts"),
           h5("Curve"),
           plotOutput(outputId = "scurve"),
           h5("Types"),
           plotOutput(outputId = "stypes"),
           h5("Colors"),
           plotOutput(outputId = "scolors"))
  )
)



#server logic
server <- function(input, output) {
  
  output$stb <- renderTable({
    not_decks = FALSE
    stbout<-jdd1s%>%
      filter(location == "MB")
    sdecklist<-str_split(input$sdecks, " ")
    for(i in 1:length(sdecklist[[1]])){
      if(!(sdecklist[[1]][i] %in% stbout$deckID)){
        not_decks = TRUE
      }
    }
    if(not_decks){
      stbout%>%
        select(name, color, types)%>%
        filter(FALSE)
    }else{
      stbout%>%
        filter(deckID %in% sdecklist[[1]])%>%
        group_by(name, color, cmc, types)%>%
        summarise(count = sum(number))%>%
        arrange(desc(count))
    }
  })
  
  output$numdecks<-renderText({
    not_decks = FALSE
    stbout2<-jdd1s%>%
      filter(location == "MB")
    sdecklist<-str_split(input$sdecks, " ")
    for(i in 1:length(sdecklist[[1]])){
      if(!(sdecklist[[1]][i] %in% stbout2$deckID)){
        not_decks = TRUE
      }
    }
    if(not_decks){
      "Please enter any number of deck IDs separated by spaces"
    }else{
      o2<-stbout2%>%
        filter(deckID %in% sdecklist[[1]])%>%
        group_by(deckID, name, color, manaCost, types)%>%
        summarise(count = sum(number))%>%
        arrange(count)
      paste("Number of Decks: ", length(unique(o2$deckID)))
    }
  })
  
  output$numcards<-renderText({
    not_decks = FALSE
    stbout2<-jdd1s%>%
      filter(location == "MB")
    sdecklist<-str_split(input$sdecks, " ")
    for(i in 1:length(sdecklist[[1]])){
      if(!(sdecklist[[1]][i] %in% stbout2$deckID)){
        not_decks = TRUE
      }
    }
    if(not_decks){
      ""
    }else{
      o2<-stbout2%>%
        filter(deckID %in% sdecklist[[1]])%>%
        group_by(deckID, name, color, manaCost, types)%>%
        summarise(count = sum(number))%>%
        arrange(count)
      paste("Number of unique cards: ", length(unique(o2$name)))
    }
  })
  
  output$scurve<-renderPlot({
    not_decks = FALSE
    stbout<-jdd1s%>%
      filter(location == "MB")
    sdecklist<-str_split(input$sdecks, " ")
    for(i in 1:length(sdecklist[[1]])){
      if(!(sdecklist[[1]][i] %in% stbout$deckID)){
        not_decks = TRUE
      }
    }
    if(not_decks){
      ggplot()
    }else{
      stbout%>%
        filter(deckID %in% sdecklist[[1]])%>%
        filter(!str_detect(types, "Land"))%>%
        group_by(cmc)%>%
        summarise(count = sum(number))%>%
        ggplot()+
        geom_col(aes(x = cmc, y = count, fill = count))+
        theme_minimal()
    }
  })
  
  output$stypes<-renderPlot({
    not_decks = FALSE
    stbout<-jdd1s%>%
      filter(location == "MB")
    sdecklist<-str_split(input$sdecks, " ")
    for(i in 1:length(sdecklist[[1]])){
      if(!(sdecklist[[1]][i] %in% stbout$deckID)){
        not_decks = TRUE
      }
    }
    if(not_decks){
      ggplot()
    }else{
      stbout<-stbout%>%
        filter(deckID %in% sdecklist[[1]])
      stbout<-split_types(stbout)
      stbout%>%
        group_by(types)%>%
        summarise(count = sum(number))%>%
        ggplot()+
        geom_col(aes(x = types, y = count, fill = types))+
        theme_minimal()
    }
  })
  
  output$scolors<-renderPlot({
    not_decks = FALSE
    stbout<-jdd1s%>%
      filter(location == "MB")
    sdecklist<-str_split(input$sdecks, " ")
    for(i in 1:length(sdecklist[[1]])){
      if(!(sdecklist[[1]][i] %in% stbout$deckID)){
        not_decks = TRUE
      }
    }
    if(not_decks){
      ggplot()
    }else{
      stbout1<-stbout%>%
        filter(deckID %in% sdecklist[[1]])%>%
        filter(!str_detect(types, "Land"))%>%
        filter(str_detect(color, ' '))
      stbout1$color<-rep("Multicolored", length(stbout1$color))
      stbout<-bind_rows(stbout, stbout1)
      stbout%>%
        filter(deckID %in% sdecklist[[1]])%>%
        filter(!str_detect(types, "Land"))%>%
        filter(!str_detect(color, ' '))%>%
        group_by(color)%>%
        summarise(count = sum(number))%>%
        ggplot()+
        geom_col(aes(x = color, y = count, fill = color))+
        theme_minimal()
    }
  })
}


split_types <- function(stbout){
  stb15<-stbout%>%
    filter(FALSE)
  
  for(i in 1:length(stbout$types)){
    stype<-str_split(stbout$types[i], ' ')
    for (k in 1:length(stype[[1]])){
      stbout$types[i]<-stype[[1]][k]
      stb15<-bind_rows(stb15, stbout[i,])
    }
  }
  return(stb15)
}


# Run the application 
shinyApp(ui = ui, server = server)

