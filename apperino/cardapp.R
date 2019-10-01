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
jdd1$color<-tolower(jdd1$color)
jdd1$text<-tolower(jdd1$text)
jdd1<-jdd1%>%
  filter(!str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))
color_vector <- c("white", "blue", "black", "red", "green")
sphere_vector <- c('midrange', 'ramp', 'control', 'combo', 'aggro')
arch_vector <- c('natural_order', 'twin', 'reanimator', 'turns', 'opposition', 
                 'storm', 'cheat', 'wildfire', 'artifacts', 'none')
type_vector <- unique(jdd1$types)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("3-0 Frequency by Card"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxInput(inputId = 'cls',
                      label = "Include Colorless Cards",
                      value = TRUE),
        checkboxInput(inputId = 'nbs',
                      label = "Include Nonbasic Lands",
                      value = TRUE),
        checkboxInput(inputId = 'powered',
                      label = "Include Powered Decks",
                      value = FALSE),
        checkboxInput(inputId = 'sb',
                      label = "Include Sideboard Cards",
                      value = FALSE),
        sliderInput(inputId = 'cmc',
                  label = "Filter by CMC",
                  min = 0,
                  max = 15,
                  value = c(0,15)),
        textInput(inputId = 'name',
                  label = 'Search Card Name',
                  value = ""),
        textInput(inputId = 'text',
                label = 'Search Oracle Text',
                value = ""),
        textInput(inputId = 'card',
                  label = 'In Decks Containing this Card',
                  value = ""),
        checkboxInput(inputId = "show_color",
                      label = "Show Color Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.show_color",
          wellPanel(
            checkboxGroupInput(inputId = "color",
                             label = "Select Color",
                             selected = color_vector,
                             choices = color_vector),
            checkboxInput(inputId = "multi",
                        label = "Include Multicolored Cards",
                        value = TRUE),
            checkboxInput(inputId = "only_multi",
                          label = "Include only Multicolored Cards",
                          value = FALSE)
        )),
        checkboxInput(inputId = "show_type",
                      label = "Show Type Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.show_type",
          wellPanel(
            checkboxGroupInput(inputId = 'type',
                         label = "Filter by Type",
                         selected = type_vector,
                         choices = type_vector)
        )),
        checkboxInput(inputId = "show_sphere",
                      label = "Show Sphere Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.show_sphere",
          wellPanel(
          checkboxGroupInput(inputId = 'sphere',
                  label = "Filter by Deck Sphere",
                  selected = sphere_vector,
                  choices = sphere_vector
                  ),
          selectInput(inputId = "spfilterType",
                  label = "Type of Sphere Filter",
                  selected = "Sphere includes",
                  choices = c("Sphere includes", "Sphere is exactly", "Sphere contains only"))
        )),
        checkboxInput(inputId = "show_archetype",
                      label = "Show Archetype Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.show_archetype",
          wellPanel(
            checkboxGroupInput(inputId = 'archetype',
                         label = "Filter by Deck Archetype",
                         selected = arch_vector,
                         choices = arch_vector),
            selectInput(inputId = "afilterType",
                  label = "Type of Archetype Filter",
                  selected = "Archetype includes",
                  choices = c("Archetype includes", "Archetype is exactly", "Archetype contains only"))
        ))
    ),
    mainPanel(
          tableOutput(outputId = "tb")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$tb <- renderTable({
     
     #exclude sideboard
     jddout<-jdd1
     if(!input$sb){
       jddout<-jddout%>%
         filter(location != "SB")
     }
     
     if(!input$powered){
       jddout<-jddout%>%
         filter(!powered)
     }
     
     #exclude lands
     if(!input$nbs){
       jddout<-jddout%>%
         filter(types != "Land")
     }
     
     #exclude colorless
     if(!input$cls){
       jddout<-jddout%>%
         filter(!(str_detect(color, "colorless") & !str_detect(types, "Land")))
     }
     #filter cmc
     jddout<-jddout%>%
       filter(cmc>=input$cmc[1],
              cmc<=input$cmc[2])
     #filter types
     if(!is.null(input$type)){
       jddout<-jddout%>%
         filter(types %in% input$type)
     }
     if(input$only_multi){
       jddout<-jddout%>%
         filter(str_detect(color, ' '))
     }
     #filter by colors
     if (is.null(input$color)){
       jddout<-jddout%>%
         filter(color == 'colorless')
     }else if(length(input$color) == 5){
     }
     else{
       jddout<-filter_multi(input$color, jddout)
     }
     if(!input$multi){
       jddout<-jddout%>%
         filter(!str_detect(color, ' '))
     }
     
     #filter by sphere
     if(is.null(input$sphere)){
     }else if(length(input$sphere)==5){
     }else{
       if(input$spfilterType == "Sphere is exactly"){
         jddout<-isex(input$sphere, jddout)
       }else if(input$spfilterType == "Sphere includes"){
         jddout<-incl(input$sphere, jddout)
       }else{
         jddout<-contonly(input$sphere, jddout)
       }
     }
     
     #filter by archetype
     if(is.null(input$archetype)){
     }else if(length(input$archetype)==length(arch_vector)){
     }else{
       if(input$afilterType == "Archetype is exactly"){
         jddout<-aisex(input$archetype, jddout)
       }else if(input$afilterType == "Archetype includes"){
         jddout<-aincl(input$archetype, jddout)
       }else{
         jddout<-acontonly(input$archetype, jddout)
       }
     }
     if(input$text != ""){
       jddout<-jddout%>%
         filter(str_detect(jddout$text, input$text))
     }
     if(input$name != ""){
       jddout<-jddout%>%
         filter(str_detect(tolower(jddout$name), tolower(input$name)))
     }
     if(input$card != ""){
       d<-jddout%>%
         filter(tolower(name) == tolower(input$card))
       jddout<-jddout%>%
         filter(deckID %in% d$deckID)
     }
     #output
     jdd_to_out(jddout)
   })
}


#define functions
#
#
#
#
filter_multi <- function(inp, jddout){
  
  neg_color = c()
  for (i in 1:length(color_vector)){
    if(!(color_vector[i] %in% inp)){
      neg_color<-c(neg_color, color_vector[i])
    }
  }

  iscol = rep(TRUE, length(jddout$color))
  
  for(i in 1:length(jddout$color)){
    for(k in 1:length(neg_color)){
      if(str_detect(jddout$color[i], neg_color[k])){
        iscol[i] = FALSE
      }
    }
  }
  
  jddout$iscol <- iscol
  
  jddout<-jddout%>%
    filter(iscol)%>%
    select(-iscol)
  return(jddout)
}



#sphere filter 1
isex <- function(inp, jddout){
  issph<-rep(TRUE, length(jddout$sphere))
  spheres<-str_split(jddout$sphere, ' ')
  for (i in 1:length(jddout$sphere)){
    for (j in 1:length(spheres[[i]])){
      if(!(spheres[[i]][j] %in% inp)){
        issph[i] = FALSE
      }
    }
    for (k in 1:length(inp)){
      if(!(inp[k] %in% spheres[[i]])){
        issph[i] = FALSE
      }
    }
  }
  
  jddout$issph <- issph
  
  jddout<-jddout%>%
    filter(issph)%>%
    select(-issph)
  return(jddout)
}

#sphere filter 2
contonly <- function(inp, jddout){
  
  neg_sphere = c()
  for (i in 1:length(sphere_vector)){
    if(!(sphere_vector[i] %in% inp)){
      neg_sphere<-c(neg_sphere, sphere_vector[i])
    }
  }
  

  issph = rep(TRUE, length(jddout$sphere))
  
  for(i in 1:length(jddout$sphere)){
    for(k in 1:length(neg_sphere)){
      if(str_detect(jddout$sphere[i], neg_sphere[k])){
        issph[i] = FALSE
      }
    }
  }
  
  jddout$issph <- issph
  
  jddout<-jddout%>%
    filter(issph)%>%
    select(-issph)
  return(jddout)
}

#sphere filter 3
incl <- function(inp, jddout){
  issph<-rep(FALSE, length(jddout$sphere))
  for (i in 1:length(jddout$sphere)){
    for (k in 1:length(inp)){
      if(str_detect(jddout$sphere[i], inp[k])){
        issph[i] = TRUE
      }
    }
  }
  
  jddout$issph <- issph
  
  jddout<-jddout%>%
    filter(issph)%>%
    select(-issph)
  return(jddout)
}

#archetype filter 1
aincl <- function(inp, jddout){
  isarch<-rep(FALSE, length(jddout$archetype))
  for (i in 1:length(jddout$archetype)){
    for (k in 1:length(inp)){
      if(str_detect(jddout$archetype[i], inp[k])){
        isarch[i] = TRUE
      }
    }
  }
  
  jddout$isarch <- isarch
  
  jddout<-jddout%>%
    filter(isarch)%>%
    select(-isarch)
  return(jddout)
}

#archetype filter 2
aisex <- function(inp, jddout){
  isarch<-rep(TRUE, length(jddout$archetype))
  archetypes<-str_split(jddout$archetype, ' ')
  for (i in 1:length(jddout$archetype)){
    for (j in 1:length(archetypes[[i]])){
      if(!(archetypes[[i]][j] %in% inp)){
        isarch[i] = FALSE
      }
    }
    for (k in 1:length(inp)){
      if(!(inp[k] %in% archetypes[[i]])){
        isarch[i] = FALSE
      }
    }
  }
  
  jddout$isarch <- isarch
  
  jddout<-jddout%>%
    filter(isarch)%>%
    select(-isarch)
  return(jddout)
}

#archetype filter 3
acontonly <- function(inp, jddout){
  

  neg_archetypes = c()
  for (i in 1:length(arch_vector)){
    if(!(arch_vector[i] %in% inp)){
      neg_archetypes<-c(neg_archetypes, arch_vector[i])
    }
  }

  isarch = rep(TRUE, length(jddout$archetype))
  
  for(i in 1:length(jddout$archetype)){
    for(k in 1:length(neg_archetypes)){
      if(str_detect(jddout$archetype[i], neg_archetypes[k])){
        isarch[i] = FALSE
      }
    }
  }
  
  jddout$isarch <- isarch
  
  jddout<-jddout%>%
    filter(isarch)%>%
    select(-isarch)
  return(jddout)
}

#output helper function
jdd_to_out<-function(table_to_output){
  t<-table_to_output%>%
    group_by(name)%>%
    summarise(count = n())%>%
    arrange(desc(count))
  return(t)
}



# Run the application 
shinyApp(ui = ui, server = server)

