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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("3-0 Decks"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxInput(inputId = 'dpowered',
                      label = "Include Powered Decks",
                      value = FALSE),
        textInput(inputId = 'dcard',
                  label = 'In Decks Containing this Card',
                  value = ""),
        checkboxInput(inputId = "dsearchsb",
                      label = "Include Sideboards in Card Search",
                      value = FALSE),
        checkboxInput(inputId = "dshow_color",
                      label = "Show Color Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.dshow_color",
          wellPanel(
            checkboxGroupInput(inputId = "dcolor",
                             label = "Select Color",
                             selected = color_vector,
                             choices = color_vector),
            checkboxInput(inputId = "dmulti",
                        label = "Include Multicolored Decks",
                        value = TRUE),
            checkboxInput(inputId = "donlymulti",
                          label = "Include only Multicolored Decks",
                          value = FALSE)
        )),
        checkboxInput(inputId = "dshow_splashcolor",
                      label = "Show Splash Color Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.dshow_splashcolor",
          wellPanel(
            checkboxGroupInput(inputId = "dsplashcolor",
                               label = "Select Color",
                               selected = none_color_vector,
                               choices = none_color_vector)
        )),
        checkboxInput(inputId = "dshow_basecolor",
                      label = "Show Base Color Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.dshow_basecolor",
          wellPanel(
            checkboxGroupInput(inputId = "dbasecolor",
                               label = "Select Base Color",
                               selected = none_color_vector,
                               choices = none_color_vector),
            checkboxInput(inputId = "dbasemulti",
                          label = "Include Base Multicolored Decks",
                          value = TRUE),
            checkboxInput(inputId = "donlybasemulti",
                          label = "Include only Base Multicolored Decks",
                          value = FALSE)
          )),
        checkboxInput(inputId = "dshow_sphere",
                      label = "Show Sphere Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.dshow_sphere",
          wellPanel(
          checkboxGroupInput(inputId = 'dsphere',
                  label = "Filter by Deck Sphere",
                  selected = sphere_vector,
                  choices = sphere_vector
                  ),
          selectInput(inputId = "dspfilterType",
                  label = "Type of Sphere Filter",
                  selected = "Sphere includes",
                  choices = c("Sphere includes", "Sphere is exactly", "Sphere contains only"))
        )),
        checkboxInput(inputId = "dshow_archetype",
                      label = "Show Archetype Filter",
                      value = FALSE),
        conditionalPanel(
          condition = "input.dshow_archetype",
          wellPanel(
            checkboxGroupInput(inputId = 'darchetype',
                         label = "Filter by Deck Archetype",
                         selected = arch_vector,
                         choices = arch_vector),
            selectInput(inputId = "dafilterType",
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
     decksout<-decks
     #exclude powered decks
     if(!input$dpowered){
       decksout<-decksout%>%
         filter(!powered)
     }

     #filter by colors
     if (is.null(input$dcolor)){
     }else if(length(input$dcolor) == 5){
     }
     else{
       decksout<-dfilter_multi(input$dcolor, decksout)
     }
     if(!input$dmulti){
       decksout<-decksout%>%
         filter(!str_detect(colors, ' '))
     }
     if(input$donlymulti){
       decksout<-decksout%>%
         filter(str_detect(colors, ' '))
     }
     
     #filter by base colors
     if (is.null(input$dbasecolor)){
     }else if(length(input$dbasecolor) == 6){
     }
     else{
       decksout<-dfilter_base_multi(input$dbasecolor, decksout)
     }
     if(!input$dbasemulti){
       decksout<-decksout%>%
         filter(!str_detect(baseColors, ' '))
     }
     if(input$donlybasemulti){
       decksout<-decksout%>%
         filter(str_detect(colors, ' '))
     }
     
     #filter by splash colors
     if (is.null(input$dsplashcolor)){
     }else if(length(input$dsplashcolor) == 6){
     }
     else{
       decksout<-dfilter_splash_multi(input$dsplashcolor, decksout)
     }
     
     #filter by sphere
     if(is.null(input$dsphere)){
     }else if(length(input$dsphere)==5){
     }else{
       if(input$dspfilterType == "Sphere is exactly"){
         decksout<-disex(input$dsphere, decksout)
       }else if(input$dspfilterType == "Sphere includes"){
         decksout<-dincl(input$dsphere, decksout)
       }else{
         decksout<-dcontonly(input$dsphere, decksout)
       }
     }
     
     #filter by archetype
     if(is.null(input$darchetype)){
     }else if(length(input$darchetype)==length(arch_vector)){
     }else{
       if(input$dafilterType == "Archetype is exactly"){
         decksout<-daisex(input$darchetype, decksout)
       }else if(input$dafilterType == "Archetype includes"){
         decksout<-daincl(input$darchetype, decksout)
       }else{
         decksout<-dacontonly(input$darchetype, decksout)
       }
     }

     if(input$dcard != ""){
       if(!input$dsearchsb){
         jdd2<-jdd1%>%
           filter(location == "MB")
       }else{
         jdd2<-jdd1
       }
       d<-jdd2%>%
         filter(tolower(name) == tolower(input$dcard))
       decksout<-decksout%>%
         filter(deckID %in% d$deckID)
     }
     
     #output
     decksout
   })
   

}


#define functions
#
#
#
#
dfilter_multi <- function(inp, decksout){
  
  neg_color = c()
  for (i in 1:length(color_vector)){
    if(!(color_vector[i] %in% inp)){
      neg_color<-c(neg_color, color_vector[i])
    }
  }
  
  iscol = rep(TRUE, length(decksout$colors))
  
  for(i in 1:length(decksout$colors)){
    for(k in 1:length(neg_color)){
      if(str_detect(decksout$colors[i], neg_color[k])){
        iscol[i] = FALSE
      }
    }
  }
  
  decksout$iscol <- iscol
  
  decksout<-decksout%>%
    filter(iscol)%>%
    select(-iscol)
  return(decksout)
}

dfilter_base_multi <- function(inp, decksout){
  
  neg_color = c()
  for (i in 1:length(none_color_vector)){
    if(!(none_color_vector[i] %in% inp)){
      neg_color<-c(neg_color, none_color_vector[i])
    }
  }
  
  iscol = rep(TRUE, length(decksout$baseColors))
  
  for(i in 1:length(decksout$baseColors)){
    for(k in 1:length(neg_color)){
      if(str_detect(decksout$baseColors[i], neg_color[k])){
        iscol[i] = FALSE
      }
    }
  }
  
  decksout$iscol <- iscol
  
  decksout<-decksout%>%
    filter(iscol)%>%
    select(-iscol)
  return(decksout)
}

#filter splash colors
dfilter_splash_multi <- function(inp, decksout){
  
  neg_color = c()
  for (i in 1:length(none_color_vector)){
    if(!(none_color_vector[i] %in% inp)){
      neg_color<-c(neg_color, none_color_vector[i])
    }
  }
  
  iscol = rep(TRUE, length(decksout$splashes))
  
  for(i in 1:length(decksout$splashes)){
    for(k in 1:length(neg_color)){
      if(str_detect(decksout$splashes[i], neg_color[k])){
        iscol[i] = FALSE
      }
    }
  }
  
  decksout$iscol <- iscol
  
  decksout<-decksout%>%
    filter(iscol)%>%
    select(-iscol)
  return(decksout)
}

#sphere filter 1
disex <- function(inp, decksout){
  issph<-rep(TRUE, length(decksout$sphere))
  spheres<-str_split(decksout$sphere, ' ')
  for (i in 1:length(decksout$sphere)){
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
  
  decksout$issph <- issph
  
  decksout<-decksout%>%
    filter(issph)%>%
    select(-issph)
  return(decksout)
}

#sphere filter 2
dcontonly <- function(inp, decksout){
  
  neg_sphere = c()
  for (i in 1:length(sphere_vector)){
    if(!(sphere_vector[i] %in% inp)){
      neg_sphere<-c(neg_sphere, sphere_vector[i])
    }
  }
  

  issph = rep(TRUE, length(decksout$sphere))
  
  for(i in 1:length(decksout$sphere)){
    for(k in 1:length(neg_sphere)){
      if(str_detect(decksout$sphere[i], neg_sphere[k])){
        issph[i] = FALSE
      }
    }
  }
  
  decksout$issph <- issph
  
  decksout<-decksout%>%
    filter(issph)%>%
    select(-issph)
  return(decksout)
}

#sphere filter 3
dincl <- function(inp, decksout){
  issph<-rep(FALSE, length(decksout$sphere))
  for (i in 1:length(decksout$sphere)){
    for (k in 1:length(inp)){
      if(str_detect(decksout$sphere[i], inp[k])){
        issph[i] = TRUE
      }
    }
  }
  
  decksout$issph <- issph
  
  decksout<-decksout%>%
    filter(issph)%>%
    select(-issph)
  return(decksout)
}

#archetype filter 1
daincl <- function(inp, decksout){
  isarch<-rep(FALSE, length(decksout$archetype))
  for (i in 1:length(decksout$archetype)){
    for (k in 1:length(inp)){
      if(str_detect(decksout$archetype[i], inp[k])){
        isarch[i] = TRUE
      }
    }
  }
  
  decksout$isarch <- isarch
  
  decksout<-decksout%>%
    filter(isarch)%>%
    select(-isarch)
  return(decksout)
}

#archetype filter 2
daisex <- function(inp, decksout){
  isarch<-rep(TRUE, length(decksout$archetype))
  archetypes<-str_split(decksout$archetype, ' ')
  for (i in 1:length(decksout$archetype)){
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
  
  decksout$isarch <- isarch
  
  decksout<-decksout%>%
    filter(isarch)%>%
    select(-isarch)
  return(decksout)
}

#archetype filter 3
dacontonly <- function(inp, decksout){
  

  neg_archetypes = c()
  for (i in 1:length(arch_vector)){
    if(!(arch_vector[i] %in% inp)){
      neg_archetypes<-c(neg_archetypes, arch_vector[i])
    }
  }

  isarch = rep(TRUE, length(decksout$archetype))
  
  for(i in 1:length(decksout$archetype)){
    for(k in 1:length(neg_archetypes)){
      if(str_detect(decksout$archetype[i], neg_archetypes[k])){
        isarch[i] = FALSE
      }
    }
  }
  
  decksout$isarch <- isarch
  
  decksout<-decksout%>%
    filter(isarch)%>%
    select(-isarch)
  return(decksout)
}





# Run the application 
shinyApp(ui = ui, server = server)

