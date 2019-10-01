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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  tabsetPanel(id = "tabset1",
    tabPanel(
     title = "Description",
     value = "desc",
     titlePanel("3-0 Cube Decks"),
     br(),
     p('This app pulls from a wonderful dataset of 3-0 cube decklists, and offers a multitude of options
       for querying. In the "Cards" tab, you can search for the cards that appear most often in 3-0
       decks using a variety of filters. In the "Decks" tab, you can search for 3-0 decks by colors,
       archetypes, spheres, and cards they contain. The "Decks" tab also offers the option to export 
       the current list of decks you are viewing to the "Deck Stats" tab, where you can view the card,
       curve, color, and type breakdowns for that subset of decks.')
    ),
              
    tabPanel(
     title = "Cards",
     value = "cards",
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
    ),
    tabPanel(
     title = "Decks",
     value = "decks",
     titlePanel("3-0 Decks"),
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
          actionLink(inputId = "linkdecks",
                     label = "View Selected Decks in Deck Stats"),
          tableOutput(outputId = "dtb")
      )
    )
    ),
    tabPanel(title = "Deck Stats",
             value = "deckstats",
             titlePanel("Deck Stats"),
             br(),
             textInput(inputId = "sdecks",
                       label = "Search by Deck ID",
                       value = "234"),
             p('Enter one or more deck IDs, separated by spaces. To view stats of larger lists of decks, filter to the list
                on the "Decks" tab, and click the link at the top of the page.'),
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
  
  
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   rv<-reactiveValues(value = decks)
   
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
   
   output$dtb <- renderTable({
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
     rv$value = decksout
     #output
     decksout
   })
   
   #Deck Stats outputs
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
         select(name, color, cmc, types)%>%
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
       ""
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
   
   observeEvent(input$linkdecks, {
     if(!is.null(rv$value)){
       v<-rv$value
        if(length(v$deckID)>0){
         updateTabsetPanel(session, "tabset1",
                           selected = "deckstats"
         )
         val = ""
         v1<-unique(v$deckID)
         for (i in 1:length(v1)){
           val = paste(val, v1[i], sep = ' ')
         }
         val<-substring(val, 2)
         
         updateTextInput(session, "sdecks",
                         value = val
         )
       }
     }
   })

}


#define functions
#
#
#
#
#
#
#
#
#
#
#
#
#
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

