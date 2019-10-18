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
library(DT)
library(leaflet)
library(stringr)
library(rvest)
library(data.table)
library(shiny)
library(stats)


#set up global dataframes and vectors
jdd1<-read_csv("jdd_newest.csv")
jdd7<-read_csv("jdd7.csv")
jdd20<-jdd1
arch_c<-read_csv("arch_c.csv")
jdd_lands<-read_csv("jdd_lands.csv")
jdd_sb<-read_csv("jdd_sb.csv")
color_vector <- c("white", "blue", "black", "red", "green")
none_color_vector <- c("white", "blue", "black", "red", "green", "-")
type_vector <- unique(jdd1$types)
sphere_vector <- c('midrange', 'ramp', 'control', 'combo', 'aggro')
arch_vector <- c("twin", "wildfire", "opposition", "reanimator", "cheat", "artifacts", "storm", "stax",
                 "burn", "stoneforge", "armageddon", "skullclamp", "counterburn", "pox", "monowhite",
                 "monored", "naturalorder", "edric", "reveillark", "spellsmatter", "pod", "oath",
                 "fivecolor", "balance", "moat", "upheaval", "mentor", "abyss", "crucible", "superfriends",
                 "survival", "blink", "tokens", "channel")


#create deck table
decks<-jdd1%>%
  group_by(deckID, colors, baseColors, splashes, sphere, archetype, powered)%>%
  summarise(count = n())%>%
  select(-count)


#make casing easier and remove basics from one dataframe
jdd1s<-jdd1
jdd1$color<-tolower(jdd1$color)
jdd1$text<-tolower(jdd1$text)
jdd1<-jdd1%>%
  filter(!str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))


# Define UI for the application
ui <- fluidPage(
   
   tabsetPanel(id = "tabset1",
    tabPanel(
     title = "Description",
     value = "desc",
     titlePanel("XMage Cube Data"),
     br(),
     p('This app pulls from a wonderful dataset of 3-0 cube decklists curated by the XMage Cube public discord server, and offers a multitude of options
       for querying. Search our data at the card or deck level, see deck and archetype analysis, and let our DraftBot make picks for you!'),
     actionLink("link_to_cards", "Cards Tab:"),
     p("Search for the cards that appear most often in 3-0 decks using a variety of filters."),
     br(),
     actionLink("link_to_decks", "Decks Tab:"),
     p("Search for 3-0 decks by colors, archetypes, spheres, and cards they contain.The 'Decks' tab also offers the option to export 
       the current list of decks you are viewing to the 'Deck Stats' tab"),
     br(),
     actionLink("link_to_sdecks", "Deck Stats Tab:"),
     p("View the card, curve, color, and type breakdowns for a subset of decks"),
     br(),
     actionLink("link_to_draftbot", "DraftBot Tab:"),
     p("Let our algorithm make your picks for you!")
     ),
    tabPanel(
     title = "Cards",
     value = "cards",
     titlePanel("3-0 Frequency by Card"),
     sidebarLayout(
       sidebarPanel(
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
                           value = FALSE),
             checkboxInput(inputId = 'cls',
                           label = "Include Colorless Cards",
                           value = TRUE)
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
             h5("Filter by Deck Archetype"),
             actionLink("selectall", "Select All"),
             br(),
             actionLink("unselectall", "Unselect All"),
             checkboxGroupInput(inputId = 'archetype',
                                label = "",
                                selected = arch_vector,
                                choices = arch_vector),
             selectInput(inputId = "afilterType",
                         label = "Type of Archetype Filter",
                         selected = "Archetype includes",
                         choices = c("Archetype includes", "Archetype is exactly", "Archetype contains only"))
           ))
       ),
       mainPanel(
         DTOutput(outputId = "tb")
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
          checkboxInput(inputId = "dsearchsb",
                        label = "Include Sideboards in Card Search",
                        value = FALSE),
          fluidRow(
            column(8,
              textInput(inputId = 'dcard',
                        label = 'Search Cards (separate cards with "|")',
                        value = "")),
            column(4,
              selectInput(inputId = 'dsearchtype',
                          label = 'Search Type',
                          selected = 'AND',
                          choices = c('AND', 'OR')))
          ),
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
              h5("Filter by Deck Archetype"),
              actionLink("dselectall", "Select All"),
              br(),
              actionLink("dunselectall", "Unselect All"),
              checkboxGroupInput(inputId = 'darchetype',
                           label = "",
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
                     label = "View Filtered Decks in Deck Stats"),
          DTOutput(outputId = "dtb")
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
                      DTOutput(outputId = "stb")),
               column(6,
                      h4("Deck Charts"),
                      selectInput(inputId = "schartdisplay",
                                  label = "Display Type",
                                  choices = c("Total", "Per Deck")),
                      h5("Curve"),
                      plotOutput(outputId = "scurve"),
                      h5("Types"),
                      plotOutput(outputId = "stypes"),
                      h5("Colors"),
                      plotOutput(outputId = "scolors"))
             )
             ),
    tabPanel(
      title = "DraftBot",
      value = "draftbot",
      titlePanel(title = "Draftbot"),
      p('Do you need some advice on a tough pack? Want to get a new perspective on some cube cards? Ask the Draftbot! 
        Input the cards you have already picked on the left under "Pool", and the cards in the pack you are 
        looking at on the right, under "Pack". Each field takes MTG card names, not case or space sensitive, seperated by
        the "|" character, for instance " char|Demonic tutor ". Then, press the "Run" button and take a look at the "Value" column in the table on the right 
        This will tell you which cards the bot recommends picking.' ),
      br(),
      actionButton(inputId = "go",
                   label = "Run"),
      br(),
      br(),
      fluidRow(
        column(6,
               textInput(inputId = "pool",
                         label = "Pool",
                         value = ""),
               tableOutput(outputId = "clrtb"),
               tableOutput(outputId = "sphtb"),
               tableOutput(outputId = "archtb")
        ),
        column(6,
               textInput(inputId = "pack",
                         label = "Pack",
                         value = ""),
               tableOutput(outputId = "packtb")
        )
      )
    )
  )
)



#server
#
#
#
#
server <- function(input, output, session) {
  
   rv<-reactiveValues(value = decks)
   
   #select/unselect all buttons
   observe({
     if(input$selectall == 0) return(NULL) 
     else{
       updateCheckboxGroupInput(session, "archetype", "", choices=arch_vector, selected = arch_vector)
     }
   })
   
   observe({
     if(input$unselectall == 0) return(NULL) 
     else{
       updateCheckboxGroupInput(session, "archetype", "", choices=arch_vector)
     }
   })
   
   observe({
     if(input$dselectall == 0) return(NULL) 
     else{
       updateCheckboxGroupInput(session, "darchetype", "", choices=arch_vector, selected = arch_vector)
     }
   })
   
   observe({
     if(input$dunselectall == 0) return(NULL) 
     else{
       updateCheckboxGroupInput(session, "darchetype", "", choices=arch_vector)
     }
   })
   
   
   #cards table
   output$tb <- renderDT({
     
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
       d<-jdd1s%>%
         filter(tolower(name) == tolower(input$card))
       jddout<-jddout%>%
         filter(deckID %in% d$deckID)
     }
     #output
     jdd_to_out(jddout)
   }, options = list(
     pageLength = 15)
   )
   
   #decks table
   output$dtb <- renderDT({
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
     
     #filter by name
     if(input$dcard != ""){
       jdd2<-jdd1s
       if(!input$dsearchsb){
         jdd2<-jdd1s%>%
           filter(location == "MB")
       }else{
         jdd2<-jdd1s
       }
       vec<-makecv(strsplit(isolate(input$dcard), "\\|"))
       
       jdd_dcs<-jdd2%>%
         filter(tolower(name)%in%tolower(vec))
       if(input$dsearchtype == 'AND'){
         jdd_dcs<-jdd_dcs%>%
           group_by(deckID)%>%
           summarise(count = n())%>%
           filter(count == length(vec))
       }
       decksout<-decksout%>%
         filter(deckID %in% jdd_dcs$deckID)
     }
     rv$value = decksout
     #output
     decksout%>%
       rename(`Deck ID` = deckID, Colors = colors, `Base Colors` = baseColors, Splashes = splashes, 
              Sphere = sphere, Archetype = archetype, Powered = powered)
   }, options = list(
     pageLength = 15)
   )
   
   #Deck Stats outputs
   output$stb <- renderDT({
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
         rename(Name = name, Color = color, CMC = cmc, Types = types, Count = count)%>%
         arrange(desc(Count))
     }
   }, options = list(
     pageLength = 40))
   
   #numdecks and numcards text fields display number of decks and unique cards
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
       paste("Number of decks: ", length(unique(o2$deckID)))
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
   
   #cmc chart
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
       if(input$schartdisplay == "Total"){
         stbout%>%
           filter(deckID %in% sdecklist[[1]])%>%
           filter(!str_detect(types, "Land"))%>%
           group_by(cmc)%>%
           summarise(count = sum(number))%>%
           ggplot()+
           geom_col(aes(x = cmc, y = count, fill = count))+
           theme_minimal()
       }
       else if(input$schartdisplay == "Per Deck"){
         stbout%>%
           filter(deckID %in% sdecklist[[1]])%>%
           filter(!str_detect(types, "Land"))%>%
           group_by(cmc)%>%
           summarise(count = sum(number))%>%
           ggplot()+
           geom_col(aes(x = cmc, y = count/length(sdecklist[[1]]), fill = count))+
           theme_minimal()+
           labs(y = "count")
       }
     }
   })
   
   #supertype chart
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
       if(input$schartdisplay == "Total"){
         stbout%>%
           group_by(types)%>%
           summarise(count = sum(number))%>%
           ggplot()+
           geom_col(aes(x = types, y = count, fill = types))+
           theme_minimal()
       }
       else if(input$schartdisplay == "Per Deck"){
         stbout%>%
           group_by(types)%>%
           summarise(count = sum(number))%>%
           ggplot()+
           geom_col(aes(x = types, y = count/length(sdecklist[[1]]), fill = types))+
           theme_minimal()+
           labs(y = "count")
       }
     }
   })
   
   #card color chart
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
       if(input$schartdisplay == "Total"){
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
       else if(input$schartdisplay == "Per Deck"){
         stbout%>%
           filter(deckID %in% sdecklist[[1]])%>%
           filter(!str_detect(types, "Land"))%>%
           filter(!str_detect(color, ' '))%>%
           group_by(color)%>%
           summarise(count = sum(number))%>%
           ggplot()+
           geom_col(aes(x = color, y = count, fill = color))+
           theme_minimal()+
           labs(y = 'count')
       }
     }
   })
   
   #handle link from decks tab to deckstats tab
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
   
   #description page links
   observeEvent(input$link_to_cards, {
     updateTabsetPanel(session, "tabset1", selected = "cards")
   })
   
   observeEvent(input$link_to_decks, {
     updateTabsetPanel(session, "tabset1", selected = "decks")
   })
   
   observeEvent(input$link_to_sdecks, {
     updateTabsetPanel(session, "tabset1", selected = "deckstats")
   })
   
   observeEvent(input$link_to_draftbot, {
     updateTabsetPanel(session, "tabset1", selected = "draftbot")
   })
   
   
   #draftbot tables
   output$clrtb <- renderTable({
     input$go
     cv<-makecv(strsplit(isolate(input$pool), "\\|"))
     createcc(cv, jdd7)%>%
       rename(Color = color, Commitment = commitment)
   })
   
   
   output$sphtb <- renderTable({
     input$go
     cv<-makecv(strsplit(isolate(input$pool), "\\|"))
     createsph(cv, jdd20)%>%
       rename(Sphere = sphere, Commitment = commitment)
   })
   
   
   output$archtb <- renderTable({
     input$go
     cv<-makecv(strsplit(isolate(input$pool), "\\|"))
     createarch(cv, jdd20)%>%
       rename(Archetype = arch, Commitment = commitment)
   })
   
   
   output$packtb <- renderTable({
     input$go
     cv<-makecv(strsplit(isolate(input$pack), "\\|"))
     cv1<-makecv(strsplit(isolate(input$pool), "\\|"))
     noncards_cv<-c()
     rmlist <- c()
     if(length(cv) != 0){
       for (i in 1:length(cv)){
         jdd_bwithin<-jdd1%>%
           filter(tolower(name) == tolower(cv[i]),
                  location == "MB")
         if(nrow(jdd_bwithin) == 0){
           noncards_cv<-c(noncards_cv, cv[i])
         }
       }
     }
     cv<-cv[!cv %in% noncards_cv]
     if(length(cv1) != 0){
       for (i in 1:length(cv1)){
         jdd_bwithin<-jdd1%>%
           filter(tolower(name) == tolower(cv1[i]),
                  location == "MB")
         if(nrow(jdd_bwithin) == 0){
           cv1<-cv1[-i]
         }
       }
     }
     powervals <- makePowerVals(cv)
     colorvals <- makeColorVals(cv, cv1)
     spherevals <- makeSphereVals(cv, cv1)
     archvals <- makeArchVals(cv, cv1)
     archvals<-archvals/1.5
     if(length(archvals) == 0|
        length(spherevals) == 0|
        length(colorvals) == 0|
        length(powervals) == 0|
        length(cv) == 0){
       archvals = c()
       spherevals = c()
       colorvals = c()
       powervals = c()
       cv = c()
     }
     df<-data.frame("Name" = cv, 
                    "Power" = powervals, 
                    "Color" = colorvals, 
                    "Sphere" = spherevals, 
                    "Archetype" = archvals)
     
     if(!is.null(noncards_cv)){
       if(length(noncards_cv) != 0){
         for (i in 1:length(noncards_cv)){
           df<-rbind(df, data.frame("Name" = c(noncards_cv[i]),
                                "Power" = c(0), 
                                "Color" = c(0), 
                                "Sphere" = c(0), 
                                "Archetype" = c(0)))
         }
       }
     }
     add_evs(df)%>%
       arrange(desc(Value))
   })

}


#define functions
#
#
#
#
#
#

#filter cards by multiple colors
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



#card sphere filter (is exactly)
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

#card sphere filter (contains only)
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

#card sphere filter (includes)
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

#card archetype filter (includes)
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

#card archetype filter (is exactly)
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

#card archetype filter (contains only)
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
  table_to_output <- table_to_output%>%
    mutate(aggro = ifelse(str_detect(sphere, "aggro"), 1, 0),
           midrange = ifelse(str_detect(sphere, "midrange"), 1, 0),
           control = ifelse(str_detect(sphere, "control"), 1, 0),
           combo = ifelse(str_detect(sphere, "combo"), 1, 0),
           ramp = ifelse(str_detect(sphere, "ramp"), 1, 0))
    
  t<-table_to_output%>%
    group_by(name)%>%
    summarise(Total = n(),
              Aggro = sum(aggro),
              Midrange = sum(midrange),
              Control = sum(control),
              Combo = sum(combo),
              Ramp = sum(ramp))%>%
    rename(Name = name)%>%
    arrange(desc(Total))
  t<-left_join(t, jdd_sb)
  return(t)
}

#filter decks by multiple colors
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
#filter decks by multiple base colors
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

#filter deck by multiple splash colors
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

#deck sphere filter (is exactly)
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

#deck sphere filter (contains only)
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

#deck sphere filter (includes)
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

#deck archetype filter (includes)
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

#deck archetype filter (is exactly)
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

#deck archetype filter (contains only)
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

#deck stats supertype table helper function
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


#turns card list into vector
makecv <- function(cardlist){
  cv<-unlist(cardlist)
  cv<-str_replace_all(cv, "^(\\s)*", "")
  cv<-str_replace_all(cv, "(\\s)*$", "")
  cv<-str_replace_all(cv, "’", "'")
  return(cv)
}

#calculates color commitment
createcc<-function(pool, jdd7){
  color_commitment = data_frame("color" = c("White", "Blue", "Black", "Red", "Green"), 
                                "commitment" = rep(0, 5))
  if(length(pool)!=0){
    for (i in 1:length(pool)){
      com = 0
      clr = ""
      if(tolower(pool[i]) %in% tolower(jdd_lands$name)){
        jdd_l2 <- jdd_lands%>%
          filter(tolower(name) == tolower(pool[i]))
        com = mean(jdd_l2$power)
        for (j in 1:length(color_commitment$color)){
          if(str_detect(jdd_l2$landColor[1], color_commitment$color[j])){
            color_commitment$commitment[j] = color_commitment$commitment[j] + com
          }
        }
      }
      else{
        for (j in 1:length(jdd7$name)){
          if (tolower(jdd7$name[j]) == tolower(pool[i])){
            com = jdd7$newcc[j]
            clr = jdd7$color[j]
          }
        }
        for (j in 1:length(color_commitment$color)){
          if(str_detect(clr, color_commitment$color[j])){
            color_commitment$commitment[j] = color_commitment$commitment[j] + com
          }
        }
      }
    }
  }
  
  return(color_commitment)
}

#calculates sphere commitment 
createsph<-function(spool, jdd20){
  sphere_commitment = data_frame("sphere" = c("midrange", "aggro", "control", "ramp", "combo"), "commitment" = rep(0, 5))
  if(length(spool) == 0){
    return(sphere_commitment)
  }
  for (i in 1:length(spool)){
    jdd8<-jdd20%>%
      filter(tolower(name) == tolower(spool[i]),
             location == "MB")%>%
      group_by(sphere)%>%
      summarise(count = n())
    for (k in 1:length(sphere_commitment$sphere)){
      for (j in 1:length(jdd8$sphere)){
        if(tolower(spool[i]) %in% tolower(jdd20$name)){
          if (str_detect(jdd8$sphere[j], sphere_commitment$sphere[k])){
            sphere_commitment$commitment[k] = sphere_commitment$commitment[k] + jdd8$count[j]
          }
        }
      }
    }
    
  }
  
  jdd9<-jdd20%>%
    group_by(sphere)%>%
    summarise(count = n())
  sphere_commitment$count = rep(0, length(sphere_commitment$sphere))
  for (k in 1:length(sphere_commitment$sphere)){
    for (i in 1:length(jdd9$sphere)){
      if(str_detect(jdd9$sphere[i], sphere_commitment$sphere[k])){
        sphere_commitment$count[k] = sphere_commitment$count[k] + jdd9$count[i]
      }
    }
    sphere_commitment$commitment[k] = 500*sphere_commitment$commitment[k]/sphere_commitment$count[k]
  }
  sphere_commitment<-sphere_commitment%>%
    select(-count)
  
  return(sphere_commitment)
}

#calculates arch commitment
createarch<-function(apool, jdd20){
  arch_commitment = data_frame("arch" = arch_vector,
                               "commitment" = rep(0, length(arch_vector)))
  if(length(apool) == 0){
    return(arch_commitment)
  }
  
  arch_c2<-arch_c%>%
    filter(tolower(name)%in%tolower(apool))
  sums<-colSums(arch_c2[-1])
  arch_commitment$commitment<-unname(sums)
  return(arch_commitment)
}

#determine pool cards' color values
makeColorVals <- function(pack, pool){
  currentcolors = createcc(pool, jdd7)
  colorVector = rep(0, length(pack))
  if(length(pack) !=0 & length(pool) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(unique(jdd1$name))){
        if(tolower(pack[i]) %in% tolower(jdd_lands$name)){
          jdd_l2<-jdd_lands%>%
            filter(tolower(name) == tolower(pack[i]))
          total = sum(currentcolors$commitment)
          if(jdd_l2$numColors[1] == 4){
            colorVector[i] = total * jdd_l2$power[1]/8
          }
          else if(jdd_l2$numColors[1] == 2){
            for (k in 1:length(currentcolors$commitment)){
              if(str_detect(jdd_l2$landColor[1], currentcolors$color[k])){
                colorVector[i] = colorVector[i] + (currentcolors$commitment[k] * jdd_l2$power[1])/8
              }
            }
          }
        }
        else{
          jdd200<-jdd7%>%
            filter(tolower(name) == tolower(pack[i]))
          
          colorval = 0
          off_color = FALSE
          card_colors = unlist(strsplit(jdd200$color[1], split=" "))
          card_cc = jdd200$newcc[1]
          for (k in 1:length(currentcolors$color)){
            for (j in 1:length(card_colors)){
              if(card_colors[j] == currentcolors$color[k] && currentcolors$commitment[k]>0){
                if(currentcolors$commitment[k] > colorval){
                  colorval = colorval + currentcolors$commitment[k]
                }
              }
              if(card_colors[j] == currentcolors$color[k] && currentcolors$commitment[k] == 0){
                off_color = TRUE
              }
            }
          }
          if(!off_color && jdd200$color != "Colorless"){
            colorVector[i] <- colorval
          }else if(jdd200$color == "Colorless"){
            colorVector[i]<-max(currentcolors$commitment)
          }
        }
      }
    }
  }
  colorVector = colorVector * 2
  return(colorVector)
}

#determine pool cards' sphere values
makeSphereVals <- function(pack, pool){
  currentsphere = createsph(pool, jdd20)
  sphVector = rep(0, length(pack))
  if(length(pack) !=0 & length(pool) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(jdd20$name)){
        sph_weight<-createsph(c(pack[i]), jdd20)
        sphval = 0
        for (k in 1:length(currentsphere$sphere)){
          for (j in 1:length(sph_weight$sphere)){
            if(sph_weight$sphere[j] == currentsphere$sphere[k]){
              sphval = sphval + currentsphere$commitment[k]*sph_weight$commitment[j]
            }
          }
        }
        sphVector[i] <- max(log(sphval), 0)
      }
    }
    return(sphVector)
  }
}

#calculates pool cards' archetype values
makeArchVals <- function(pack, pool){
  currentarch = createarch(pool, jdd20)
  archVector = rep(0, length(pack))
  if(length(pack) !=0 & length(pool) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(jdd20$name)){
        arch_weight<-createarch(c(pack[i]), jdd20)
        archval = 0
        for (k in 1:length(currentarch$arch)){
          for (j in 1:length(arch_weight$arch)){
            if(arch_weight$arch[j] == currentarch$arch[k]){
              archval = archval + currentarch$commitment[k]*arch_weight$commitment[j]
            }
          }
        }
        archVector[i] <- max(log(archval), 0)
      }
    }
  }
  return(archVector)
}

#returns pool cards' power values
makePowerVals<- function(pack){
  powervals <- rep(0, length(pack))
  if(length(pack) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(jdd_lands$name)){
        jdd_l2<-jdd_lands%>%
          filter(tolower(name) == tolower(pack[i]))
        powervals[i]<-jdd_l2$power[1]
      }
      else{
        if(tolower(pack[i]) %in% tolower(jdd7$name)){
          jdd202<-jdd7%>%
            filter(tolower(name) == tolower(pack[i]))
          powervals[i]<-jdd202$power[1]/2
        }
      }
    }
  }
  return(powervals)
}

#adds up values to create overall pool card value column
add_evs<-function(df){
  if(length(df$Name)>0){
    df$Value = rep(0, length(df$Name))
    for(i in 1:length(df$Value)){
      df$Value[i]<-df$Power[i] + df$Archetype[i] + df$Sphere[i] + df$Color[i]
    }
  }
  return(df)
}


# Run the application 
shinyApp(ui = ui, server = server)

