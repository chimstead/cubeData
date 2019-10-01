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

jdd7<-read_csv("jdd7.csv")
jdd20<-read_csv("jdd_newest.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(title = "Draftbot"),
  actionButton(inputId = "go",
               label = "Run"),
  br(),
  fluidRow(
    column(6,
      textInput(inputId = "pool",
                label = "Your Card Pool (card names, | separated)",
                value = ""),
      tableOutput(outputId = "clrtb"),
      tableOutput(outputId = "sphtb"),
      tableOutput(outputId = "archtb")
    ),
    column(6,
      textInput(inputId = "pack",
                label = "Pack of Cards to Select from (| separated)",
                value = ""),
      tableOutput(outputId = "packtb"),
      tableOutput(outputId = "landtb")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$clrtb <- renderTable({
    input$go
     cv<-makecv(strsplit(isolate(input$pool), "\\|"))
     createcc(cv, jdd7)
   })
   
   
  output$sphtb <- renderTable({
    input$go
     cv<-makecv(strsplit(isolate(input$pool), "\\|"))
     createsph(cv, jdd20)
   })
   
   
  output$archtb <- renderTable({
    input$go
     cv<-makecv(strsplit(isolate(input$pool), "\\|"))
     createarch(cv, jdd20)
   })
   
   
  output$packtb <- renderTable({
    input$go
     cv<-makecv(strsplit(isolate(input$pack), "\\|"))
     cv1<-makecv(strsplit(isolate(input$pool), "\\|"))
     powervals <- makePowerVals(cv)
     colorvals <- makeColorVals(cv, cv1)
     spherevals <- makeSphereVals(cv, cv1)
     archvals <- makeArchVals(cv, cv1)
     if(length(archvals) == 0|
        length(spherevals) == 0|
        length(colorvals) == 0|
        length(powervals) == 0|
        length(cv) == 0){
       archvals = rep(0, length(powervals))
       spherevals = rep(0, length(powervals))
       colorvals = rep(0, length(powervals))
     }
     df<-data.frame("Name" = cv, 
                "Power" = powervals, 
                "Color" = colorvals, 
                "Sphere" = spherevals, 
                "Archetype" = archvals)
     df<-left_join(df, jdd7, by = c(Name, name))
     add_evs(df)%>%
       arrange(desc(ev))
  })
  
  output$landtb <- renderTable({
    input$go
    cv<-makecv(strsplit(isolate(input$pack), "\\|"))
    cv1<-makecv(strsplit(isolate(input$pool), "\\|"))
    colorvals <- makeColorVals(cv, cv1)
    spherevals <- makeSphereVals(cv, cv1)
    archvals <- makeArchVals(cv, cv1)
    if(length(archvals) == 0|
       length(spherevals) == 0|
       length(colorvals) == 0|
       length(cv) == 0){
      archvals = rep(0, length(cv))
      spherevals = rep(0, length(cv))
      colorvals = rep(0, length(cv))
    }
    df<-data.frame("Name" = cv, 
                   "Color" = colorvals, 
                   "Sphere" = spherevals, 
                   "Archetype" = archvals)
    add_land_evs(df)
  })



}

makecv <- function(cardlist){
  cv<-unlist(cardlist)
  cv<-str_replace_all(cv, "^(\\s)*", "")
  cv<-str_replace_all(cv, "(\\s)*$", "")
  cv<-str_replace_all(cv, "’", "'")
  return(cv)
}


createcc<-function(pool, jdd7){
  color_commitment = data_frame("color" = c("White", "Blue", "Black", "Red", "Green"), 
                                "commitment" = rep(0, 5))
  if(length(pool)!=0){
    for (i in 1:length(pool)){
      com = 0
      clr = ""
      for (j in 1:length(jdd7$name)){
        if (tolower(jdd7$name[j]) == tolower(pool[i])){
          com = jdd7$newcc[j]
          clr = jdd7$color[j]
        }
      }
      for (k in 1:length(color_commitment$color)){
        if(str_detect(clr, color_commitment$color[k])){
          color_commitment$commitment[k] = color_commitment$commitment[k] + com
        }
      }
    }
  }
  return(color_commitment)
}

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


createarch<-function(apool, jdd20){
  arch_commitment = data_frame("arch" = c('natural_order', 'twin', 'reanimator', 'opposition', 
                                          'cheat', 'wildfire', 'artifacts'),
                               "commitment" = rep(0, 7))
  if(length(apool) == 0){
    return(arch_commitment)
  }
  
  for (i in 1:length(apool)){
    jdd8<-jdd20%>%
      filter(tolower(name) == tolower(apool[i]),
             location == "MB")%>%
      group_by(archetype)%>%
      summarise(count = n())
    for (k in 1:length(arch_commitment$arch)){
      for (j in 1:length(jdd8$archetype)){
        if(tolower(apool[i]) %in% tolower(jdd20$name)){
          if (str_detect(jdd8$archetype[j], arch_commitment$arch[k])){
            arch_commitment$commitment[k] = arch_commitment$commitment[k] + jdd8$count[j]
          }
        }
      }
    }
    
  }
  
  jdd9<-jdd20%>%
    group_by(archetype)%>%
    summarise(count = n())
  arch_commitment$count = rep(0, length(arch_commitment$arch))
  for (k in 1:length(arch_commitment$arch)){
    for (i in 1:length(jdd9$archetype)){
      if(str_detect(jdd9$archetype[i], arch_commitment$arch[k])){
        arch_commitment$count[k] = arch_commitment$count[k] + jdd9$count[i]
      }
    }
    arch_commitment$commitment[k] = 500*arch_commitment$commitment[k]/arch_commitment$count[k]
  }
  arch_commitment<-arch_commitment%>%
    select(-count)
  return(arch_commitment)
}


makeColorVals <- function(pack, pool){
  currentcolors = createcc(pool, jdd7)
  colorVector = rep(0, length(pack))
  if(length(pack) !=0 & length(pool) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(jdd7$name)){
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
  return(colorVector)
}


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


makePowerVals<- function(pack){
  powervals <- rep(0, length(pack))
  if(length(pack) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(jdd7$name)){
        jdd202<-jdd7%>%
          filter(tolower(name) == tolower(pack[i]))
        powervals[i]<-jdd202$power[1]/2
      }
    }
  }
  return(powervals)
}


add_evs<-function(df){
  if(length(df$Name)>0){
    df$ev = rep(0, length(df$Name))
    for(i in 1:length(df$ev)){
      df$ev[i]<-df$Power[i] + df$Archetype[i] + df$Sphere[i] + df$Color[i]
    }
  }
  return(df)
}

add_land_evs<-function(df){
  if(length(df$Name)>0){
    df$ev = rep(0, length(df$Name))
    for(i in 1:length(df$ev)){
      df$ev[i]<- df$Archetype[i] + df$Sphere[i] + df$Color[i]
    }
  }
  return(df)
}

# Run the application 
shinyApp(ui = ui, server = server)

