#!/usr/bin/env Rscript

#begin here
#
#Make cube cards data

library(tidytext)
library(tidyverse)
library(knitr)
library(ggplot2)
library(reshape2)
library(stringr)
library(data.table)
library(shiny)
library(rjson)
library(lubridate)
library(data.table)

#load data
setwd("/Users/conorhimstead/Desktop/xcd")

cube_cards<-read.csv('deckfile.csv')


#Clean the data

cube_cards$deckID<-cube_cards$deckID+1

cube_cards$name = str_replace_all(cube_cards$name, "  ", " // ")%>%
  str_replace_all("Sword of Dungeons // Dragons", 'Sword of Dungeons & Dragons')


#fix mismatched basics in different listings
c3<-cube_cards%>%
  filter(str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))%>%
  group_by(name, deckID, location, deckName, date)%>%
  summarise(number = sum(number))

c4<-cube_cards%>%
  filter(!str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))

c4<-unique(c4)

c5<-bind_rows(c4, c3)

c5$name <- as.character(c5$name)

c6<-c5


#from make_cards_data.R

##create the cards database

cards <- fromJSON(file = "scryfall-oracle-cards.json")

card_layout = rep("", length(cards))
for (i in 1:length(cards)){
  card_layout[i]<-cards[[i]]$layout
}



#iterating through the json file to create color vector

card_color = rep("", length(cards))


for (i in 1:length(cards)){
  if (length(cards[[i]]$colors) == 0){
    card_color[i] = 'Colorless'
  }
  else{
    for (g in 1:length(cards[[i]]$colors)){
      card_color[i]<-paste(card_color[i], cards[[i]]$colors[g])
    }
  }
  if(card_layout[i]=='transform'){
    card_color[i] = ""
    if (length(cards[[i]]$card_faces[[1]]$colors) != 0){
      for (g in 1:length(cards[[i]]$card_faces[[1]]$colors)){
        card_color[i]<-paste(card_color[i], cards[[i]]$card_faces[[1]]$colors[g])
      }
    }else{
      card_color[i] = 'Colorless'
    }
  }
}

card_color[8576]
card_color<-str_replace_all(card_color, "(?<=(^| ))B(?=( |$))", "Black")
card_color<-str_replace_all(card_color, "(?<=(^| ))R(?=( |$))", "Red")
card_color<-str_replace_all(card_color, "(?<=(^| ))W(?=( |$))", "White")
card_color<-str_replace_all(card_color, "(?<=(^| ))G(?=( |$))", "Green")
card_color<-str_replace_all(card_color, "(?<=(^| ))U(?=( |$))", "Blue")

card_color<-str_replace_all(card_color, "^ | $", "")
card_color[10:20]

#iterating through the json cards list to create names vector

card_name = rep("", length(cards))

for (i in 1:length(cards)){
  card_name[i] = cards[[i]]$name
}

card_name[10:20]




#iterating through the json cards list to create power/toughness vector


card_power = rep(0, length(cards))
card_toughness = rep(0, length(cards))
for (i in 1:length(cards)){
  if (is.null(cards[[i]]$toughness)){
    card_toughness[i] = ''
  }
  else{
    card_toughness[i] = cards[[i]]$toughness
  }
  if (is.null(cards[[i]]$power)){
    card_power[i] = ''
  }
  else{
    card_power[i] = cards[[i]]$power
  }
  if(card_layout[i]=='transform'){
    if (is.null(cards[[i]]$card_face[[1]]$toughness)){
      card_toughness[i] = ''
    }
    else{
      card_toughness[i] = cards[[i]]$card_face[[1]]$toughness
    }
    if (is.null(cards[[i]]$card_face[[1]]$power)){
      card_power[i] = ''
    }
    else{
      card_power[i] = cards[[i]]$card_face[[1]]$power
    }
  }
}


card_power[10:20]
card_toughness[10:20]


#card text

card_text = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$oracle_text)){
    card_text[i] = ''
  }
  else{
    card_text[i] = cards[[i]]$oracle_text
  }
  if(card_layout[i]%in%c('transform', 'split', 'adventure', 'flip')){
    card_text[i] = paste(cards[[i]]$card_face[[1]]$oracle_text, cards[[i]]$card_face[[2]]$oracle_text)
  }
}


#card_text <- str_replace_all(card_text, "\\\n", " ")
#card_text <- str_replace_all(card_text, '\\', "")
card_text[1:25]


#type
card_type = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$type)){
    card_type[i] = ''
  }else if(card_layout[i]=='transform'||card_layout[i]=='adventure'){
    card_type[i] = cards[[i]]$card_face[[1]]$type
  }else{
    card_type[i] = cards[[i]]$type
  }
}

card_type <- str_replace_all(card_type, " — (.*)", "")
card_type <- str_replace_all(card_type, "//", "")
card_type <- str_replace_all(card_type, "Instant  Instant", "Instant")
card_type <- str_replace_all(card_type, "Sorcery  Sorcery", "Sorcery")
card_type[2800:3000]

#subtypes
card_types = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$type)||!str_detect(cards[[i]]$type, "—")){
    card_types[i] = ''
  }else if(card_layout[i]%in%c('transform', 'adventure', 'modal_dfc')){
    card_types[i] = cards[[i]]$card_face[[1]]$type
  }
  else{
    card_types[i] = cards[[i]]$type
    
  }
}

card_subtypes <- str_replace_all(card_types, "(.*)— ", "")
card_subtypes[2800:3000]



#cmc

card_cmc = rep(0, length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$cmc)){
    card_cmc[i] = NA
  }
  else{
    card_cmc[i]<-cards[[i]]$cmc
  }
}


#mana cost
card_manaCost = c('', length(cards))
for (i in 1:length(cards)){
  if (is.null(cards[[i]]$mana_cost)){
    card_manaCost[i] = ''
  }
  else{
    card_manaCost[i] = cards[[i]]$mana_cost
  }
  if(card_layout[i]=='transform'){
    card_manaCost[i] = cards[[i]]$card_faces[[1]]$mana_cost
  }
}

card_manaCost[8576]


#put all the attributes together
crads <- data.frame('name' = card_name, 
                    'color' = card_color, 
                    'cmc' = card_cmc,
                    'manaCost' = card_manaCost,
                    "power" = card_power, 
                    'toughness' = card_toughness,
                    'text' = card_text,
                    'types' = card_type,
                    'subtypes' = card_subtypes,
                    'layout' = card_layout,
                    'id' = c(1:length(cards)))

#remove cards that do not matter
omit_list = c('token', 'double_faced_token', 'scheme', 'vanguard', 'augment', 'planar', 'host', 'art_series', 'emblem')
#make double face and split cards work, split crads first
crads<-crads%>%
  filter(!layout%in%omit_list)

#fix weird cards
crads<-crads%>%
  mutate(name = ifelse(layout%in%c('transform', 'flip', 'adventure', 'modal_dfc'), str_match(name, ".*(?= //)"), str_match(name, ".*")))


crads2<-crads

#deal with hybrids, phyrexian mana
for (i in 1:length(crads2$color)){
  if(str_detect(crads2$manaCost[i], "\\{(.*)[^ ]/[^ ](.*)\\}")){
    crads2$color[i] = "Colorless"
  }
}

crads2<-crads2%>%
  select(-id, -layout)

write_csv(crads2, "crads2.csv")

#continue to join_deck.R, joining in the the scryfall data
#and creating the deck-level data

crads2<-read_csv("crads2.csv")

joined_cards<-left_join(c6, crads2, by = c('name' = 'name'))

joined_cards<-joined_cards%>%
  mutate(date = mdy(date))%>%
  arrange(deckID)

#fix any mistaken deckIDs
didc<-unique(joined_cards$deckID)
dids<-c(1:length(didc))
diddf<-data.frame(didc, dids)
joined_cards<-joined_cards%>%
  left_join(diddf, by = c('deckID' ='didc'))%>%
  mutate(deckID = dids)%>%
  select(-dids)


#create deck color column
dck_by_color = data.frame("deckID" = c(), 'color' = c(), "count" = c())
for (i in 1:length(unique(joined_cards$deckID))){
  cd<-joined_cards%>%
    filter(deckID == i,
           !str_detect(types, "Land"),
           !str_detect(color, "Colorless"),
           location == "MB",
           cmc<7)%>%
    group_by(deckID, color)%>%
    summarise(count = n())%>%
    mutate('red' = str_detect(color, "Red"),
           'blue' = str_detect(color, "Blue"),
           'green' = str_detect(color, "Green"),
           'black' = str_detect(color, "Black"),
           'white' = str_detect(color, "White"))%>%
    group_by(deckID)%>%
    summarise('red' = sum(red*count),
              'green' = sum(green*count),
              'blue' = sum(blue*count),
              'black' = sum(black*count),
              'white' = sum(white*count))%>%
    gather(key = "color", value = "count", 2:6)
  
  dck_by_color<-bind_rows(dck_by_color, cd)
}


#create deck color, split into splashes and base colors
deck_colors = rep("", length(unique(joined_cards$deckID)))
deck_splashes = rep("", length(unique(joined_cards$deckID)))
deck_baseColors = rep("", length(unique(joined_cards$deckID)))
clrs = c("red", "green", "blue", "black", "white")
for (i in 1:length(deck_colors)){
  print(i)
  for (k in 1:length(clrs)){
    num <-dck_by_color%>%
      filter(deckID == i,
             color == clrs[k])%>%
      select(count)
    print(num)
    num1<-num[1,1]
    if(num1 > 0){
      deck_colors[i]<-paste(deck_colors[i], clrs[k])
      if(num1>4){
        deck_baseColors[i] = paste(deck_baseColors[i], clrs[k])
      }
      else{
        deck_splashes[i] = paste(deck_splashes[i], clrs[k])
      }
    }
  }
}


#create decks dataframe
decks<-data.frame("deckID" = unique(joined_cards$deckID), 
                  "colors" = deck_colors, 
                  'baseColors' = deck_baseColors,
                  'splashes' = deck_splashes)





#create joined decks, cards table
jdd_new<-left_join(joined_cards, decks, by = c("deckID" = "deckID"))

jdd10<-jdd_new%>%
  mutate(sphere = "",
         archetype = "")

#add spheres
jdd10$sphere<-rep("", length(jdd10$sphere))
for (i in 1:length(jdd10$sphere)){
  if(str_detect(tolower(jdd10$deckName[i]), "combo")){
    jdd10$sphere[i] <- paste(jdd10$sphere[i], "combo ")
  }
  if(str_detect(tolower(jdd10$deckName[i]), "midrange")){
    jdd10$sphere[i] <- paste(jdd10$sphere[i], "midrange ")
  }
  if(str_detect(tolower(jdd10$deckName[i]), "aggro")){
    jdd10$sphere[i] <- paste(jdd10$sphere[i], "aggro ")
  }
  if(str_detect(tolower(jdd10$deckName[i]), "control")){
    jdd10$sphere[i] <- paste(jdd10$sphere[i], "control ")
  }
  if(str_detect(tolower(jdd10$deckName[i]), "ramp")){
    jdd10$sphere[i] <- paste(jdd10$sphere[i], "ramp ")
  }
  if(str_detect(tolower(jdd10$deckName[i]), "tempo")){
    jdd10$sphere[i] <- paste(jdd10$sphere[i], "aggro control")
  }
}

for (i in 1:length(jdd10$sphere)){
  if(jdd10$deckName[i] == 'Mono Black Devotion'){
    jdd10$sphere[i] <- "Midrange"
  }
}

#create archetype column
jdd10$archetype<-rep("", length(jdd10$archetype))
for (i in 1:length(jdd10$archetype)){
  if (str_detect(tolower(jdd10$deckName[i]), "kiki")|str_detect(tolower(jdd10$deckName[i]), "twin")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "twin ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "wildfire")|
      str_detect(tolower(jdd10$deckName[i]), "burning")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "wildfire ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "opposition")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "opposition ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "reanimator")|
      str_detect(tolower(jdd10$deckName[i]), "nightmare")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "reanimator ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "cheat")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "cheat ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "artifact")|
      str_detect(tolower(jdd10$deckName[i]), "tinker")|
      str_detect(tolower(jdd10$deckName[i]), "antiquities")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "artifacts ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "storm")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "storm ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "stax") |
      str_detect(tolower(jdd10$deckName[i]), "smokestack") |
      str_detect(tolower(jdd10$deckName[i]), "braids")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "stax ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "burn")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "burn ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "stoneforge")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "stoneforge ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "armageddon")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "armageddon ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "skullclamp")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "skullclamp ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "counterburn")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "counterburn ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "pox")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "pox ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "mono white")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "monowhite ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "mono red")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "monored ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "natural")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "naturalorder ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "edric")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "edric ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "reveillark")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "reveillark ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "spells matter")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "spellsmatter ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "pod")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "pod ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "oath")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "oath ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "5 color")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "fivecolor ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "balance")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "balance ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "moat")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "moat ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "upheaval")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "upheaval ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "mentor")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "mentor ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "abyss")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "abyss ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "crucible")|
      str_detect(tolower(jdd10$deckName[i]), "excavator")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "crucible ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "superfriends")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "superfriends ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "survival")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "survival ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "blink")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "blink ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "tokens")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "tokens ")
  }
  if (str_detect(tolower(jdd10$deckName[i]), "channel")){
    jdd10$archetype[i] <- paste(jdd10$archetype[i], "channel ")
  }
}

#give a name to no archetype, splashes
for(i in 1:length(jdd10$archetype)){
  if(jdd10$archetype[i] == ''){
    jdd10$archetype[i]<-'-'
  }
}

jdd10$splashes <- as.character(jdd10$splashes)
for(i in 1:length(jdd10$splashes)){
  if(jdd10$splashes[i] == ""){
    jdd10$splashes[i] <- '-'
  }
}

#add in cube types from deck names
jdd10 <- jdd10%>%
  mutate(cubeType = case_when(
    !str_detect(deckName, '\\[') ~ "Vintage Unpowered",
    str_detect(deckName, '\\[WOTC Vintage\\]') ~ "WOTC Vintage",
    str_detect(deckName, '\\[WOTC VIntage\\]') ~ "WOTC Vintage",
    str_detect(deckName, '\\[Vintage\\]') ~  "Vintage Powered",
    str_detect(deckName, '\\[Vintage Powered\\]') ~  "Vintage Powered",
    str_detect(deckName, '\\[Set\\]') ~  "Set Cube",
    str_detect(deckName, '\\[Arena\\]') ~  "Arena (Historic) Cube",
    str_detect(deckName, '\\[WOTC Arena\\]') ~  "WOTC Arena (Historic) Cube",
    str_detect(deckName, '\\[Legacy\\]') ~  "Legacy",
    str_detect(deckName, '\\[WOTC Legacy\\]') ~  "WOTC Legacy",
    str_detect(deckName, '\\[WOTC Modern\\]') ~  "WOTC Modern",
    str_detect(deckName, '\\[Modern\\]') ~  "Modern",
    str_detect(deckName, '\\[Pioneer\\]') ~  "Pioneer",
    str_detect(deckName, '\\[WOTC Pioneer\\]') ~  "WOTC Pioneer",
    str_detect(deckName, '\\[WOTC Standard\\]') ~  "WOTC Standard",
    str_detect(deckName, '\\[Standard\\]') ~  "Standard",
    str_detect(deckName, '\\[Peasant\\]') ~  "Peasant",
    str_detect(deckName, '\\[Pauper\\]') ~  "Pauper",
    str_detect(deckName, '\\[WOTC Peasant\\]') ~  "WOTC Peasant",
    str_detect(deckName, '\\[WOTC Pauper\\]') ~  "WOTC Pauper"
  ))


setwd("/Users/conorhimstead/Desktop/xcd/app")
write_csv(jdd10, "jdd_newest.csv")


jdd1<-read_csv("jdd_newest.csv")


#add card pip totals
jdd2<-jdd1%>%
  filter(!str_detect(jdd1$types, "Land"))%>%
  group_by(name, color, cmc, manaCost, types)%>%
  summarise(count = n())%>%
  mutate(pips = str_count(manaCost, "\\{(B|G|W|R|U)\\}"))


#fix cmc pip totals for strange cards
for (i in 1:length(jdd2$pips)){
  if(is.na(jdd2$pips[i])){
    jdd2$pips[i] <- '-'
  }
  if(jdd2$cmc[i]>7){
    jdd2$pips[i] <- 'ramp/cheat'
    if(jdd2$name[i] == "Craterhoof Behemoth"){
      jdd2$pips[i] <- 3
    }
    if(jdd2$name[i] == "Greater Gargadon"){
      jdd2$pips[i] <- 1
      jdd2$cmc[i] <- 1
    }
    if(jdd2$name[i] == "Woodfall Primus"){
      jdd2$pips[i] <- 3
    }
    if(jdd2$name[i] == "Nicol Bolas, Planeswalker"){
      jdd2$pips[i] <- 4
    }
  }
  if(jdd2$name[i] == "Treasure Cruise"){
    jdd2$pips[i] = 1
    jdd2$cmc[i] = 4
  }
  if(jdd2$name[i] == "Tombstalker"){
    jdd2$pips[i] = 2
    jdd2$cmc[i] = 4
  }
  if(jdd2$name[i] == "Dig Through Time"){
    jdd2$pips[i] = 2
    jdd2$cmc[i] = 4
  }
  if(jdd2$name[i] == "Ancestral Vision"){
    jdd2$pips[i] = 1
    jdd2$cmc[i] = 1
  }
  if(jdd2$name[i] == "Lotus Bloom"){
    jdd2$pips[i] = 0
    jdd2$cmc[i] = 0
  }
}

jdd3<-jdd2

#more card info
jdd3$colorType <-rep("", length(jdd3$name))
for (i in 1:length(jdd3$name)){
  if(jdd3$color[i] == "Colorless"){
    jdd3$colorType[i]  <- "Colorless"
  }else if(str_detect(jdd3$color[i], " ")){
    jdd3$colorType[i]  <- "Multicolored"
  }else{
    jdd3$colorType[i] <- "Monocolored"
  }
}

#how cards compare to others within their categories
jdd3$colordiff<-rep("", length(jdd3$name))
jdd3$cmcdiff<-rep("", length(jdd3$name))
jdd3$pipdiff<-rep("", length(jdd3$name))
jdd3$colortypediff<-rep("", length(jdd3$name))
for (i in 1:length(jdd3$colorType)){
  if(jdd3$colorType[i] == "Monocolored"|jdd3$colorType[i] == "Colorless"){
    jdd4<-jdd3%>%
      filter(color == jdd3$color[i])
    jdd3$colordiff[i]<-jdd3$count[i] - mean(jdd4$count)
    jdd3$colordiff[i] <- as.numeric(jdd3$colordiff[i])/sd(jdd4$count)
  }
  if(jdd3$colorType[i] == "Multicolored"){
    jdd4<-jdd3%>%
      filter(str_detect(color, ' '))
    jdd3$colordiff[i]<-jdd3$count[i] - mean(jdd4$count)
    jdd3$colordiff[i] <- as.numeric(jdd3$colordiff[i])/sd(jdd4$count)
  }
  print(i)
}


for (i in 1:length(jdd3$colorType)){
  jdd4<-jdd3%>%
    filter(colorType == jdd3$colorType[i])
  jdd3$colortypediff[i] <- as.numeric(jdd3$count[i] - mean(jdd4$count))/sd(jdd4$count)
  print(i)
}

for (i in 1:length(jdd3$cmc)){
  if(jdd3$cmc[i]<8){
    jdd4<-jdd3%>%
      filter(cmc == jdd3$cmc[i])
    jdd3$cmcdiff[i] <- as.numeric(jdd3$count[i] - mean(jdd4$count))/sd(jdd4$count)
  }else{
    jdd4<-jdd3%>%
      filter(cmc > 8)
    jdd3$cmcdiff[i] <- as.numeric(jdd3$count[i] - mean(jdd4$count))/sd(jdd4$count)
  }
  print(i)
}

for (i in 1:length(jdd3$pips)){
  jdd4<-jdd3%>%
    filter(pips == jdd3$pips[i])
  jdd3$pipdiff[i] <- as.numeric(jdd3$count[i] - mean(jdd4$count))/sd(jdd4$count)
  print(i)
}



jdd3<-jdd3%>%
  mutate(power = sum(as.numeric(pipdiff), 
                     as.numeric(colordiff), 
                     as.numeric(cmcdiff), 
                     as.numeric(colortypediff)))

#account for heavier commitments to 1 drops that must be played on curve
jdd6<-jdd3%>%
  mutate(colorCommitment = ifelse(str_detect(types, "Creature") | str_detect(color, "Black"), (power + 5) * as.numeric(pips)/cmc, 
                                  (power + 5) * as.numeric(pips)/(cmc + 2)))

#calculate new color commitments
jdd7<-jdd6%>%
  mutate(newcc = ifelse(is.na(max(log(colorCommitment), 0))|is.nan(max(log(colorCommitment), 0)), 
                        0,
                        max(log(colorCommitment), 0)))%>%
  select(-colorCommitment)



for(i in 1:length(jdd7$newcc)){
  if(is.na(jdd7$newcc[i])){
    jdd7$newcc[i] <- 0
  }
  if(jdd7$name[i] == "Woodfall Primus"){
    jdd7$newcc[i] <- 0
  }
  if(str_detect(jdd7$color[i], ' ')){
    jdd7$newcc[i] = jdd7$newcc[i]/2
  }
}



write_csv(jdd7, "jdd7.csv")

#lands
jdd1<-read_csv("jdd_newest.csv")


jdd2<-jdd1%>%
  filter(str_detect(jdd1$types, "Land"),
         !(name %in% c("Mountain", "Forest", "Plains", "Swamp", "Island")))%>%
  group_by(name, text)%>%
  summarise(count = n())%>%
  mutate(landColor = "")

for (i in 1:length(jdd2$landColor)){
  if(!is.na(jdd2$text[i])){
    if(str_detect(jdd2$text[i], "any color")){
      jdd2$landColor[i] <- "White Blue Black Red Green"
    }
    else{
      if(str_detect(jdd2$text[i], "\\{W\\}")|
         str_detect(jdd2$text[i], "Plains")){
        jdd2$landColor[i]<- "White "
      }
      if(str_detect(jdd2$text[i], "\\{U\\}")|
         str_detect(jdd2$text[i], "Island")){
        jdd2$landColor[i]<- paste(jdd2$landColor[i], "Blue ", sep = "")
      }
      if(str_detect(jdd2$text[i], "\\{B\\}")|
         str_detect(jdd2$text[i], "Swamp")){
        jdd2$landColor[i]<- paste(jdd2$landColor[i], "Black ", sep = "")
      }
      if(str_detect(jdd2$text[i], "\\{R\\}")|
         str_detect(jdd2$text[i], "Mountain")){
        jdd2$landColor[i]<- paste(jdd2$landColor[i], "Red ", sep = "")
      }
      if(str_detect(jdd2$text[i], "\\{G\\}")|
         str_detect(jdd2$text[i], "Forest")){
        jdd2$landColor[i]<- paste(jdd2$landColor[i], "Green ", sep = "")
      }
    }
  }
  if(jdd2$landColor[i] == ""){
    jdd2$landColor[i] <- "White Blue Black Red Green"
  }
}

jdd2<-jdd2%>%
  mutate(numColors = str_count(landColor, ' '))
jdd11<-jdd2
jdd11<-jdd11%>%
  group_by(numColors)%>%
  summarise(avg = mean(count),
            stdev = sd(count))


jdd2$power = rep(0, length(jdd2$name))

for (i in 1:length(jdd2$power)){
  jdd2$power[i] = as.numeric((jdd2$count[i] - jdd11[ifelse(jdd2$numColors[i] == 4, 3, jdd2$numColors[i]), 2])/jdd11[ifelse(jdd2$numColors[i] == 4, 3, jdd2$numColors[i]), 3])
  
  jdd2$power[i] = as.numeric(ifelse(jdd2$numColors[i] == 1, (as.numeric(jdd2$power[i]) + 0.2) * 1/0.7, jdd2$power[i]))
  
  jdd2$power[i] = as.numeric(ifelse(jdd2$numColors[i] == 2, (as.numeric(jdd2$power[i]) + 0.25) * 1/0.6, jdd2$power[i]))
  
  jdd2$power[i] = max(as.numeric(jdd2$power[i]), 0)
  
}


write_csv(jdd2, "jdd_lands.csv")


#make arch table
jdd1<-read_csv("jdd_newest.csv")
jdd7<-read_csv("jdd7.csv")
jdd20<-read_csv("jdd_newest.csv")

createarch<-function(apool, jdd20){
  arch_commitment = data_frame("arch" = arch_vector,
                               "commitment" = rep(0, length(arch_vector)))
  if(length(apool) == 0){
    return(arch_commitment)
  }
  
  for (i in 1:length(apool)){
    jdd8<-jdd20%>%
      filter(tolower(name) == tolower(apool[i]),
             location == "MB")%>%
      group_by(archetype)%>%
      summarise(count = n())
    if(length(jdd8$archetype != 0)){
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


arch_vector <- c("twin", "wildfire", "opposition", "reanimator", "cheat", "artifacts", "storm", "stax",
                 "burn", "stoneforge", "armageddon", "skullclamp", "counterburn", "pox", "monowhite",
                 "monored", "naturalorder", "edric", "reveillark", "spellsmatter", "pod", "oath",
                 "fivecolor", "balance", "moat", "upheaval", "mentor", "abyss", "crucible", "superfriends",
                 "survival", "blink", "tokens", "channel")

arch_c = tibble("name" = unique(jdd1$name))

for (i in 1:length(arch_vector)){
  eval(parse(text = paste("arch_c$", arch_vector[i], "= rep(0, length(arch_c$name))", sep = "")))
}

length(arch_c$name)
for (i in 1:length(arch_c$name)){
  print(arch_c$name[i])
  outtable<-createarch(arch_c$name[i], jdd20)
  print(i)
  for (j in 1:(length(outtable$arch))){
    arch_c[i, j+1] = outtable[j, 2]
  }
}


arch_c<-arch_c%>%
  filter(!(name %in% c("Mountain", "Island", "Plains", "Forest", "Swamp")))

write_csv(arch_c, "arch_c.csv")


arch_c<-read_csv("arch_c.csv")


#create sb% numbers
sb <- jdd10%>%
  filter(location == "SB")

jdd_sb <- jdd10%>%
  filter(deckID %in% sb$deckID)%>%
  mutate(isSB = ifelse(location == "SB", 1, 0))%>%
  group_by(name)%>%
  summarise(`sb%` = as.character(round(100*sum(isSB)/n(), digits = 1)))

nonsb<-jdd10%>%
  filter(!name %in% jdd_sb$name)%>%
  group_by(name)%>%
  summarise(`sb%` = 'Low Data')

jdd_sb<-rbind(jdd_sb, nonsb)

colnames(jdd_sb)<-c("Name", "sb%")
write_csv(jdd_sb, "jdd_sb.csv")




#create both time trend .csv's
jdd1<-read_csv("jdd_newest.csv")
jdd7<-read_csv("jdd7.csv")
jdd20<-jdd1
arch_c<-read_csv("arch_c.csv")
jdd_lands<-read_csv("jdd_lands.csv")
jdd_sb<-read_csv("jdd_sb.csv")
color_vector <- c("white", "blue", "black", "red", "green")
none_color_vector <- c("white", "blue", "black", "red", "green", "-")
type_vector <- c("Artifact", "Creature", "Enchantment", "Instant", "Land", "Planeswalker", "Sorcery")
sphere_vector <- c('midrange', 'ramp', 'control', 'combo', 'aggro')
arch_vector <- c("twin", "wildfire", "opposition", "reanimator", "cheat", "artifacts", "storm", "stax",
                 "burn", "stoneforge", "armageddon", "skullclamp", "counterburn", "pox", "monowhite",
                 "monored", "naturalorder", "edric", "reveillark", "spellsmatter", "pod", "oath",
                 "fivecolor", "balance", "moat", "upheaval", "mentor", "abyss", "crucible", "superfriends",
                 "survival", "blink", "tokens", "channel")



decks<-jdd1%>%
  group_by(deckID, colors, baseColors, splashes, sphere, archetype, deckName, date, cubeType)%>%
  summarise(count = n())%>%
  select(-count)

decks %>% 
  write_csv("decks.csv")

#get into format: sphere, date, decks_to_date, 
sptb<-jdd1%>%
  mutate(aggro = str_detect(sphere, "aggro"),
         control = str_detect(sphere, "control"),
         midrange = str_detect(sphere, "midrange"),
         combo = str_detect(sphere, "combo"),
         ramp = str_detect(sphere, "ramp"))

sptb1<-sptb%>%
  group_by(deckID, date, aggro, control, midrange, combo, ramp)%>%
  summarise(count = n())%>%
  select(-count)

sptb2<-sptb1%>%
  gather("sphere", "isSphere", aggro, control, midrange, ramp, combo)%>%
  filter(isSphere)%>%
  select(-isSphere)%>%
  arrange(date)

dates<-unique(sptb2$date)
ramp<-rep(0, length(dates))
aggro<-rep(0, length(dates))
midrange<-rep(0, length(dates))
combo<-rep(0, length(dates))
control<-rep(0, length(dates))

for(i in 1:length(dates)){
  aggro[i] = length(filter(sptb2, sphere == "aggro", date<=dates[i] & date>=dates[i]-365)$deckID)
  ramp[i] = length(filter(sptb2, sphere == "ramp", date<=dates[i] & date>=dates[i]-365)$deckID)
  midrange[i] = length(filter(sptb2, sphere == "midrange", date<=dates[i] & date>=dates[i]-365)$deckID)
  combo[i] = length(filter(sptb2, sphere == "combo", date<=dates[i] & date>=dates[i]-365)$deckID)
  control[i] = length(filter(sptb2, sphere == "control", date<=dates[i] & date>=dates[i]-365)$deckID)
  print(i)
}

sptb4<-data.frame(dates, aggro, midrange, control, ramp, combo)

v <- decks$date
retvec = rep(0, length(decks$date))

for (i in 1:length(decks$date)) {
  retvec[i] = length(decks$date[decks$date<=v[i] & decks$date>=v[i]-365])
}

decks2<-decks

decks2$numbefore = retvec

decks2<-decks2%>%
  group_by(date, numbefore)%>%
  summarise(count = n())%>%
  select(-count)


sptb5<-sptb4%>%
  gather(key = "sphere", value = "decksBefore", aggro, control, midrange, combo, ramp)%>%
  rename(date = dates)%>%
  left_join(decks2, by = "date")%>%
  mutate(pct_of_decks = 100*decksBefore/numbefore)

write_csv(sptb5, "sphere_time.csv")





#color popularity
ctb<-jdd1%>%
  mutate(red = str_detect(colors, "red"),
         blue = str_detect(colors, "blue"),
         green = str_detect(colors, "green"),
         black = str_detect(colors, "black"),
         white = str_detect(colors, "white"))

ctb1<-ctb%>%
  group_by(deckID, date, red, blue, green, black, white)%>%
  summarise(count = n())%>%
  select(-count)

ctb2<-ctb1%>%
  gather("color", "isColor", red, blue, white, black, green)%>%
  filter(isColor)%>%
  select(-isColor)%>%
  arrange(date)

ctb3<- ctb1%>%
  gather("col", "isColor", red, blue, white, black, green)%>%
  group_by(col, date)%>%
  summarise(count = n())%>%
  select(-count)%>%
  rename(deckDate = date)


dates<-unique(ctb3$deckDate)
red<-rep(0, length(dates))
blue<-rep(0, length(dates))
green<-rep(0, length(dates))
black<-rep(0, length(dates))
white<-rep(0, length(dates))

for(i in 1:length(dates)){
  red[i] = length(filter(ctb2, color == "red", date<=dates[i] & date>=dates[i]-365)$deckID)
  blue[i] = length(filter(ctb2, color == "blue", date<=dates[i] & date>=dates[i]-365)$deckID)
  green[i] = length(filter(ctb2, color == "green", date<=dates[i] & date>=dates[i]-365)$deckID)
  black[i] = length(filter(ctb2, color == "black", date<=dates[i] & date>=dates[i]-365)$deckID)
  white[i] = length(filter(ctb2, color == "white", date<=dates[i] & date>=dates[i]-365)$deckID)
  print(i)
}

ctb4<-data.frame(dates, red, blue, green, black, white)

ctb5<-ctb4%>%
  gather(key = "color", value = "decksBefore", red, blue, black, white, green)%>%
  rename(date = dates)%>%
  left_join(decks2, by = "date")%>%
  mutate(pct_of_decks = 100*decksBefore/numbefore)

write_csv(ctb5, "color_time.csv")




