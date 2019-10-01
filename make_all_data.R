#!/usr/bin/env Rscript

#begin here
#after making .csv file

library(RCurl)
library(tidytext)
library(tidyverse)
library(knitr)
library(ggplot2)
library(reshape2)
library(leaflet)
library(stringr)
library(rvest)
library(data.table)
library(shiny)
library(stats)
library(openintro)
library(wordcloud)
library(rjson)

setwd("/Users/conorhimstead/Desktop/math216/finalProject/data")

cube_cards<-read_csv('deckfile.csv')

k<-cube_cards%>%
  select(deckID, number, name, location, deckName)

cube_cards<-k%>%
  select(deckID, number, name, location, deckName)

num = 0
for (i in 1:length(cube_cards$name)){
  if(cube_cards$name[i]=="15 Mountainpar"){
    num = cube_cards$deckID[i]
    print(cube_cards[i,])
    print(num)
  }
}

cube_cards<-cube_cards%>%
  filter(deckID!=num)

cube_cards$deckID<-cube_cards$deckID+1

cube_cards<-cube_cards%>%
  filter(name != "Jeskai Oath - not possible",
         name != "Mardu Wildfire Superfriends - Looks more Ramp to me",
         name != 'going to assume all "Shenanigans" decks are Midrange'
         )




cube_cards<-cube_cards%>%
  mutate(deckID = ifelse(deckID>num, deckID-1, deckID))

cube_cards$name <- recode_factor(cube_cards$name, "Celestial Collonade" = "Celestial Colonnade",
                                 "Assassins Trophy" = "Assassin's Trophy",
                                 "Heros Downfall" = "Hero's Downfall",
                                 "Kodamas Reach" = "Kodama's Reach",
                                 "Avacyns Pilgrim" = "Avacyns Pilgrim",
                                 "Life  Death" = "Death",
                                 "Nights Whisper" = "Night's Whisper",
                                 "Elsepth, Knight-Errant" = "Elspeth, Knight-Errant",
                                 "Geist of Saint Taft" = "Geist of Saint Traft",
                                 "Smugglers Copter" = "Smuggler's Copter",
                                 "Umezawas Jitte" = "Umezawa's Jitte",
                                 "Swords to Plowshare" = "Swords to Plowshares",
                                 "Nissa, Steward of The Elements" = "Nissa, Steward of Elements",
                                 "Elspeth, Suns Champion" = "Elspeth, Sun's Champion",
                                 "Senseis Divining Top" = "Sensei's Divining Top",
                                 "Selvalas Stampede" = "Selvala's Stampede",
                                 "Abbot of the Keral Keep" = "Abbot of Keral Keep",
                                 "Verdant Catacomb" = "Verdant Catacombs",
                                 "Devils Play" = "Devil's Play",
                                 "Gerrards Verdict" = "Gerrard's Verdict",
                                 "Kolaghans Command" = "Kolaghan's Command",
                                 "Jace, Vryns Prodigy" = "Jace, Vryn's Prodigy",
                                 "Srams Expertise" = "Sram's Expertise",
                                 "Gaeas Cradle" = "Gaea's Cradle",
                                 "Stomping Grounds" = "Stomping Ground",
                                 "Green Suns Zenith" = "Green Sun's Zenith",
                                 "Mishras Factory" = "Mishra's Factory",
                                 "Councils Judgment" = "Council's Judgment",
                                 "Shambling Vents" = "Shambling Vents",
                                 "Chat a Course" = "Chart a Course",
                                 "Oonas Prowler" = "Oonas Prowler",
                                 "Elesh Norn" = "Elesh Norn, Grand Cenobite",
                                 "Insult  Injury" = "Insult",
                                 "Legions Landing" = "Legion's Landing",
                                 "Never Return" = "Never",
                                 "Natures Claim" = "Nature's Claim",
                                 "Commit  Memory" = "Commit",
                                 "Vraskas Contempt" = "Vraskas Contempt",
                                 "Liliana, Deaths Majesty" = "Liliana, Death's Majesty",
                                 "Chainers Edict" = "Chainer's Edict",
                                 "Meloku, the Clouded Mirror" = "Meloku the Clouded Mirror",
                                 "Horne t Queen" = "Hornet Queen",
                                 "Bone shredder" = "Bone Shredder",
                                 "Braids, Cabal Minon" = "Braids, Cabal Minion",
                                 "Zealous CONSCRIPTS" = "Zealous Conscripts",
                                 "Hymn to Touch" = "Hymn to Tourach",
                                 "Leovold, Spymaster of Trest" = "Leovold, Emissary of Trest",
                                 "Lions Eye Diamond" = "Lion's Eye Diamond",
                                 "Yawgmoths Will" = "Yawgmoth's Will",
                                 "Miraris Wake" = "Mirari's Wake",
                                 "Volraths Stronghold" = "Volrath's Stronghold",
                                 "Oracle of Mul Day" = "Oracle of Mul Daya",
                                 "Sulfurous Spring" = "Sulfurous Springs",
                                 "Man-o-War" = "Man-o'-War",
                                 "Chandras Phoenix" = "Chandra's Phoenix",
                                 "Natures Lore" = "Nature's Lore",
                                 "Nevinyrrals Disk" = "Nevinyrral's Disk",
                                 "Fact of Fiction" = "Fact or Fiction",
                                 "Maze of Its" = "Maze of Ith",
                                 "Isareth, the Awakener" = "Isareth the Awakener",
                                 "Survival of Fittest" = "Survival of the Fittest",
                                 "Fetid Pools1 Karplusan Forest" = "Fetid Pools",
                                 "Nicol Bolas, God-Pharoah" = "Nicol Bolas, God-Pharaoh",
                                 "Faiths Fetters" = "Faith's Fetters",
                                 "Deck Fayden" = "Dack Fayden",
                                 "NIssa, Vastwood Seer" = "Nissa, Vastwood Seer",
                                 "Nissa. Voice of Zendikar" = "Nissa, Voice of Zendikar",
                                 "Mishras Workshop" = "Mishra's Workshop",
                                 "Wear  Tear" = "Wear",
                                 "Kinjallis Sunwing" = "Kinjalli's Sunwing",
                                 "Olivias Dragoon" = "Olivia's Dragoon",
                                 "Rakdoss Return" = "Rakdos's Return",
                                 "Dissenters Deliverance" = "Dissenter's Deliverance",
                                 "Thalias Lancers" = "Thalia's Lancers",
                                 "Elsepth, Suns Champion" = "Elsepth, Suns Champion",
                                 'Avacyns Pilgrim' = "Avacyn's Pilgrim",
                                 "Elsepth, Suns Champion" = "Elspeth, Sun's Champion",
                                 "Swords to Splowshares" = "Swords to Plowshares",
                                 "Bontus Last Reckoning" = "Bontu's Last Reckoning",
                                 "Brand Coliseum" = "Grand Coliseum",
                                 "Experiment one" = "Experiment One",
                                 "Faeire Conclave" = "Faerie Conclave",
                                 "Fire Ice" = "Ice",
                                 "Floded Strand" = "Flooded Strand",
                                 "Fracutred Identity" = "Fractured Identity",
                                 "Glen Elendra Archmarge" = "Glen Elendra Archmage",
                                 "Grand Coliseium" = "Grand Coliseum",
                                 "Hymn to Torach" = "Hymn to Tourach",
                                 "Mardue Woe-Reaper" = "Mardu Woe-Reaper",
                                 "Mirrodins Core" = "Mirrodin's Core",
                                 "Mystic Confliuence" = "Mystic Confluence",
                                 "Mystical Confluence" = "Mystic Confluence",
                                 "Oracale of Mul Daya" = "Oracle of Mul Daya",
                                 "Stoneforge mystic" = "Stoneforge Mystic",
                                 "Tajic, Legions Edge" = "Tajic, Legion's Edge",
                                 "Yaviamaya Coast" = "Yavimaya Coast",
                                 "Yawgmoths Bargain" = "Yawgmoth's Bargain",
                                 "JackaL Pup" = "Jackal Pup",
                                 "Llanowar Waste" = "Llanowar Wastes",
                                 "Wooded Foothill" = "Wooded Foothills",
                                 "Hadanas Climb" = "Hadana's Climb",
                                 "Rishkhar, Peema Renegade" = "Rishkar, Peema Renegade",
                                 "Fatihless Looting" = "Faithless Looting",
                                 "Alesha, Who Smile at Death" = "Alesha, Who Smiles at Death",
                                 "Hallowed Foundtain" = "Hallowed Fountain",
                                 "inquisition of Kozilek" = "Inquisition of Kozilek",
                                 "Bloodbraif Elf" = "Bloodbraid Elf",
                                 "Windsweapt Heath" = "Windswept Heath",
                                 "Greater Gargdon" = "Greater Gargadon",
                                 "Kozilek, the Butcher" = "Kozilek, Butcher of Truth",
                                 "Coperline Gorge" = "Copperline Gorge",
                                 "Deranged hermit" = "Deranged Hermit",
                                 "Rofellos, Llanowar Emmissary" = "Rofellos, Llanowar Emissary",
                                 "of Destiny" = "Figure of Destiny",
                                 "Stormblood Beserker" = "Stormblood Berserker",
                                 "True-name Nemesis" = "True-Name Nemesis",
                                 "Senseis Diving Top" = "Sensei's Divining Top",
                                 "Zealous Conscript" = "Zealous Conscripts",
                                 "Vendillion Clique" = "Vendilion Clique",
                                 "Mardu Woe- Reaper" = "Mardu Woe-Reaper",
                                 "Than Dynamo" = "Thran Dynamo",
                                 "Faiths Feather" = "Faith's Fetters",
                                 "Sulfuric Cortex" = "Sulfuric Vortex",
                                 "Nissa, Stewart of Elements" = "Nissa, Steward of Elements",
                                 "Wall of Omen" = "Wall of Omens",
                                 "NIssa, Voice of Zendikar" = "Nissa, Voice of Zendikar",
                                 "MIraris Wake" = "Mirari's Wake",
                                 "Elspeth, Kinght-Errant" = "Elspeth, Knight-Errant",
                                 "Gideon of Trials" = "Gideon of the Trials",
                                 "Sakura Tribe-Elder" = "Sakura-Tribe Elder"
)
cube_cards$name <- recode_factor(cube_cards$name,
                                 "Shambling Vents" = "Shambling Vent")
cube_cards$name <- recode_factor(cube_cards$name,
                                 "Never Return" = "Never")
cube_cards$name <- recode_factor(cube_cards$name,
                                 "Oonas Prowler" = "Oona's Prowler")
cube_cards$name <- recode_factor(cube_cards$name,
                                 "Avacyns Pilgrim" = "Avacyn's Pilgrim")
cube_cards$name <- recode_factor(cube_cards$name,
                                 "Nissa, Steward of the Elements" = "Nissa, Steward of Elements")
cube_cards$name <- recode_factor(cube_cards$name,
                                 "Vraskas Contempt" = "Vraska's Contempt")
cube_cards$name <- recode_factor(cube_cards$name,
                                 "Elsepth, Suns Champion" = "Elspeth, Sun's Champion")




c3<-cube_cards%>%
  filter(str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))%>%
  group_by(name, deckID, location, deckName)%>%
  summarise(number = sum(number))

c4<-cube_cards%>%
  filter(!str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))

c5<-bind_rows(c4, c3)


c5$name <- as.character(c5$name)



c6<-c5%>%
  filter(str_sub(name, 1, 1) != ',')


c6<-c6%>%
  filter(name != "aggro",
         name != "for the best",
         name != "Twin Combo",
         name != "Arid Hub",
         name != "Twin",
         name != "Storm",
         name != "Ramp",
         name != "OPPOSITION-SLAVER")


c6$name<-recode_factor(c6$name, 
                       "Aetherligng" = "Aetherling",
                       "plains" = "Plains",
                       "LifeDeath" = "Death",
                       "NeverReturn" = "Never",
                       "FireIce" = "Ice",
                       "Shreikmaw" = "Shriekmaw",
                       "Yahennis Expertise" = "Yahenni's Expertise",
                       "Pestermine" = "Pestermite",
                       "Ruin-Raider" = "Ruin Raider",
                       "Rofellos, Llanowar Emmisary" = "Rofellos, Llanowar Emissary",
                       "Carnoophage" = "Carnophage",
                       "Damnatinon" = "Damnation",
                       "Horne	t Queen" = "Hornet Queen",
                       "Man-o-war" = "Man-o'-War",
                       "Rofellos, Llanowar Emmissary" = "Rofellos, Llanowar Emissary",
                       "Winter Orb" = "Winter Orb",
                       "Hornet Queen" = "Hornet Queen",
                       "Sphinxs Revelation" = "Sphinx's Revelation",
                       "Minds Desire" = "Mind's Desire",
                       "Fire  Ice" = "Ice",
                       "Never  Return" = "Never",
                       "Arguels Blood Fast" = "Arguel's Blood Fast",
                       "Consuls Lieutenant" = "Consul's Lieutenant")


######
num = 0

for (i in 1:length(c6$name)){
  if(c6$name[i]=="13 Plains"){
    num = i
  }
}

c6[num,]
c6$number[num] = 13
c6$name[num] = "Plains"
c6[num,]


num = 0

for (z in 1:length(c6$name)){
  if(c6$name[z]=="1 Verdant Catacombs"){
    num = z
    print(c6[z,])
  }
}

c6[num,]
c6$number[num] = 1
c6$name[num] = "Verdant Catacombs"




write_csv(c6, "c5.csv")
#continue to join_deck.R

crads2<-read_csv("crads2.csv")
c5<-read_csv("c5.csv")

joined_cards<-left_join(c5, crads2, by = c('name', 'name'))

dck_by_color = data.frame("deckID" = c(), 'color' = c(), "count" = c())

for (i in 1:length(unique(joined_cards$deckID))){
  cd<-joined_cards%>%
    filter(deckID == i,
           !str_detect(types, "Land"),
           !str_detect(color, "Colorless"),
           location == "MB")%>%
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



decks<-data.frame("deckID" = unique(joined_cards$deckID), 
                  "colors" = deck_colors, 
                  'baseColors' = deck_baseColors,
                  'splashes' = deck_splashes)

powered<-rep(FALSE, length(decks$deckID))
for (i in 1:length(joined_cards$name)){
  if(joined_cards$name[i] == "Black Lotus" |
     joined_cards$name[i] == "Mox Jet" |
     joined_cards$name[i] == "Time Walk" |
     joined_cards$name[i] == "Ancestral Recall" |
     joined_cards$name[i] == "Mox Sapphire" |
     joined_cards$name[i] == "Mox Emerald" |
     joined_cards$name[i] == "Mox Ruby" |
     joined_cards$name[i] == "Mox Pearl" |
     joined_cards$name[i] == "Sol Ring" |
     joined_cards$name[i] == "Library of Alexandria" |
     joined_cards$name[i] == "Mana Crypt"){
    powered[joined_cards$deckID[i]] <- TRUE
  }
}

decks$powered <- powered



jdd_new<-left_join(joined_cards, decks, by = c("deckID", "deckID"))

jdd10<-jdd_new%>%
  mutate(sphere = "",
         archetype = "")

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
  if(jdd10$deckName[i] == 'Dimir (UB) Reanimator 2.11.18.txt'){
    jdd10$sphere[i] <- "combo"
  }
  if(jdd10$deckName[i] == 'Dimir (UB) Reanimator 2.5.18.txt'){
    jdd10$sphere[i] <- "combo"
  }
  if(jdd10$deckName[i] == 'Esper (UBW) Balance Moat Contro 6.16.18.txt'){
    jdd10$sphere[i] <- "control"
  }
  if(jdd10$deckName[i] == 'Esper (UBW) Reanimator 3.24.18.txt'){
    jdd10$sphere[i] <- "combo"
  }
  if(jdd10$deckName[i] == 'Golgari (BG) Reanimator 3.24.18.txt'){
    jdd10$sphere[i] <- "combo"
  }
}

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

setwd("/Users/conorhimstead/Desktop/math216/finalProject/data/apperino")
write_csv(jdd10, "jdd_newest.csv")

#new shit bb
#start here
jdd1<-read_csv("jdd_newest.csv")


jdd2<-jdd1%>%
  filter(!str_detect(jdd1$types, "Land"),
         !powered)%>%
  group_by(name, color, cmc, manaCost, types)%>%
  summarise(count = n())%>%
  mutate(pips = str_count(manaCost, "\\{(B|G|W|R|U)\\}"))



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

jdd6<-jdd3%>%
  mutate(colorCommitment = ifelse(str_detect(types, "Creature") | str_detect(color, "Black"), (power + 5) * as.numeric(pips)/cmc, 
                                  (power + 5) * as.numeric(pips)/(cmc + 2)))

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

arch_c = data_frame("name" = unique(jdd1$name))

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


###

jdd_sb <- jdd10%>%
  mutate(isSB = ifelse(location == "SB", 1, 0))%>%
  group_by(name)%>%
  summarise(`sb%` = round(100*sum(isSB)/n(), digits = 1))
colnames(jdd_sb)<-c("Name", "sb%")
write_csv(jdd_sb, "jdd_sb.csv")



