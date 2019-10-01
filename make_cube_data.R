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

cube_cards$deckID<-cube_cards$deckID+1

cube_cards<-cube_cards%>%
  filter(name != "Jeskai Oath - not possible",
         name != "Mardu Wildfire Superfriends - Looks more Ramp to me",
         name != 'going to assume all "Shenanigans" decks are Midrange')

cube_cards<-cube_cards%>%
  mutate(deckID = ifelse(deckID>298, deckID-1, deckID))

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
