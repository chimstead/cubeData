getwd()
setwd("/Users/conorhimstead/Desktop/math216/finalProject/data")
cube_cards<-read_csv('deckfile.csv')
library(rjson)

install.packages('taRifx')

cards <- fromJSON(file = "AllCards.json")

k<-cube_cards%>%
  select(deckID, number, name, location)

cube_cards<-k%>%
  select(deckID, number, name, location)

c2<-cube_cards%>%
  filter(str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'), location == 'MB')%>%
  group_by(name)%>%
  summarize(avg_count = mean(number))


c2
strVector = c('')

#iterating through the json file to create color vector

card_color = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$colors)){
    card_color[i] = 'Colorless'
  }
  else{
    for (g in 1:length(cards[[i]]$colors)){
      card_color[i]<-paste(card_color[i], cards[[i]]$colors[g])
    }
  }
}

card_color[2800:3000]

#iterating through the json cards list to create names vector

card_name = rep("", length(cards))

for (i in 1:length(cards)){
    card_name[i] = cards[[i]]$name
}

card_name[2800:3000]

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
}


card_power[2800:3000]

#card text


card_text = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$text)){
    card_text[i] = ''
  }
  else{
    card_text[i] = cards[[i]]$text
  }
}

card_text[2800:3000]


#type
card_type = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$type)){
    card_type[i] = ''
  }
  else{
    card_type[i] = cards[[i]]$type
  }
}


card_type[2800:3000]

#types
card_types = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$types)){
    card_types[i] = ''
  }
  else{
    for (g in 1:length(cards[[i]]$types)){
      card_types[i]<-paste(card_types[i], cards[[i]]$types[g])
    }
  }
}

card_types[2800:3000]

cards[[500]]$subtypes

#subtypes
card_subtypes = rep("", length(cards))

for (i in 1:length(cards)){
  if (is.null(cards[[i]]$subtypes)){
    card_subtypes[i] = ''
  }
  else{
    for (g in 1:length(cards[[i]]$subtypes)){
      card_subtypes[i]<-paste(card_subtypes[i], cards[[i]]$subtypes[g])
    }
  }
}


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
  if (is.null(cards[[i]]$manaCost)){
    card_manaCost[i] = ''
  }
  else{
    card_manaCost[i] = cards[[i]]$manaCost
  }
}

cards[[7198]]


crads <- data.frame('name' = card_name, 
                    'color' = card_color, 
                    'cmc' = card_cmc,
                    'manaCost' = card_manaCost,
                    "power" = card_power, 
                    'toughness' = card_toughness,
                    'text' = card_text,
                    'types' = card_types,
                    'subtypes' = card_subtypes)

jc_mistakes<-joined_cards%>%
  filter(is.na(color))
names_to_fix<-unique(jc_mistakes$name)
names_to_fix
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
                                 "Hallowed Foundtain" = "Hallowed Fountain"
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
  group_by(name, deckID, location)%>%
  summarise(number = sum(number))

c4<-cube_cards%>%
  filter(!str_detect(name, '^Forest$|^Island$|^Mountain$|^Swamp$|^Plains$'))

c5<-bind_rows(c4, c3)

c5$name <- as.character(c5$name)
c5$name[1282] <- "Never"
c5$name[1549] <- "Never"
c5$name[1939] <- "Never"
c5$name[2410] <- "Never"
c5$name[6847] <- "Never"
c5$name[2388] <- "Ice"

crads$color <- as.character(crads$color)
crads$color[1374] = "Colorless"
crads2<-crads

c5$deckID<-c5$deckID+1

for (i in 1:length(crads2$color)){
  if(str_detect(crads2$manaCost[i], "/")){
    crads2$color[i] = "Colorless"
  }
}

joined_cards<-left_join(c5, crads2, by = c('name', 'name'))
joined_cards<-joined_cards%>%
  filter(name != "for the best",
         name != "Twin Combo",
         name != "Arid Hub")

j1<-joined_cards%>%
  filter(location == "MB")

getwd()
write_csv(joined_cards, "joined3.csv")


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

j1<-joined_cards%>%
  filter(location == "MB")

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

decks<-decks[1:236,]


deck_sphere<-rep("", length(unique(joined_cards$deckID)))

for (i in 1:length(deck_sphere)){
  nms<-j1%>%
    filter(deckID == i)%>%
    select(name)
  
  if("Goblin Guide" %in% nms$name |
     "Sulfuric Vortex" %in% nms$name){
    deck_sphere[i] <- paste(deck_sphere[i], "aggro ")
  }
  if("Oath of Druids" %in% nms$name |
     "Splinter Twin" %in% nms$name |
     "Kiki-Jiki, Mirror Breaker" %in% nms$name |
     "Tendrils of Agony" %in% nms$name |
     "Brain Freeze" %in% nms$name |
     "Griselbrand" %in% nms$name){
    deck_sphere[i] <- paste(deck_sphere[i], "combo ")
  }
  if("Rofellos, Llanowar Emissary" %in% nms$name){
    deck_sphere[i] <- paste(deck_sphere[i], "ramp ")
  }
}


decks<-data.frame("deckID" = unique(joined_cards$deckID), 
                  "colors" = deck_colors, 
                  'baseColors' = deck_baseColors,
                  'splashes' = deck_splashes,
                  'sphere' = deck_sphere)
