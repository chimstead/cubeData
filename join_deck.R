crads2<-read_csv("crads2.csv")
c5<-read_csv("c5.csv")

joined_cards<-left_join(c5, crads2, by = c('name', 'name'))


#optional if importing only a portion of the dataset
ret<-rep("", length(unique(joined_cards$deckID)))
for (i in 1:length(joined_cards$name)){
  ret[joined_cards$deckID[i]]<-paste(ret[joined_cards$deckID[i]], substring(joined_cards$name[i], 1,1), sep = "")
}

joined_cards$key = rep("", length(joined_cards$deckID))
for (i in 1:length(joined_cards$name)){
  joined_cards$key[i] = ret[joined_cards$deckID[i]]
}

jdd4<-jdd
ret<-rep("", length(unique(jdd4$deckID)))
for (i in 1:length(jdd4$name)){
  ret[jdd4$deckID[i]]<-paste(ret[jdd4$deckID[i]], substring(jdd4$name[i], 1,1), sep = "")
}


jdd4$key = rep("", length(jdd4$deckID))
for (i in 1:length(jdd4$name)){
  jdd4$key[i] = ret[jdd4$deckID[i]]
}

joined_less<-joined_cards%>%
  filter(!(key %in% jdd4$key))


joined_cards<-joined_less%>%
  arrange(desc(deckID))

did = 1
for(i in 1:(length(joined_cards$name)-1)){
  joined_cards$deckID[i] = did
  if(joined_cards$key[i+1]!=joined_cards$key[i]){
    did = did + 1
  }
}
joined_cards$deckID[2033] = 66


j1<-joined_cards%>%
  filter(location == "MB")


write_csv(joined_cards, "joined5.csv")

?paste


#begin again here
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

#ignore
sa<-read_csv("sa2.csv")

sa$archetype<-str_replace_all(sa$archetype, '"', "")

sa<-data.frame(sa, "deckID" = c(1:66))


decks<-merge(decks, sa, by = "deckID")

#return
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

#######
for (i in 1:length(jdd10$sphere)){
  if(jdd10$deckID[i] == 91){
    jdd10$sphere[i] <- "combo"
  }
  if(jdd10$deckID[i] == 92){
    jdd10$sphere[i] <- "combo"
  }
  if(jdd10$deckID[i] == 112){
    jdd10$sphere[i] <- "control"
  }
  if(jdd10$deckID[i] == 131){
    jdd10$sphere[i] <- "combo"
  }
  if(jdd10$deckID[i] == 175){
    jdd10$sphere[i] <- "combo"
  }
}
########


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

#omit
jdd4<-jdd_new2

#return
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



#omit
jdd_new$deckID<-jdd_new$deckID+rep(249, length(jdd_new$deckID))

jdd_new2<-bind_rows(jdd, jdd_new)

jdd_new2<-jdd_new2%>%
  select(-key)
jdd_<-jdd_new2

for (i in 1:length(jdd_$archetype)){
  if(str_detect(jdd_$archetype[i], regex("no$|no\\s"))){
    loc<-str_locate(jdd_$archetype[i], regex("no$|no\\s"))
    str_sub(jdd_$archetype[i], loc[1], loc[2])<-"natural_order"
  }
}

jdd_new2<-jdd_


write_csv(jdd, "jdd.csv")
write_csv(jdd_new2, "jdd_new.csv")
#return
setwd("/Users/conorhimstead/Desktop/math216/finalProject/data/apperino")
write_csv(jdd10, "jdd_newest.csv")

#continue to make_stats.R

#omit
jdd_new2<-read_csv("jdd_new.csv")

jdd_new2$number[9652]<-1
jdd10<-read_csv("jdd_newest.csv")

for (i in 1:length(jdd_new2$deckID)){
  if(jdd_new2$archetype[i] == 'none'){
    jdd_new2$archetype[i] = '-'
  }
  if(jdd_new2$baseColors[i] == 'none'){
    jdd_new2$baseColors[i] = '-'
  }
  if(jdd_new2$splashes[i] == 'none'){
    jdd_new2$splashes[i] = '-'
  }
}

