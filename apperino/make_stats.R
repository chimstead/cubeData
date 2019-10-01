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




