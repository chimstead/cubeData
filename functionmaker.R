filter_multi <- function(inp){
  jdd2<-jdd1%>%
    filter(color != "colorless")
  
  jdd2$color<-tolower(jdd2$color)
  
  #get neg color
  color_vector <- c("white", "blue", "black", "red", "green")
  neg_color = c()
  for (i in 1:length(color_vector)){
    if(!(color_vector[i] %in% inp)){
      neg_color<-c(neg_color, color_vector[i])
    }
  }
  
  
  
  
  
  #filter not including neg color
  iscol = rep(TRUE, length(jdd2$color))
  
  for(i in 1:length(jdd2$color)){
    for(k in 1:length(neg_color)){
      if(str_detect(jdd2$color[i], neg_color[k])){
        iscol[i] = FALSE
      }
    }
  }
  
  jdd2$iscol <- iscol
  
  jout<-jdd2%>%
    filter(iscol)%>%
    group_by(name)%>%
    summarise(count = n())%>%
    arrange(desc(count))
  return(jout)
}

View(filter_multi(c('red', 'white', 'blue')))
  
  


crads$text[15094]

jdd3<-jdd1
#e1
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




response<-isex(c('aggro', "control", "midrange"), jdd3)


spheres<-strsplit(jdd3$sphere[920], " ")


spheres[[920]][2]

contonly <- function(inp, jddout){
  
  #get neg sphere
  neg_sphere = c()
  for (i in 1:length(sphere_vector)){
    if(!(sphere_vector[i] %in% inp)){
      neg_sphere<-c(neg_sphere, sphere_vector[i])
    }
  }
  
  #filter not including neg color
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

response<-incl(c('combo', 'control'), jdd3)


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


q = FALSE
inp<-c("combo", "control")
for (k in 1:length(inp)){
  if(str_detect(jdd3$sphere[930], inp[k])){
    print(inp[k])
     q = TRUE
  }
}
q

jdd3$sphere[2460]

print('combo' %in% c('midrange aggro'))

jdd3$sphere <- as.character(jdd3$sphere)


unique(jdd3$archetype)


jdd3<-jdd4
acontonly <- function(inp, jddout){
  
  #get neg sphere
  neg_archetypes = c()
  for (i in 1:length(arch_vector)){
    if(!(arch_vector[i] %in% inp)){
      neg_archetypes<-c(neg_archetypes, arch_vector[i])
    }
  }
  #filter not including neg color
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


response<-acontonly(c('wildfire', 'twin'), jdd3)

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

response<-aisex(c('wildfire', 'twin'), jdd3)

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


response<-aincl(c('wildfire', 'twin'), jdd3)

?unique

tincl <- function(inp, jddout){
  istype<-rep(FALSE, length(jddout$types))
  for (i in 1:length(jddout$types)){
    for (k in 1:length(inp)){
      if(str_detect(jddout$types[i], inp[k])){
        istype[i] = TRUE
      }
    }
  }
  
  jddout$istype <- istype
  
  jddout<-jddout%>%
    filter(istype)%>%
    select(-istype)
  return(jddout)
}

response<-tincl(c('Artifact', 'Creature'), jdd3)


?conditionalPanel

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

response <- dfilter_multi(c('red', 'blue'), decks)



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

response <- dfilter_base_multi(c('red', 'blue'), decks)

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


response <- dfilter_splash_multi(c('red', 'blue'), decks)

?ifelse



sdecklist<-str_split("234 314 12 9 17 12", " ")
sdecklist[[1]][1]
not_decks <-FALSE
for(i in 1:length(sdecklist[[1]])){
  if(!(sdecklist[[1]][i] %in% stbout$deckID)){
    not_decks = TRUE
  }
}
not_decks



jdd1%>%
  filter(deckID %in% c('234'))%>%
  filter(!str_detect(types, "Land"))%>%
  group_by(cmc)%>%
  summarise(count = sum(number))%>%
  ggplot()+
  geom_col(x = cmc, y = count)

jdd1%>%
  group_by(cmc)%>%
  summarise(count = sum(number))%>%
  ggplot()+
  geom_col(x = cmc, y = count)

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

stbout<-jdd1%>%
  filter(deckID == 1)

stb14<-split_types(stbout)


stbout%>%
  filter(deckID %in% sdecklist[[1]])%>%
  group_by(types)%>%
  summarise(count = sum(number))%>%
  ggplot()+
  geom_col(aes(x = types, y = count, fill = types))+
  theme_minimal()


jdd1<-read_csv("jdd_new.csv")
jd_dx<-jdd1%>%
  group_by(deckID)%>%
  summarise(count = sum(number))%>%
  arrange(count)


#new shit bb
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


#power, archetype, sphere, color

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
}



write_csv(jdd7, "jdd7.csv")


pool = c("Rofellos, Llanowar Emissary", "Llanowar Elves", "Channel", "Fractured Identity")

color_commitment = data_frame("color" = c("White", "Blue", "Black", "Red", "Green"), "commitment" = rep(0, 5))
for (i in 1:length(pool)){
  com = 0
  clr = ""
  for (j in 1:length(jdd7$name)){
    if (jdd7$name[j] == pool[i]){
      com = jdd7$newcc[j]
      clr = jdd7$color[j]
    }
  }
  for (k in 1:length(color_commitment$color)){
    if(str_detect(clr, color_commitment$color[k])){
      color_commitment$commitment[k] = color_commitment$commitment[k] + com
    }
  }
  print(clr)
}



spool = "char| demonic tutor | shock"
cardlist = strsplit(spool, "\\|")
v<-unlist(cardlist)
v<-str_replace_all(v, "\\s", "")

createsph<-function(spool, jdd1){
  sphere_commitment = data_frame("sphere" = c("midrange", "aggro", "control", "ramp", "combo"), "commitment" = rep(0, 5))
  if(length(spool) == 0){
    return(sphere_commitment)
  }
  for (i in 1:length(spool)){
    jdd8<-jdd1%>%
      filter(name == spool[i])%>%
      group_by(sphere)%>%
      summarise(count = n())
    for (k in 1:length(sphere_commitment$sphere)){
      for (j in 1:length(jdd8$sphere)){
        if(spool[i] %in% jdd1$name){
          if (str_detect(jdd8$sphere[j], sphere_commitment$sphere[k])){
            sphere_commitment$commitment[k] = sphere_commitment$commitment[k] + jdd8$count[j]
          }
        }
      }
    }
  
  }

  jdd9<-jdd1%>%
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

createsph(c("Tarmogoyf", "Liliana of the Veil", "Thoughtseize"), jdd1)
"" %in% jdd1$name
  



jdd1<-read_csv("jdd_newest.csv")

jdd8<-jdd1%>%
  filter(tolower(name) == "avacyn's pilgrim")%>%
  group_by(sphere)%>%
  summarise(count = n())


tolower(jdd1$name[948]) == "avacyn's pilgrim"

createarch<-function(apool, jdd1){
  arch_commitment = data_frame("arch" = c('natural_order', 'twin', 'reanimator', 'opposition', 
                                            'cheat', 'wildfire', 'artifacts'),
                               "commitment" = rep(0, 7))
  if(length(apool) == 0){
    return(arch_commitment)
  }

  for (i in 1:length(apool)){
    jdd8<-jdd1%>%
      filter(tolower(name) == tolower(apool[i]),
             location == "MB")%>%
      group_by(archetype)%>%
      summarise(count = n())
    for (k in 1:length(arch_commitment$arch)){
      for (j in 1:length(jdd8$archetype)){
        if(tolower(apool[i]) %in% tolower(jdd1$name)){
          if (str_detect(jdd8$archetype[j], arch_commitment$arch[k])){
            arch_commitment$commitment[k] = arch_commitment$commitment[k] + jdd8$count[j]
          }
        }
      }
    }
    
  }

  jdd9<-jdd1%>%
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
  print("after")
  return(arch_commitment)
}


createarch(c("splinter twin"), jdd1)

makeColorVals <- function(pack, pool){
  currentcolors = createcc(pool, jdd7)
  colorVector = rep(0, length(pack))
  for (i in 1:length(pack)){
    print("")
    print("Color Vector:")
    print(colorVector)
    if(tolower(pack[i]) %in% tolower(jdd7$name)){
      jdd10<-jdd7%>%
        filter(tolower(name) == tolower(pack[i]))
      
      colorval = 0
      off_color = FALSE
      card_colors = unlist(strsplit(jdd10$color[1], split=" "))
      card_cc = jdd10$newcc[1]
      for (k in 1:length(currentcolors$color)){
        for (j in 1:length(card_colors)){
          if(card_colors[j] == currentcolors$color[k] && !done && currentcolors$commitment[k]>0){
            if(currentcolors$commitment[k] > colorval){
              colorval = colorval + currentcolors$commitment[k]
            }
          }
          if(card_colors[j] == currentcolors$color[k] && currentcolors$commitment[k] == 0){
            off_color = TRUE
            print("in")
          }
        }
      }
      if(!off_color && jdd10$color != "Colorless"){
        colorVector[i] <- colorval
      }else if(jdd10$color == "Colorless"){
        colorVector[i]<-max(currentcolors$commitment)
      }
    }
  }
  return(colorVector)
}





makeSphereVals <- function(pack, pool){
  currentsphere = createsph(pool, jdd1)
  sphVector = rep(0, length(pack))
  for (i in 1:length(pack)){
    if(tolower(pack[i]) %in% tolower(jdd1$name)){
      sph_weight<-createsph(c(pack[i]), jdd1)
      sphval = 0
      for (k in 1:length(currentsphere$sphere)){
        for (j in 1:length(sph_weight$sphere)){
          if(sph_weight$sphere[j] == currentsphere$sphere[k]){
            sphval = sphval + currentsphere$commitment[k]*sph_weight$commitment[j]
          }
        }
      }
      sphVector[i] <- sphval
    }
  }
  return(sphVector)
}



makeArchVals <- function(pack, pool){
  currentarch = createarch(pool, jdd1)
  archVector = rep(0, length(pack))
  for (i in 1:length(pack)){
    print("")
    print("Arch Vector:")
    print(archVector)
    if(tolower(pack[i]) %in% tolower(jdd1$name)){
      arch_weight<-createarch(c(pack[i]), jdd1)
      archval = 0
      for (k in 1:length(currentarch$arch)){
        for (j in 1:length(arch_weight$arch)){
          if(arch_weight$arch[j] == currentarch$arch[k]){
            archval = archval + currentarch$commitment[k]*arch_weight$commitment[j]
          }
        }
      }
      archVector[i] <- archval
    }
  }
  return(archVector)
}







makePowerVals<- function(pack){
  powervals <- rep(0, length(pack))
  for (i in 1:length(pack)){
    print(powervals)
    if(tolower(pack[i]) %in% tolower(jdd7$name)){
      jdd12<-jdd7%>%
        filter(tolower(name) == tolower(pack[i]))
      powervals[i]<-jdd12$power[1]
    }
  }
  return(powervals)
}

add_evs(df){
  df_ret<-df%>%
    mutate(ev = sum(Power, Color, Sphere, Archetype))
  return(df_ret)
}




df<-data.frame("Name" = c("wasteland"), 
               "Power" = c(0), 
               "Color" = c(0), 
               "Sphere" = c(0), 
               "Archetype" = c(0))
df$name1 = tolower(df$Name)
jdd13<-jdd1%>%
  group_by(name, color, types, )%>%
  summarise
  mutate(name1 = tolower(name))
df<-left_join(df, jdd13)



jdd14<-jdd20%>%
  filter(types == "Land")%>%
  group_by(name, color, types, text)%>%
  summarise(count = n())%>%
  filter(count<100)



jdd1<-read_csv("jdd_newest.csv")

jddpw<-jdd1%>%
  filter(location != "SB",
         powered == FALSE)%>%
  mutate(is_pw = str_detect(types, "Planeswalker"))%>%
  group_by(deckID)%>%
  summarise(pwcount = sum(is_pw))

sum(jddpw$pwcount) / length(jddpw$pwcount)







#start
#
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

arch_c = data_frame("name" = jdd7$name)

for (i in 1:length(arch_vector)){
  eval(parse(text = paste("arch_c$", arch_vector[i], "= rep(0, length(arch_c$name))", sep = "")))
}
  
length(arch_c$name)
for (i in 1:length(arch_c$name)){
  print(arch_c$name[i])
  outtable<-createarch(arch_c$name[i], jdd20)
  print("done")
  for (j in 1:(length(outtable$arch)-1)){
    arch_c[i, j+1] = outtable[j, 2]
  }
}


write_csv(arch_c, "arch_c.csv")




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


ret<-createarch(c("Char", "Fractured Identity", "Beast Within"), jdd20)


arch_vector




jddcolor



jdd1<-read_csv("jdd_newest.csv")
jdd7<-read_csv("jdd7.csv")
jdd20<-read_csv("jdd_newest.csv")
arch_c<-read_csv("arch_c.csv")
jdd_lands<-read_csv("jdd_lands.csv")

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
  colorVector = colorVector * 2
  return(colorVector)
}




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

createcc(c("Char", "Goblin Guide", "hellrider", "verdant catacombs"), jdd7)


#old
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


jdd1<-read_csv("jdd_newest.csv")
jdd7<-read_csv("jdd7.csv")
jdd20<-read_csv("jdd_newest.csv")
arch_c<-read_csv("arch_c.csv")
jdd_lands<-read_csv("jdd_lands.csv")

makeColorVals <- function(pack, pool){
  currentcolors = createcc(pool, jdd7)
  colorVector = rep(0, length(pack))
  if(length(pack) !=0 & length(pool) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(jdd7$name)){
        if(tolower(pack[i]) %in% tolower(jdd_lands$name)){
          jdd_l2<-jdd_lands%>%
            filter(tolower(name) == tolower(pack[i]))
          total = sum(currentcolors$commitment)
          if(jdd_l2$numColors[1] == 4){
            colorVector[i] = total * jdd_l2$power[1]
          }
          else if(jdd_l2$numColors[1] == 2){
            for (k in 1:length(currentcolors$commitment)){
              if(str_detect(jdd_l2$landColor[1], currentcolors$color[k])){
                colorVector[i] = colorVector[i] + (currentcolors$commitment[k] * jdd_l2$power[1])/2
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

makeColorVals(c("char", "goblin guide", "verdant catacombs", "arid mesa"), c("char", "goblin guide", "thalia, guardian of thraben"))


#old
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
  colorVector = colorVector * 2
  return(colorVector)
}



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

makePowerVals(c("verdant catacombs"))



makeArchVals <- function(pack, pool){
  currentarch = createarch(pool, jdd20)
  archVector = rep(0, length(pack))
  if(length(pack) !=0 & length(pool) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i])%in%tolower(jdd_lands)){
        
      }
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





makeColorVals <- function(pack, pool){
  print("start")
  currentcolors = createcc(pool, jdd7)
  colorVector = rep(0, length(pack))
  if(length(pack) !=0 & length(pool) !=0){
    for (i in 1:length(pack)){
      if(tolower(pack[i]) %in% tolower(unique(jdd1$name))){
        print("here")
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
                print("setting colorvector")
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




makeColorVals(c("bloodstained mire", "verdant catacombs", "breeding pool", "city of brass", "mana confluence"), c("verdant catacombs"))

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
          print(sphere_commitment$sphere[k])
          print(jdd8$sphere[j])
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


createsph(c("mystic confluence"), jdd20)
