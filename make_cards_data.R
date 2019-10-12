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
  }else if(card_layout[i]=='transform'||card_layout[i]=='adventure'){
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

#remove stuff that does not matter
omit_list = c('token', 'double_faced_token', 'scheme', 'vanguard', 'augment', 'planar', 'host', 'art_series', 'emblem')
#make double face and split cards work, split crads first
crads<-crads%>%
  filter(!layout%in%omit_list)

#fix wierd cards
crads<-crads%>%
  mutate(name = ifelse(layout%in%c('transform', 'flip', 'adventure'), str_match(name, ".*(?= //)"), str_match(name, ".*")))
  


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
