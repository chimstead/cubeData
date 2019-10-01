cards <- fromJSON(file = "AllCards.json")


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


crads <- data.frame('name' = card_name, 
                    'color' = card_color, 
                    'cmc' = card_cmc,
                    'manaCost' = card_manaCost,
                    "power" = card_power, 
                    'toughness' = card_toughness,
                    'text' = card_text,
                    'types' = card_types,
                    'subtypes' = card_subtypes)

crads2<-crads

for (i in 1:length(crads2$color)){
  if(str_detect(crads2$manaCost[i], "/")){
    crads$color[i] = "Colorless"
  }
}

crads2$color[1476] = "Colorless"

write_csv(crads2, "crads2.csv")
