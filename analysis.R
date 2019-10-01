jdd<-read_csv("jdd.csv")

jdd$color<-tolower(jdd$color)

num_splashes<-jdd%>%
  filter(!str_detect(color, " "),
         color != "colorless")%>%
  filter(str_detect(color, splashes))%>%
  group_by(name)%>%
  summarise(count = n())%>%
  arrange(desc(count))


most_frequent_cards<-jdd%>%
  filter(!str_detect(types, "Land"))%>%
  filter(str_detect(sphere, "control"))%>%
  group_by(name)%>%
  summarise(count = n())%>%
  arrange(desc(count))
  
jdd_<-jdd4
for (i in 1:length(jdd_$deckID)){
  if(jdd_$deckID[i] == ){
    jdd_$archetype[i]<- "twin"
  }
}

jdd_$deckID[1680] == 50


jdd4<-jdd_
