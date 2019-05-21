library(tidytext)
library(ggplot2)
library(dplyr)

#Dataset
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

#Extracting words with tidytext
text_minning<- nobel_winners %>% unnest_tokens(word, motivation) %>% anti_join(stop_words)

#Some words that aren't relevant
irrelevants<-  c(
    "pioneering",
    "discovery",
    "discoveries",
    "studies",
    "recognition",
    "investigations",
    "contributions", 
    "development",
    "theory", 
    "theoretical", 
    "economic", 
    "foundations", 
    "analysis", 
    "analyses",
    "analyzing",
    "research", 
    "understanding"
  ) 

#Words by year (first the order)
text_minning_2<-text_minning %>% 
  group_by(category=="Economics")%>%
  filter(!is.na(word) &
           !(word %in% irrelevants) &
           category=="Economics") %>%
  count(word, sort=TRUE) %>%
  mutate(word=reorder(word,n)) %>%
  filter(n>3)

text_minning_3<-text_minning %>% 
  group_by(category=="Economics", prize_year) %>%
  filter(word %in% b$word & !is.na(word) &
           !(word %in% irrelevants) &
           category=="Economics") %>%
  count(word, sort=TRUE) 

text_minning_4<-merge(text_minning_3,text_minning_2,by="word", all.x=TRUE, all.y = FALSE)

text_minning_4 %>% mutate(word=reorder(word,n.y)) %>%
  ggplot(aes(word,n.x,fill=prize_year))+
  geom_col()+
  coord_flip()+
  labs(title="Nobel Winners - Economics", 
       x="Repeated award motivation word", 
       y="Frequency", 
       fill="Year", 
       subtitle="TidyTuesday, Rstats")+
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "right")+
  theme_minimal()+
  scale_fill_distiller(palette = "Greens") 
