library(tidyverse)

model_15_df <- read.csv("model_15_topic_13.csv")[1:10,]

model_15_df %>% ggplot(aes(x=reorder(word,probability), y=probability)) + 
  geom_col() + 
  coord_flip() +
  theme_bw() + 
  xlab("word")
  
model_34_df <- read.csv("model_34_topic_26.csv")[1:10,]

model_34_df %>% ggplot(aes(x=reorder(word,probability), y=probability)) + 
  geom_col() + 
  coord_flip() +
  theme_bw() + 
  xlab("word")

coherence_df <- read.csv("coherence_df.csv")

coherence_df %>% ggplot(aes(x=topics, y=coherence)) +
  geom_line(color="blue") +
  theme_bw() +
  ylim(0.38,0.54)
  
