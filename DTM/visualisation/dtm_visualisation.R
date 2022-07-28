library(tidyverse)

yearly_df <- read.csv("yearly_df.csv") %>%
  rename(year = X)

yearly_df %>% 
  pivot_longer(country:territory, names_to = "word", values_to = "probability") %>%
  ggplot(aes(x=year, y= probability, color=word)) +
  geom_line() +
  theme_bw() + 
  scale_color_brewer(palette = "Paired") 
