library(tidyverse)
library(quanteda)
library(stm)
library(stargazer)
library(effsize)

data_lemmatized_df <- read.csv("../dataset/data_lemmatized_df.csv")[,-1]

random_training_index_df <- read.csv("../dataset/random_training_index_df.csv") %>%
  mutate(training_index = training_index+1)

random_training_index <- as.vector(random_training_index_df[["training_index"]])

sample_data_df <- data_lemmatized_df[random_training_index,]

clean_data_df <- sample_data_df %>% 
  filter(text!="") %>%
  filter(nationality != "") %>%
  mutate(date = strptime(date, "%Y-%m-%d")) %>%
  mutate(crisis=ifelse(date >= strptime("2013-10-03", "%Y-%m-%d"), TRUE, FALSE)) %>%
  mutate(affectedness=ifelse(grepl("Hungary|Italy|Greece|Croatia", nationality),
                                 TRUE, FALSE)) %>%
  mutate(euroscepticism = ifelse(grepl("*Alliance of Liberals and Democrats for Europe*|*European People's Party*|*Greens/European Free Alliance*|*Progressive Alliance of Socialists and Democrats*|*European Liberal, Democrat and Reform Party*|*European Socialists*|*Socialist Group*|*Renew Europe*",
                                       political_group),
                                 FALSE, TRUE))

political_groups_df <- clean_data_df %>% count(euroscepticism, political_group)
nationalities_df <- clean_data_df %>% count(affectedness, nationality)
date_df <- clean_data_df %>% count(crisis, date)

corpus <- corpus(clean_data_df, text_field = "text")

tokens <- tokens(corpus)

dfm <- dfm(tokens)

stm <- stm_affectedness_50 <- stm(dfm, 
                                  K=50, 
                                  seed=42,
                                  data=docvars(dfm),
                                  max.em.its = 10)

topic_6 <- labelTopics(stm, topics = c(6), n=10)

topic_6[1]

plot(stm, type = "labels", topics=6)

topicQuality(stm, dfm)

## Simple regressions
### Affectedness
stm_affectedness_50 <- stm(dfm, 
           K=50, 
           seed=42, 
           prevalence = ~affectedness, 
           content = ~affectedness,
           data=docvars(dfm),
           max.em.its = 10)

stm_affectedness_50 <- readRDS("models/stm_affectedness_50")

summary(stm_affectedness_50)

prep_affectedness <- estimateEffect(as.integer(6) ~ affectedness, stm_affectedness_50, metadata = docvars(dfm))

summary(prep_affectedness, 6)

plot(stm_affectedness_50,
     type = "perspectives",
     topics = 6)

### Euroscepticism
stm_euroscepticism_50 <- stm(dfm, 
                           K=50, 
                           seed=42, 
                           prevalence = ~euroscepticism, 
                           content = ~euroscepticism,
                           data=docvars(dfm),
                           max.em.its = 10)

stm_euroscepticism_50 <- readRDS("models/stm_euroscepticism_50")

summary(stm_euroscepticism_50)

findThoughts(stm_euroscepticism_50, 
             texts = clean_data_df$text,
             topics = 6,
             n = 1)

prep_euroscepticism <- estimateEffect(as.integer(6) ~ euroscepticism, stm_euroscepticism_50, metadata = docvars(dfm))

summary(prep_euroscepticism, 6)

plot(stm_euroscepticism_50,
     type = "perspectives",
     topics = 6)

### Crisis
stm_crisis_50 <- stm(dfm, 
                     K=50, 
                     seed=42, 
                     prevalence = ~crisis, 
                     content = ~crisis,
                     data=docvars(dfm),
                     max.em.its = 10)

stm_crisis_50 <- readRDS("models/stm_crisis_50")

summary(stm_crisis_50)

prep_crisis <- estimateEffect(as.integer(6) ~ crisis, stm_crisis_50, metadata = docvars(dfm))

summary(prep_crisis, 6)

plot(stm_crisis_50,
     type = "perspectives",
     topics = 6)


## Multiple regressions
### Crisis & Eurosceptcism
stm_crisis_euroscepticism_50 <- stm(dfm, 
                     K=50, 
                     seed=42, 
                     prevalence =~ crisis + euroscepticism, 
                     data=docvars(dfm),
                     max.em.its = 100)

stm_crisis_euroscepticism_50 <- readRDS("models/stm_crisis_euroscepticism_50")

summary(stm_crisis_euroscepticism_50)

prep_crisis_euroscepticism <- estimateEffect(as.integer(6) ~ crisis + euroscepticism + crisis*euroscepticism , stm_crisis_euroscepticism_50, metadata = docvars(dfm))

summary(prep_crisis_euroscepticism, 6)

plot(stm_crisis_euroscepticism_50,
     type = "perspectives",
     topics = 6)

### Crisis & Affectedness
stm_crisis_affectedness_50 <- stm(dfm, 
                                    K=50, 
                                    seed=42, 
                                    prevalence =~ crisis + affectedness, 
                                    data=docvars(dfm),
                                    max.em.its = 100)

stm_crisis_affectedness_50 <- readRDS("models/stm_crisis_affectedness_50")

summary(stm_crisis_affectedness_50)

prep_crisis_affectedness <- estimateEffect(as.integer(6) ~ crisis + affectedness + crisis*affectedness, stm_crisis_affectedness_50, metadata = docvars(dfm))

summary(prep_crisis_affectedness, 6)

### euroscepticism & Affectedness
stm_euroscepticism_affectedness_50 <- stm(dfm, 
                                  K=50, 
                                  seed=42, 
                                  prevalence =~ euroscepticism + affectedness, 
                                  data=docvars(dfm),
                                  max.em.its = 100)

stm_euroscepticism_affectedness_50 <- readRDS("models/stm_euroscepticism_affectedness_50")

summary(stm_euroscepticism_affectedness_50)

prep_euroscepticism_affectedness <- estimateEffect(as.integer(6) ~ euroscepticism + affectedness + euroscepticism*affectedness, stm_euroscepticism_affectedness_50, metadata = docvars(dfm))

summary(prep_euroscepticism_affectedness, 6)


### Crisis & Euroscepticism & Affectedness
stm_crisis_euroscepticism_affectedness_50 <- stm(dfm, 
                                          K=50, 
                                          seed=42, 
                                          prevalence =~ crisis + euroscepticism + affectedness, 
                                          data=docvars(dfm),
                                          max.em.its = 100)

stm_crisis_euroscepticism_affectedness_50 <- readRDS("models/stm_crisis_euroscepticism_affectedness_50")

summary(stm_crisis_euroscepticism_affectedness_50)

prep_crisis_euroscepticism_affectedness <- estimateEffect(as.integer(6) ~ crisis + euroscepticism + affectedness + crisis*euroscepticism + crisis*affectedness + euroscepticism*affectedness, stm_crisis_euroscepticism_affectedness_50, metadata = docvars(dfm))

summary(prep_crisis_euroscepticism_affectedness, 6)

prep_crisis_euroscepticism_affectedness$varlist

