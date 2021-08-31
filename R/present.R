##################################
### HOUSE HUNTERS
# 2 processing 
##################################

## packages
library(tidyverse)
library(gt)
library(tidytext)
library(NLTK)


## palette
pal <- scico::scico(n = 9, palette = 'hawaii')

## table 1
tibble(activity = c("sex", "socializing", "relaxing", "praying", "eating", "exercising", "housework", "working", "commuting"), 
           happiness = c(4.7, 4.0, 3.9, 3.8, 3.8, 3.8, 3.0, 2.7, 2.6), 
           hours = c(0.2, 2.3, 2.2, 0.4, 0.2, 0.2, 1.1, 6.9, 1.6)) %>%
  gt() %>%
  data_color(
    columns = vars(happiness),
    colors = scales::col_numeric(
      palette = scico::scico(n = 5, palette = 'hawaii'),
      domain = c(2, 5))
  ) %>% 
  tab_header(
    title = "Reported Happiness by Activity",
    subtitle = "Relating Time and Satisfaction"
  ) %>% 
  tab_source_note(
    source_note = md("Reference: Layard, R. (2011) 'Happiness'")
  ) %>%
  gtsave("happiness.png", expand = 10)

## load in corpus
corpus <- 
  bind_rows(read_csv("data/captions_old.csv"),  
            read_csv("data/captions_new.csv") %>% 
              filter(!str_detect(text, "\\[*\\]")) %>%
              unnest_tokens(word, text) %>% 
              anti_join(stop_words))

## names data
names <- 
  read_csv("https://raw.githubusercontent.com/hadley/data-baby-names/master/baby-names.csv") %>%
  mutate(name = tolower(name)) %>%
  filter(name)
  rename(word = name)


