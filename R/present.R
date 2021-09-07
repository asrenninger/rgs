##################################
### HOUSE HUNTERS
# 2 processing 
##################################

## packages
library(tidyverse)
library(gt)
library(tidytext)
library(tm)
library(widyr)
library(sf)
library(glue)


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
new_stops <- filter(stop_words, !str_detect(word, "want|need|like|love"))

read_csv("data/words_new.csv") %>% 
  filter(!str_detect(text, "\\[.*?\\]")) %>%
  mutate(end = case_when(str_detect(lag(text), "\\.") ~ "start",
                         str_detect(text, "\\.") ~ "end",
                         line == "3" ~ "start")) %>% 
  group_by(uniqueID, end) %>% 
  mutate(start = 1:n()) %>% 
  mutate(start = case_when(!is.na(end) ~ start)) %>%
  ungroup() %>% 
  fill(start) %>% 
  group_by(start, uniqueID) %>% 
  view()

read_csv("data/words_new.csv") %>%
  filter(!str_detect(text, "\\[.*?\\]")) %>%
  mutate(end = case_when(str_detect(lag(text), "\\.|\\?|\\!|\\]") ~ "start",
                         str_detect(text, "\\.|\\?|\\!|\\]") ~ "end",
                         line == "3" ~ "start")) %>% 
  mutate(end = case_when(str_detect(text, "\\[") ~ "start",
                         str_detect(text, "\\]") ~ "end",
                         str_detect(lead(text), "\\[") ~ "end",
                         TRUE ~ end)) %>%
  mutate(x = paste(end, lag(end), sep = "_")) %>%
  filter(x != "end_end" & x != "start_start") %>% 
  select(-x) %>% 
  group_by(uniqueID, end) %>% 
  mutate(start = 1:n()) %>% 
  mutate(start = case_when(!is.na(end) ~ start)) %>%
  ungroup() %>%
  fill(start) %>% 
  group_by(start, uniqueID) %>%
  summarise(text = str_c(text, collapse = " ")) %>%
  arrange(uniqueID, start) %>%
  filter(!str_detect(text, "\\[.*?\\]")) %>% 
  rename(line = start) %>% 
  write_csv("data/captions_new.csv")
  
videos <- 
  bind_rows(read_csv("data/videos_old.csv"),
            read_csv("data/videos_new.csv")) %>% 
  select(-date, -captions)
  
corpus_line <-
  bind_rows(read_csv("data/captions_old.csv"),
            read_csv("data/captions_new.csv")) %>%
  mutate(text = str_to_lower(text))
  
corpus_tidy <- 
  bind_rows(read_csv("data/words_old.csv"),  
            read_csv("data/words_new.csv") %>% 
              filter(!str_detect(text, "\\[.*?\\]")) %>%
              unnest_tokens(word, text) %>% 
              anti_join(stop_words)) %>% 
  left_join(videos)

counts <- videos %>% filter(type == "national") %>% group_by(location) %>% summarise(n = n())

rnaturalearth::ne_download(scale = 'large', type = 'populated_places', category = 'cultural', returnclass = 'sf')

tigris::states(cb = TRUE, class = 'sf') %>% 
  filter(STUSPS %in% unique(tigris::fips_codes$state)[1:51][-c(2, 12)]) %>% 
  transmute(location = STUSPS) %>%
  left_join(counts) %>% 
  st_as_sf() %>%
  st_transform(2163) %>%
  select(n) %>% 
  plot()

## names data
names <- 
  read_csv("https://raw.githubusercontent.com/hadley/data-baby-names/master/baby-names.csv") %>%
  mutate(name = tolower(name)) %>%
  rename(word = name)

places <- 
  read_delim("https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv", delim = '|') %>%
  janitor::clean_names() %>% 
  mutate(city = tolower(city)) %>%
  filter(city != "beach")

library(ggraph)
library(igraph)

cities <- 
  places %>% 
  filter(state_short %in% videos$location) %>% 
  distinct(city, .keep_all = TRUE) %>%
  drop_na(state_short) %>%
  group_by(state_short) %>%
  group_split() %>%
  map(function(x){ x %>% pull(city) %>% glue_collapse("|") })

library(furrr)
plan(multisession, workers = 6)

tictoc::tic()

corpus_line %>% 
  mutate(text = future_map_chr(text, 
                               function(x){ 
                                 
                                 reduce(map_chr(cities, function(y){ str_remove_all(x, y) }), str_c)
                              
                                 })) %>% 
  filter(str_detect(text, "rapids"))

tictoc::tic()

corpus_line %>%
  mutate(text = str_to_lower(text)) %>% 
  group_by(word) %>%
  filter(n() > 9) %>%
  pairwise_cor(word, line, sort = TRUE) %>% 
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

library(igraph)
library(ggraph)

corpus_line %>% 
  mutate(text = str_to_lower(text)) %>% 
  mutate(word = str_replace_all(text, ",", "")) %>%
  unnest_tokens(word, text) %>% 
  filter(!word %in% new_stops$word,
         !word %in% c("yeah", "hunter", "house", "hmm", "feel", "like", "music", "", "suzanne", "scripps", "network", "gonna", "week", "ago")) %>% 
  mutate(word = SnowballC::wordStem(word)) %>% 
  mutate(word = removeNumbers(word)) %>% 
  filter(word != "") %>%
  group_by(word) %>%
  filter(n() > 100) %>%
  pairwise_cor(word, line, sort = TRUE) %>% 
  slice(1:500) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(size = 1, colour = '#c7c7c7') +
  geom_node_point(colour = pal[1], size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  theme_void() +
  ggsave("viz/correlations.png", height = 6, width = 8, dpi = 300)

bigrams <- 
  corpus_line %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  mutate(word = str_replace_all(word, ",", "")) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% new_stops$word,
         !word2 %in% new_stops$word,
         !word1 %in% c("yeah", "hunter", "house", "hmm", "feel", "like", "music", "", "suzanne"),
         !word2 %in% c("yeah", "hunter", "house", "hmm", "feel", "like", "music", "", "suzanne")) %>% 
  mutate(word1 = SnowballC::wordStem(word1),
         word2 = SnowballC::wordStem(word2)) %>% 
  mutate(word1 = removeNumbers(word1),
         word2 = removeNumbers(word2)) %>% 
  filter(word1 != "", 
         word2 != "") 

corpus_line %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  mutate(word = str_replace_all(word, ",", "")) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% new_stops$word,
         !word2 %in% new_stops$word,
         !word1 %in% c("yeah", "hunter", "house", "hmm", "feel", "like", "music", "", "suzanne", "scripps", "network", "gonna", "week", "ago"),
         !word2 %in% c("yeah", "hunter", "house", "hmm", "feel", "like", "music", "", "suzanne", "scripps", "network", "gonna", "week", "ago")) %>% 
  mutate(word1 = SnowballC::wordStem(word1),
         word2 = SnowballC::wordStem(word2)) %>% 
  mutate(word1 = removeNumbers(word1),
         word2 = removeNumbers(word2)) %>% 
  filter(word1 != "", 
         word2 != "") %>% 
  drop_na() %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 50) %>% 
  ggplot(aes(x = reorder(word1,-n), y = reorder(word2, -n), fill = n)) +
  geom_tile(alpha = 0.8, colour = "white") +
  viridis::scale_fill_viridis(option = 'plasma') + 
  # scico::scale_fill_scico(palette = 'hawaii') +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "first word in pair",
       y = "second word in pair") +
  ggsave("viz/correlations.png", height = 6, width = 8, dpi = 300)


viridis::scale_fill_viridis(option = 'magma')
