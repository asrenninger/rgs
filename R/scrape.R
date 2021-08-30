##################################
### HOUSE HUNTERS
# 1 scraping 
##################################

## packages

## load lsit
videos <- read_csv("videos_new.csv")

## load old data
captions <- read_csv("captions_new.csv")

## loop through each video
update <- map_df(27:nrow(videos), 
                 function(x){
                   
                   episode <- 
                     jsonlite::fromJSON(videos$captions[x]) %>% 
                     ## pull out the "events" data frame nested within it the json
                     magrittr::use_series(events) %>% 
                     ## this is a weird artifact of the data: each cell of the data frame is a single 1x1 dataframe, so just strip that out with the unnest function
                     unnest(segs, names_repair = 'unique') %>% 
                     ## select the columns you want
                     transmute(line = wpWinPosId,
                               text = utf8)  %>%
                     ## this step is just to clean any stray line breaks
                     mutate(text = str_replace_all(text, "\\n", " ")) %>%
                     ## pull the text column to go to work on it more
                     pull(text) %>%
                     ## collapse it from a list into one long string that has all the captions
                     glue::glue_collapse(sep = " ") %>%
                     ## split it by space so that each word is a line
                     str_split(" ") %>%
                     ## the result is structured like a list of lists [[1]][[1]], so pluck out the data
                     pluck(1) %>%
                     ## create a data frame
                     as_tibble() %>%
                     ## assign each word to a line with 1:n()
                     transmute(line = 1:n(),
                               text = value,
                               uniqueID = videos$uniqueID[x]) %>% 
                     filter(text != "")
                   
                 })

## combine
new <- bind_rows(captions, mutate(update, text = str_remove_all(text, "\"")))

## save new file
write_csv("captions_new.csv")


