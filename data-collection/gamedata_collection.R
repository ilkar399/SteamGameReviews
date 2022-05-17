#Gamedata collection from Steam API

install.packages("tidyjson")
install.packages("tidyr")
install.packages("rjson")
install.packages("dplyr")

library(dplyr)
library(tidyjson)
library(tidyr)
library(jsonlite)


app_ids <- c(1889981, 1184370, 1889980)
base_url <- "https://store.steampowered.com/api/appdetails?appids="
steamapp_list = data.frame()

for (app_id in app_ids) {
  temp_data <- fromJSON(paste0(c(base_url,app_id), collapse=""))
  temp_spread <- temp_data[paste(app_id)] %>% 
    #spreading other columns (and renaming to be consistent)
    spread_values(
      success = jlogical(success),
      steam_appid=jnumber(data,steam_appid),
      name=jstring(data,name),
      short_description = jstring(data,short_description),
      metacritic.score = jnumber(data,metacritic,score),
      release_date = jstring(data,release_date,date),
    ) %>%
    #getting categorical lists.
    #Probably not the most efficient method but it works for now
    mutate(..JSON2 = ..JSON) %>% 
    enter_object(data,categories,description) %>%
    mutate(categories=..JSON) %>%
    mutate(..JSON=..JSON2) %>%
    enter_object(data,developers) %>%
    mutate(developers=..JSON) %>%
    mutate(..JSON=..JSON2) %>%
    enter_object(data,publishers) %>%
    mutate(publishers=..JSON) %>%
    mutate(..JSON=..JSON2) %>% 
    enter_object(data,genres,description) %>%
    mutate(genres=..JSON) %>% 
    #converting tibble_json to the dataframe and removing columns created by 
    #conversion
    as.data.frame() %>% 
    select(-..JSON,-document.id,-..JSON2)
  steamapp_list <- rbind(steamapp_list,temp_spread)
}

#Saving dataset, replacing lists with '|' separated values
steamapp_list %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) %>% 
  write.csv("data/steamapps.csv", row.names = FALSE)
