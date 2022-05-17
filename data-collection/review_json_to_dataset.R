# Steam Review json format:
# recommendationid - The unique id of the recommendation
# author
# 
# steamid - the user’s SteamID
# num_games_owned - number of games owned by the user
# num_reviews - number of reviews written by the user
# playtime_forever - lifetime playtime tracked in this app
# playtime_last_two_weeks - playtime tracked in the past two weeks for this app
# playtime_at_review - playtime when the review was written
# last_played - time for when the user last played
# 
# language - language the user indicated when authoring the review
# review - text of written review
# timestamp_created - date the review was created (unix timestamp)
# timestamp_updated - date the review was last updated (unix timestamp)
# voted_up - true means it was a positive recommendation
# votes_up - the number of users that found this review helpful
# votes_funny - the number of users that found this review funny
# weighted_vote_score - helpfulness score
# comment_count - number of comments posted on this review
# steam_purchase - true if the user purchased the game on Steam
# received_for_free - true if the user checked a box saying they got the app for free
# written_during_early_access - true if the user posted this review while the game was in Early Access
# developer_response - text of the developer response, if any
# timestamp_dev_responded - Unix timestamp of when the developer responded, if applicable

#importing and loading packages
install.packages("tidyjson")
install.packages("rjson")

library(tidyjson)
library(jsonlite)
library(tidyverse)

#app_ids to convert
app_ids <- c(1889980, 1184370, 1889981)
for (app_id in app_ids) {
  #loading json file
  #temp_data <- fromJSON(file = paste0(c("data/review_",app_id,".json"), collapse=""))
  temp_data <- fromJSON(paste0(c("data/review_",app_id,".json"), collapse=""))
  temp_spread <- temp_data ["reviews"] %>% 
    #gather review_id object into a pair review_id column
    gather_object("review_id") %>% 
    #spreading other columns (and renaming to be consistent)
    spread_values(
      recommendationid = jstring(recommendationid),
      language = jstring(language),
      review_text = jstring(review),
      timestamp_created = jnumber(timestamp_created),
      timestamp_updated = jnumber(timestamp_updated),
      voted_up = jlogical(voted_up),
      votes_up = jnumber(votes_up),
      votes_funny = jnumber(votes_funny),
      weighted_vote_score = jstring(weighted_vote_score),
      comment_count = jnumber(comment_count),
      steam_purchase = jlogical(steam_purchase),
      received_for_free = jlogical(received_for_free),
      written_during_early_access = jlogical(written_during_early_access),
      author.steamid = jstring(author, steamid),
      author.num_games_owned = jnumber(author, num_games_owned),
      author.num_reviews = jnumber(author, num_reviews),
      author.playtime_forever = jnumber(author, playtime_forever),
      author.playtime_last_two_weeks = jnumber(author, playtime_last_two_weeks),
      author.playtime_at_review = jnumber(author, playtime_at_review),
      author.last_played = jnumber(author, last_played),
    ) %>%
    #converting tibble_json to the dataframe and removing columns created by 
    #conversion
    as.data.frame() %>% 
    select(-..JSON,-document.id)
   #saving the final file
   temp_spread %>%
     write.csv(paste0(c("data/review_",app_id,".csv"), collapse=""))
   #writing a summary on the available data to compare with the downloaded data
   #during cleanup
   temp_summary <- temp_data["query_summary"] %>%
     spread_all() %>%
     as.data.frame() %>%
     select(-..JSON,-document.id)
   temp_summary$num_reviews = nrow(temp_spread)
   temp_summary %>%
     write.csv(paste0(c("data/summary_",app_id,".csv"), collapse=""))
   print(paste0("Data written for AppID ", app_id, colllapse=""))
}
print("Done")
