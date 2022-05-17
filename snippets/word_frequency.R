install.packages("stopwords")
install.packages("tidytext")
install.packages("ggwordcloud")
install.packages("sentimentr")
install.packages("udpipe")

library(dplyr)
library(stopwords)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(sentimentr)
library(SnowballC)
library(udpipe)

game_freq_pos <- basegame_cleared %>%
  ungroup() %>% 
  filter(language == "english") %>% 
  filter(nchar(review_text, type="chars") > 0) %>%
  select(review_id,voted_up,timestamp_created,review_text) %>% 
  unnest_tokens(review_words, review_text) %>% 
  anti_join(as_tibble(stopwords("en")),by=c("review_words" = "value")) %>% 
  mutate (words_stem = wordStem(review_words, language = "en")) %>% 
  group_by(review_words) %>% 
  mutate(words_count = n()) %>%
  ungroup %>% 
  group_by(words_stem) %>% 
  mutate(review_words = review_words[which.max(words_count)]) %>% 
  ungroup() %>% 
  select(-words_count,-words_stem)

dlc_freq_pos <- dlc_reviews_cleared %>%
  ungroup() %>% 
  filter(language == "english") %>% 
  filter(nchar(review_text, type="chars") > 0) %>%
  select(review_id,voted_up,timestamp_created,review_text) %>% 
  unnest_tokens(review_words, review_text) %>% 
  anti_join(as_tibble(stopwords("en")),by=c("review_words" = "value")) %>% 
  mutate (words_stem = wordStem(review_words, language = "en")) %>% 
  group_by(review_words) %>% 
  mutate(words_count = n()) %>%
  ungroup %>% 
  group_by(words_stem) %>% 
  mutate(review_words = review_words[which.max(words_count)]) %>% 
  ungroup() %>% 
  select(-words_count,-words_stem)

dlc2_freq_pos <- dlc2_reviews_cleared %>%
  ungroup() %>% 
  filter(language == "english") %>% 
  filter(nchar(review_text, type="chars") > 0) %>%
  select(review_id,voted_up,timestamp_created,review_text) %>% 
  unnest_tokens(review_words, review_text) %>% 
  anti_join(as_tibble(stopwords("en")),by=c("review_words" = "value")) %>% 
  mutate (words_stem = wordStem(review_words, language = "en")) %>% 
  group_by(review_words) %>% 
  mutate(words_count = n()) %>%
  ungroup %>% 
  group_by(words_stem) %>% 
  mutate(review_words = review_words[which.max(words_count)]) %>% 
  ungroup() %>% 
  select(-words_count,-words_stem)

dlc_freq_pos %>%
  filter(voted_up == TRUE) %>% 
  count(review_words, sort = TRUE) %>%
  slice_head(n = 50) %>%
  mutate(review_words = reorder(review_words, n)) %>%
  ggplot(aes(n, review_words)) +
  geom_col() +
  labs(y = NULL)

dlc_freq_pos %>%
  group_by(voted_up) %>% 
  count(review_words, sort = TRUE) %>%
  head(n=50) %>% 
  View

ck3_freq_pos %>%
  group_by(voted_up) %>% 
  count(review_words, sort = TRUE) %>%
  as_tibble() %>% 
  rename(word_count = n) %>% 
  group_by(review_words) %>% 
  filter(n() == 2) %>% 
  mutate(word_min = min(word_count)) %>%
  as_tibble() %>% 
  slice_max(word_min,n = 100, with_ties = FALSE) %>% 
  ggplot(aes(x = word_count, y = review_words, fill = factor(voted_up))) + 
  geom_col(position="stack")

temp1 <- dlc_freq_pos %>%
  unique() %>% 
  count(review_words)

temp2 <- ck3_freq_pos %>%
  unique() %>% 
  count(review_words)

reviews_wordcount <- inner_join(dlc_freq_pos %>%
                      unique() %>% 
                      count(review_words), ck3_freq_pos %>%
                      unique() %>% 
                      count(review_words), by = "review_words") %>% 
  mutate(min_word_count = pmin(n.x, n.y)) %>% 
  select(review_words, min_word_count) %>% 
  filter(min_word_count > 5) %>% 
  arrange(desc(min_word_count))


temp3 <- inner_join(temp1, temp2, by = "review_words") %>% 
  mutate(min_word_count = pmin(n.x, n.y)) %>% 
  select(review_words, min_word_count) %>% 
  filter(min_word_count > 5) %>% 
  arrange(desc(min_word_count))

game_terms <- c(
  "worth",#
  "content",
  "price",
  "new",
  "culture",
  "time",
  "fun",#
  "mechanics",
  "events",
  "features", 
  "money",#
  "long",#
  "interesting",#
  "changes",#
  "bugs",
  "map",
  "amount",#
  "depth",
  "artifacts",#
  "characters",
  "immersion",#
  "value",#
  "religion",
  "war",
  "world",
  "prestige",#
  "quality",#
  "strategy")

#Word clouds

set.seed(1)
#Base game
ck3_freq_pos %>%
  mutate(review_group = case_when(
    (voted_up == TRUE) ~ "Positive reviews",
    (voted_up == FALSE) ~ "Negative reviews")) %>%
  group_by(review_group) %>% 
  count(review_words, sort = TRUE) %>%
  rename(word_count = n) %>% 
  mutate(count_normalise = word_count/sum(word_count)) %>% 
  slice_max(count_normalise,n = 50, with_ties = FALSE) %>% 
  as_tibble() %>% 
  ggplot(aes(label = review_words,
             color = review_group,
             size = count_normalise,
             alpha = count_normalise
  )
  ) +
  geom_text_wordcloud_area() +
  labs(
    title = "Word cloud for the base game"
  ) +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  scale_alpha_continuous(range = c(0.6,1)) +
  scale_fill_brewer(palette = "Set1"
                    , name = "review_group") +
  facet_wrap(~review_group)
#DLC
dlc_freq_pos %>%
  mutate(review_group = case_when(
    (voted_up == TRUE) ~ "Positive reviews",
    (voted_up == FALSE) ~ "Negative reviews")) %>%
  group_by(review_group) %>% 
  count(review_words, sort = TRUE) %>%
  rename(word_count = n) %>% 
  mutate(count_normalise = word_count/sum(word_count)) %>% 
  slice_max(count_normalise,n = 50, with_ties = FALSE) %>% View()
  as_tibble() %>% 
  ggplot(aes(label = review_words,
             color = review_group,
             size = count_normalise,
             alpha = count_normalise
  )
  ) +
  geom_text_wordcloud_area() +
  labs(
    title = "Word cloud for the Crusader Kings III: Royal Court"
  ) +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  scale_alpha_continuous(range = c(0.6,1)) +
  scale_fill_brewer(palette = "Set1"
                    , name = "review_group") +
  facet_wrap(~review_group)

#Sentiment analysis
#Setting important terms
game_terms <- c("price",
                " ai",
                "map",
                "war",
                "diplomacy",
                "music",
                "mechanics",
                "bug",
                "graphics",
                " ui",
                "content",
                "events")

#Base Game
ck3_sentences <- ck3_reviews_cleared %>%
  ungroup() %>% 
  filter(language == "english") %>% 
  filter(nchar(review_text, type="chars") > 0) %>%
  select(review_id,voted_up,timestamp_created,review_text) %>% 
  unnest_tokens(review_sentence, review_text, token="sentences") %>% 
  filter(grepl(paste0(game_terms, collapse = "|"),review_sentence)) %>% 
  rowid_to_column("rowid")
#Getting sentiment scores and joining with review sentences
ck3_sentiments <- sentiment_by(ck3_sentences$review_sentence, ck3_sentences$rowid)
ck3_sentiments <- inner_join(ck3_sentiments, ck3_sentences)
#Spreading keywords
ck3_sentiments <- ck3_sentiments %>% 
  mutate(keywords = str_extract_all(review_sentence, paste(game_terms, collapse = "|")))
ck3_sentiments <- ck3_sentiments %>% 
  unnest(keywords) %>% 
  unique() %>% 
  mutate(game = "Crusader Kings III")

#DLC
dlc_sentences <- dlc_reviews_cleared %>%
  ungroup() %>% 
  filter(language == "english") %>% 
  filter(nchar(review_text, type="chars") > 0) %>%
  select(review_id,voted_up,timestamp_created,review_text) %>% 
  unnest_tokens(review_sentence, review_text, token="sentences") %>% 
  filter(grepl(paste0(game_terms, collapse = "|"),review_sentence)) %>% 
  rowid_to_column("rowid")
#Getting sentiment scores and joining with review sentences
dlc_sentiments <- sentiment_by(dlc_sentences$review_sentence, dlc_sentences$rowid)
dlc_sentiments <- inner_join(dlc_sentiments, dlc_sentences)
#Spreading keywords
dlc_sentiments <- dlc_sentiments %>% 
  mutate(keywords = str_extract_all(review_sentence, paste(game_terms, collapse = "|")))
dlc_sentiments <- dlc_sentiments %>% 
  unnest(keywords) %>% 
  unique() %>% 
  mutate(game = "Crusader Kings III: Royal Court")

game_sentimens <- rbind(dlc_sentiments, ck3_sentiments)

game_sentimens %>% 
  filter(ave_sentiment !=0) %>% 
  group_by(voted_up,keywords,game) %>% 
  summarise(average_sentiment = median(ave_sentiment)) %>% 
  filter(average_sentiment > mean(average_sentiment) || average_sentiment < mean(average_sentiment)) %>% 
  View()


#making plots
game_sentimens %>% 
  filter(ave_sentiment != 0) %>% 
  group_by(voted_up,keywords, game) %>% 
  summarise(average_sentiment = median(ave_sentiment)) %>% 
  #Putting the plot
  ggplot(aes(x = keywords, y  = average_sentiment, fill = keywords)) + 
  geom_col(
    position="stack") +
  labs(
    x = NULL,
    y = "Average Sentiment",
    title = "Sentence sentiment for selected keywords for DLC"
  ) + 
  #Adding labels
  geom_text(
    aes(label = round(average_sentiment,2)),
    position = position_stack(vjust = 0.8),
    angle = 45,
    size = 3
  ) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5, angle=45),
        legend.position="none") +
  facet_wrap(~voted_up + game,
             labeller = labeller(
               voted_up = c( `TRUE` = "Positive Reviews",
                             `FALSE` = "Negative Reviews")
             ))

game_sentimens %>% 
  select(review_id,voted_up,keywords, game) %>%
  unique %>% 
#  summarise(word_count = nrow()) %>% 
  #Putting the plot
  ggplot(aes(x = keywords, fill = keywords)) + 
  geom_bar(
    position="stack") +
  labs(
    x = NULL,
    y = "Count",
    title = "Sentence sentiment for selected keywords for DLC"
  ) + 
  #Adding labels
  geom_text(
    stat='count',aes(label = ..count..),
    position = position_stack(vjust = 0.8),
    angle=90,
    size = 3
  ) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5, angle=45),
        legend.position="none") +
  facet_wrap(~voted_up + game,
             labeller = labeller(
               voted_up = c( `TRUE` = "Positive Reviews",
                             `FALSE` = "Negative Reviews")
             ))

#DLC
dlc_sentiments %>% 
  filter(ave_sentiment != 0) %>% 
  group_by(voted_up,keywords) %>% 
  summarise(average_sentiment = median(ave_sentiment)) %>% 
  #Putting the plot
  ggplot(aes(x = keywords, y  = average_sentiment, fill = keywords)) + 
  geom_col(
    position="stack") +
  labs(
    x = NULL,
    y = "Average Sentiment",
    title = "Sentence sentiment for selected keywords for DLC"
  ) + 
  #Adding labels
  geom_text(
    aes(label = round(average_sentiment,2)),
    position = position_stack(vjust = 0.8),
    size = 4
  ) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5, angle=45)) +
  facet_wrap(~voted_up,
             ncol = 1,
             labeller = labeller(
               voted_up = c( `TRUE` = "Positive Reviews",
                             `FALSE` = "Negative Reviews")
             ))

#Base Game
ck3_sentiments %>% 
  filter(ave_sentiment != 0) %>% 
  group_by(voted_up,keywords) %>% 
  summarise(average_sentiment = median(ave_sentiment)) %>% 
  #Putting the plot
  ggplot(aes(x = keywords, y  = average_sentiment, fill = keywords)) + 
  geom_col(
    position="stack") +
  labs(
    x = NULL,
    y = "Average Sentiment",
    title = "Sentence sentiment for selected keywords for Base Game"
  ) + 
  #Adding labels
  geom_text(
    aes(label = round(average_sentiment,2)),
    position = position_stack(vjust = 0.8),
    size = 4
  ) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5, angle=45)) +
  facet_wrap(~voted_up,
             ncol = 1,
             labeller = labeller(
               voted_up = c( `TRUE` = "Positive Reviews",
                             `FALSE` = "Negative Reviews")
             ))


#without positive/negative reviews
game_sentiments %>% 
  filter(ave_sentiment != 0) %>% 
  group_by(keywords, game) %>% 
  summarise(average_sentiment = median(ave_sentiment)) %>% 
  #Putting the plot
  ggplot(aes(x = keywords, y  = average_sentiment, fill = keywords)) + 
  lims(y = c(-0.5,0.5)) +
  geom_col(
    position="stack") +
  labs(
    x = NULL,
    y = "Average Sentiment",
    title = "Sentence sentiments",
    subtitle = "Sentence sentiments for selected keywords for the base game and the DLC",
    caption = "Created using sentimentr package"
  ) + 
  #Adding labels
  geom_text(
    aes(label = round(average_sentiment,2)),
    angle = 45,
    size = 3
  ) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5, angle=45),
        legend.position="none") +
  facet_wrap(~game,
             ncol = 1)

#with negative/positive reviews
game_sentiments %>% 
  filter(ave_sentiment != 0) %>% 
  group_by(voted_up,keywords, game) %>% 
  summarise(average_sentiment = median(ave_sentiment)) %>% 
  #Putting the plot
  ggplot(aes(x = keywords, y  = average_sentiment, fill = keywords)) + 
  lims(y = c(-0.5,0.5)) +
  geom_col(
    position="stack") +
  labs(
    x = NULL,
    y = "Average Sentiment",
    title = "Sentence sentiments",
    subtitle = "Sentence sentiments for selected keywords for the base game and the DLC",
    caption = "Created using sentimentr package"
  ) + 
  #Adding labels
  geom_text(
    aes(label = round(average_sentiment,2)),
    angle = 45,
    size = 3
  ) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5, angle=45),
        legend.position="none") +
  facet_wrap(~voted_up + game,
             ncol = 2,
             labeller = labeller(
               voted_up = c( `TRUE` = "Positive Reviews",
                             `FALSE` = "Negative Reviews")
             ))
