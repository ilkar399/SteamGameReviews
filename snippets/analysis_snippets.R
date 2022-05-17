#The old date chart
#Regrouping data and getting percentages of positive/negative reviews for each title/date
reviews_cleared %>%
  mutate(date = floor_date(timestamp_created,"month")) %>%
  group_by(date, game) %>% 
  mutate(review_count = n()) %>% 
  ungroup() %>% 
  group_by(date, game, voted_up, review_count) %>% 
  summarize(
    review_voted = n(),
  ) %>% 
  as_tibble() %>% 
  mutate(percentage = review_voted/review_count) %>% 
  select(date, game, voted_up, percentage) %>% 
  #Putting the plot
  ggplot(aes(x = date, y  = percentage, fill = factor(voted_up))) + 
  geom_col(
    position="stack") +
  labs(
    x = "Date",
    y = "Percentage",
    title = "Reviews by month"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  #Adding labels
  geom_text(
    aes(label = percent(percentage,accuracy = 1)),
    position = position_fill(vjust = 0.5),
    angle = 90,
    size = 3
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_blank()) + 
  scale_x_datetime(date_breaks = "1 month", date_labels="%b %y") + 
  facet_wrap(~game, ncol = 1)

#reviews by month - numbers
reviews_cleared %>%
  mutate(date = floor_date(timestamp_created,"month")) %>%
  group_by(date, game) %>% 
  mutate(review_count = n()) %>% 
  ungroup() %>% 
  group_by(date, game, voted_up, review_count) %>% 
  summarize(
    review_voted = n(),
  ) %>% 
  as_tibble() %>% 
  #  mutate(percentage = review_voted/review_count) %>% 
  select(date, game, voted_up, review_voted) %>% 
  #Putting the plot
  ggplot(aes(x = date, y  = review_voted, fill = factor(voted_up))) + 
  geom_col(
    position="stack") +
  labs(
    x = "Date",
    y = "Reviews",
    title = "Reviews by month"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  #  scale_y_continuous(labels=scales::percent) + 
  #Adding labels
  geom_text(
    aes(label = review_voted),
    position = position_stack(vjust = 0.5),
    size = 2
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.y = element_blank()) + 
  scale_x_datetime(date_breaks = "1 month", date_labels="%b %y") + 
  facet_wrap(~game, ncol = 1, scales = "free_y")

#reviews by date
ggplot(data = reviews_cleared, aes(x = game, fill = factor(voted_up))) + 
  geom_bar(
    position="fill") +
  labs(
    x = "Game",
    y = "Percentage",
    title = "Percentage of positive/negative reviews"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  #  geom_text(aes(label = ..count..), stat = "count", position = "fill")
  geom_text(aes(label = percent(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)],accuracy = 0.01)),
            stat = "count",
            position = position_fill(vjust = 0.5))

# Reviews - line plot
reviews_cleared %>% 
  mutate(date = floor_date(timestamp_created,"month")) %>% 
  filter(voted_up == TRUE) %>% 
  ggplot(aes(x=date, colour = game)) + 
  geom_line(stat="count") + 
  labs(
    x = "Game",
    y = "Percentage",
    title = "Percentage of positive reviews",
    color = "Game:"
  ) + 
  theme(legend.position = c(0.8, 0.8))

reviewsbydate_chart_data <- reviews_cleared %>%
  mutate(date = floor_date(timestamp_created,"month")) %>%
  group_by(date, game) %>% 
  mutate(review_count = n()) %>% 
  ungroup() %>% 
  group_by(date, game, voted_up, review_count) %>% 
  summarize(
    review_voted = n(),
  ) %>% 
  as_tibble() %>% 
  mutate(percentage = review_voted/review_count) %>% 
  select(date, game, voted_up, percentage)

reviewsbydate_chart_text <- reviewsbydate_chart_data %>% 
  group_by(game,voted_up) %>% 
  filter((percentage >= max(percentage) | percentage <= min(percentage)) 
         & voted_up == TRUE) %>% 
  mutate(label=ifelse(percentage == max(percentage),
                      paste0("Max rate - ",percent(percentage,accuracy = 0.01)),
                      paste0("Min rate - ", percent(percentage,accuracy = 0.01)))) %>% 
  ungroup

# Single line plot
reviewsbydate_chart_data %>% 
  filter(voted_up == TRUE) %>% 
  ggplot(aes(x = date, y  = percentage, colour=str_wrap(game,15))) + 
  geom_line(size = 2) +
  geom_point(size = 3) +
  labs(
    x = NULL,
    y = NULL,
    title = "Positive reviews %",
    subtitle = "By date for the base game and the DLC"
  ) +
  #  scale_fill_discrete(name = "Game:") + 
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) + 
  geom_text(data = reviewsbydate_chart_text,
            aes(label = label),
            check_overlap = TRUE,
            vjust="outward",
            hjust="inward",
            size = 4
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "right",
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.5, "cm")
  ) +
  scale_x_datetime(date_breaks = "1 month", date_labels="%b %y") +
  guides(color = guide_legend(title = "Game:", byrow = TRUE))

# Single fill plot
reviewsbydate_chart_data %>% 
  filter(voted_up == TRUE) %>% 
  ggplot(aes(x = date, y  = percentage, fill=game)) + 
  geom_area(stat="identity", position=position_dodge(width = 0.1)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Positive reviews %",
    subtitle = "By date for the base game and the DLC"
  ) +
  scale_fill_discrete(name = "Game:") + 
  scale_y_continuous(labels=scales::percent) + 
  geom_text(data = reviewsbydate_chart_text,
            aes(label = label),
            check_overlap = TRUE,
            vjust="inward",
            hjust="inward",
            size = 4
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        legend.direction = "vertical") +
  scale_x_datetime(date_breaks = "1 month", date_labels="%b %y")

# Multiple fill plots
reviewsbydate_chart_data %>% 
  ggplot(aes(x = date, y  = percentage, fill = factor(voted_up))) + 
  geom_area(
    position="stack") +
  labs(
    x = "Date",
    y = "Percentage",
    title = "Percentage of positive/negative reviews"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  geom_text(data = reviewsbydate_chart_text,
    aes(label = percent(percentage,accuracy = 0.01)),
    position = position_fill(vjust = 0.8),
    size = 3
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(date_breaks = "1 month", date_labels="%b %y") + 
  facet_wrap(~game, ncol = 1)  

reviews_cleared %>% 
  mutate(date = floor_date(timestamp_created,"month")
  ) %>% 
  ggplot(aes(x = date, fill = factor(voted_up))) + 
  geom_bar(
    position="fill") +
  labs(
    x = "Date",
    y = "Percentage",
    title = "Percentage of positive/negative reviews"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes(label = percent(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)],accuracy = 0.01)),
            stat = "count",
            position = position_fill(vjust = 0.5),
            angle = 90,
            size = 3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(date_breaks = "1 month") + 
  facet_wrap(~game, ncol = 1)

#review changed between game and dlc
reviews_cleared %>% 
  group_by(author.steamid) %>% 
  filter(n() == 2) %>% 
  select(author.steamid,game,voted_up) %>% 
  spread(game,voted_up) %>% 
  mutate(Vote_change = case_when(
    (`Crusader Kings III` & `Crusader Kings III: Royal Court`) ~ "Remained positive",
    (`Crusader Kings III`==TRUE & `Crusader Kings III: Royal Court`==FALSE) ~ "Changed from positive to negative",
    (`Crusader Kings III`==FALSE & `Crusader Kings III: Royal Court`==TRUE) ~ "Changed from negative to positive",
    (`Crusader Kings III`==FALSE & `Crusader Kings III: Royal Court`==FALSE) ~ "Remained negative",
  )) %>% 
  group_by(Vote_change) %>% 
  summarise(count = n()) %>% 
  mutate(Percentage = count/sum(count)) %>% 
  ggplot(aes(x=Vote_change, fill = Vote_change, y = Percentage)) + 
  geom_col() + 
  geom_text(aes(label = percent(Percentage)), vjust = -0.25) +
  scale_fill_discrete(name = "Rate changes:") +
  scale_y_continuous(labels=scales::percent) + 
  theme(axis.text.x = element_blank())

reviews_cleared <- reviews_cleared %>% 
  group_by(author.steamid) %>% 
  mutate(author.playtime_forever = max(author.playtime_forever)) %>% 
  mutate(author.playtime_last_two_weeks = max(author.playtime_last_two_weeks)) %>% 
  mutate(author.playtime_at_review = max(author.playtime_at_review)) %>% 
  mutate(author.last_played = max(author.last_played)) %>% 
  ungroup()

#updated version
#making data
#grouping by author
#spreading the game rate up/down
#summarising by game group
reviews_cleared %>% 
  group_by(author.steamid) %>% 
  filter(n() > 1) %>% 
  select(author.steamid,game,voted_up) %>% 
  spread(game,voted_up) %>% 
  mutate(vote_change01 = case_when(
    (!!sym(game_name) & !!sym(dlc1)) ~ "Remained positive",
    (!!sym(game_name)==TRUE & !!sym(dlc1)==FALSE) ~ "Changed to negative from positive",
    (!!sym(game_name)==FALSE & !!sym(dlc1)==TRUE) ~ "Changed to positive from negative",
    (!!sym(game_name)==FALSE & !!sym(dlc1)==FALSE) ~ "Remained negative",
  ),
  vote_change02 = case_when(
    (!!sym(game_name) & !!sym(dlc2)) ~ "Remained positive",
    (!!sym(game_name)==TRUE & !!sym(dlc2)==FALSE) ~ "Changed to negative from positive",
    (!!sym(game_name)==FALSE & !!sym(dlc2)==TRUE) ~ "Changed to positive from negative",
    (!!sym(game_name)==FALSE & !!sym(dlc2)==FALSE) ~ "Remained negative",
  ),
  vote_change12 = case_when(
    (!!sym(dlc1) & !!sym(dlc2)) ~ "Remained positive",
    (!!sym(dlc1)==TRUE & !!sym(dlc2)==FALSE) ~ "Changed to negative from positive",
    (!!sym(dlc1)==FALSE & !!sym(dlc2)==TRUE) ~ "Changed to positive from negative",
    (!!sym(dlc1)==FALSE & !!sym(dlc2)==FALSE) ~ "Remained negative",
  )) %>% 
  select(-!!sym(game_name),-!!sym(dlc1),-!!sym(dlc2)) %>% 
  pivot_longer(!author.steamid,
               names_to = "games_cat",
               values_to = "vote_change") %>% 
  filter(!is.na(vote_change)) %>%
  group_by(games_cat,vote_change) %>% 
  summarise(count = n(), .groups = "drop_last") %>% 
  mutate(Percentage = count/sum(count)) %>% 
#making plot
ggplot(aes(x=vote_change, fill = vote_change, y = Percentage)) + 
  geom_col() + 
  geom_text(aes(label = percent(Percentage)), vjust = -0.25) +
  scale_fill_discrete(name = "Rate changes:") +
  labs(
    title = "Vote rate changes",
    subtitle = "Reviewers changed their opinion between the base game and the DLCs"
  ) + 
  scale_x_discrete(labels = (\(x) gsub("\\s","\n",x))) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  facet_wrap(~games_cat,
             ncol = 1,
             labeller = labeller(
               games_cat = c(`vote_change01` = str_wrap(paste("From", game_name, "to", dlc1),40),
                        `vote_change02` = str_wrap(paste("From", game_name, "to", dlc2),40),
                        `vote_change12` = str_wrap(paste("From", dlc1, "to", dlc2),40))
              )
             )

#reviews by playtime
reviews_cleared %>%
  mutate(date = floor_date(timestamp_created,"month"),
         playtime_group = case_when(
           (author.playtime_forever == 0) ~ "Didn't review the base game",
           (author.playtime_forever < 600 & author.playtime_forever > 0) ~ "Less than 10 hours played total",
           (author.playtime_forever < 3000 & author.playtime_forever >= 600) ~"Betveen 10 and 50 hours played total",
           (author.playtime_forever >= 3000) ~"50 and more hours played total")) %>%
  group_by(playtime_group, game) %>% 
  mutate(review_count = n()) %>% 
  ungroup() %>% 
  group_by(playtime_group, game, voted_up, review_count) %>% 
  summarize(
    review_voted = n(),
  ) %>% 
  as_tibble() %>% 
  mutate(percentage = review_voted/review_count) %>% 
  select(playtime_group, game, voted_up, percentage) %>% 
  #Putting the plot
  ggplot(aes(x = playtime_group, y  = percentage, fill = factor(voted_up))) + 
  geom_col(
    position="stack") +
  labs(
    x = "Playtime",
    y = "Percentage",
    title = "Percentage of positive/negative reviews"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  #Adding labels
  geom_text(
    aes(label = percent(percentage,accuracy = 0.01)),
    position = position_fill(vjust = 0.5),
    size = 3
  ) + 
  scale_x_discrete(labels = (\(x) gsub("([^\\s]+\\s[^\\s]+)\\s","\\1\n",x))) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  facet_wrap(~game, ncol = 1)

#High weighted reviews
reviews_cleared %>% 
  filter(weighted_vote_score > 0.5) %>% 
  group_by(game) %>% 
  mutate(review_count = n()) %>% 
  ungroup() %>% 
  group_by(game, voted_up, review_count) %>% 
  summarize(
    review_voted = n(),
  ) %>% 
  as_tibble() %>% 
  mutate(percentage = review_voted/review_count) %>% 
  select(game, voted_up, percentage) %>% 
  #Putting the plot
  ggplot(aes(x = game, y  = percentage, fill = factor(voted_up))) + 
  geom_col(
    position="stack") +
  labs(
    x = "Game",
    y = "Percentage",
    title = "Percentage of positive/negative reviews"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  #Adding labels
  geom_text(
    aes(label = percent(percentage,accuracy = 0.01)),
    position = position_fill(vjust = 0.5),
    size = 3
  ) + 
  scale_x_discrete(labels = (\(x) gsub("([^\\s]+\\s[^\\s]+)\\s","\\1\n",x))) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))


#Players owning different number of games
reviews_cleared %>%
  mutate(gamesowned_group = case_when(
    (author.num_games_owned >= 0 & author.num_games_owned < 50) ~ "Less than 50 games owned",
    (author.num_games_owned >= 50 & author.num_games_owned < 100) ~ "Between 50 and 100 games owned",
    (author.num_games_owned >= 100 & author.num_games_owned < 150) ~ "Between 100 and 150 games owned",
    (author.num_games_owned >= 150) ~"150 and more games owned")) %>%
  group_by(gamesowned_group, game) %>% 
  mutate(review_count = n()) %>% 
  ungroup() %>% 
  group_by(gamesowned_group, game, voted_up, review_count) %>% 
  summarize(
    review_voted = n(),
  ) %>% 
  as_tibble() %>% 
  mutate(percentage = review_voted/review_count) %>% 
  select(gamesowned_group, game, voted_up, percentage) %>% 
  #Putting the plot
  ggplot(aes(x = gamesowned_group, y  = percentage, fill = factor(voted_up))) + 
  geom_col(
    position="stack") +
  labs(
    x = "Playtime",
    y = "Percentage",
    title = "Reviews by games owned"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  #Adding labels
  geom_text(
    aes(label = percent(percentage,accuracy = 0.01)),
    position = position_fill(vjust = 0.5),
    size = 3
  ) + 
  scale_x_discrete(labels = (\(x) gsub("([^\\s]+\\s[^\\s]+)\\s","\\1\n",x,perl=TRUE)))  + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_wrap(~game, ncol = 1)

#Reviews by the player writing activities
reviews_cleared %>%
  #  mutate(reviews_group = case_when(
  #    (author.num_reviews >= 0 & author.num_reviews < 10) ~ "Less than 10 games reviewed",
  #    (author.num_reviews >= 10 & author.num_reviews < 50) ~ "Between 10 and 50 games reviewed",
  #    (author.num_reviews >= 50) ~"50 and more games reviewed")) %>%
  group_by(group_10 = cut(author.num_reviews,breaks = c(0,10,20,30,40,50,60,70,80,90,100,Inf)), game) %>% 
  mutate(review_count = n()) %>% 
  ungroup() %>% 
  group_by(group_10, game, voted_up, review_count) %>% 
  summarize(
    review_voted = n(),
  ) %>% 
  as_tibble() %>% 
  mutate(percentage = review_voted/review_count) %>% 
  select(group_10, game, voted_up, percentage) %>% 
  #Putting the plot
  ggplot(aes(x = group_10, y  = percentage, fill = factor(voted_up))) + 
  geom_col(
    position="stack") +
  labs(
    x = "Playtime",
    y = "Percentage",
    title = "Reviews by total reviews written"
  ) + 
  scale_fill_discrete(name = "Review:", labels=c("Negative", "Positive")) + 
  scale_y_continuous(labels=scales::percent) + 
  #Adding labels
  geom_text(
    aes(label = percent(percentage,accuracy = 0.01)),
    position = position_fill(vjust = 0.5),
    size = 3
  ) + 
  scale_x_discrete(labels = (\(x) gsub("([^\\s]+\\s[^\\s]+)\\s","\\1\n",x,perl=TRUE)))  + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_wrap(~game, ncol = 1)
