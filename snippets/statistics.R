#library(car)
#library(AICcmodavg)

author_stats_data <- reviews_cleared %>% 
  #select columns
  select(game,
         author.num_games_owned,
         author.num_reviews,
         author.playtime_forever,
         voted_up) %>% 
  #remove outliers (3-sigma)
  filter(!(abs(author.num_games_owned - median(author.num_games_owned)) > 3*sd(author.num_games_owned))) %>%
  filter(!(abs(author.num_reviews - median(author.num_reviews)) > 3*sd(author.num_reviews))) %>% 
  filter(!(abs(author.playtime_forever - median(author.playtime_forever)) > 3*sd(author.playtime_forever)))

# Stats_table1
# add a game bitmask 
# group by author.steamid
# add columns by summing bitmasks
# ungroup
# group by game
# summarize 
#basegame_cleared$game_mask <- 1
#dlc1_cleared$game_mask <- 2
#dlc2_cleared$game_mask <- 4

summary_table <- reviews_cleared %>% 
  mutate(author.playtime_forever = ifelse(
    test = (author.playtime_forever == 0),
    yes = NA,
    no = author.playtime_forever
  )) %>% 
  #replace game with bitmask
  group_by(author.steamid) %>% 
  select(author.steamid,
         game,
         game_mask,
         voted_up,
         author.num_games_owned,
         author.num_reviews,
         author.playtime_forever
        ) %>% 
  mutate(game_mask_combo = sum(game_mask)) %>% 
  ungroup %>% 
  group_by(game) %>% 
  summarize(total = n(),
            voted_up = sum(voted_up == TRUE),
            author.median_num_games = median(author.num_games_owned),
            author.median_num_reviews = median(author.num_reviews),
            author.median_playtime = median(author.playtime_forever, na.rm = TRUE)/60,
            author.mean_num_games = mean(author.num_games_owned),
            author.mean_num_reviews = mean(author.num_reviews),
            author.mean_playtime = mean(author.playtime_forever, na.rm = TRUE)/60,
            games.only_basegame = sum(game_mask_combo == 1),
            games_base_dlc1 = sum(game_mask_combo == 3),
            games.only_dlc1 = sum(game_mask_combo == 2),
            games.only_dlc2 = sum(game_mask_combo == 4),
            games.base_dlc2 = sum(game_mask_combo == 5),
            games.dlc1_dlc2 = sum(game_mask_combo == 6),
            games.base_dlc2_dlc3 = sum(game_mask_combo == 7),
            )
# Boxplot
# data
qqplot_data <- author_stats_data  %>% 
  gather(key, value, -game)

# plot
ggplot(qqplot_data, aes(x=str_wrap(game,15),
                        y = value,
                        fill = str_wrap(game,15))) + 
  geom_boxplot(varwidth = TRUE, alpha=0.2) +
  theme(legend.position="none") +
  labs(
    x = "Game",
    y = "Value",
    title = "Box plot for review authors",
    subtitle = "Numbers of games owned, total reviews and playtime"
  ) + 
  guides(color = guide_legend(title = "Game:", byrow = TRUE)) +
  facet_wrap(~key,
             ncol = 1,
             scale = 'free',
             labeller = labeller(
               key = c( `author.num_games_owned` = "Games Owned",
                        `author.num_reviews` = "Total Reviews",
                        `author.playtime_forever` = "Total Playtime, minutes")
             )
  )

# ggstatplot
betweenstats1 <- ggbetweenstats(
  data = author_stats_data,
  x = game,
  y = author.num_games_owned,
  title = "By games owned",
  xlab = "Game",
  ylab = "Games Owned"
)
betweenstats2 <- ggbetweenstats(
  data = author_stats_data,
  x = game,
  y = author.num_reviews,
  title = "By number of reviews",
  xlab = "Game",
  ylab = "Total Reviews"
)
betweenstats3 <- ggbetweenstats(
  data = author_stats_data,
  x = game,
  y = author.playtime_forever,
  title = "By total playtime",
  xlab = "Game",
  ylab = "Total Playtime, minutes"
)

combine_plots(
  list(betweenstats1, betweenstats2, betweenstats3),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "Distribution tests for review authors",
    caption = "Using ggbetweenstats package"
  )
)

# QQ-Plots
qqplot_data <- reviews_cleared %>% 
  select(game,
         author.num_games_owned,
         author.num_reviews,
         author.playtime_forever) %>% 
  #remove outliers (3-sigma)
  filter(!(abs(author.num_games_owned - median(author.num_games_owned)) > 3*sd(author.num_games_owned))) %>%
  filter(!(abs(author.num_reviews - median(author.num_reviews)) > 3*sd(author.num_reviews))) %>% 
  filter(!(abs(author.playtime_forever - median(author.playtime_forever)) > 3*sd(author.playtime_forever))) %>% 
  gather(key, value, -game)

ggplot(
      data = qqplot_data,
      aes(sample = value,
          colour = str_wrap(game,15)),
  ) + 
  stat_qq() + 
  stat_qq_line() +
  theme_bw() +
  labs(
    title = "QQ-plot for review authors",
    subtitle = "Numbers of games owned, total reviews and playtime"
  ) + 
  guides(color = guide_legend(title = "Game:", byrow = TRUE)) +
  facet_wrap(~key,
             ncol = 1,
             scale = 'free',
             labeller = labeller(
               key = c( `author.num_games_owned` = "Games Owned",
                        `author.num_reviews` = "Total Reviews",
                        `author.playtime_forever` = "Total Playtime")
             )
  )

# Z-test

# ANOVA (probably shouldn't be used in this but just in case)
# data
anova_data <- reviews_cleared %>% 
  select(game,
         author.num_games_owned,
         author.num_reviews,
         author.playtime_forever)

# Edit from here
x <- which(names(anova_data) == "game") # name of grouping variable
y <- which(names(anova_data) == "author.num_games_owned" # names of variables to test
           | names(anova_data) == "author.num_reviews" |
             names(anova_data) == "author.playtime_forever")
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("Pathfinder: Wrath of the Righteous", "Pathfinder: Wrath of the Righteous - Inevitable Excess"),
                       c("Pathfinder: Wrath of the Righteous", "Pathfinder: Wrath of the Righteous - Through the Ashes")
                       ) # comparisons for post-hoc tests
# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(anova_data,
                   x = colnames(anova_data[j]), y = colnames(anova_data[i]),
                   color = colnames(anova_data[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter"
    )
    print(
      p + stat_compare_means(aes(label = paste0(..method..,
                                                ", p-value = ",
                                                ..p.format..)),
                             method = method1,
                             label.y = max(anova_data[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons,
                           method = method2,
                           label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}


# Checking dataset filtered by texts
summary_table_text <- reviews_cleared %>% 
  filter(language == "english") %>% 
  filter(nchar(review_text, type="chars") > 0) %>%
  mutate(author.playtime_forever = ifelse(
    test = (author.playtime_forever == 0),
    yes = NA,
    no = author.playtime_forever
  )) %>% 
  #replace game with bitmask
  group_by(author.steamid) %>% 
  select(author.steamid,
         game,
         game_mask,
         voted_up,
         author.num_games_owned,
         author.num_reviews,
         author.playtime_forever
  ) %>% 
  mutate(game_mask_combo = sum(game_mask)) %>% 
  ungroup %>% 
  group_by(game) %>% 
  summarize(total = n(),
            voted_up = sum(voted_up == TRUE),
            author.median_num_games = median(author.num_games_owned),
            author.median_num_reviews = median(author.num_reviews),
            author.median_playtime = median(author.playtime_forever, na.rm = TRUE)/60,
            games.only_basegame = sum(game_mask_combo == 1),
            games.base_dlc1 = sum(game_mask_combo == 3),
            games.only_dlc1 = sum(game_mask_combo == 2),
            games.only_dlc2 = sum(game_mask_combo == 4),
            games.base_dlc2 = sum(game_mask_combo == 5),
            games.dlc1_dlc2 = sum(game_mask_combo == 6),
            games.base_dlc2_dlc3 = sum(game_mask_combo == 7),
  )

text_test_data <- reviews_cleared %>% 
  #remove outliers (2-sigma)
  filter(!(abs(author.num_games_owned - median(author.num_games_owned)) > 2*sd(author.num_games_owned))) %>%
  filter(!(abs(author.num_reviews - median(author.num_reviews)) > 2*sd(author.num_reviews))) %>% 
  filter(!(abs(author.playtime_forever - median(author.playtime_forever)) > 2*sd(author.playtime_forever))) %>% 
  filter(language == "english") %>% 
  filter(nchar(review_text, type="chars") > 0) %>%
  select(
        game,
        author.num_games_owned,
        author.num_reviews,
        author.playtime_forever
  ) %>% 
  mutate(source = "Reviews with text")

text_test_data<-bind_rows(text_test_data,author_stats_data) %>% 
  replace_na(list(source = "All reviews"))

text_test_data %>% 
  select(-game) %>% 
  gather(key, value, -source) %>% 
#making plot
  ggplot(
    aes(sample = value,
        colour = source),
  ) + 
  stat_qq() + 
  stat_qq_line() +
  theme_bw() +
  labs(
    title = "QQ-plot for reviews with text",
    subtitle = "Numbers of games owned, total reviews and playtime"
  ) + 
  guides(color = guide_legend(title = "Datasets:",
                              byrow = TRUE,
                              )) +
  facet_wrap(~key,
             ncol = 1,
             scale = 'free',
             labeller = labeller(
               key = c( `author.num_games_owned` = "Games Owned",
                        `author.num_reviews` = "Total Reviews",
                        `author.playtime_forever` = "Total Playtime")
             )
  )
