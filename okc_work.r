library(tidyverse)

awards <- read_csv("awards_data.csv")
player_data <- read_csv("player_stats.csv")
team_data <- read_csv("team_stats.csv")
rebounding_data <- read_csv("team_rebounding_data_22.csv")

## Part 1 Question 1

# Filter the awards dataset for the relevant seasons and awards
awards_filter <- awards %>%
  filter(season %in% 2007:2021,
         `All NBA First Team` == 1 | 
         `All NBA Second Team` == 1 |
         `All NBA Third Team` == 1 |
          all_star_game == 1)

# Join the filtered awards dataset with the player_data dataset
awards_players <- awards_filter %>%
  left_join(player_data, by = join_by(season, nbapersonid))

# Calculate the average points per game for each award category
ppg_1st_team <- awards_players %>%
  filter(`All NBA First Team` == 1) %>%
  summarize(ppg = mean(points / games))

ppg_2nd_team <- awards_players %>%
  filter(`All NBA Second Team` == 1) %>%
  summarize(ppg = mean(points / games))

ppg_3rd_team <- awards_players %>%
  filter(`All NBA Third Team` == 1) %>%
  summarize(ppg = mean(points / games))

ppg_all_star <- awards_players %>%
  filter(all_star_game == 1) %>%
  summarize(ppg = mean(points / games))

# Print the results in the requested format
cat(" 1st Team:", round(ppg_1st_team$ppg, 1), "points per game\n",
"2nd Team:", round(ppg_2nd_team$ppg, 1), "points per game\n",
"3rd Team:", round(ppg_3rd_team$ppg, 1), "points per game\n",
"All-Star:", round(ppg_all_star$ppg, 1), "points per game\n")

## Part 1 Question 2

# Filter the awards dataset for players who won at least one All NBA selection and were drafted in 2007 or later
# Only include the earliest observation for any given player
awards_draft <- awards %>%
  filter(`All NBA First Team` == 1 | 
         `All NBA Second Team` == 1 | 
         `All NBA Third Team` == 1) %>%
  group_by(nbapersonid) %>%
  slice_min(season) %>%
  ungroup() %>%
  left_join(player_data, by = join_by(season, nbapersonid)) %>%
  filter(draftyear >= 2007)

# Calculate the average number of years between rookie season and the first All NBA selection season
years_all_nba <- awards_draft %>%
  mutate(years = season - draftyear) %>%
  summarize(avg = mean(years))

# Print the result
cat(round(years_all_nba$avg, 1), "Years\n")

## Part 1 Data Cleaning Interlude

# Some players have played in different teams in a given season:
# we need to aggregate their stats in order to perform a join with the awards data and avoid a many-to-many relationship.
# We only aggregate the stats of relevance to this question and the modeling one
# When aggregating VORP, WS and efg, I take their weighted average over minutes,
# in order to reflect the portion of a season a player has spent playing in a given team.
# Take the last used player name in the dataset to avoid same nbapersonid, different name cases.
players_aggregated <- player_data %>%
  group_by(nbapersonid, season, draftyear) %>%
  summarize(games = sum(games),
            games_start = sum(games_start),
            VORP = sum(VORP*mins)/sum(mins),
            WS = sum(WS*mins)/sum(mins),
            efg = sum(efg*mins)/sum(mins),
            points = sum(points),
            mins = sum(mins),
            .groups = "drop") %>%
  left_join(select(player_data, nbapersonid, player),
            by = join_by(nbapersonid), multiple = "last") %>%
  relocate(player, .after = nbapersonid)

# Prepare the dataset by updating games and minutes for incomplete seasons
season_outcomes <- players_aggregated %>%
  left_join(awards, by = join_by(nbapersonid, season)) %>%
  mutate(games = case_when(
    season %in% c(2019, 2020) ~ round(games * (82/72)),
    season == 2011 ~ round(games * (82/66)),
    TRUE ~ games),
  games_start = case_when(
    season %in% c(2019, 2020) ~ round(games_start * (82/72)),
    season == 2011 ~ round(games_start * (82/66)),
    TRUE ~ games_start),
  mins = case_when(
    season %in% c(2019, 2020) ~ round(mins * (82/72)),
    season == 2011 ~ round(mins * (82/66)),
    TRUE ~ mins)) %>%
  # Create the Outcome column based on the given conditions and assign levels
  mutate(Outcome = case_when(
    `All NBA First Team` == 1 | 
    `All NBA Second Team` == 1 | 
    `All NBA Third Team` == 1 |
    `Most Valuable Player_rk` == 1 |
    `Defensive Player Of The Year_rk` == 1 ~ 1,

    all_star_game == 1 ~ 2,
    
    games_start >= 41 | mins >= 2000 ~ 3,
    
    mins >= 1000 ~ 4,

    mins >= 1 ~ 5,
    
    TRUE ~ 6
  )) %>% # Convert to factors
  mutate(Outcome = factor(Outcome, levels = 1:6,
    labels = c("Elite", "All-Star", "Starter",
    "Rotation", "Roster", "Out of the League")))

# Filter the dataset in order to remove the first 4 seasons in the dataset for each player
# Then, compute the career outcome based on the 2nd highest level any player has obtained
# (after the first 4 seasons in the dataset, which have been filtered out).
# Note that due to lack of pre-2007 season data, this means that career outcomes
# are only accurately computed for players drafted after or in 2007
career_outcomes <- season_outcomes %>%
  arrange(nbapersonid, season) %>%
  group_by(nbapersonid, draftyear) %>%
  slice(-(1:4)) %>%
  arrange(nbapersonid, Outcome) %>%
  summarize(Career = nth(Outcome, 2), .groups = "drop") %>%
  mutate(Career = replace_na(Career, "Out of the League"))

## Part 1 Question 3

# Filter by draft year and convert factors to numeric for ease of extraction
career_outcomes_2010 <- career_outcomes %>%
  filter(draftyear == 2010) %>%
  mutate(Career = as.numeric(Career))

# Count the number of players who have 2010 as their draft year (should be 73)
draft2010_players <- player_data %>% 
  filter(draftyear == 2010) %>% 
  distinct(nbapersonid) %>% 
  nrow()

# Print the results in the requested format
cat(" Elite:", sum(career_outcomes_2010$Career == 1), "players.\n",
"All-Star:", sum(career_outcomes_2010$Career == 2), "players.\n",
"Starter:", sum(career_outcomes_2010$Career == 3), "players.\n",
"Rotation:", sum(career_outcomes_2010$Career == 4), "players.\n",
"Roster:", sum(career_outcomes_2010$Career == 5), "players.\n",
"Out of League:", draft2010_players - sum(career_outcomes_2010$Career < 6), "players.\n")

## Open Ended Modeling Question

library(tidymodels) # Fundamental package for model fitting

# Re-compute career outcomes based on the 3rd best season after the first 4
# (as specified in the modeling question requirements)
career_outcomes_model <- season_outcomes %>%
  arrange(nbapersonid, season) %>%
  group_by(nbapersonid, draftyear) %>%
  slice(-(1:4)) %>%
  arrange(nbapersonid, Outcome) %>%
  summarize(Career = nth(Outcome, 3), .groups = "drop") %>%
  mutate(Career = replace_na(Career, "Out of the League"))

# Prepare the dataset for the modeling stage
# Only keep players drafted in or after 2007, for which we have complete data
# (we only have accurate career outcomes for players drafted post-2017)
model_ds <- season_outcomes %>%
  filter(draftyear >= 2007) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  arrange(nbapersonid, season) %>%
  group_by(nbapersonid) %>%
  mutate(season_number = row_number()) %>%
  filter(season_number <= 4) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(nbapersonid, player, draftyear),
              names_from = season_number,
              values_from = c(Outcome, VORP, WS, points, efg)) %>%
  left_join(career_outcomes_model, by = join_by(nbapersonid, draftyear))

model_ds %>% glimpse

set.seed(123) # set random seed for random forest generation

# Perform random forest imputation on the prediction dataset
imputed_ds <- model_ds %>%
  select(-Career) %>%
  missRanger::missRanger() %>%
  bind_cols(Career = model_ds$Career)

# Split in training and testing based on the requirements
# Do not include players with null career outcomes in the training set
training <- imputed_ds %>%
  drop_na(Career) %>%
  filter(draftyear <= 2015)

prediction <- imputed_ds %>%
  filter(draftyear >= 2018)

# Set a tidymodels data recipe
data_recipe <- recipe(Career ~ ., data = imputed_ds) %>%
  update_role(nbapersonid, player, draftyear,
              new_role = "ID")
  # update_role(contains(c("all","player","year","rookie","finals")),
  #             new_role = "Award") %>%
  # update_role(contains(c("games","mins")),
  #             new_role = "Games") %>%
  # step_normalize(all_integer()) %>%
  # step_relevel(all_factor(), ref_level = "Out of the League")

# Fit a random forest model
fit_rf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rand_forest(mode = "classification") %>%
  set_engine(engine = "ranger",
             num.threads = 6,
             importance = "impurity")) %>%
  fit(training)

# Plot variable importance scores
extract_fit_engine(fit_rf)$variable.importance %>% 
  tibble(Variable = names(.), Importance = .) %>%
  ggplot(aes(reorder(Variable, Importance), Importance, fill = Variable)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature", y = "Importance",
       title = "Variable Importance scores for random forest model fit") +
  guides(fill = "none")

# The plot below shows the distribution of minimal depth among the trees of the random forest
randomForestExplainer::plot_min_depth_distribution(
  extract_fit_engine(fit_rf), k = 20,
  main = "Distribution of mean and minimal depth for random forest model fit")

# Compute prediction results
prediction_results <- prediction %>%
  select(nbapersonid, player, draftyear) %>%
  mutate(class = predict(fit_rf, new_data = prediction, "class")$.pred_class,
         predict(fit_rf, new_data = prediction, type = "prob"))

# Show results for the 4 requested players
prediction_results %>%
  select(-nbapersonid,-draftyear) %>%
  filter(str_detect(player, "Shai|Zion|Wiseman|Giddey"))

prediction_results %>% View

# Print out a table containing prediction results
prediction_results %>%
  filter(draftyear >= 2019) %>%
  arrange(class, desc(`.pred_All-Star`)) %>%
  select(-nbapersonid) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  reactable::reactable(searchable = T, striped = T, outlined = T)

## Part 2 Question 1

# Filter the dataset for OKC's games (team = "OKC")
# And calculate the total number of offensive rebounds and offensive rebound chances for OKC
okc_oreb <- rebounding_data %>%
  filter(team == "OKC", game_number <= 80)

# Predict the offensive rebound percentage for game 81
predicted_rebound_pct <- sum(okc_oreb$offensive_rebounds) / sum(okc_oreb$off_rebound_chances)

# Print the result
cat(label_percent(0.1)(predicted_rebound_pct),"\n")

## Part 2 Question 2


## Part 2 Question 3

