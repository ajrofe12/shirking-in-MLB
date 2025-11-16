setwd("/Users/aaronrofe/Documents/Thesis Data")
library(tidyverse)
pitcher_data <- read_csv("pitcher_data.csv")
average_pitcher <- read_csv("average_pitcher_performance.csv")
pitcher_ages <- read_csv("player_age.csv")
batter_ages <- read_csv("batter_ages.csv")
batter_data <- read_csv("batter_data.csv")
average_batter <- read_csv("average_batter_performance.csv")

names(pitcher_data)
names(average_pitcher)
names(pitcher_ages)
names(batter_ages)
names(batter_data)
names(average_batter)


# Step 1: Clean player names in both batter and pitcher ages data
batter_ages <- batter_ages %>%
  mutate(Player = str_replace(Player, "[*#]$", ""))

pitcher_ages <- pitcher_ages %>%
  mutate(Player = str_replace(Player, "[*#]$", ""))

pitcher_data <- pitcher_data %>%
  left_join(pitcher_ages %>% select(Player, Age, Season), by = c("Name" = "Player", "Season" = "Season"))

average_pitcher <- average_pitcher %>%
  left_join(pitcher_ages %>% dplyr::select(Player, Season, Age), by = c("Name" = "Player", "Start" = "Season")) %>%
  rename(Age_at_signing = Age)

pitcher_data <- pitcher_data %>% 
  mutate(Team = if_else(Team == "- - -", "2 Teams", Team))

average_pitcher <- average_pitcher %>% 
  mutate(team_name = if_else(team_name == "- - -", "2 Teams", team_name))

pitcher_data <- pitcher_data %>%
  distinct(Name, Season, .keep_all = TRUE)

average_pitcher <- average_pitcher %>%
  distinct(Name, Start, End, .keep_all = TRUE)


# Step 2: Remove players with multiple teams (e.g., "2TM", "3TM") in team name for `batter_ages` and `pitcher_ages`
batter_ages <- batter_ages %>% 
  filter(!grepl("TM", Team))

pitcher_ages <- pitcher_ages %>% 
  filter(!grepl("TM", Team))

# Step 2: Identify players who played for multiple teams in a season, retaining only the most-played team
batter_ages_multiple_teams <- batter_ages %>% 
  group_by(Player, Year) %>% 
  filter(n_distinct(Team) > 1) %>% 
  mutate(most_played_team = Team[which.max(PA)]) %>% 
  ungroup() %>% 
  distinct(Player, Year, most_played_team)

pitcher_ages_multiple_teams <- pitcher_ages %>% 
  group_by(Player, Season) %>% 
  filter(n_distinct(Team) > 1) %>% 
  mutate(most_played_team = Team[which.max(G)]) %>% 
  ungroup() %>% 
  distinct(Player, Season, most_played_team)

# Step 3: Join cleaned player data (without rejoining Age)
# Join `pitcher_data` with `pitcher_ages_multiple_teams` to include the most played team for players who were on multiple teams
batter_data <- batter_data %>%
  left_join(batter_ages_multiple_teams, by = c("PlayerName" = "Player", "Season" = "Year")) %>%
  select(PlayerName, Season,team_name, most_played_team, everything())  # Keep relevant columns

# Join `pitcher_data` with `pitcher_ages_multiple_teams` to include the most played team for pitchers who were on multiple teams
pitcher_data <- pitcher_data %>%
  left_join(pitcher_ages_multiple_teams, by = c("Name" = "Player", "Season" = "Season")) %>%
  rename(most_played_team = most_played_team) %>%
  select(Name, Season,Team,  most_played_team, everything())  # Keep relevant columns

# Join `average_pitcher` with `pitcher_ages_multiple_teams` to include the most played team for pitchers who were on multiple teams
average_pitcher <- average_pitcher %>%
  left_join(pitcher_ages_multiple_teams, by = c("Name" = "Player", "Start" = "Season")) %>% 
  select(Name, team_name,  most_played_team, everything())

average_batter <- average_batter %>%
  left_join(batter_ages_multiple_teams, by = c("PlayerName" = "Player", "Start" = "Year")) %>%
  rename(most_played_team = most_played_team)

# Step 8: Finalize team name columns for `batter_data`, `pitcher_data`, and `average_pitcher`


write_csv(average_batter, "average_batter_performance.csv")
write_csv(batter_data, "batter_data.csv")
write_csv(pitcher_data, "pitcher_data.csv")
write_csv(average_pitcher, "average_pitcher_performance.csv")
