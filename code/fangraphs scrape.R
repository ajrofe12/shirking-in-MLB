library(tidyverse)
library(dplyr)
library(baseballr)
library(devtools)
setwd("/Users/aaronrofe/Documents/Thesis Data")

##### Scrape Batter data from Fangraphs #####

# Initialize an empty list to store data frames for each year
batter_list <- list()

# Loop through each year from 2011 to 2023
for (year in 2010:2024) {
  # Fetch batter data for the current year
  temp_data <- fg_batter_leaders(
    pos = "np", # No pitchers
    stats = "bat", #Batter stats
    startseason = as.character(year), 
    endseason = as.character(year), 
    ind = "1" # to not aggregate seasons together
  )
  
  
  # Append the data to the list
  batter_list[[as.character(year)]] <- temp_data
}

# Combine all the data frames in the list into a single data frame
batter_data <- bind_rows(batter_list)

# Write to csv

write.csv(batter_data, file = "batter_data_2010_2024.csv", row.names = FALSE)



##### Scrape Pitcher Data from Fangraphs #####

# Initialize an empty list to store data frames for each year
pitcher_list <- list()

# Loop through each year from 2011 to 2023
for (year in 2010:2024) {
  # Fetch pitcher data for the current year
  temp_data <- fg_pitcher_leaders(
    stats = "pit",# Only include qualified pitchers
    startseason = as.character(year), 
    endseason = as.character(year), 
    ind = "1"            # Aggregate stats across the season
  )
  
  # Add a column for the year
  temp_data$year <- year
  
  # Append the data to the list
  pitcher_list[[as.character(year)]] <- temp_data
}

# Combine all the data frames in the list into a single data frame
pitcher_data <- bind_rows(pitcher_list)

# Write the combined data to a CSV file
write.csv(pitcher_data, file = "pitcher_data_2011_2023.csv", row.names = FALSE)


fg_pitcher_leaders(startseason = 2023, endseason = 2023)


##### Scrape For Birth Places #####

# Initialize an empty data frame to store all the results
all_players_data <- data.frame()

# Loop through the years 2011-2023
for (year in 2011:2023) {
  # Fetch player data for the given year and sport_id = 1
  players_data <- mlb_sports_players(sport_id = 1, season = year)
  
  # Add a column for the year to keep track of the data source
  players_data <- players_data %>%
    mutate(season_year = year)
  
  # Combine the results into the main data frame
  all_players_data <- bind_rows(all_players_data, players_data)
}

# View the final combined data frame
print(all_players_data)

write.csv(all_players_data, file = "player_pob.csv", row.names = FALSE)











pks <- get_game_pks_mlb("2024-09-18", level_ids = 1)
