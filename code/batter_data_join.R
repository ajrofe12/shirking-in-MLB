setwd("/Users/aaronrofe/Documents/Thesis Data")
library(tidyverse)
library(xgboost)
library(tidymodels)
library(dplyr)

#Hiiter data 2010-2023
batter_data <- read_csv("batter_data_2010_2024.csv")
#Pitcher data 2010-2023
pitcher <- read_csv("pitcher_data_2010_2024.csv")
# Data with player place of birth
player_pob <- read_csv("player_pob.csv")
# County GDP
county_gdp <- read_csv("lagdp1223.csv")
# Player Contracts
all_contracts <- read_csv("all_contracts.csv")
# Country GDP for international GDP
international_gdp <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_31795.csv")
# Cities by each county 
city_county <- read_csv("uscities.csv")

### Joining Tables for batters####

# 1. Modify player_pob to include international column
player_pob <- player_pob %>% 
  mutate(international = if_else(birth_country != "USA", 1, 0))

# 2. Join player hometowns without filtering distinct player names
batter_data <- batter_data %>% 
  left_join(player_pob %>% dplyr::select(full_name, birth_city, birth_state_province, birth_country, international), 
            by = c("PlayerName" = "full_name"))

# 3. Create city_county_unique based on city_county data
city_county_unique <- city_county %>% 
  group_by(city_ascii, state_id) %>% 
  filter(population == max(population)) %>% 
  ungroup()

# 4. Join economics data for US players
batter_data_us <- batter_data %>% 
  filter(birth_country == "USA") %>% 
  left_join(city_county_unique %>% dplyr::select(city_ascii, state_id, county_name), 
            by = c("birth_city" = "city_ascii", "birth_state_province" = "state_id")) %>% 
  left_join(county_gdp %>% dplyr::select(county_name, rgdp_2022_thousands), 
            by = "county_name")

# 5. Join GDP for international players
international_players <- batter_data %>% 
  filter(international == 1)

# Filter the international GDP dataset for the year 2022
international_gdp_filtered <- international_gdp %>% 
  dplyr::select(`Country Name`, `2022`) %>% 
  rename(Country = `Country Name`, Value = `2022`)

# Ensure unique rows for international GDP data
international_gdp_unique <- international_gdp_filtered %>% 
  distinct(Country, Value, .keep_all = TRUE)

# Join filtered international GDP data with international players
international_players <- international_players %>% 
  left_join(international_gdp_unique, by = c("birth_country" = "Country"))

# 6. Combine both US and international player data without removing duplicates
batter_data_combined <- bind_rows(batter_data_us, international_players)

# 7. Rename columns for clarity
batter_data_combined <- batter_data_combined %>% 
  rename(International_GDP = Value, County_GDP = rgdp_2022_thousands)

# 8. Remove duplicate player + year combos
batter_data_combined <- batter_data_combined %>%
  distinct(PlayerName, Season, .keep_all = TRUE)

# Now, batter_data_combined retains all seasons while including the hometowns and GDPs


# Ensure the column names in all_contracts match for merging
all_contracts <- all_contracts %>%
  rename(PlayerName = Player,
         ContractLength = Yrs)  # Rename to match the naming in batter_data_combined

# Join the all_contracts data to batter_data_combined
batter_data_combined <- batter_data_combined %>%
  left_join(all_contracts %>% 
              dplyr::select(PlayerName, year, ContractLength, Start, End, Value, AAV), 
            by = c("PlayerName" = "PlayerName", "Season" = "year"))
# Filter out NAs in 
batter_data_combined <- batter_data_combined %>% 
  rename(Salary = AAV) %>% 
  filter(!is.na(Salary), !is.na(Value))



##### Join for the pitcher data #####

#Remove unneeded columns
batter_data_combined <- batter_data_combined %>%
  dplyr::select(-c(101:300))
# Calculate Lag WAR
batter_data <- batter_data %>%
  arrange(PlayerName, Season)

# Calculate lagged WAR for each player to get the change in WAR
batter_data <- batter_data %>%
  group_by(playerid) %>%
  mutate(WAR_change = WAR - dplyr::lag(WAR, 1)) %>%
  ungroup()

# Filter out 2010 rows since you won't need those in the final combined dataset
batter_data_original_filtered <- batter_data %>%
  filter(Season != 2009)

# Join the WAR_change back to the batter_data_combined
batter_data_combined <- batter_data_combined %>%
  left_join(batter_data_original_filtered %>% dplyr::select(PlayerName, Season, WAR_change),
            by = c("PlayerName" = "PlayerName", "Season" = "Season"))

batter_data_combined %>% 
  arrange(-WAR_change) %>% 
  dplyr::select(PlayerName, Season, WAR_change)

# Make the dataset smaller
batter_data_combined <- batter_data_combined %>% 
  dplyr::select(1:58, wOBA, WAR, wRC_plus, position, team_name_abb, teamid, xwOBA:HardHit_pct, 150:162)

batter_data_combined <- batter_data_combined %>%
  group_by(PlayerName, playerid) %>%
  arrange(PlayerName, Season) %>%
  mutate(new_team = if_else(team_name != dplyr::lag(team_name, default = first(team_name)), 1, 0)) %>%
  ungroup()

# To get years remaining on contract
batter_data_combined <- batter_data_combined %>%
  group_by(PlayerName, playerid, ContractLength, Start, End, Salary) %>%  # Group by contract-related columns
  arrange(PlayerName, Season) %>%
  mutate(
    Start = as.numeric(Start),  # Ensure Start is numeric
    End = as.numeric(End),  # Ensure End is numeric
    Season = as.numeric(Season),  # Ensure Season is numeric
    ContractLength = End - Start + 1,  # Calculate Contract Length as End - Start + 1
    years_remaining = if_else(Season >= Start & Season <= End, ContractLength - (Season - Start), NA_real_)  # Calculate years remaining for each season
  ) %>%
  ungroup()

# Update team_name to replace "- - -" with "2 teams"
batter_data_combined <- batter_data_combined %>%
  mutate(team_name = if_else(team_name == "- - -", "2 Teams", team_name))

batter_data_combined <- batter_data_combined %>%
  group_by(PlayerName, playerid, Start, End) %>%
  mutate(team_name_adjusted = case_when(
    n_distinct(team_name) == 2 ~ "2 Teams",        # Set to "2 Teams" if exactly two teams
    n_distinct(team_name) > 2 ~ "Multiple Teams",  # Set to "Multiple Teams" if more than two teams
    TRUE ~ first(team_name)                        # Keep original team name if only one team
  )) %>%
  ungroup()

batter_data_combined %>% filter(team_name_adjusted == "Multiple Teams")

batter_data_combined$international
#Average performance over contract span
# Calculate average player performance over the span of the contract
counting_stats <- c("G","AB", "PA", "H", "HR", "R", "RBI", "BB", "IBB", "SO", "HBP", "SF", "SH", "GDP", "SB", "CS", "WAR")
rate_stats <- c("AVG", "OBP", "SLG", "OPS", "wOBA", "wRC_plus", "BB_pct", "K_pct", "BB_K", "ISO", "BABIP", "GB_pct", "FB_pct", "LD_pct", "IFFB_pct", "HR_FB", "IFH_pct", "BUH_pct", "TTO_pct")


average_performance <- batter_data_combined %>%
  group_by(PlayerName, playerid, Bats, position, Start, End, ContractLength, Value, Salary,
           birth_city, birth_state_province, birth_country,
           international, county_name, County_GDP, International_GDP) %>%
  summarize(
    # Adjust team_name to reflect "Multiple Teams" if applicable
    team_name = if_else(n_distinct(team_name) > 1, "Multiple Teams", first(team_name)),
    
    # Regular means for counting stats
    across(all_of(counting_stats), mean, .names = "mean_{.col}"),
    
    # Sum of counting stats
    across(all_of(counting_stats), sum, .names = "total_{.col}"),
    
    # Weighted means for rate stats
    across(all_of(rate_stats), ~weighted.mean(.x, G, na.rm = TRUE), .names = "weighted_mean_{.col}"),
    
    # Age at the start of the contract
    age_at_signing = first(Age[Season == Start]),
    .groups = 'drop'
  ) %>%
  
  # Recode "2 Teams" to "Multiple Teams" in team_name
  mutate(team_name = if_else(team_name == "2 Teams", "Multiple Teams", team_name)) %>%
  
  arrange(PlayerName, Start) %>%
  dplyr::select(PlayerName, playerid, team_name, everything()) %>% 
  
  # Add new team dummy variable, indicating if player switched teams between contracts
  group_by(PlayerName) %>%
  mutate(new_team = if_else(team_name != dplyr::lag(team_name, default = first(team_name)), 1, 0)) %>%
  ungroup() %>%
  
  # Fill in NA for the last row of each group (where there’s no next team to compare to)
  mutate(new_team = if_else(is.na(new_team), 0, new_team)) %>%
  
  # Reorder columns as required
  dplyr::select(PlayerName, playerid, team_name, new_team, age_at_signing, everything())




ages <- average_performance %>% 
  dplyr::select(PlayerName, Start, age_at_signing) %>% 
  arrange(PlayerName, Start)




write_csv(batter_data_combined, "batter_data.csv")
write_csv(average_performance, "average_batter_performance.csv")

  