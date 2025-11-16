setwd("/Users/aaronrofe/Documents/Thesis Data")
library(tidyverse)
library(xgboost)
library(tidymodels)
library(dplyr)

#Hiiter data 2010-2023
batter_data <- read_csv("batter_data_2010_2024.csv")
#Pitcher data 2010-2023
pitcher_data <- read_csv("pitcher_data_2010_2024.csv")
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

# 1. Modify player_pob to include international column
player_pob <- player_pob %>%  
  mutate(international = if_else(birth_country != "USA", 1, 0))

# 2. Join player hometowns without filtering distinct player names
pitcher_data <- pitcher_data %>%  
  left_join(player_pob %>% dplyr::select(full_name, birth_city, birth_state_province, birth_country, international),  
            by = c("Name" = "full_name"))

# 3. Create city_county_unique based on city_county data
city_county_unique <- city_county %>%  
  group_by(city_ascii, state_id) %>%  
  filter(population == max(population)) %>%  
  ungroup()

# 4. Join economics data for US players
pitcher_data_us <- pitcher_data %>%  
  filter(birth_country == "USA") %>%  
  left_join(city_county_unique %>% dplyr::select(city_ascii, state_id, county_name),  
            by = c("birth_city" = "city_ascii", "birth_state_province" = "state_id")) %>%  
  left_join(county_gdp %>% dplyr::select(county_name, rgdp_2022_thousands),  
            by = "county_name")

# 5. Join GDP for international players
international_pitchers <- pitcher_data %>%  
  filter(international == 1)

# Filter the international GDP dataset for the year 2022
international_gdp_filtered <- international_gdp %>%  
  dplyr::select(`Country Name`, `2022`) %>%  
  rename(Country = `Country Name`, Value = `2022`)

# Ensure unique rows for international GDP data
international_gdp_unique <- international_gdp_filtered %>%  
  distinct(Country, Value, .keep_all = TRUE)

# Join filtered international GDP data with international pitchers
international_pitchers <- international_pitchers %>%  
  left_join(international_gdp_unique, by = c("birth_country" = "Country"))

# 6. Combine both US and international player data without removing duplicates
pitcher_data_combined <- bind_rows(pitcher_data_us, international_pitchers)

# 7. Rename columns for clarity
pitcher_data_combined <- pitcher_data_combined %>%  
  rename(International_GDP = Value, County_GDP = rgdp_2022_thousands)

# 8. Remove duplicate player + year combos
pitcher_data_combined <- pitcher_data_combined %>%  
  distinct(Name, Season, .keep_all = TRUE)

# Ensure the column names in all_contracts match for merging
all_contracts <- all_contracts %>%  
  rename(PlayerName = Player,  
         ContractLength = Yrs)  # Rename to match the naming in pitcher_data_combined

# Join the all_contracts data to pitcher_data_combined
pitcher_data_combined <- pitcher_data_combined %>%  
  left_join(all_contracts %>%  
              dplyr::select(PlayerName, year, ContractLength, Start, End, Value, AAV),  
            by = c("Name" = "PlayerName", "Season" = "year"))

# Filter out NAs
pitcher_data_combined <- pitcher_data_combined %>%  
  rename(Salary = AAV) %>%  
  filter(!is.na(Salary), !is.na(Value))

# Calculate Lag WAR
pitcher_data <- pitcher_data %>%  
  arrange(Name, Season)

# Calculate lagged WAR for each player to get the change in WAR
pitcher_data <- pitcher_data %>%
  group_by(PlayerId) %>%
  arrange(Season) %>%
  mutate(WAR_change = WAR - dplyr::lag(WAR, 1),
         new_team = ifelse(Team != dplyr::lag(Team), 1, 0)) %>%
  mutate(new_team = replace_na(new_team, 0),
         WAR_change = replace_na(WAR_change, 0)) %>%  # Replace NA with 0
  ungroup()

# Filter out 2010 rows since you won't need those in the final combined dataset
pitcher_data_original_filtered <- pitcher_data %>%  
  filter(Season != 2009)

# Join the WAR_change back to the pitcher_data_combined
pitcher_data_combined <- pitcher_data_combined %>%  
  left_join(pitcher_data_original_filtered %>% dplyr::select(Name, Season, WAR_change, new_team),  
            by = c("Name" = "Name", "Season" = "Season"))

pitcher_data_combined <- pitcher_data_combined %>%
  distinct(Season, Name, Team, PlayerId, .keep_all = TRUE)

# To get years remaining on contract
pitcher_data_combined <- pitcher_data_combined %>%  
  group_by(Name, PlayerId, ContractLength, Start, End, Salary) %>%  # Group by contract-related columns
  arrange(Name, Season) %>%  
  mutate(  
    Start = as.numeric(Start),  # Ensure Start is numeric
    End = as.numeric(End),  # Ensure End is numeric
    Season = as.numeric(Season),  # Ensure Season is numeric
    ContractLength = End - Start + 1,  # Calculate Contract Length as End - Start + 1
    years_remaining = if_else(Season >= Start & Season <= End, ContractLength - (Season - Start), NA_real_)  # Calculate years remaining for each season
  ) %>%  
  ungroup()
names(pitcher_data_combined)
counting_stats_pitcher <- c("G", "GS", "W", "L", "SV", "IP", "WAR")
rate_stats_pitcher <- c("ERA", "K/9", "BB/9", "HR/9", "xERA", "FIP", "xFIP", "BABIP", "LOB%", "GB%", "HR/FB")

# Average performance over contract span for pitcher data
average_performance <- pitcher_data_combined %>%
  group_by(Name, PlayerId, Start, End, ContractLength, Value, Salary, 
           birth_city, birth_state_province, birth_country, 
           international, county_name, County_GDP, International_GDP) %>%
  summarize(
    # Adjust team_name to reflect "Multiple Teams" if applicable
    team_name = if_else(n_distinct(Team) > 1, "Multiple Teams", first(Team)),
    
    # Regular means for counting stats
    across(all_of(counting_stats_pitcher), mean, .names = "mean_{.col}"),
    
    # Sum for counting stats
    across(all_of(counting_stats_pitcher), sum, .names = "total_{.col}"),
    
    # Weighted means for rate stats, weighted by innings pitched (IP)
    across(all_of(rate_stats_pitcher), ~weighted.mean(.x, IP, na.rm = TRUE), .names = "weighted_mean_{.col}"),
    .groups = 'drop'
  ) %>%
  
  # Recode any "2 Teams" or "---" values in team_name to "Multiple Teams"
  mutate(team_name = if_else(team_name == "2 Teams" | team_name == "- - -", "Multiple Teams", team_name)) %>%
  
  arrange(Name, Start) %>%
  dplyr::select(Name, PlayerId, team_name, everything()) %>%
  
  # Add new team dummy variable, indicating if the player switched teams between contracts
  group_by(Name) %>%
  mutate(new_team = if_else(team_name != dplyr::lag(team_name, default = first(team_name)), 1, 0)) %>%
  ungroup() %>%
  
  # Fill in NA for the last row of each group (where there’s no next team to compare to)
  mutate(new_team = if_else(is.na(new_team), 0, new_team)) %>%
  
  # Reorder columns as required
  dplyr::select(Name, PlayerId, team_name, new_team, everything())

unique(pitcher_data_combined$Team)



write_csv(pitcher_data_combined, "pitcher_data.csv")
write_csv(average_performance, "average_pitcher_performance.csv")

innings <- average_performance %>% 
  select(Name, mean_IP)
