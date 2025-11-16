library(lmtest)
library(glmmML)
library(tidymodels)
library(prediction)
library(xgboost)
library(finetune)
library(interplot)
library(ggplot2)
library(stargazer)
library(plm)
library(tidyverse)
library(dplyr)
library(car)
library(texreg)
library(maps)
library(VGAM)
library(mgcv)
markets <- us.cities
canada <- canada.cities

setwd("/Users/aaronrofe/Documents/Thesis Data")
#Average performance over the course of the contract
#average_pitcher <- read_csv("average_pitcher_performance.csv")
#average_batter <- read_csv("average_batter_performance.csv")
# Panel datasets
batter_data <- read_csv("batter_data.csv")
pitcher_data <- read_csv("pitcher_data.csv")


# Step 1: Preprocess the `batter_data` and `pitcher_data` for "Multiple Teams"
batter_data <- batter_data %>%
  mutate(team_name = if_else(team_name == "2 Teams", "Multiple Teams", team_name))

pitcher_data <- pitcher_data %>%
  mutate(Team = if_else(Team == "2 Teams", "Multiple Teams", Team))

# Step 2: Create `multiple_teams` dummy variable for both datasets
batter_data <- batter_data %>%
  mutate(multiple_teams = if_else(team_name == "Multiple Teams", 1, 0))

pitcher_data <- pitcher_data %>%
  mutate(multiple_teams = if_else(Team == "Multiple Teams", 1, 0))

# Step 3: Fix `most_played_team` for both datasets (for market size)
batter_data <- batter_data %>%
  mutate(most_played_team = if_else(is.na(most_played_team), team_name, most_played_team))

pitcher_data <- pitcher_data %>%
  mutate(most_played_team = if_else(is.na(most_played_team), Team, most_played_team))

# Define stats vectors for batters and pitchers
counting_stats <- c("G", "AB", "PA", "H", "HR", "R", "RBI", "BB", "IBB", "SO", "HBP", "SF", "SH", "GDP", "SB", "CS", "WAR")
rate_stats <- c("AVG", "OBP", "SLG", "OPS", "wOBA", "wRC_plus", "BB_pct", "K_pct", "BB_K", "ISO", "BABIP", "GB_pct", "FB_pct", "LD_pct", "IFFB_pct", "HR_FB", "IFH_pct", "BUH_pct", "TTO_pct")
counting_stats_pitcher <- c("G", "GS", "W", "L", "SV", "IP", "WAR")
rate_stats_pitcher <- c("ERA", "K/9", "BB/9", "HR/9", "xERA", "FIP", "xFIP", "BABIP", "LOB%", "GB%", "HR/FB")

# Step 4: Calculate average performance for batters
# Counting and rate stats for batters and pitchers
### Average Batter Calculation:
names(batter_data)
average_batter <- batter_data %>% 
  # Step 1: Compute `average_batter` summary with adjustments
  group_by(PlayerName, playerid, Bats, position, Start, End, ContractLength, Value, Salary, 
           birth_city, birth_state_province, birth_country, international, county_name, 
           County_GDP, International_GDP, team_name) %>% 
  summarize(
    # Most played team based on PA (Plate Appearances)
    most_played_team = most_played_team[which.max(PA)],
    
    # Regular means for counting stats
    across(all_of(counting_stats), mean, .names = "mean_{.col}"),
    
    # Sum for counting stats
    across(all_of(counting_stats), sum, .names = "total_{.col}"),
    
    # Weighted means for rate stats, weighted by Games Played (G)
    across(all_of(rate_stats), ~weighted.mean(.x, G, na.rm = TRUE), .names = "weighted_mean_{.col}"),
    
    # Age at the start of the contract
    age_at_signing = first(Age[Season == Start]),
    
    .groups = 'drop'
  ) %>% 
  # Step 2: Adjust `team_name` based on whether player was on multiple teams
  mutate(
    team_name_adjusted = if_else(team_name == "2 Teams", "Multiple Teams", team_name),
    most_played_team = if_else(is.na(most_played_team), team_name_adjusted, most_played_team)
  ) %>%
  arrange(PlayerName, Start) %>% 
  dplyr::select(PlayerName, playerid, team_name_adjusted, everything()) %>%
  
  # Step 3: Add `new_team` and `multiple_teams` dummy variables
  group_by(PlayerName) %>%
  mutate(
    # Check if the player changed teams across contracts
    new_team = if_else(team_name_adjusted != dplyr::lag(team_name_adjusted, default = first(team_name_adjusted)), 1, 0),
    new_team = if_else(is.na(new_team), 0, new_team)  # Fill NA for the first contract entry
  ) %>%
  # Add a dummy variable for multiple teams
  mutate(multiple_teams = if_else(team_name_adjusted == "Multiple Teams", 1, 0)) %>%
  ungroup() %>%
  mutate(most_played_team = if_else(is.na(most_played_team), team_name_adjusted, most_played_team))

### Average Pitcher Calculation:
average_pitcher <- pitcher_data %>% 
  group_by(Name, PlayerId, Start, End, ContractLength, Value, Salary,
           birth_city, birth_state_province, birth_country,
           international, county_name, County_GDP, International_GDP) %>% 
  reframe(
    # Identify the team with the most games played
    most_played_team = if_else(
      any(Team == "Multiple Teams"), 
      first(most_played_team),  # Use most_played_team if "Multiple Teams"
      first(Team)  # Otherwise, use the actual Team value
    ),
    
    # Regular means for counting stats
    across(all_of(counting_stats_pitcher), mean, .names = "mean_{.col}"),
    
    # Sum for counting stats
    across(all_of(counting_stats_pitcher), sum, .names = "total_{.col}"),
    
    # Weighted means for rate stats, weighted by innings pitched (IP)
    across(all_of(rate_stats_pitcher), ~weighted.mean(.x, IP, na.rm = TRUE), .names = "weighted_mean_{.col}"),
    
    # Age at the start of the contract
    age_at_signing = first(Age[Season == Start]),
    
    .groups = 'drop'
  ) %>% 
  # Step 1: Create `multiple_teams` column after summarizing data
  mutate(multiple_teams = if_else(most_played_team == "Multiple Teams", 1, 0)) %>% 
  
  # Step 2: Adjust `team_name` based on multiple teams
  mutate(
    team_name = if_else(multiple_teams == 1, "Multiple Teams", most_played_team),
    most_played_team = if_else(is.na(most_played_team), team_name, most_played_team)
  ) %>% 
  arrange(Name, Start) %>% 
  dplyr::select(Name, PlayerId, team_name, everything()) %>% 
  
  # Step 3: Add `new_team` dummy variable for pitcher data
  group_by(Name) %>% 
  mutate(
    new_team = if_else(team_name != dplyr::lag(team_name, default = first(team_name)), 1, 0)
  ) %>% 
  ungroup() %>% 
  mutate(new_team = if_else(is.na(new_team), 0, new_team)) %>% 
  
  # Step 4: Ensure both `average_batter` and `average_pitcher` have consistent `multiple_teams` and `most_played_team`
  mutate(
    multiple_teams = if_else(team_name == "Multiple Teams", 1, 0),
    most_played_team = if_else(is.na(most_played_team), team_name, most_played_team)
  )
##### Add in the arb data #####
arb <- read.csv("arbitration_data.csv") %>%
  # Create 'arb_deal' and 'last_arb_year' columns based on 'Arb Level'
  mutate(
    arb_deal = 1,
    last_arb_year = if_else(Arb.Level %in% c("Arb 3", "Arb 4 (S2)"), 1, 0)
  ) %>%
  rename(PlayerName = `Player..1966.`) # Adjust to match join keys in `average_batter` and `average_pitcher`

average_batter <- average_batter %>%
  left_join(arb %>% dplyr::select(PlayerName, Arb.Year, arb_deal, last_arb_year), by = c("PlayerName" = "PlayerName", "Start" = "Arb.Year")) %>%
  mutate(
    arb_deal = if_else(is.na(arb_deal), 0, arb_deal),  # Fill NA with 0
    last_arb_year = if_else(is.na(last_arb_year), 0, last_arb_year)  # Fill NA with 0
  )

# Join arbitration data into `average_pitcher`
average_pitcher <- average_pitcher %>%
  left_join(arb %>%  dplyr::select(PlayerName, Arb.Year, arb_deal, last_arb_year), by = c("Name" = "PlayerName", "Start" = "Arb.Year")) %>%
  mutate(
    arb_deal = if_else(is.na(arb_deal), 0, arb_deal),  # Fill NA with 0
    last_arb_year = if_else(is.na(last_arb_year), 0, last_arb_year)  # Fill NA with 0
  )

average_batter <- average_batter %>%
  group_by(PlayerName, playerid) %>%
  arrange(PlayerName, playerid, Start) %>%
  mutate(
    prev_last_arb_year = dplyr::lag(last_arb_year, default = 0),  # If the previous year was their last year of arb
    contract_type_arb_to_fa = if_else(prev_last_arb_year == 1 & last_arb_year == 0, 1, 0),
    contract_type_fa_to_fa = if_else(prev_last_arb_year == 0 & last_arb_year == 0, 1, 0)
  ) %>%
  ungroup()

# For `average_pitcher`
average_pitcher <- average_pitcher %>%
  group_by(Name, PlayerId) %>%
  arrange(Name, PlayerId, Start) %>%
  mutate(
    prev_last_arb_year = dplyr::lag(last_arb_year, default = 0),  # Default to 0 if no previous contract
    contract_type_arb_to_fa = if_else(prev_last_arb_year == 1 & last_arb_year == 0, 1, 0),
    contract_type_fa_to_fa = if_else(prev_last_arb_year == 0 & last_arb_year == 0, 1, 0)
  ) %>%
  ungroup()

# Add arb data into the panel data
# Join arbitration data into `batter_data`
batter_data <- batter_data %>%
  left_join(arb %>% dplyr::select(PlayerName, Arb.Year, arb_deal, last_arb_year), 
            by = c("PlayerName" = "PlayerName", "Start" = "Arb.Year")) %>%
  # Fill missing values in 'arb_deal' and 'last_arb_year' with 0
  mutate(
    arb_deal = if_else(is.na(arb_deal), 0, arb_deal),  # Fill NA with 0
    last_arb_year = if_else(is.na(last_arb_year), 0, last_arb_year)  # Fill NA with 0
  ) %>%
  # Create contract type dummies for `batter_data`
  group_by(PlayerName, playerid) %>%
  arrange(PlayerName, playerid, Start) %>%
  mutate(
    prev_last_arb_year = dplyr::lag(last_arb_year, default = 0),  # Default to 0 if no previous contract
    contract_type_arb_to_fa = if_else(prev_last_arb_year == 1 & last_arb_year == 0, 1, 0),  # Arb-to-FA
    contract_type_fa_to_fa = if_else(prev_last_arb_year == 0 & last_arb_year == 0, 1, 0)  # FA-to-FA
  ) %>%
  ungroup()

# Join arbitration data into `pitcher_data`
pitcher_data <- pitcher_data %>%
  left_join(arb %>% dplyr::select(PlayerName, Arb.Year, arb_deal, last_arb_year), 
            by = c("Name" = "PlayerName", "Start" = "Arb.Year")) %>%
  # Fill missing values in 'arb_deal' and 'last_arb_year' with 0
  mutate(
    arb_deal = if_else(is.na(arb_deal), 0, arb_deal),  # Fill NA with 0
    last_arb_year = if_else(is.na(last_arb_year), 0, last_arb_year)  # Fill NA with 0
  ) %>%
  # Create contract type dummies for `pitcher_data`
  group_by(Name, PlayerId) %>%
  arrange(Name, PlayerId, Start) %>%
  mutate(
    prev_last_arb_year = dplyr::lag(last_arb_year, default = 0),  # Default to 0 if no previous contract
    contract_type_arb_to_fa = if_else(prev_last_arb_year == 1 & last_arb_year == 0, 1, 0),  # Arb-to-FA
    contract_type_fa_to_fa = if_else(prev_last_arb_year == 0 & last_arb_year == 0, 1, 0)  # FA-to-FA
  ) %>%
  ungroup()


#### Add in the market sizes #####
team_city_mapping <- data.frame(
  team_name = c("LAD", "MIA", "SDP", "CHW", "TEX", "ATL", "NYY", "ARI", "MIL", "KCR",
                "BOS", "WSN", "CLE", "PIT", "SEA", "BAL", "OAK", "CHC", "NYM", "LAA",
                "STL", "DET", "MIN", "HOU", "SFG", "COL", "PHI", "TBR", "CIN"),
  city_name = c("Los Angeles CA", "Miami FL", "San Diego CA", "Chicago IL", "Arlington TX", "Atlanta GA",
                "New York NY", "Phoenix AZ", "Milwaukee WI", "Kansas City MO", "Boston MA", "Washington DC",
                "Cleveland OH", "Pittsburgh PA", "Seattle WA", "Baltimore MD", "Oakland CA", "Chicago IL",
                "New York NY", "Los Angeles CA", "Saint Louis MO", "Detroit MI", "Minneapolis MN", "Houston TX",
                "San Francisco CA", "Denver CO", "Philadelphia PA", "Tampa FL", "Cincinnati OH")  # Excluded Toronto
)

# Ensure city names are in lowercase with no extra spaces
team_city_mapping <- team_city_mapping %>%
  mutate(city_name = tolower(trimws(city_name)))

markets <- markets %>%
  mutate(name = tolower(trimws(name)))

# Load Canadian cities data from maps package
canada <- maps::canada.cities

# Clean up the city names to lowercase and remove any extra spaces
canada <- canada %>%
  mutate(name = tolower(trimws(name)))

# Filter for Toronto, Ontario
toronto_data <- canada %>%
  filter(name == "toronto on")  # Filter specifically for "toronto on"

# Debug: Check if Toronto was found
print(toronto_data)

# Initialize team_population with the existing US data
team_population <- team_city_mapping %>%
  left_join(markets, by = c("city_name" = "name")) %>%
  dplyr::select(team_name, city_name, pop)

# Add Toronto's population directly to team_population
toronto_population <- toronto_data$pop[1]  # Get the population for Toronto

# Add Toronto's population directly to team_population
team_population <- team_population %>%
  bind_rows(data.frame(team_name = "TOR", city_name = "toronto", pop = toronto_population))

# Debug: Print team_population to check if Toronto is included
print(team_population)

# Merge population data into batter_data
batter_data <- batter_data %>% 
  left_join(team_population, by = c("most_played_team" = "team_name"))

# Rename 'Team' to 'team_name' in pitcher_data
pitcher_data <- pitcher_data %>%
  rename(team_name = Team)

# Merge population data into pitcher_data
pitcher_data <- pitcher_data %>%
  left_join(team_population, by = c("most_played_team" = "team_name"))

average_batter <- average_batter %>% 
  left_join(team_population %>% dplyr::select(team_name, pop), by = c("most_played_team" = "team_name"))

# Add market size to average_pitcher using most_played_team
average_pitcher <- average_pitcher %>% 
  left_join(team_population %>% dplyr::select(team_name, pop), by = c("most_played_team" = "team_name"))

#Rename population to market size and convert salary and total contract value into millions
batter_data <- batter_data %>%
  rename(market_size = pop) %>%
  mutate(Value = Value / 1e6, Salary = Salary / 1e6)

pitcher_data <- pitcher_data %>%
  rename(market_size = pop) %>%
  mutate(Value = Value / 1e6, Salary = Salary / 1e6)

average_batter <- average_batter %>%
  rename(market_size = pop) %>%
  mutate(Value = Value / 1e6, Salary = Salary / 1e6)

# For average_pitcher, also rename Age_at_signing:
average_pitcher <- average_pitcher %>%
  rename(market_size = pop) %>%
  mutate(Value = Value / 1e6, Salary = Salary / 1e6)


# Data to model changes between first and last years of contracts
##### Batter first and last year #####
contract_change_batter <- batter_data %>%
  group_by(PlayerName, playerid) %>%
  arrange(PlayerName, playerid, Season) %>%
  
  # Keep the last year of each contract and the first year of the next contract
  mutate(
    last_year = if_else(Season == End, 1, 0),   # Flag for last year of the contract
    first_year = if_else(Season == Start, 1, 0) # Flag for first year of the contract
  ) %>%
  
  # Use lag to get the WAR from the last year of the previous contract
  mutate(
    previous_WAR = dplyr::lag(WAR, order_by = Season), 
    previous_Salary = dplyr::lag(Salary, order_by = Season),        # Salary from last year of previous contract
    previous_ContractLength = dplyr::lag(ContractLength, order_by = Season), # Contract Length from last year
    previous_MarketSize = dplyr::lag(market_size, order_by = Season), # Market size from last year of previous contract
    previous_Age_at_signing = dplyr::lag(Age, order_by = Season),# WAR from last year of previous contract
    previous_Season = dplyr::lag(Season, order_by = Season), # Previous contract season
    previous_End = dplyr::lag(End, order_by = Season),        # Previous contract end
    previous_G = dplyr::lag(G, order_by = Season),
    previous_PA = dplyr::lag(PA, order_by = Season),
    previous_HR = dplyr::lag(HR, order_by = Season),
    previous_OBP = dplyr::lag(OBP, order_by = Season),
    previous_SLG = dplyr::lag(SLG, order_by = Season),
    previous_ISO = dplyr::lag(ISO, order_by = Season),
    previous_BB_pct = dplyr::lag(BB_pct, order_by = Season),
    previous_K_pct = dplyr::lag(K_pct, order_by = Season),
    previous_OPS = dplyr::lag(OPS, order_by = Season),
    previous_wOBA = dplyr::lag(wOBA, order_by = Season),
    previous_wRC_plus = dplyr::lag(wRC_plus, order_by = Season),
    previous_EV = dplyr::lag(EV, order_by = Season),
    previous_LA = dplyr::lag(LA, order_by = Season),
    previous_Barrel_pct = dplyr::lag(Barrel_pct, order_by = Season),
    previous_HardHit_pct = dplyr::lag(HardHit_pct, order_by = Season),
    WAR_change = WAR - previous_WAR,
    Salary_change = Salary - previous_Salary
  ) %>%
  
  # Only keep rows where the current season is the first year of a contract, and the previous season was the last year of a contract
  filter(first_year == 1 & !is.na(previous_WAR)) %>%
  
  # Ensure the seasons are consecutive (gap of 1 year between contracts)
  filter(Season - previous_Season == 1) %>% 
  filter(arb_deal != 1)
##### First and last year pitchers ##### 
contract_change_pitchers <- pitcher_data %>%
  group_by(Name, PlayerId) %>%
  arrange(Name, PlayerId, Season) %>%
  
  # Flag the last year and the first year of each contract
  mutate(
    last_year = if_else(Season == End, 1, 0),   # Flag for last year of the contract
    first_year = if_else(Season == Start, 1, 0) # Flag for first year of the contract
  ) %>%
  
  # Use lag to get the WAR, salary, contract length, market size, and age from the last year of the previous contract
  mutate(
    previous_WAR = dplyr::lag(WAR, order_by = Season),              # WAR from last year of previous contract
    previous_Salary = dplyr::lag(Salary, order_by = Season),        # Salary from last year of previous contract
    previous_ContractLength = dplyr::lag(ContractLength, order_by = Season), # Contract Length from last year
    previous_MarketSize = dplyr::lag(market_size, order_by = Season), # Market size from last year of previous contract
    previous_Age_at_signing = dplyr::lag(Age, order_by = Season), # Age at signing of previous contract
    previous_G = dplyr::lag(G, order_by = Season),
    previous_W = dplyr::lag(W, order_by = Season),
    previous_L = dplyr::lag(L, order_by = Season),
    previous_IP = dplyr::lag(IP, order_by = Season),
    previous_k_9 = dplyr::lag(`K/9`, order_by = Season),
    previous_BB_9 = dplyr::lag(`BB/9`, order_by = Season),
    previous_SV = dplyr::lag(SV, order_by = Season),
    previous_GS = dplyr::lag(GS, order_by = Season),
    previous_FIP = dplyr::lag(FIP, order_by = Season),
    previous_xFIP = dplyr::lag(xFIP, order_by = Season),
    previous_ERA = dplyr::lag(ERA, order_by = Season),
    previous_Season = dplyr::lag(Season, order_by = Season),        # Previous contract season
    previous_End = dplyr::lag(End, order_by = Season),             # Previous contract end year
    WAR_change = WAR - previous_WAR,
    Salary_change = Salary - previous_Salary
  ) %>%
  
  # Only keep rows where the current season is the first year of a contract, and the previous season was the last year of a contract
  filter(first_year == 1 & !is.na(previous_WAR)) %>%
  
  # Ensure the seasons are consecutive (gap of 1 year between contracts)
  filter(Season - previous_Season == 1) %>% 
  filter(arb_deal != 1)

##### Modeling #####
dim(contract_change_batter)
dim(contract_change_pitchers)

contract_change_batter <- contract_change_batter %>%
  mutate(
    SalaryChange = Salary_change,
    ContractLength = ContractLength,
    MarketSize = market_size,
    NewTeam = new_team,
    MultipleTeams = multiple_teams,
    International = international,
    ContractTypeFAtoFA = contract_type_fa_to_fa
  )

# Rename variables in the pitcher dataset
contract_change_pitchers <- contract_change_pitchers %>%
  mutate(
    SalaryChange = Salary_change,
    ContractLength = ContractLength,
    MarketSize = market_size,
    NewTeam = new_team,
    MultipleTeams = multiple_teams,
    International = international,
    ContractTypeFAtoFA = contract_type_fa_to_fa
  ) 

##### Create dataset with no outliers
Q1_batter <- quantile(contract_change_batter$Salary, 0.25, na.rm = TRUE)
Q3_batter <- quantile(contract_change_batter$Salary, 0.75, na.rm = TRUE)
IQR_batter <- Q3_batter - Q1_batter
lower_batter <- Q1_batter - 1.5 * IQR_batter
upper_batter <- Q3_batter + 1.5 * IQR_batter

# Filter out outliers for Batters
contract_change_batter_no_outliers <- contract_change_batter %>%
  filter(Salary >= lower_batter & Salary <= upper_batter)

# For Pitchers: Remove Salary outliers based on IQR
Q1_pitcher <- quantile(contract_change_pitchers$Salary, 0.25, na.rm = TRUE)
Q3_pitcher <- quantile(contract_change_pitchers$Salary, 0.75, na.rm = TRUE)
IQR_pitcher <- Q3_pitcher - Q1_pitcher
lower_pitcher <- Q1_pitcher - 1.5 * IQR_pitcher
upper_pitcher <- Q3_pitcher + 1.5 * IQR_pitcher

contract_change_pitchers_no_outliers <- contract_change_pitchers %>%
  filter(Salary >= lower_pitcher & Salary <= upper_pitcher)


### change between last and first year of contract
gam_batter_first_last <- gam(
  WAR_change ~ s(Age) + SalaryChange + ContractLength +
    MarketSize + NewTeam + MultipleTeams + 
    NewTeam * MarketSize + International + ContractTypeFAtoFA,
  data = contract_change_batter,
  method = "REML"
)

# GAM model for pitchers
gam_pitcher_first_last <- gam(
  WAR_change ~ s(Age, k = 3) + SalaryChange +
    ContractLength + MarketSize +
    NewTeam + MultipleTeams +
    NewTeam * MarketSize + International + ContractTypeFAtoFA,
  data = contract_change_pitchers,
  method = "REML"
)

# GAM for batters no outliers
gam_batter_first_last_no_outliers <- gam(
  WAR_change ~ s(Age) + SalaryChange + ContractLength +
    MarketSize + NewTeam + MultipleTeams + 
    NewTeam * MarketSize + International + ContractTypeFAtoFA,
  data = contract_change_batter_no_outliers,
  method = "REML"
)

# GAM model for pitchers (without salary outliers)

gam_pitcher_first_last_no_outliers <- gam(
  WAR_change ~ s(Age, k = 3) + SalaryChange +
    ContractLength + MarketSize +
    NewTeam + MultipleTeams +
    NewTeam * MarketSize + International + ContractTypeFAtoFA,
  data = contract_change_pitchers_no_outliers,
  method = "REML"
)

# Display results with custom names
screenreg(
  list(gam_batter_first_last, gam_pitcher_first_last, gam_batter_first_last_no_outliers, gam_pitcher_first_last_no_outliers),
  digits = 3,
  single.row = TRUE,
  custom.model.names = c("Batters", "Pitchers", "Batters (No Outliers)", "Pitchers (No Outliers)")
)

# Export the regression results as HTML
htmlreg(
  list(gam_batter_first_last, gam_pitcher_first_last, gam_batter_first_last_no_outliers, gam_pitcher_first_last_no_outliers),
  file = "regression_output_no_outliers.html",
  digits = 3,
  custom.model.names = c("Batters", "Pitchers", "Batters (No Outliers)", "Pitchers (No Outliers)")
)


##### Compare RMSE of Restricted and Un-Restricted models
library(caret)
library(mgcv)

# Split for batters 80/20
set.seed(42)
batter_split <- createDataPartition(contract_change_batter$WAR_change, p = 0.8, list = FALSE)
train_data_batter <- contract_change_batter[batter_split,]
test_data_batter <- contract_change_batter[-batter_split,]

# 80/20 Split for pitchers
pitcher_split <- createDataPartition(contract_change_pitchers$WAR_change, p = 0.8, list = FALSE)
train_data_pitcher <- contract_change_pitchers[pitcher_split,]
test_data_pitcher <- contract_change_pitchers[-pitcher_split,]


#Define the unrestricted model
unrestricted_model <- WAR_change ~ s(Age) + Salary_change + ContractLength +
  market_size + new_team + multiple_teams + 
  new_team * market_size + international + contract_type_fa_to_fa

# Restricted model
restricted_model <- WAR_change ~ s(Age) +
  market_size + new_team + multiple_teams + 
  new_team * market_size + international + contract_type_fa_to_fa

#Train and test for batters
# Full model
batter_full_model <- gam(unrestricted_model, data = train_data_batter, method = "REML")
test_data_batter$predicted_full <- predict(batter_full_model, newdata = test_data_batter)

#Reduced Model
batter_reduced_model <- gam(restricted_model, data = train_data_batter, method = "REML")
test_data_batter$predicted_reduced <- predict(batter_reduced_model, newdata = test_data_batter)

# RMSE for batters
batter_rmse_full <- sqrt(mean((test_data_batter$WAR_change - test_data_batter$predicted_full)^2, na.rm = TRUE))
batter_rmse_reduced <- sqrt(mean((test_data_batter$WAR_change - test_data_batter$predicted_reduced)^2, na.rm = TRUE))

# Train and test for pitchers
# Full model
pitcher_full_model <- gam(unrestricted_model, data = train_data_pitcher, method = "REML")
test_data_pitcher$predicted_full <- predict(pitcher_full_model, newdata = test_data_pitcher)

#Reduced Model
pitcher_reduced_model <- gam(restricted_model, data = train_data_pitcher, method = "REML")
test_data_pitcher$predicted_reduced <- predict(pitcher_reduced_model, newdata = test_data_pitcher)

#RMSE for pitchers
pitcher_rmse_full <- sqrt(mean((test_data_pitcher$WAR_change - test_data_pitcher$predicted_full)^2, na.rm = TRUE))
pitcher_rmse_reduced <- sqrt(mean((test_data_pitcher$WAR_change - test_data_pitcher$predicted_reduced)^2, na.rm = TRUE))

# Output results
cat("Batter RMSE - Full Model:", batter_rmse_full, "\n")
cat("Batter RMSE - Reduced Model:", batter_rmse_reduced, "\n")
cat("Pitcher RMSE - Full Model:", pitcher_rmse_full, "\n")
cat("Pitcher RMSE - Reduced Model:", pitcher_rmse_reduced, "\n")

# Contract change including arb deals
contract_change_batter_with_arb <- batter_data %>%
  group_by(PlayerName, playerid) %>%
  arrange(PlayerName, playerid, Season) %>%
  
  # Keep the last year of each contract and the first year of the next contract
  mutate(
    last_year = if_else(Season == End, 1, 0),   # Flag for last year of the contract
    first_year = if_else(Season == Start, 1, 0) # Flag for first year of the contract
  ) %>%
  mutate(
    previous_WAR = dplyr::lag(WAR, order_by = Season), 
    previous_Salary = dplyr::lag(Salary, order_by = Season),        # Salary from last year of previous contract
    previous_ContractLength = dplyr::lag(ContractLength, order_by = Season), # Contract Length from last year
    previous_MarketSize = dplyr::lag(market_size, order_by = Season), # Market size from last year of previous contract
    previous_Age_at_signing = dplyr::lag(Age, order_by = Season),# WAR from last year of previous contract
    previous_Season = dplyr::lag(Season, order_by = Season), # Previous contract season
    previous_End = dplyr::lag(End, order_by = Season),        # Previous contract end
    previous_G = dplyr::lag(G, order_by = Season),
    previous_PA = dplyr::lag(PA, order_by = Season),
    previous_HR = dplyr::lag(HR, order_by = Season),
    previous_OBP = dplyr::lag(OBP, order_by = Season),
    previous_SLG = dplyr::lag(SLG, order_by = Season),
    previous_ISO = dplyr::lag(ISO, order_by = Season),
    previous_BB_pct = dplyr::lag(BB_pct, order_by = Season),
    previous_K_pct = dplyr::lag(K_pct, order_by = Season),
    previous_OPS = dplyr::lag(OPS, order_by = Season),
    previous_wOBA = dplyr::lag(wOBA, order_by = Season),
    previous_wRC_plus = dplyr::lag(wRC_plus, order_by = Season),
    previous_EV = dplyr::lag(EV, order_by = Season),
    previous_LA = dplyr::lag(LA, order_by = Season),
    previous_Barrel_pct = dplyr::lag(Barrel_pct, order_by = Season),
    previous_HardHit_pct = dplyr::lag(HardHit_pct, order_by = Season),
    WAR_change = WAR - previous_WAR,
    Salary_change = Salary - previous_Salary
  ) %>% 
  # Use lag to get the WAR from the last year of the previous contract
  
  # Only keep rows where the current season is the first year of a contract, and the previous season was the last year of a contract
  filter(first_year == 1 & !is.na(previous_WAR)) %>%
  
  # Ensure the seasons are consecutive (gap of 1 year between contracts)
  filter(Season - previous_Season == 1) 

names(contract_change_batter_with_arb)

##### First and last year pitchers ##### 
contract_change_pitchers_with_arb <- pitcher_data %>%
  group_by(Name, PlayerId) %>%
  arrange(Name, PlayerId, Season) %>%
  
  # Flag the last year and the first year of each contract
  mutate(
    last_year = if_else(Season == End, 1, 0),   # Flag for last year of the contract
    first_year = if_else(Season == Start, 1, 0) # Flag for first year of the contract
  ) %>%
  
  # Use lag to get the WAR, salary, contract length, market size, and age from the last year of the previous contract
  mutate(
    previous_WAR = dplyr::lag(WAR, order_by = Season),              # WAR from last year of previous contract
    previous_Salary = dplyr::lag(Salary, order_by = Season),        # Salary from last year of previous contract
    previous_ContractLength = dplyr::lag(ContractLength, order_by = Season), # Contract Length from last year
    previous_MarketSize = dplyr::lag(market_size, order_by = Season), # Market size from last year of previous contract
    previous_Age_at_signing = dplyr::lag(Age, order_by = Season), # Age at signing of previous contract
    previous_G = dplyr::lag(G, order_by = Season),
    previous_W = dplyr::lag(W, order_by = Season),
    previous_L = dplyr::lag(L, order_by = Season),
    previous_IP = dplyr::lag(IP, order_by = Season),
    previous_k_9 = dplyr::lag(`K/9`, order_by = Season),
    previous_BB_9 = dplyr::lag(`BB/9`, order_by = Season),
    previous_SV = dplyr::lag(SV, order_by = Season),
    previous_GS = dplyr::lag(GS, order_by = Season),
    previous_FIP = dplyr::lag(FIP, order_by = Season),
    previous_xFIP = dplyr::lag(xFIP, order_by = Season),
    previous_ERA = dplyr::lag(ERA, order_by = Season),
    previous_Season = dplyr::lag(Season, order_by = Season),        # Previous contract season
    previous_End = dplyr::lag(End, order_by = Season),             # Previous contract end year
    WAR_change = WAR - previous_WAR,
    Salary_change = Salary - previous_Salary
  ) %>%
  
  # Only keep rows where the current season is the first year of a contract, and the previous season was the last year of a contract
  filter(first_year == 1 & !is.na(previous_WAR)) %>%
  
  # Ensure the seasons are consecutive (gap of 1 year between contracts)
  filter(Season - previous_Season == 1)

# Read in the 2025 free agents
fa_2025 <- read_csv("FA_2025.csv")

fa_2025 <- fa_2025 %>% 
  mutate(FA_2025 = 1) %>% 
  rename(PlayerName = `PLAYER (199)`)

#Join in the FA dummies from the fa_2025 data
contract_change_batter_with_arb <- contract_change_batter_with_arb %>%
  left_join(fa_2025 %>%
              dplyr::select(PlayerName, FA_2025), by = "PlayerName")

contract_change_pitchers_with_arb <- contract_change_pitchers_with_arb %>%
  rename(PlayerName = Name) %>% 
  left_join(fa_2025 %>%
              dplyr::select(PlayerName, FA_2025), by = "PlayerName")


# filter for 2025 free agents, and their 2024 season stats
batter_free_agents <- contract_change_batter_with_arb %>% 
  filter(FA_2025 == 1, Season == 2024) %>% 
  rename(previous_Age = previous_Age_at_signing)


pitcher_free_agents <- contract_change_pitchers_with_arb %>% 
  filter(FA_2025 == 1, Season == 2024) %>% 
  rename(previous_Age = previous_Age_at_signing)

#Get the player ages for 2025

batter_free_agents <- batter_free_agents %>% 
  dplyr::select(-Age) %>% 
  left_join(fa_2025 %>% dplyr::select(PlayerName, AGE), by = "PlayerName") %>% 
  rename(Age = AGE)

pitcher_free_agents <- pitcher_free_agents %>% 
  dplyr::select(-Age) %>% 
  left_join(fa_2025 %>% dplyr::select(PlayerName, AGE), by = "PlayerName") %>% 
  rename(Age = AGE)

# If player is in last arb year in 2024, then there contract in 2025 is Arb to FA, if not then FA to FA
batter_free_agents <- batter_free_agents %>%
  mutate(
    contract_type_2025 = ifelse(last_arb_year == 1, "arb_to_FA", "FA_to_FA")
  )

names(batter_free_agents)
names(pitcher_free_agents)

# Update contract_type for pitcher free agents
pitcher_free_agents <- pitcher_free_agents %>%
  mutate(
    contract_type_2025 = ifelse(last_arb_year == 1, "arb_to_FA", "FA_to_FA")
  )

names(batter_free_agents)
names(pitcher_free_agents)
names(contract_change_pitchers_with_arb)

# Cleaning
batter_free_agents <- batter_free_agents %>%
  dplyr::select(-starts_with("previous")) %>%
  rename_with(~ paste0("previous_", .x), 
              .cols = c("G","AB","PA","H", "1B", "2B","3B","HR","R",
                        "RBI","BB","IBB","SO","HBP","SF","SH","GDP","SB","CS",
                        "AVG", "GB", "FB","LD","IFFB","BB_pct","K_pct","BB_K","OBP","SLG","OPS","ISO",
                        "BABIP","wOBA","WAR","wRC_plus","xwOBA","xAVG", "xSLG", "EV", "LA", "Season",
                        "ContractLength", "Salary"))

# Create dummy variables for contract type
batter_free_agents <- batter_free_agents %>%
  mutate(
    contract_type_arb_to_fa = ifelse(contract_type_2025 == "arb_to_FA", 1, 0),
    contract_type_fa_to_fa = ifelse(contract_type_2025 == "FA_to_FA", 1, 0)
  )

pitcher_free_agents <- pitcher_free_agents %>%
  dplyr::select(-starts_with("previous")) %>%
  rename_with(~ paste0("previous_", .x), 
              .cols = c("W", "L", "SV", "G", "GS", "IP", 
                        "K/9", "BB/9", "HR/9", "BABIP", "LOB%", 
                        "GB%", "HR/FB", "vFA (pi)", "ERA", "xERA", 
                        "FIP", "xFIP", "WAR", "Season", "ContractLength", "Salary"))  # Retain new_team for the previous season

# Create dummy variables for contract type
pitcher_free_agents <- pitcher_free_agents %>%
  mutate(
    contract_type_arb_to_fa = ifelse(contract_type_2025 == "arb_to_FA", 1, 0),
    contract_type_fa_to_fa = ifelse(contract_type_2025 == "FA_to_FA", 1, 0)
  )

batter_free_agents <- batter_free_agents %>%
  mutate(
    new_team = 1,  # Set new_team to 1
    market_size = mean(team_population$pop, na.rm = TRUE)  # Set market_size to the mean population from teampopulation
  )

# For pitchers
pitcher_free_agents <- pitcher_free_agents %>%
  mutate(
    new_team = 1,  # Set new_team to 1
    market_size = mean(team_population$pop, na.rm = TRUE)  # Set market_size to the mean population from teampopulation
  )



names(batter_free_agents)
names(pitcher_free_agents)
names(contract_change_batter)
names(contract_change_pitchers)

#### Models for predicting salary and contract length ####
# Load required libraries
library(mgcv)
library(caret)
library(dplyr)

# Data split (already provided in the prompt)
# 80/20 Split for batters
batter_split <- createDataPartition(contract_change_batter$WAR_change, p = 0.8, list = FALSE)
train_data_batter <- contract_change_batter[batter_split, ]
test_data_batter <- contract_change_batter[-batter_split, ]

# 80/20 Split for pitchers
pitcher_split <- createDataPartition(contract_change_pitchers$WAR_change, p = 0.8, list = FALSE)
train_data_pitcher <- contract_change_pitchers[pitcher_split, ]
test_data_pitcher <- contract_change_pitchers[-pitcher_split, ]

# Step 1: Train GAM models
# Salary and contract length for batters
names(train_data_batter)
names(train_data_pitcher)

salary_gam_batter <- gam(
  Salary ~ s(previous_WAR, k = 3) +
    s(previous_Salary, k = 3) +
    s(previous_ContractLength, k = 3)+
    contract_type_fa_to_fa +
    s(previous_ISO, k = 3) +
    s(previous_wRC_plus, k = 3) +
    s(previous_G, k = 3) +
    s(market_size, k = 3) +
    new_team +
    s(Age, k = 3),
  family = Gamma(link = "log"),
  data = train_data_batter
)

contract_length_gam_batter <- gam(
  ContractLength ~ s(previous_WAR, k = 3) +
    s(previous_ContractLength, k = 3) +
    s(previous_Salary, k = 3) + 
    contract_type_fa_to_fa +
    s(previous_ISO, k = 3) +
    s(previous_wRC_plus, k = 3) +
    s(previous_G, k = 3) +
    s(market_size, k = 3) +
    new_team +
    s(Age, k = 3),
  family = poisson(link = "log"),
  data = train_data_batter
)

# Salary and contract length for pitchers
salary_gam_pitcher <- gam(
  Salary ~ s(previous_WAR, k = 3) +
    s(previous_Salary, k = 3) +
    s(previous_ContractLength, k = 3) + 
    s(previous_GS, k = 3) +
    contract_type_fa_to_fa +
    s(previous_IP, k = 3) +
    s(previous_FIP, k = 3) +
    s(previous_SV, k = 3) +
    s(market_size, k = 3) +
    new_team +
    s(Age, k = 3),
  family = Gamma(link = "log"),
  data = train_data_pitcher
)

contract_length_gam_pitcher <- gam(
  ContractLength ~ s(previous_WAR, k = 3) +
    s(previous_ContractLength, k = 3) +
    s(previous_Salary, k = 3) +
    s(previous_GS, k = 3) +
    contract_type_fa_to_fa +
    s(previous_IP, k = 3) +
    s(previous_FIP, k = 3) +
    s(previous_SV, k = 3) +
    s(market_size, k = 3) +
    new_team +
    s(Age, k = 3),
  family = poisson(link = "log"),
  data = train_data_pitcher
)



# Step 2: Evaluate models on the test data
# Predictions for batters
test_data_batter$predicted_salary <- predict(salary_gam_batter, newdata = test_data_batter, type = "response")
test_data_batter$predicted_contract_length <- predict(contract_length_gam_batter, newdata = test_data_batter, type = "response")

# Compute RMSE for batters
batter_salary_rmse <- sqrt(mean((test_data_batter$predicted_salary - test_data_batter$Salary)^2, na.rm = TRUE))
batter_contract_rmse <- sqrt(mean((test_data_batter$predicted_contract_length - test_data_batter$ContractLength)^2, na.rm = TRUE))

# Predictions for pitchers
test_data_pitcher$predicted_salary <- predict(salary_gam_pitcher, newdata = test_data_pitcher, type = "response")
test_data_pitcher$predicted_contract_length <- predict(contract_length_gam_pitcher, newdata = test_data_pitcher, type = "response")

# Compute RMSE for pitchers
# Compute RMSE for pitchers
pitcher_salary_rmse <- sqrt(mean((test_data_pitcher$predicted_salary - test_data_pitcher$Salary)^2, na.rm = TRUE))
pitcher_contract_rmse <- sqrt(mean((test_data_pitcher$predicted_contract_length - test_data_pitcher$ContractLength)^2, na.rm = TRUE))


#Print RMSE Values
cat("Batters - Salary RMSE:", batter_salary_rmse, "\n")
cat("Batters - Contract Length RMSE:", batter_contract_rmse, "\n")
cat("Pitchers - Salary RMSE:", pitcher_salary_rmse, "\n")
cat("Pitchers - Contract Length RMSE:", pitcher_contract_rmse, "\n")



# Step 3: Predict for 2025 free agents
# Batters - Predictions
batter_free_agents$salary <- predict(salary_gam_batter, newdata = batter_free_agents, type = "response")
batter_free_agents$contract_length <- predict(contract_length_gam_batter, newdata = batter_free_agents, type = "response")

# Pitchers - Predictions
pitcher_free_agents$salary <- predict(salary_gam_pitcher, newdata = pitcher_free_agents, type = "response")
pitcher_free_agents$contract_length <- predict(contract_length_gam_pitcher, newdata = pitcher_free_agents, type = "response")


# Step 4: Clean the datasets

batter_free_agents_cleaned <- batter_free_agents %>%
  dplyr::select(PlayerName, playerid, Age, salary, previous_Salary, contract_length,previous_ContractLength, market_size,
         international, new_team, multiple_teams, contract_type_arb_to_fa,
         contract_type_fa_to_fa, previous_WAR) %>%
  mutate(
    Salary_change = salary - previous_Salary,
    multiple_teams = 0
  ) %>%
  rename(ContractLength = contract_length)

pitcher_free_agents_cleaned <- pitcher_free_agents %>%
  dplyr::select(PlayerName, PlayerId, Age, salary, previous_Salary, contract_length,previous_ContractLength, market_size,
         international, new_team, multiple_teams, contract_type_arb_to_fa,
         contract_type_fa_to_fa, previous_WAR) %>%
  mutate(
    Salary_change = salary - previous_Salary,
    multiple_teams = 0
  ) %>%
  rename(ContractLength = contract_length)

# Step 5: Compare predictions with actual values

# Batters: Predicted vs. Actual
batter_predictions_vs_actual <- test_data_batter %>%
  dplyr::select(PlayerName, playerid, Age,Season, Salary, predicted_salary, ContractLength, predicted_contract_length)

# Pitchers: Predicted vs. Actual
pitcher_predictions_vs_actual <- test_data_pitcher %>%
  dplyr::select(Name, PlayerId, Age, Season, Salary, predicted_salary, ContractLength, predicted_contract_length)

##### Plotting predicted vs actual values for salary/contract length #####
ggplot(test_data_batter, aes(x = Salary, y = predicted_salary)) +
  geom_point(color = 'black', alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +  # Line for perfect predictions
  labs(title = "Batters: Predicted vs Actual Salary",
       x = "Actual Salary",
       y = "Predicted Salary") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# Contract Length comparison
ggplot(test_data_batter, aes(x = ContractLength, y = predicted_contract_length)) +
  geom_point(color = 'black', alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +  # Line for perfect predictions
  labs(title = "Batters: Predicted vs Actual Contract Length",
       x = "Actual Contract Length",
       y = "Predicted Contract Length") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create scatter plots for Pitchers

# Salary comparison
ggplot(test_data_pitcher, aes(x = Salary, y = predicted_salary)) +
  geom_point(color = 'red', alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = 'black') +  # Line for perfect predictions
  labs(title = "Pitchers: Predicted vs Actual Salary",
       x = "Actual Salary",
       y = "Predicted Salary") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Contract Length comparison
ggplot(test_data_pitcher, aes(x = ContractLength, y = predicted_contract_length)) +
  geom_point(color = 'red', alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = 'black') +  # Line for perfect predictions
  labs(title = "Pitchers: Predicted vs Actual Contract Length",
       x = "Actual Contract Length",
       y = "Predicted Contract Length") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


###### Gam model for modelling change between last and first year ####

gam_batter_first_last <- gam(
  WAR_change ~ s(Age) + Salary_change + ContractLength +
    market_size + new_team + multiple_teams + 
    new_team * market_size + international + contract_type_fa_to_fa,
  data = contract_change_batter,
  method = "REML"
)

# Model for Pitcher Year-to-Year Contract Change
gam_pitcher_first_last <- gam(
  WAR_change ~ s(Age, k = 3) + Salary_change +
    ContractLength + market_size +
    new_team + market_size + multiple_teams +
    new_team * market_size + international + contract_type_fa_to_fa,
  data = contract_change_pitchers,
  method = "REML"
)

###### Summary stats #####
batter_model_variables <- contract_change_batter %>% 
  dplyr::select(Age, Salary, Salary_change, ContractLength, market_size, 
                new_team, multiple_teams, international, contract_type_fa_to_fa, contract_type_arb_to_fa)

pitcher_model_variables <- contract_change_pitchers %>% 
  dplyr::select(Age, Salary,Salary_change, ContractLength, market_size, 
                new_team, multiple_teams, international, contract_type_fa_to_fa, contract_type_arb_to_fa)

summary_stats_batter <- batter_model_variables %>%
  ungroup() %>% 
  summarise(
    min_age = min(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    max_age = max(Age, na.rm = TRUE),
    q1_age = quantile(Age, 0.25, na.rm = TRUE),
    q3_age = quantile(Age, 0.75, na.rm = TRUE),
    
    min_salary = min(Salary, na.rm = TRUE),
    median_salary = median(Salary, na.rm = TRUE),
    max_salary = max(Salary, na.rm = TRUE),
    q1_salary = quantile(Salary, 0.25, na.rm = TRUE),
    q3_salary = quantile(Salary, 0.75, na.rm = TRUE),
    
    min_contract_length = min(ContractLength, na.rm = TRUE),
    median_contract_length = median(ContractLength, na.rm = TRUE),
    max_contract_length = max(ContractLength, na.rm = TRUE),
    q1_contract_length = quantile(ContractLength, 0.25, na.rm = TRUE),
    q3_contract_length = quantile(ContractLength, 0.75, na.rm = TRUE),
    
    min_marketsize = min(market_size, na.rm = TRUE),
    max_marketsize = max(market_size, na.rm = TRUE),
    median_marketsize = median(market_size, na.rm = TRUE),
    q1_marketsize = quantile(market_size, 0.25, na.rm = TRUE),
    q3_marketsize = quantile(market_size, 0.75, na.rm = TRUE)
  )

dummy_counts_batter <- batter_model_variables %>%
  ungroup() %>% 
  summarise(
    count_new_team = sum(new_team, na.rm = TRUE),              # Count of 1s in `new_team`
    count_multiple_teams = sum(multiple_teams, na.rm = TRUE),  # Count of 1s in `multiple_teams`
    count_international = sum(international, na.rm = TRUE),    # Count of 1s in `international`
    count_contract_type_fa_to_fa = sum(contract_type_fa_to_fa, na.rm = TRUE), # Count of FA to FA
    count_contract_type_arb_to_fa = sum(contract_type_arb_to_fa, na.rm = TRUE) # Count of Arb to FA
  )

# Combine summary statistics and dummy counts into one dataframe
options(scipen = 999)

summary_batter <- bind_cols(summary_stats_batter, dummy_counts_batter)

summary_batter_long <- summary_batter %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

summary_stats_pitcher <- pitcher_model_variables %>%
  ungroup() %>%
  summarise(
    min_age = min(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    max_age = max(Age, na.rm = TRUE),
    q1_age = quantile(Age, 0.25, na.rm = TRUE),
    q3_age = quantile(Age, 0.75, na.rm = TRUE),
    
    min_salary = min(Salary, na.rm = TRUE),
    median_salary = median(Salary, na.rm = TRUE),
    max_salary = max(Salary, na.rm = TRUE),
    q1_salary = quantile(Salary, 0.25, na.rm = TRUE),
    q3_salary = quantile(Salary, 0.75, na.rm = TRUE),
    
    min_contract_length = min(ContractLength, na.rm = TRUE),
    median_contract_length = median(ContractLength, na.rm = TRUE),
    max_contract_length = max(ContractLength, na.rm = TRUE),
    q1_contract_length = quantile(ContractLength, 0.25, na.rm = TRUE),
    q3_contract_length = quantile(ContractLength, 0.75, na.rm = TRUE),
    
    min_marketsize = min(market_size, na.rm = TRUE),
    max_marketsize = max(market_size, na.rm = TRUE),
    median_marketsize = median(market_size, na.rm = TRUE),
    q1_marketsize = quantile(market_size, 0.25, na.rm = TRUE),
    q3_marketsize = quantile(market_size, 0.75, na.rm = TRUE)
  )

# Summary for dummy variables
dummy_counts_pitcher <- pitcher_model_variables %>%
  ungroup() %>%
  summarise(
    count_new_team = sum(new_team, na.rm = TRUE),
    count_multiple_teams = sum(multiple_teams, na.rm = TRUE),
    count_international = sum(international, na.rm = TRUE),
    count_contract_type_fa_to_fa = sum(contract_type_fa_to_fa, na.rm = TRUE),
    count_contract_type_arb_to_fa = sum(contract_type_arb_to_fa, na.rm = TRUE)
  )

# Combine summary statistics and dummy counts
summary_pitcher <- bind_cols(summary_stats_pitcher, dummy_counts_pitcher)

# Pivot the summary_pitcher to long format
summary_pitcher_long <- summary_pitcher %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")


  

#### Predict WAR_change for Batters ####
batter_free_agents_cleaned$predicted_WAR_change <- predict(
  gam_batter_first_last, 
  newdata = batter_free_agents_cleaned
)

# Predict WAR_change for Pitchers
pitcher_free_agents_cleaned$predicted_WAR_change <- predict(
  gam_pitcher_first_last, 
  newdata = pitcher_free_agents_cleaned
)

#Calculate the predicted 2025 WAR
batter_free_agents_cleaned <- batter_free_agents_cleaned %>% 
  mutate(predicted_2025_WAR = previous_WAR + predicted_WAR_change)

pitcher_free_agents_cleaned <- pitcher_free_agents_cleaned %>% 
  mutate(predicted_2025_WAR = previous_WAR + predicted_WAR_change)

### Get the top and bottom 10 values

batter_free_agents_top_10 <- batter_free_agents_cleaned %>% 
  dplyr::select(PlayerName, salary, previous_Salary, Salary_change, ContractLength, previous_ContractLength, 
                previous_WAR, predicted_WAR_change, predicted_2025_WAR) %>% 
  arrange(-predicted_WAR_change)

batter_free_agents_bot_10 <- batter_free_agents_cleaned %>% 
  dplyr::select(PlayerName, salary, previous_Salary, Salary_change, ContractLength, previous_ContractLength, 
                previous_WAR, predicted_WAR_change, predicted_2025_WAR) %>% 
  arrange(predicted_WAR_change)

pitcher_free_agents_top_10 <- pitcher_free_agents_cleaned %>% 
  dplyr::select(PlayerName, salary, previous_Salary, Salary_change, ContractLength, previous_ContractLength, 
                previous_WAR, predicted_WAR_change, predicted_2025_WAR) %>% 
  arrange(-predicted_WAR_change)

pitcher_free_agents_bot_10 <- pitcher_free_agents_cleaned %>% 
  dplyr::select(PlayerName, salary, previous_Salary, Salary_change, ContractLength, previous_ContractLength, 
                previous_WAR, predicted_WAR_change, predicted_2025_WAR) %>% 
  arrange(predicted_WAR_change)

