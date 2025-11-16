library(tidyverse)
library(dplyr)
library(baseballr)
library(devtools)
setwd("/Users/aaronrofe/Documents/Thesis Data/contract_data")

contracts_2011 <- read_csv("contracts_2011.csv")
contracts_2012 <- read_csv("contracts_2012.csv")
contracts_2013 <- read_csv("contracts_2013.csv")
contracts_2014 <- read_csv("contracts_2014.csv")
contracts_2015 <- read_csv("contracts_2015.csv")
contracts_2016 <- read_csv("contracts_2016.csv")
contracts_2017 <- read_csv("contracts_2017.csv")
contracts_2018 <- read_csv("contracts_2018.csv")
contracts_2019 <- read_csv("contracts_2019.csv")
contracts_2020 <- read_csv("contracts_2020.csv")
contracts_2021 <- read_csv("contracts_2021.csv")
contracts_2022 <- read_csv("contracts_2022.csv")
contracts_2023 <- read_csv("contracts_2023.csv")
contracts_2024 <- read_csv("contracts_2024.csv")
setwd("/Users/aaronrofe/Documents/Thesis Data")
batter_data <- read_csv("batter_data_2010_2024.csv")
player_pob <- read_csv("player_pob.csv")
pitcher_data <- read_csv("pitcher_data_2010_2024.csv")


##### Contracts #####

#create a list of the 13 data frames
list_of_dfs <- list(contracts_2011, contracts_2012, contracts_2013, contracts_2014, contracts_2015,
                    contracts_2016, contracts_2017, contracts_2018, contracts_2019, contracts_2020,
                    contracts_2021, contracts_2022, contracts_2023, contracts_2024)



# Use rbind to combine them
all_contracts <- do.call(rbind, list_of_dfs[1:13])

names(contracts_2024)[4] <- "team_signed_with"
names(all_contracts)[4] <- "team_signed_with"

all_contracts <- rbind(all_contracts, contracts_2024)




#Replace all the $ and commas with empty strings and make them numeric
all_contracts$Value <- as.numeric(gsub("[\\$,]", "", all_contracts$Value))
all_contracts$AAV <- as.numeric(gsub("[\\$,]", "", all_contracts$AAV))



# Makes the start and end years numeric
all_contracts$Start <- as.numeric(all_contracts$Start)
all_contracts$End <- as.numeric(all_contracts$End)


# Filters out NA value from the years ana AAV
all_contracts <- all_contracts %>%
  filter(!is.na(Start) & !is.na(End) & !is.na(AAV))


#Expand the contract 
expanded_contracts <- all_contracts %>%
  group_by(Player) %>%
  mutate(year = map2(Start, End, ~seq(.x, .y))) %>%  # Create sequence of years for each contract
  unnest(year)  # Unnest the list of years into individual rows

# Helps with overlapping contracts in the case of opt outs and extensions
expanded_contracts <- expanded_contracts %>%
  arrange(Player, year, desc(Start)) %>% # Sort by Player, year, and Start (desc)
  distinct(Player, year, .keep_all = TRUE)

write.csv(expanded_contracts, "all_contracts.csv", row.names = FALSE)

##### Player POB #####

names(player_pob)

player_pob <- player_pob %>% 
  dplyr::select(player_id, full_name,current_age, birth_date, birth_city, birth_country, birth_state_province,
         primary_position_code, primary_position_name)

player_pob <- player_pob %>% 
  mutate(international = as.factor(ifelse(birth_country == "USA", 0,1)))


summary(player_pob$international)





