library(WDI)
library(tidyverse)
library(baseballr)
### City GDP data
WDIsearch("gdp")
gdp <- WDI(country = "all")

setwd("/Users/aaronrofe/Documents/Thesis Data")
city_data <- read_csv("FUA_CITY_10042024192347844.csv")

city_data$Variable

city_gdp_cap <- city_data %>% 
  filter(Variable == "GDP per capita (USD, constant prices, constant PPP, base year 2015)")

city_gdp_cap

# MLB player data
chadwick <- chadwick_player_lu()
mlbam_ids <- chadwick$key_mlbam

biographical_data <- map(mlbam_ids, mlb_people)
mlb_people