library(rvest)
library(dplyr)

url <- "https://www.baseball-reference.com/schools/bat_secondary.shtml"
webpage <- read_html(url)

# Step 2: Extract the high school table
school_table <- webpage %>%
  html_node("#school_stats") %>%
  html_table(fill = TRUE)

# Step 3: Get the high school names and their URLs
schools <- school_table %>%
  mutate(URL = webpage %>%
           html_nodes("tr a") %>%
           html_attr("href")) %>%
  dplyr::select(School = School, URL)

# Step 4: Create full URLs for each school
schools$URL <- ifelse(startsWith(schools$URL, "https://"),
                      schools$URL,
                      paste0("https://www.baseball-reference.com", schools$URL))

# Step 5: Initialize an empty data frame for player data
player_data <- data.frame()

# Step 6: Loop through the schools to extract player names and years
for (i in 1:nrow(schools)) {
  school_url <- schools$URL[i]
  
  # Add a random delay between requests to avoid being rate-limited
  Sys.sleep(sample(60:120, 1))  # Random sleep between 60 and 120 seconds
  
  # Try to read the school page and handle possible errors
  school_page <- tryCatch({
    read_html(school_url)
  }, error = function(e) {
    message(paste("Error fetching:", school_url))
    return(NULL)
  })
  
  # Proceed if the school page was fetched successfully
  if (!is.null(school_page)) {
    # Use the appropriate selector for the player table on the school page
    player_table <- school_page %>%
      html_node("table") %>%
      html_table(fill = TRUE) 
    
    # Clean up the player table to include only relevant columns
    player_table <- player_table %>%
      dplyr::select(Name = `Name`, Yrs = `Yrs`, From = `From`, To = `To`) %>%
      filter(!grepl("Totals", Name), !is.na(Name))  # Exclude totals and NA players
    
    # Filter to include players who played any time between 2011 and 2023
    player_table <- player_table %>%
      filter((From <= 2023 | To >= 2011))
    
    # Add the school name to the player data
    player_table$School <- schools$School[i]
    
    # Combine the data
    player_data <- bind_rows(player_data, player_table)
  }
}

# Step 7: Clean up and save the data to a CSV file
write.csv(player_data, "mlb_players_high_schools.csv", row.names = FALSE)

# View the first few rows of the scraped data
head(player_data)
