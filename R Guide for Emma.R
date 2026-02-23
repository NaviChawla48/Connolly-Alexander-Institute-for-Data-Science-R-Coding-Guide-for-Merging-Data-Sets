#General notes on now to use R:

#Hashtag before any code or words makes the line not be run, so for text or notes.
#Run code using the run buttom in the top right. 
#This pane is for code, pane below titled console is for your ouput ...
#... pane to the right is global environment which shows variables ...
#... pane beneath global environment shows plots and help tools.
#Save your code by clicking file in the top left and save.

#Feel free to look up each step if you want to understand more thoroughly what the code is doing.
#When you run data you can select the code to run and click run or if u click run by itself it will run the entire page.

#Step 1: Important the necessary libraries.
#These libraries store the necessary commands that R uses for different coding mechanisms.
library(tidyverse)
library(janitor)
library(readxl)
library(fuzzyjoin)

#Step 2: Set your working directory.
#This can be done by clicking session above and set working directory or the following command
#setwd("input working directory"), look up on google if do not know how to find your working directory


#Step 3: Read in your datasets
osa <- read_excel("OSA Study Abroad Student Data 24-25.xlsx") %>% clean_names()
travel <- read_excel("Student Travel Sustainability_July 1, 2024_July 1, 2025.xlsx", skip = 5) %>% clean_names()

#Step 4: Standardize Tulane IDs and names

#For ids, OSA file has numeric ids while the travel file has both numeric and character
#So we make both file character in order to merge
osa <- osa %>% mutate(
      tulane_id = as.character(tulane_id),
      full_name = name
    )
travel <- travel %>%  mutate(
      employee_id = as.character(employee_id),
      full_name = employee
    )

#Rename ID column in travel dataset to match osa dataset
travel <- travel %>% rename(tulane_id = employee_id)

#Step 5: Merge using Tulane ID
merged <- osa %>% left_join(travel, by = "tulane_id")

#Step 6: Fuzzy match on names for unmatched rows
unmatched <- merged %>% 
  filter(is.na(co2_emission)) %>%
  filter(!is.na(full_name.x))   # remove NA names

travel_nomiss <- travel %>% 
  filter(!is.na(full_name))     # remove NA travel names

fuzzy_matches <- stringdist_left_join(
  unmatched,
  travel_nomiss %>% select(full_name, co2_emission, route_distance, departure_city, arrival_city),
  by = c("full_name.x" = "full_name"),
  max_dist = 3
)

#Step 7: Combine ID matches and fuzzy name matches
final_dataset <- merged %>%
  filter(!is.na(co2_emission)) %>%  # keep ID matches with CO2
  bind_rows(
    fuzzy_matches %>% filter(!is.na(co2_emission.y))  # Change to co2_emission.y
  )

#Step 8: Create output file with final dataset
write_csv(final_dataset, "OSA_Students_Carbon_Emissions.csv")

#After step 8 your final dataset will download to your computer, but if you want to view it in R:
view(final_dataset)

#And to collapse into dataset with fewer columns and more easily understandable use "select command" which more info can be find using google, but would like like:
#final_dataset <- final_dataset %>% select(tulane_id, full_name.x, co2_emission, route_distance, departure_city, arrival_city)
