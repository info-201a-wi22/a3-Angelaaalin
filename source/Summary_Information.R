# Summary Information #
# load packages #
library("tidyverse")
library("dplyr")
# Read data #
incarceration_trends <- read.csv(file = "../data/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)
 View(incarceration_trends)
# state with  the highest total jail population #
highest_jail_pop_state <- incarceration_trends %>%
  group_by(state) %>%
  summarise(total_popu = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(total_popu == max(total_popu)) %>%
  pull(state)

# race with highest total jail population %
aapi <- sum(incarceration_trends$aapi_jail_pop, na.rm = TRUE)
black <- sum(incarceration_trends$black_jail_pop, na.rm = TRUE)
latinx <- sum(incarceration_trends$latinx_jail_pop, na.rm = TRUE)
native <- sum(incarceration_trends$latinx_jail_pop, na.rm = TRUE)
white <- sum(incarceration_trends$white_jail_pop, na.rm = TRUE)
other <- sum(incarceration_trends$other_race_jail_pop, na.rm = TRUE)
race <- c(aapi, black, latinx, native, white, other)
data <- data.frame(race = c("aapi", "black", "latinx", "native", "white", "other"),
                   population = c(aapi, black, latinx, native, white, other))
highest_jail_pop <- data %>%
  filter(population == max(population)) %>%
  pull(race)

# state with the current highest total jail population#
state_highest_jail_recent <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(state)

# ratio of female in jail to male in jail in the most recent year #
male <- incarceration_trends %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  summarise(male_in_jail = sum(male_jail_pop, na.rm = TRUE)) %>%
  filter(male_in_jail == max(male_in_jail)) %>%
  pull(male_in_jail)
female <- incarceration_trends %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  summarise(female_in_jail = sum(female_jail_pop, na.rm = TRUE)) %>%
  filter(female_in_jail == max(female_in_jail)) %>%
  pull(female_in_jail)
gender_ratio <- round(female/male, 3)

# state with the highest average ratio of total_jail_pop to total_pop # 
incarceration_trends$ratio <- paste0(incarceration_trends$total_jail_pop/incarceration_trends$total_pop)
highest_ratio <- incarceration_trends %>%
  group_by(state) %>%
  summarise(ave_ratio = mean(ratio, na.rm = TRUE)) %>%
  filter(ave_ratio == max(ave_ratio, na.rm = TRUE)) %>%
  pull(state)
  
    


# ratio of juvenile in jail to adult in jail #

juvenile <- sum(incarceration_trends$female_juvenile_jail_pop, na.rm = TRUE) +
   sum(incarceration_trends$male_juvenile_jail_pop, na.rm = TRUE)

adult <- sum(incarceration_trends$female_adult_jail_pop, na.rm = TRUE) +
  sum(incarceration_trends$male_adult_jail_pop, na.rm = TRUE)
ratio_age <- round(juvenile/adult, 3)
  
 
  
  
   





 
 
 
 
 
 
 
 
 
 