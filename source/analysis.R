# Summary Information #
# load packages #
library("tidyverse")
library("dplyr")
library("ggplot2")
library("reshape")
library("leaflet")
# Read data #
incarceration_trends <- read.csv(file = "../data/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

# state with  the highest total jail population #
highest_jail_pop_state <- incarceration_trends %>%
  group_by(state) %>%
  summarise(total_popu = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(total_popu == max(total_popu)) %>%
  pull(state)

# race with highest total jail population #
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

# state with the current highest total jail population #
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

# state with the highest average ratio of total_jail_pop to total_pop in the most recent year # 
incarceration_trends["ratio"] = incarceration_trends$total_jail_pop/incarceration_trends$total_pop
recent_highest_ratio_ <- incarceration_trends %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  summarise(ave_ratio = mean(ratio, na.rm = TRUE)) %>%
  filter(ave_ratio == max(ave_ratio, na.rm = TRUE)) %>%
  pull(state)

# ratio of juvenile in jail to adult in jail #

juvenile <- sum(incarceration_trends$female_juvenile_jail_pop, na.rm = TRUE) +
   sum(incarceration_trends$male_juvenile_jail_pop, na.rm = TRUE)

adult <- sum(incarceration_trends$female_adult_jail_pop, na.rm = TRUE) +
  sum(incarceration_trends$male_adult_jail_pop, na.rm = TRUE)
ratio_age <- round(juvenile/adult, 3)

summary_info <- list("race with highest total jail population" = highest_jail_pop,
                     " state with  the highest total jail population" = highest_jail_pop_state,
                     "state with the current highest total jail population" = state_highest_jail_recent,
                     "ratio of female in jail to male in jail in the most recent year" = gender_ratio,
                     "state with the highest average ratio of total_jail_pop to total_pop in the most recent year" = recent_highest_ratio_,
                     "ratio of juvenile in jail to adult in jail" = ratio_age)



# Trends Over Time Chart #
# Different races of population in jail over time #
race_pop_over_time <- function() { 
  all_races <- incarceration_trends %>%
    group_by(year) %>%
    summarise("AAPI" = sum(aapi_jail_pop, na.rm = TRUE),
              "Black" = sum(black_jail_pop, na.rm = TRUE),
              "Latinx" = sum(latinx_jail_pop, na.rm = TRUE),
              "Native" = sum(native_jail_pop, na.rm = TRUE),
              "White" = sum(white_jail_pop, na.rm = TRUE),
              "Other" = sum(other_race_jail_pop, na.rm = TRUE))
  df <- reshape(data = all_races,
                idvar="year",
                varying = c("AAPI","Black", "Latinx", "Native", "White", "Other"),
                v.names = "population",
                times = c("AAPI","Black", "Latinx", "Native", "White", "Other"),
                new.row.names = 1:1000,
                direction = "long")
  names(df)[2] <- 'Race'
  cols <- c("black", "red", "blue", "yellow", "green", "purple")
  ggplot(df, aes(x = year, y = population, color = Race)) +
    geom_point(aes(x = year, y = population, color = Race)) +
    geom_line(mapping = aes(x = year, y = population, color = Race)) +
    labs(title = "Different races of population in jail over time") +
    scale_color_manual(values = cols)
  
}

# Variable Comparison Chart #
# Male v.s Female population in jail #

summary_info <- list("race with highest total jail population" = highest_jail_pop,
                     " state with  the highest total jail population" = highest_jail_pop_state,
                     "state with the current highest total jail population" = state_highest_jail_recent,
                     "ratio of female in jail to male in jail in the most recent year" = gender_ratio,
                     "state with the highest average ratio of total_jail_pop to total_pop in the most recent year" = recent_highest_ratio_,
                     "ratio of juvenile in jail to adult in jail" = ratio_age)
  
gender_pop_over_time <- function() { 
  diff_gender <- incarceration_trends %>%
    group_by(year) %>%
    summarise("Female" = sum(female_jail_pop, na.rm = TRUE),
              "Male" = sum(male_jail_pop, na.rm = TRUE)) 
  df <- reshape(data = diff_gender,
                idvar="year",
                varying = c("Female", "Male"),
                v.names = "population",
                times = c("Female", "Male"),
                new.row.names = 1:1000,
                direction = "long")
  names(df)[2] <- 'Gender'
  cols <- c("red", "blue")
  ggplot(df, aes(x = year, y = population)) +
    geom_point(aes(x = year, y = population, color = Gender)) +
    labs(title = "Female v.s Male population in jail") +
    scale_color_manual(values = cols)
  
}

 
 
 
 