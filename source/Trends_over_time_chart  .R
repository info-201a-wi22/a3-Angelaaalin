# Trends over time chart #
# load packages #
library("dplyr")
library("ggplot2")
library("reshape")
# Read data #
incarceration_trends <- read.csv(file = "../data/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

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
  
  
  
  


