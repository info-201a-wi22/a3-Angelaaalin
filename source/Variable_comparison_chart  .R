# Variable comparison chart #
# load packages #
library("dplyr")
library("ggplot2")
library("reshape")
# Read data #
incarceration_trends <- read.csv(file = "../data/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

# Male v.s Female population in jail #
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
            
              
  