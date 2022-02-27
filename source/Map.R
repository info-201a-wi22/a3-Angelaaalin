# Map #
# load packages #
library("leaflet")
library("dplyr")
library("RColorBrewer")
library("giscoR")
# Read data #
incarceration_trends <- read.csv(file = "../data/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)
# Total population in jail distributed in WA #
map <- function() {wa <- incarceration_trends %>%
  filter(state == "WA") %>%
  group_by(county_name) %>%
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
location_wa <- data.frame(
  county = c(wa$county_name),
  population = c(wa$total_jail_pop),
  latitude = c(46.983202, 46.1998773, 46.2665231, 47.8831813, 48.0498087,
               45.7998375, 46.299864, 46.1998345, 47.7165297, 48.4665565,
               46.5165273, 46.4498747, 47.1498615, 47.0831463, 48.1331513,
               47.6164807, 47.4664913, 47.6164836, 47.1165124, 45.8665133,
               46.5831628, 47.5498735, 47.3498148, 48.5498712, 46.5664905,
               48.533245, 47.0331582, 48.5664898, 48.4665011, 46.0331699,
               48.0498261, 47.6165594, 48.433234, 46.9164873, 46.2831646,
               46.2831925, 48.9165138, 46.9165542, 46.4665147),
  longitude = c(-118.5177593, -117.1843266, -119.4844685, -120.6345386, -123.8346442,
                -122.4845414, -117.9343772, -122.6845524, -119.7344949, -118.5344594,
                -118.934448, -117.551019, -119.4844771, -123.8346127, -122.584604,
                -123.0846057, -121.8178891, -122.6679217, -120.6845195, -120.7678415,
                -122.4012208, -118.4844318, -123.1846005, -119.7345105, -123.7179352,
                -117.2843943, -122.1178904, -122.9679582, -121.7845841, -121.9011957,
                -121.7179007, -117.367709, -117.8344205, -122.8179101, -123.4345775,
                -118.484424, -121.7845869, -117.4843562, -120.6678459))
epsg_code <- 8103
US_countries <- gisco_get_countries(region = "US") %>%
  st_transform(epsg_code)
ggplot() +
  geom_sf(data = location_wa)


leaflet(location_wa) %>%
  addMarkers(data = location_wa) %>%
  addTiles("Total population in jail distributed in WA") 
addMarkers(lng= -120.740135, lat = 47.751076)


}



ggplot() %>%
  geom_sf()
View(location_wa)

# Total population in jail distributed in WA #
