# Map #
# load packages #
library("leaflet")
library("dplyr")
library("RColorBrewer")
library("giscoR")
# Read data #
incarceration_trends <- read.csv(file = "../data/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

# Total population in jail distributed in WA #
