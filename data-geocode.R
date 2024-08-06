library(ggmap)
library(tidyverse)


empty.df <- data.frame(
    name = character(),
    description = character(),
    streetaddress = character(),
    city = character(),
    state = character(),
    amenityfeatures = character(),
    unclear_address = character(),
    year = numeric(),
    notes = character(),
    stringsAsFactors = FALSE # This ensures that string data does not get converted to factors
)
columns <- colnames(empty.df)
year <- c(1975, 1977, 1979, 1981, 1983, 1987, 1989)
