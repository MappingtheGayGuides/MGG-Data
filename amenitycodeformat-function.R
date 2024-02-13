#this function recodes amenity features to include a year range per the data model. For example, the amenity (C) stood for coffee house from 1965-1989. It was absent from the Damron categorizations from 1990-1998. In 1999 it reappeared. But this time it meant Cabaret. This function recodes entries of C to reflect the dates for each category and to distinguish between the two meanings. 
mggdata <- readRDS("data.rds")
mgg.amenitycode.format <- function(year1, year2, originalamenity, newamenity) {
  included.data <<- mggdata %>% filter(Year %in% year1:year2)
  print(paste("included data has ", length(included.data$ID), " locations."))
  print(paste("included.data is filtering by", year1, " and ", year2, "."))
  included.data$amenityfeatures <<- sub(originalamenity, newamenity, included.data$amenityfeatures, fixed = TRUE)
  excluded.data <- mggdata %>% filter(!(Year %in% year1:year2))
  print(paste("excluded data has ", length(excluded.data$ID), " locations."))
  mggdata <<- rbind(included.data, excluded.data)
  print(paste("data has been tranformed and now has ", length(mggdata$ID), " locations."))
}
  mgg.amenitycode.format(1970,1993,"(B)", "(B-1970-1993)")  
  mgg.amenitycode.format(1965, 1989,"(C)", "(C-1965-1989)")


