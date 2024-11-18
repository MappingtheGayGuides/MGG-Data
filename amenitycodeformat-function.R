# this function recodes amenity features to include a year range per the data model. For example, the amenity (C) stood for coffee house from 1965-1989. It was absent from the Damron categorizations from 1990-1998. In 1999 it reappeared. But this time it meant Cabaret. This function recodes entries of C to reflect the dates for each category and to distinguish between the two meanings.
library(tidyverse)
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
mgg.amenitycode.format(1970, 1989, "(B)", "(B-1970-1989)")
mgg.amenitycode.format(1990, 1993, "(B)", "(B-1990-1993)")
mgg.amenitycode.format(1999, 2005, "(B)", "(B-1999-2005)")

mgg.amenitycode.format(1965, 1989, "(C)", "(C-1965-1989)")
mgg.amenitycode.format(2003, 2005, "(C)", "(C-2003-2005)")

mgg.amenitycode.format(1965, 1989, "(H)", "(H-1965-1989)")
mgg.amenitycode.format(1990, 1995, "(H)", "(H-1990-1995)")

mgg.amenitycode.format(1980, 1989, "(L)", "(L-1980-1989)")
mgg.amenitycode.format(1990, 2005, "(L)", "(L-1990-2005)")

mgg.amenitycode.format(1996, 2005, "(M)", "(M-1996-2005)")
mgg.amenitycode.format(1965, 1989, "(M)", "(M-1965-1989)")

mgg.amenitycode.format(1988, 1989, "(N)", "(N-1988-1989)")
mgg.amenitycode.format(1990, 2005, "(N)", "(N-1990-2005)")

mgg.amenitycode.format(1965, 1989, "(P)", "(P-1965-1989)")
mgg.amenitycode.format(1990, 2002, "(P)", "(P-1990-2002)")
mgg.amenitycode.format(2003, 2005, "(P)", "(P-2003-2005)")

mgg.amenitycode.format(1965, 1989, "(R)", "(R-1965-1989)")
mgg.amenitycode.format(2003, 2005, "(R)", "(R-2003-2005)")

mgg.amenitycode.format(1965, 2002, "(S)", "(S-1965-2002)")
mgg.amenitycode.format(2003, 2005, "(S)", "(S-2003-2005)")

mgg.amenitycode.format(1972, 1989, "(W)", "(W-1972-1989)")
mgg.amenitycode.format(1990, 2005, "(W)", "(W-1990-2005)")

write.csv(mggdata, "data.csv")
saveRDS(mggdata, "data.rds")
