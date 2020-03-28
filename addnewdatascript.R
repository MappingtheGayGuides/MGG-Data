library(dplyr)
library(magrittr)
#add path to new data
newdata <- "NGSData/CodedData/DC-data.csv"

new.data <- read.csv(newdata)
old.data <- read.csv(file = "NGSData/CodedData/South-data.csv")
#rename status column becasue thats always somehow an issue

new.data <- new.data %>% rename(Status = status)
mergeddata <- rbind(new.data, old.data)
write.csv(mergeddata, "NGSData/CodedData/CompleteDataset-revised.csv")
saveRDS(mergeddata, file = "NGSData/Data.rds")

