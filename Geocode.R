# This script formats the downloaded csv from Airtable and formats it for geocoding. Steps are commented out below.


#load ggmap
library(ggmap)
library(tidyverse)

# Read in CSV
#origAddress <- read.csv("MGG-App/NGSData/1965.csv", header = TRUE, sep =",", stringsAsFactors = FALSE)


########READ IN DATA AND PREP FOR GEOCODING########

origAddress <- read.csv("2-OrigDataforGeocoding/originaldata-2000.csv", header = TRUE)
origAddress <- origAddress %>%
  mutate_if(is.character, trimws)

# paste together the street address, city and state in order to ensure we use full addresses for geocoding. Will minimize mistakes caused by common streetnames. 
origAddress$full.address <- paste(origAddress$streetaddress, ", ", origAddress$city, ", ", origAddress$state) 

##########GEOCODE DATA############################


# Register the google api code for the georeferencing service.
#register_google(key = Sys.getenv("MGG_GOOGLE_KEY"))
register_google(key = Sys.getenv("GOOGLEGEOCODE_API_KEY"))

# Loop through the addresses to get the latitude and longitude of each address and add it to the origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress)) {
  # Print("Working...")
  result <- tryCatch(geocode(origAddress$full.address[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}


##########MANIPULATE UNCLEAR DATA#################

#Make unclear address match the geocoded dataset 
uncleardata <- read.csv(file = "1-UnclearData/uncleardata-2000.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

#trim white space
uncleardata <- uncleardata %>%
  mutate_if(is.character, trimws)

#create full address column
uncleardata$full.address <- paste(uncleardata$streetaddress, ", ", uncleardata$city, ", ", uncleardata$state, sep="") 

#make sure both dfs have same columns
origAddress['status'] = 'Geocoded'
uncleardata['geoAddress'] = 'unclear_coded_by_hand'

#check col names
colnames(origAddress)
colnames(uncleardata)

colnames(origAddress)[1] = "ID"

colnames(uncleardata)[2] = "title"

colnames(origAddress)[10] = "uncleartype"

colnames(uncleardata)[9] = "unclear_address"

colnames(uncleardata)[17] = "Last.Modified"

uncleardata <- uncleardata[,-16] #remove "dateadded" column

#origAddress <- origAddress[,-11] #remove "notes column"

alldata <- rbind(origAddress, uncleardata)

alldata$lat <- as.numeric(alldata$lat)
alldata$lon <- as.numeric(alldata$lon)



########MERGE THE TWO DATASETS########

# Write a CSV file containing origAddress to the working directory
write.csv(alldata, "3-GeocodedDatasets/data-2000.csv", row.names=FALSE)
write.csv(alldata, "4-FullVerifiedDatasets/data-2000.csv", row.names=FALSE)

#COMMIT HERE LUC


#NO NEED TO CONTINUE, VERIFY IN KEPPLER

maindataset <- read.csv("data.csv", header = TRUE)
newdata <- read.csv("4-FullVerifiedDatasets/data-1989.csv", header = TRUE)
colnames(newdata)
newdata <- newdata %>% select(-unclearaddress, -uncleartype, -lastmodified, -full.address, -geoAddress)

mergeddata <- rbind(mergeddata, newdata)




unique(mergeddata$status)
mergeddata <- mergeddata %>% mutate(Status_Revised = fct_collapse(status, 
                                                "Google Verified Location" = "Geocoded", 
                                                "Verified Location" = c("Found", "For Review"),
                                                "Location could not be verified. General city or location coordinates used." = c("General Coordinates Used", "Genderal Coordinates Used")))

unique(mergeddata$Status_Revised)

unique(mergeddata$state)
mergeddata <- mergeddata %>% mutate(state = fct_collapse(state, "DC" = c("D.C.", "District of Columbia")))
                                                                 
mergeddata <- mergeddata %>% select(-"status") 
mergeddata <- dplyr::rename(mergeddata, status = Status_Revised)



write.csv(mergeddata, "data.csv")
saveRDS(mergeddata, "data.rds")
#create a list of amenities
amenities <- origAddress %>% select(amenityfeatures)

#unclear 2917 9%
#verified 7218 21%
#G verified 24029 70%
