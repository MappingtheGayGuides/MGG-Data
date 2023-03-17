# This script formats the downloaded csv from Airtable and formats it for geocoding. Steps are commented out below.


#load ggmap
library(ggmap)
library(tidyverse)

# Read in CSV
#origAddress <- read.csv("MGG-App/NGSData/1965.csv", header = TRUE, sep =",", stringsAsFactors = FALSE)


########READ IN DATA AND PREP FOR GEOCODING########

origAddress <- read.csv("2-OrigDataforGeocoding/originaldata-1989.csv", header = TRUE)
origAddress <- origAddress %>%
  mutate_if(is.character, trimws)

# paste together the street address, city and state in order to ensure we use full addresses for geocoding. Will minimize mistakes caused by common streetnames. 
origAddress$full.address <- paste(origAddress$streetaddress, ", ", origAddress$city, ", ", origAddress$state, sep="") 

##########GEOCODE DATA############################


# Register the google api code for the georeferencing service.
register_google(key = Sys.getenv("MGG_GOOGLE_KEY"))

#mgg.geocode <- function(dataset) {
#  origAddress <- read.csv(paste("2-OrigDataforGeocoding/originaldata-",dataset,".csv", sep=""), header = TRUE)
#  origAddress <- origAddress %>% mutate_if(is.character, trimws)
  # paste together the street address, city and state in order to ensure we use full addresses for geocoding. Will minimize mistakes caused by common streetnames. 
#  origAddress$full.address <- paste(origAddress$streetaddress, ", ", origAddress$city, ", ", origAddress$state, sep="") 
  # Loop through the addresses to get the latitude and longitude of each address and add it to the origAddress data frame in new columns lat and lon
#  for(i in 1:10) {
    # Print("Working...")
#    result <- tryCatch(geocode(origAddress$full.address[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
#    origAddress$lon[i] <- as.numeric(result[1])
#    origAddress$lat[i] <- as.numeric(result[2])
#    origAddress$geoAddress[i] <- as.character(result[3])
#  }
 # fundata <- as.data.frame(origAddress)
  #return(as.data.frame(fundata))
#}

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
uncleardata <- read.csv(file = "1-UnclearData/uncleardata-1989.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

#trim white space
uncleardata <- uncleardata %>%
  mutate_if(is.character, trimws)

#create full address column
uncleardata$full.address <- paste(uncleardata$streetaddress, ", ", uncleardata$state, sep="") 

#split Lat/Long out into two columns
#separate(uncleardata$Lat.Lon, c("lat", "lon"), ",")
## as of 6-2022 no longer needed?
#uncleardata <- separate(uncleardata, col = lat.lon, into = c("lat","lon"), sep = ",")

#make sure both dfs have same columns
origAddress['status'] = 'Geocoded'
uncleardata['geoAddress'] = 'unclear_coded_by_hand'

#check col names
colnames(origAddress)
colnames(uncleardata)

alldata <- rbind(origAddress, uncleardata)


########MERGE THE TWO DATASETS########


#remove empty rows or NAs if there are any
#origAddress <- origAddress[!apply(is.na(origAddress) | origAddress == "", 1, all),]

#Convert all lon/lat columns to numeric
#alldata$lat <- as.numeric(alldata$lat)
#alldata$lon <- as.numeric(alldata$lon)
# Write a CSV file containing origAddress to the working directory
write.csv(alldata, "3-GeocodedDatasets/data-1989.csv", row.names=FALSE)
write.csv(alldata, "4-FullVerifiedDatasets/data-1989.csv", row.names=FALSE)
#saveRDS(alldata, "DC-Data.rds")


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
