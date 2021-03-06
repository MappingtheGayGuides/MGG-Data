# This script formats the downloaded csv from Airtable and formats it for geocoding. Steps are commented out below.


#load ggmap
library(ggmap)
library(dplyr)
library(stringr)
#library(plyr)
library(tidyr)
library(forcats)

# Read in CSV
#origAddress <- read.csv("MGG-App/NGSData/1965.csv", header = TRUE, sep =",", stringsAsFactors = FALSE)


########FUNCTION FOR READING IN ALL DATA FILES########

readData <- function( filename ) {
  
  # read in the data
  data <- read.csv( paste("NGSData/", filename, sep=""),
                    header = TRUE, 
                    sep =",", 
                    stringsAsFactors = FALSE )
  
  # add a "Year" column by removing both "yob" and ".txt" from file name
  data$Year <- gsub( ".csv", "", filename )
  
  return( data )
}

########READ IN DATA AND PREP FOR GEOCODING########

#origAddress <- ldply( .data = list.files(path="NGSData/", pattern="*.csv", include.dirs = TRUE),
#                  .fun = readData,
#                 .parallel = TRUE )

origAddress <- read.csv("2-OrigDataforGeocoding/originaldata-virginislands.csv", header = TRUE)
origAddress <- origAddress %>%
  mutate_if(is.character, trimws)

# paste together the street address, city and state in order to ensure we use full addresses for geocoding. Will minimize mistakes caused by common streetnames. 
origAddress$full.address <- paste(origAddress$streetaddress, ", ", origAddress$city, ", ", origAddress$state, sep="") 

#drop unclear addresses. We may want to subset them into another data frame that we investigate further later in this process. 
#unclearaddresses <- origAddress %>% filter(str_detect(unclearaddress, "checked"))
#origAddress <- subset(origAddress, unclearaddress!="checked")


##########GEOCODE DATA############################


# Register the google api code for the georeferencing service.
register_google(key = Sys.getenv("GOOGLEAPI_KEY"))

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
uncleardata <- read.csv(file = "1-UnclearData/uncleardata-guam.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

#trim white space
uncleardata <- uncleardata %>%
  mutate_if(is.character, trimws)

#create full address column
uncleardata$full.address <- paste(uncleardata$streetaddress, ", ", uncleardata$state, sep="") 

#split Lat/Long out into two columns
#separate(uncleardata$Lat.Lon, c("lat", "lon"), ",")
uncleardata <- separate(uncleardata, col = lat.lon, into = c("lat","lon"), sep = ",")

#make sure both dfs have same columns
origAddress['status'] = 'Geocoded'
uncleardata['geoAddress'] = 'unclear_coded_by_hand'
#uncleardata <- uncleardata %>% select(-"unclearaddress")
uncleardata <- uncleardata %>% select(-"lastmodified.2")
alldata <- rbind(origAddress, uncleardata)


########MERGE THE TWO DATASETS########


#remove empty rows or NAs if there are any
#origAddress <- origAddress[!apply(is.na(origAddress) | origAddress == "", 1, all),]

#Convert all lon/lat columns to numeric
#alldata$lat <- as.numeric(alldata$lat)
#alldata$lon <- as.numeric(alldata$lon)
# Write a CSV file containing origAddress to the working directory
write.csv(alldata, "3-GeocodedDatasets/data-guam.csv", row.names=FALSE)
write.csv(alldata, "4-FullVerifiedDatasets/data-guam.csv", row.names=FALSE)
#saveRDS(alldata, "DC-Data.rds")

##Existing data
de <- read.csv("4-FullVerifiedDatasets/data-delaware.csv")
id <- read.csv("4-FullVerifiedDatasets/data-idaho.csv")  
mt <- read.csv("4-FullVerifiedDatasets/data-montana.csv")
ne <- read.csv("4-FullVerifiedDatasets/data-nebraska.csv")
nh <- read.csv("4-FullVerifiedDatasets/data-newhampshire.csv")
ok <- read.csv("4-FullVerifiedDatasets/data-oklahoma.csv")
sd <- read.csv("4-FullVerifiedDatasets/data-southdakota.csv")
south <- read.csv("4-FullVerifiedDatasets/data-south.csv")
cali <- read.csv("4-FullVerifiedDatasets/data-cali.csv")
dc <- read.csv("4-FullVerifiedDatasets/data-dc.csv")
nd <- read.csv("4-FullVerifiedDatasets/data-northdakota.csv")
vt <- read.csv("4-FullVerifiedDatasets/data-vermont.csv")
ks <- read.csv("4-FullVerifiedDatasets/data-kansas.csv")
ak <- read.csv("4-FullVerifiedDatasets/data-alaska.csv")
ut <- read.csv("4-FullVerifiedDatasets/data-utah.csv")
wy <- read.csv("4-FullVerifiedDatasets/data-wyoming.csv")
pn <- read.csv("4-FullVerifiedDatasets/data-pennsylvania.csv")
mi <- read.csv("4-FullVerifiedDatasets/data-michigan.csv")
wi <- read.csv("4-FullVerifiedDatasets/data-wisconsin.csv")
il <- read.csv("4-FullVerifiedDatasets/data-illinois.csv")
mass <- read.csv("4-FullVerifiedDatasets/data-mass.csv")
nv <- read.csv("4-FullVerifiedDatasets/data-nevada.csv")
nm <- read.csv("4-FullVerifiedDatasets/data-newmexico.csv")
wv <- read.csv("4-FullVerifiedDatasets/data-westvirginia.csv")
az <- read.csv("4-FullVerifiedDatasets/data-arizona.csv")
ri <- read.csv("4-FullVerifiedDatasets/data-rhodeisland.csv")
or <- read.csv("4-FullVerifiedDatasets/data-oregon.csv")
oh <- read.csv("4-FullVerifiedDatasets/data-ohio.csv")
ny <- read.csv("4-FullVerifiedDatasets/data-newyork.csv")
me <- read.csv("4-FullVerifiedDatasets/data-maine.csv")
hi <- read.csv("4-FullVerifiedDatasets/data-hawaii.csv")
co <- read.csv("4-FullVerifiedDatasets/data-colorado.csv")
ia <- read.csv("4-FullVerifiedDatasets/data-iowa.csv")
ct <- read.csv("4-FullVerifiedDatasets/data-conneticut.csv")
mn <- read.csv("4-FullVerifiedDatasets/data-minnesota.csv")
wa <- read.csv("4-FullVerifiedDatasets/data-washington.csv")
mo <- read.csv("4-FullVerifiedDatasets/data-missouri.csv")
md <- read.csv("4-FullVerifiedDatasets/data-maryland.csv")
nj <- read.csv("4-FullVerifiedDatasets/data-newjersey.csv")
ind <- read.csv("4-FullVerifiedDatasets/data-indiana.csv")
gu <- read.csv("4-FullVerifiedDatasets/data-guam.csv")
pr <- read.csv("4-FullVerifiedDatasets/data-puertorico.csv")
vi <- read.csv("4-FullVerifiedDatasets/data-virginislands.csv")
territories <- rbind(gu,pr,vi)
states <- rbind(de,id,mt,ne,nh,ok,sd, nd, vt, ks, ak, ut, wy, pn, mi, wi, il, mass, nv, nm, wv, az, ri, or, oh, ny, me, hi, co, ia, ct, mn, wa, mo, md, nj, ind)
                
states <- states %>% select(-"lastmodified", -"full.address", -"dateadded", -"geoAddress", -"unclearaddress", "Field.1")
territories <- territories %>% select(-"lastmodified", -"full.address", -"dateadded", -"geoAddress", -"unclearaddress", -"uncleartype")
cali <- cali %>% select(-"lastmodified", -"full.address", -"dateadded", -"geoAddress", -"Field.1")

mergeddata <- rbind(states, cali, south, dc)
mergeddata <- mergeddata %>% select(-"Field.1") 
mergeddata <- rbind(mergeddata, territories)

unique(mergeddata$status)
mergeddata <- mergeddata %>% mutate(Status_Revised = fct_collapse(status, 
                                                "Google Verified Location" = "Geocoded",
                                                "Verified Location" = "Found",
                                                "Location could not be verified. General city or location coordinates used." = c("General Cooordinates Used", "General Coordinates Used", "General City Coordinates", "General City Coordinates Used", "General City Coordinates Found")))
unique(mergeddata$Status_Revised)
mergeddata <- mergeddata %>% select(-"status") 
mergeddata <- dplyr::rename(mergeddata, status = Status_Revised)



write.csv(mergeddata, "data.csv")
saveRDS(mergeddata, "data.rds")
#create a list of amenities
amenities <- origAddress %>% select(amenityfeatures)

#unclear 2917 9%
#verified 7218 21%
#G verified 24029 70%
