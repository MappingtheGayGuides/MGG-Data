library(ggmap)
library(tidyverse)


empty.df <- data.frame(
    title = character(),
    description = character(),
    streetaddress = character(),
    city = character(),
    state = character(),
    amenityfeatures = character(),
    unclear_address = character(),
    Year = numeric(),
    type = character(),
    notes = character(),
    stringsAsFactors = FALSE # This ensures that string data does not get converted to factors
)
columns <- colnames(empty.df)
years <- c(2003)

load_mgg_data <- function(df, columns, years) {
    # Get a list of files in the GG-Data subfolder that match the "gg-XXXX.csv" pattern
    # files <- list.files(path = "GG-Data", pattern = "gg-\\d{4}\\.csv$", full.names = FALSE)
    # years <- gsub("gg-(\\d{4})\\.csv", "\\1", files) # Extract the year from each filename
    # years <- as.numeric(years) # Convert the years to numeric
    # years <- unique(years) # Get a list of unique years

    # Loop through each unique year in the subfolder to open them
    for (year in years) {
        mgg.filename <- file.path("2-OrigDataforGeocoding", paste("originaldata-", year, ".csv", sep = ""))
        print(paste("Reading in ", mgg.filename, sep = ""))
        # load data and add a publication and country column to match the other datasets
        mgg.data <- read.csv(mgg.filename, header = TRUE)
        mgg.data <- select(mgg.data, intersect(columns, names(mgg.data)))
        df <- rbind(df, mgg.data)
    }
    return(df)
}

mgg.data <- load_mgg_data(empty.df, columns, years)

# getting google API key and registering with the service
getGoogleAPI <- function() {
    google_key <- readline(prompt = "Please enter your Google API key: ")
    print(google_key)
    register_google(key = google_key)
}

getGoogleAPI()

geocoding_function <- function(mgg.geocode.data) {
    # Trim whitespace from character columns
    mgg.geocode.data <- mgg.geocode.data %>% mutate_if(is.character, trimws)

    # Create a full address by concatenating street address, city, and state
    mgg.geocode.data$full.address <- paste(mgg.geocode.data$streetaddress, mgg.geocode.data$city, mgg.geocode.data$state, sep = ", ")

    # Initialize columns for longitude, latitude, and geocoded address
    mgg.geocode.data$lon <- NA
    mgg.geocode.data$lat <- NA
    mgg.geocode.data$geoAddress <- NA

    # Print the first 10 full addresses for debugging
    print(mgg.geocode.data$full.address[1:10])

    # Iterate through each row of the data frame
    for (i in 1:nrow(mgg.geocode.data)) {
        # Attempt to geocode the full address, handle errors by returning NA values
        result <- tryCatch(
            {
                res <- geocode(mgg.geocode.data$full.address[i], output = "latlona", source = "google")
                if (nrow(res) == 0) {
                    data.frame(lon = NA, lat = NA, address = NA)
                } else {
                    res
                }
            },
            error = function(e) {
                data.frame(lon = NA, lat = NA, address = NA)
            }
        )

        # Check if the result has the expected columns
        if (ncol(result) >= 2) {
            mgg.geocode.data$lon[i] <- as.numeric(result$lon)
            mgg.geocode.data$lat[i] <- as.numeric(result$lat)
            if (ncol(result) >= 3) {
                mgg.geocode.data$geoAddress[i] <- as.character(result$address)
            } else {
                mgg.geocode.data$geoAddress[i] <- NA
            }
        } else {
            mgg.geocode.data$lon[i] <- NA
            mgg.geocode.data$lat[i] <- NA
            mgg.geocode.data$geoAddress[i] <- NA
        }

        # Print the result for debugging purposes
        print(paste("Result: ", toString(result)))
    }

    # Filter out rows where geocoding failed (lon is NA)
    failed_geocode <- mgg.geocode.data %>% filter(is.na(lon))

    # Save the failed geocoding attempts to a CSV file
    write.csv(failed_geocode, "3-GeocodedDatasets/failed-geocode.csv", row.names = FALSE)

    # Return the updated data frame with geocoding results
    return(mgg.geocode.data)
}
mgg.data.geocoded <- geocoding_function(mgg.data)

save.data <- function(data) {
    for (year in years) {
        filtered_data <- filter(data, Year == year)
        # print("filtering by ", year)
        filename <- file.path("3-GeocodedDatasets", paste("data-", year, ".csv", sep = ""))
        write.csv(filtered_data, filename, row.names = FALSE)
    }
}

save.data(mgg.data.geocoded)
