library(tidyverse)


remove_extra_columns_and_save <- function(folder_path, desired_columns) {
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  data_list <- lapply(csv_files, read.csv)

  for (i in seq_along(data_list)) {
    current_columns <- names(data_list[[i]])
    extra_columns <- setdiff(current_columns, desired_columns)
    missing_columns <- setdiff(desired_columns, current_columns)

    # Add missing columns with NA as default values
    for (missing_col in missing_columns) {
      data_list[[i]][[missing_col]] <- NA
    }

    # Remove extra columns
    data_list[[i]] <- data_list[[i]][, names(data_list[[i]]) %in% desired_columns]

    # Write the modified dataframe back to disk, overwriting the original file
    write.csv(data_list[[i]], csv_files[i], row.names = FALSE)

    # Print message about updates
    if (length(extra_columns) > 0 || length(missing_columns) > 0) {
      cat("Updated file", basename(csv_files[i]), "- Removed extra columns:", paste(extra_columns, collapse = ", "), "- Added missing columns:", paste(missing_columns, collapse = ", "), "\n")
    }
  }
}

# Example usage
folder_path <- "4-FullVerifiedDatasets"
desired_columns <- c("title", "amenityfeatures", "description", "streetaddress", "type", "city", "state", "Year", "unclear_address", "notes", "full.address", "lon", "lat", "geoAddress", "status")
remove_extra_columns_and_save(folder_path, desired_columns)


add_unique_id_to_csv_files <- function(folder_path = "4-FullVerifiedDatasets") {
  # List all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

  # Iterate through each CSV file
  for (csv_file in csv_files) {
    # Read the CSV file
    df <- read.csv(csv_file, stringsAsFactors = FALSE)

    # Check if the ID column already exists
    if (!"ID" %in% colnames(df)) {
      # Create the ID column
      # Corrected line for creating the ID column
      df$ID <- sprintf("%s-%s-%04d", df$Year, df$state, seq_along(df$state))

      # Write the modified dataframe back to disk, overwriting the original file
      write.csv(df, csv_file, row.names = FALSE)
    } else {
      cat("Skipped file (ID column already exists):", basename(csv_file), "\n")
    }
  }
}

# Example usage
add_unique_id_to_csv_files()





merge_csv_files_into_dataframe <- function(folder_path = "4-FullVerifiedDatasets") {
  # List all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

  # Read each CSV file and store in a list
  dataframes_list <- lapply(csv_files, read.csv, stringsAsFactors = FALSE)

  # Merge all dataframes into one
  merged_dataframe <- do.call(rbind, dataframes_list)

  return(merged_dataframe)
}

# Example usage
merged_df <- merge_csv_files_into_dataframe()
unique(merged_df$status)
unique(merged_df$state)



fix_state_entries <- function(df) {
  # Replace "D.C.", "D.C", and "District of Columbia" with "DC"
  df <- df %>%
    mutate(state = ifelse(str_detect(state, "District of Columbia|D\\.C\\.|D\\.C"), "DC", state))

  # Replace "Virgin Islands" with "VI"
  df <- df %>%
    mutate(state = ifelse(str_detect(state, "Virgin Islands"), "VI", state))

  return(df)
}
merged_df <- fix_state_entries(merged_df)
unique(merged_df$state)

amend.status.col <- function(df) {
  print(unique(df$status))
  df <- df %>% mutate(Status_Revised = fct_collapse(status,
    "Google Verified Location" = "Geocoded",
    "Verified Location" = c("Found", "For Review"),
    "Location could not be verified. General city or location coordinates used." = c("General Coordinates Used", "Genderal Coordinates Used")
  ))
  df <- df %>% select(-"status")
  df <- rename(df, status = Status_Revised)
}

merged_df <- amend.status.col(merged_df)
unique(merged_df$status)
