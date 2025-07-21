# Calls the AirGradient API to fetch sensor data and update the database
# This script is designed to run periodically (every 5 min) 
# Be aware to fill timezone and sensor_coords in variables.R with ID and locations

# ----

library(httr)
library(jsonlite)
library(DBI)
library(dplyr)
library(lubridate)
library(RMariaDB)
library(here)

# ----

# source(variables.R) that can be called from any directory
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/update_sensor.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)


# ----

# call the API
fetch_sensor_data <- function(access_token, 
                              sql_con, 
                              sensor_coords) {
  # request
  url <- paste0("https://api.airgradient.com/public/api/v1/locations/measures/current?token=",access_token)
  

  headers <- add_headers(
    accept = "application/json",
    authorization = paste("Bearer", access_token)
  )
  
  response <- GET(url, headers)
  
  if (status_code(response) == 200) {
    data_json <- content(response, "text", encoding = "UTF-8")
    data_list <- fromJSON(data_json, flatten = TRUE)
   
  
    if (!is.data.frame(data_list)) {
      data_list <- list(data_list)
    }

    df <- as_tibble(data_list)
    
    # format timestamp column with the right timezone
    if ("timestamp" %in% colnames(df)) {
      df <- df %>% 
        mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>% 
        mutate(timestamp = with_tz(timestamp, timezone)) %>% 
        mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
    }
    
    # add latitude and longitude
    if (!"latitude" %in% colnames(df)) {
      df$latitude <- NA
    }
    if (!"longitude" %in% colnames(df)) {
      df$longitude <- NA
    }
    
    df <- df %>%
      mutate(
        latitude = sapply(locationId, function(id) sensor_coords[[as.character(id)]][1]),
        longitude = sapply(locationId, function(id) sensor_coords[[as.character(id)]][2])
      )
    
    # update coordinates in DB 
    for (i in seq_len(nrow(df))) {
      dbExecute(sql_con, "UPDATE sensors SET latitude = ?, longitude = ? WHERE locationId = ?",
                params = list(df$latitude[i], df$longitude[i], df$locationId[i]))
    }
    
    # write to sql table data
    dbWriteTable(sql_con, "sensors", df, append = TRUE, row.names = FALSE)
    dbDisconnect(sql_con)
    message("Sensor data inserted into 'sensors' table.")
  } else {
    message(sprintf("Error: %s, %s", status_code(response), content(response, "text")))
  }
  
  invisible(NULL)
}


fetch_sensor_data(token_sensor, con, sensor_coords)
