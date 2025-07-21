# Calls the TMD API to fetch weather data and update the database
# This script is designed to run periodically (every hour)
# Be aware to fill timezone, bottom_left, top_right, fields_weather & domain_weather in variables.R

# ----

library(httr)
library(jsonlite)
library(dplyr)
library(DBI)
library(RMariaDB)
library(lubridate)
library(xml2)

# ----

# source(variables.R) that can be called from any directory
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/update_weather.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)

# ----

# call the API
fetch_weather_data <- function(access_token, 
                               bottom_left, 
                               top_right, 
                               fields, 
                               domain, 
                               sql_connection) {
  # round time to the next hour -> TMD provides data forecast hourly
  now <- Sys.time()
  round_time <- ceiling_date(now, unit = "hour")
  timestamp <- format(round_time, "%Y-%m-%dT%H:%M:%S")
  
  # build API URL
  fields_str <- paste(fields, collapse = ",")
  url <- paste0(
    "https://data.tmd.go.th/nwpapi/v1/forecast/area/box?",
    "domain=", domain,
    "&bottom-left=", bottom_left[1], ",", bottom_left[2],
    "&top-right=", top_right[1], ",", top_right[2],
    "&fields=", fields_str,
    "&starttime=", timestamp
  )
  
  # request
  headers <- add_headers(
    accept = "application/json",
    authorization = paste("Bearer", access_token)
  )
  response <- GET(url, headers)
  
  
  if (status_code(response) != 200) {
    message("Error: HTTP status ", status_code(response))
    print(content(response, as = "text"))
    return(invisible(NULL))
  }

  # parse xml content (request json but gives xml)
  xml_content <- content(response, as = "text", encoding = "UTF-8")
  xml_doc <- read_xml(xml_content)
  weather_nodes <- xml_find_all(xml_doc, ".//WeatherForecast")
  
  # prepare the right format
  rows_list <- list()

  for (wf in weather_nodes) {
    # extract lat/lon
    lat <- xml_double(xml_find_first(wf, "./location/lat"))
    lon <- xml_double(xml_find_first(wf, "./location/lon"))

    # extract all forecast nodes inside forecasts
    forecasts <- xml_find_all(wf, ".//forecast")

    for (fc in forecasts) {
      data_node <- xml_find_first(fc, "./data")

      # for each field, extract value as double
      for (feature in fields){}
      field_values <- lapply(fields, function(f) {
        xml_double(xml_find_first(data_node, paste0("./", f)))
      })
      names(field_values) <- fields
      time <- xml_text(xml_find_first(fc, "./time"))
      time_asia <- with_tz(ymd_hms(time), tzone = timezone)
      time_asia <- format(time_asia, "%Y-%m-%d %H:%M:%S")
      
      # combine into a data frame row
      row <- data.frame(
        as.list(field_values),
        lat = lat,
        lon = lon,
        time = time_asia,
        stringsAsFactors = FALSE
      )
      rows_list <- append(rows_list, list(row))  
    }
  }
  
  weather_df <- bind_rows(rows_list)

  # write in sql weather table
  dbWriteTable(sql_connection, "weather", weather_df, append = TRUE, row.names = FALSE)

  message("Weather data inserted into 'weather' table.")

  return(invisible(weather_df))
}


# call
fetch_weather_data(token_weather, 
                   bottom_left, 
                   top_right, 
                   fields_weather, 
                   domain_weather, 
                   con)