# Archive data from the last 15 minutes in the database
# ----

library(sp)
library(sf)
library(DBI)
library(RMariaDB)
library(dplyr)


# source variables
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/data_storage.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# Use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)

# ----

# get last measurments
query = paste("
        SELECT p.*
        FROM sensors p
        WHERE p.timestamp > (NOW() - INTERVAL", old_sensor_storage_frequency, ");"
        )

latest_data <- dbGetQuery(con, query)

# ----

# average last measurments
average_data <- latest_data %>%
  group_by(locationId) %>%
  summarise(
    # Mean for numeric columns
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    # First value for non-numeric columns
    across(where(~!is.numeric(.)), ~ first(.)),
    .groups = "drop"
  )

time <- format(as.POSIXct(Sys.time(), tz = "UTC"), tz = timezone, usetz = FALSE)
average_data$timestamp <- time

dbWriteTable(con, 'historic_sensors', average_data, append = TRUE, row.names = FALSE)

# ----

# store last measurments
latest_time <- dbGetQuery(con, "SELECT MAX(timestamp) AS max_time FROM sensors")$max_time
cutoff_time <- as.POSIXct(latest_time) - days(sensor_storage_time)

query <- paste0("DELETE FROM sensors WHERE timestamp < '", format(cutoff_time, "%Y-%m-%d %H:%M:%S"), "';")
dbExecute(con, query)

dbDisconnect(con)  