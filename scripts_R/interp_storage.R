# Keep last X hours interpolations in the active database
# archive the deleted data in the historic_interpolation table

# ----

library(sp)
library(sf)
library(DBI)
library(RMariaDB)
library(dplyr)


# ----

# source variables 
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/interp_storage.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)

# ----

# get the last interpolations
query = paste("
        SELECT p.*
        FROM interpolation p
        WHERE p.Timestamp > (NOW() - INTERVAL", old_interp_storage_frequency, ");"
        ) 
latest_data <- dbGetQuery(con, query)

# ----

# average data and store in historic_interpolation
average_data <- latest_data %>%
  group_by(latitude, longitude) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

time <- format(as.POSIXct(Sys.time(), tz = "UTC"), tz = timezone, usetz = FALSE)

average_data$Timestamp <- time
average_data$interpolation_method <- latest_data$interpolation_method[1]  

dbWriteTable(con, 'historic_interpolation', average_data, append = TRUE, row.names = FALSE)

# ----

# clean up old data
latest_time <- dbGetQuery(con, "SELECT MAX(Timestamp) AS max_time FROM interpolation")$max_time
cutoff_time <- as.POSIXct(latest_time) - days(interp_storage_time)

query <- paste0("DELETE FROM interpolation WHERE Timestamp < '", format(cutoff_time, "%Y-%m-%d %H:%M:%S"), "';")
dbExecute(con, query)

dbDisconnect(con)  