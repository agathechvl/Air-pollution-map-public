# Updates fire data from NASA FIRMS API and saves it to a SQL database


library(readr)
library(DBI)
library(httr)
library(dplyr)

# ----

# source(variables.R/tools.R) that can be called from any directory
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/update_fires.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# Use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)
tools_path <- file.path(project_root, "scripts_R", "tools.R")
source(tools_path)




# Call the API
fetch_fire_data <- function(map_key, 
                            satellite, 
                            coordinates, 
                            day_range, 
                            sql_con) {
  # Construct the API URL
  url <- sprintf(
    "https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/%s/%s/%d",
    map_key, satellite, coordinates, day_range
  )
  
  tryCatch({
    # Read CSV directly from URL
    df <- read_csv(url)
    
    # For spatio-temporal random forest
    # Duplicate each row 24 times and add a timestamp column 
    # (today's date with each hour)

    expanded_df <- df %>%
      slice(rep(1:n(), each = 24)) %>%
      mutate(timestamp = as.POSIXct(sprintf("%s %02d:00:00", Sys.Date(), rep(0:23, times = nrow(df)))))
    
    # Write duplicated rows with timestamp to SQL table 'fire'
    dbWriteTable(sql_con, "fire", expanded_df, append = TRUE, row.names = FALSE)
    message("Fire data inserted into 'fire' table.")
  }, error = function(e) {
    message(sprintf("Failed to fetch or save fire data: %s", e$message))
  })
  
  invisible(NULL)
}

fetch_fire_data(
  map_key = token_fires,
  satellite = satellite,
  coordinates = coords_fires,
  day_range = day_range_fires,
  sql_con = con
)

