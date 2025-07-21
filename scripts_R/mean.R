# select last x interpolations, calculate mean and return the average raster


library(dplyr)
library(DBI)
library(raster)
library(lubridate)
library(RMariaDB)

# ----

# source variables and tools
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/mean.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# Use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)
tools_path <- file.path(project_root, "scripts_R", "tools.R")
source(tools_path)


# ----

# get the mean

query_mean <- "
        SELECT DISTINCT p.*
        FROM historic_interpolation p
        JOIN (
            SELECT MAX(Timestamp) AS latest_timestamp
            FROM historic_interpolation
        ) latest
        ON p.Timestamp = latest_timestamp;
        "

data <- dbGetQuery(con, query_mean)
matrix <- data$PM_25

# create raster
save_raster(lon_min, lat_min, lon_max, lat_max, matrix, 
            length(matrix)**(1/2), file.path(project_root, 
            'folder', 'filepath.tif'))
