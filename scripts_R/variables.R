# Contains all the variables used in the R scripts of the project

library(sf)
library(dplyr)
library(DBI)
library(raster)
library(readr)
library(lubridate)
library(RMariaDB)
library(sp)
library(glue)
library(leaflet)

# ----

# get root path
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/variables.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# ----

# GPS DATA
park <- st_read(file.path(project_root, 'folder', 'file_path.shp'))
roads_shp <- st_read(file.path(project_root, 'folder', 'file_path.shp'))
studied_zone <- st_read(file.path(project_root, 'folder', 'file_path.geojson'))
villages <- read_csv(file.path(project_root, 'folder', 'file_path.csv'))

park <- st_transform(park, 4326)
roads <- st_transform(roads, 4326)

park_2d <- sf::st_set_geometry(park, 
               sf::st_cast(sf::st_zm(sf::st_geometry(park), drop = TRUE)))

# ----

# TOPOGRAPHIC RASTERS
ndvi <- file.path(project_root, 'folder', 'file_path.tif')
evi <- file.path(project_root, 'folder', 'file_path.tif')
pop <- file.path(project_root, 'folder', 'file_path.tif')
lc <-  file.path(project_root, 'folder', 'file_path.tif')
dem <-  file.path(project_root, 'folder', 'file_path.tif')
roads <-  file.path(project_root, 'folder', 'file_path.tif')

# ----

# LIMITS OF THE STUDIED ZONE
bounds <- sf::st_bbox(studied_zone)  
lon_min <- bounds["xmin"] + 0.0001
lon_max <- bounds["xmax"] - 0.0001
lat_min <- bounds["ymin"] + 0.0001
lat_max <- bounds["ymax"] - 0.0001

# ----

# CONNECTION TO THE DATABASE
con <- dbConnect(
  MariaDB(),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME")
)

# ----

# REAL TIME DATA
query_fires <- "
        SELECT p.*
        FROM fire p
        JOIN (
            SELECT MAX(acq_date) AS latest_timestamp
            FROM fire
        ) latest
        ON p.acq_date = latest_timestamp;
        "

query_weather <- "
        SELECT p.*
        FROM weather p
        JOIN (
            SELECT MAX(time) AS latest_timestamp
            FROM weather
        ) latest
        ON p.time = latest_timestamp;
        "


query_pm25 <- "
    SELECT p.*
    FROM sensors p
    JOIN (
        SELECT MAX(timestamp) AS latest_timestamp
        FROM sensors
    ) latest
    ON ABS(TIMESTAMPDIFF(SECOND, p.timestamp, latest.latest_timestamp)) <= 30;
"

weather_rt <- dbGetQuery(con, query_weather)
fires_rt <- dbGetQuery(con, query_fires)
pm25_rt <- dbGetQuery(con, query_pm25)

if (is.na(fires_rt$timestamp[1]) || fires_rt$timestamp[1] < Sys.time() - hours(1)) {
  fires_rt <- data.frame(
    timestamp = as.POSIXct(character()),
    longitude = numeric(),
    latitude = numeric(),
    frp = numeric()
  )
}


# ----

# ALL DATA
weather <- dbGetQuery(con, "SELECT * FROM weather")
pm25 <- dbGetQuery(con, "SELECT * FROM historic_sensors")
fires <- dbGetQuery(con, "SELECT * FROM fire")

# ----

# PREDICTION
nb_points <- 200
prediction_raster <-  file.path(project_root, 'folder', 'file_path.tif')
r_mean <-  file.path(project_root, 'folder', 'file_path.tif')

# ----

# LINK TO THE HTML MAP
html_path <-  file.path(project_root, 'folder', 'file_path.html')

# ----

# UPDATE_FIRES SETTINGS
token_fires <- ''
satellite <- 'VIIRS_NOAA21_NRT'
coords_fires <- ''  # lon min,lat min,lon max,lat max
day_range_fires <- 2 

# ----

# UPDATE_SENSOR SETTINGS
sensor_coords <- list(
  "Location_ID" = c(lat, lon)
  "Location_ID" = c(lat, lon)
  "Location_ID" = c(lat, lon)
  "Location_ID" = c(lat, lon)
  "Location_ID" = c(lat, lon)
)

token_sensor <- ""

# ----

# UPDATE_WEATHER SETTINGS
token_weather <- ''
bottom_left <- c(lat, lon) # defining area
top_right <- c(lat, lon) # defining area
fields_weather <- c('tc', 'rh', 'rain', 'slp', 'ws10m', 'wd10m')
domain_weather <- 2

# ----

# DATA STORAGE
sensor_storage_time <- 30 #days
old_sensor_storage_frequency <- "15 MINUTE"
interp_storage_time <- 5 #days
old_interp_storage_frequency <- "8 HOUR"

# ----

# TIMEZONE
timezone <- "" # e.g., "Asia/Bangkok"

