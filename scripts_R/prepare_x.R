# Creates a set of features for the Random Forest model

library(gstat)
library(sp)
library(raster)
library(sf)
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(terra)
library(FNN)
library(geosphere)
library(tidyr)

# ----

# source(tools.R) that can be called from any directory
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/prepare_x.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

tools_path <- file.path(project_root, "scripts_R", "tools.R")
source(tools_path)

# ----

# apply the interpolation method to the target coordinates
interpolate <- function(coords_target, 
                        coords_source, 
                        values_source, 
                        interpolation_method) {
  method <- interpolation_method[[1]]
  variogram <- if (length(interpolation_method) > 1) interpolation_method[[2]]
    else "Exp"
  k <- if (length(interpolation_method) > 2) as.numeric(interpolation_method[[3]])
    else 4


  if (method == "OK") {
    ok_interpolation(coords_target, coords_source, values_source,
                     variogram_model = variogram)
  } else if (method == "CKD") {
    kd_tree_interpolation(coords_target, coords_source, values_source, k = k)
  } else if (method == "IDW") {
    idw_interpolation(coords_target, coords_source, values_source, k = k)
  } else {
    stop("Invalid interpolation method. Choose 'OK', 'IDW' or 'CKD'.")
  }
}

# ----

# creates the set of features
prepare_x <- function(coords_target, 
                      weather, 
                      rasters, 
                      fires_sql, 
                      co2, 
                      tvoc, 
                      temp, 
                      hum,
                      temporal_features = NULL) {

  coords_weather <- weather[c("lon", "lat")]
  weather_vars <- c("tc", "rh", "slp", "rain", "ws10m", "wd10m")
  
  # interpolate weather variables on target coordinates
  features <- lapply(weather_vars, function(var) {
    if (var == "rain") {
      interpolate(coords_target, coords_weather, weather[[var]], c("CKD"))
    } else {
      interpolate(coords_target, coords_weather, weather[[var]], c("IDW"))
    }
  })
  
  # extract raster values for each target coordinate
  raster_features <- lapply(rasters, function(r)
                              extract_raster_value(r, coords_target))
  
  features <- c(coords_target, 
              features,  
              raster_features)
  
  # get the fire value 
  fire_data <- dplyr::select(fires_sql, longitude, latitude, frp)
  fires <- distance_fire(coords_target, fire_data)
  
  if (!is.null(fires)) {
    features <- c(features, list(fire = fires))
  } else {
    features <- c(features, list(fire = rep(NA, nrow(coords_target))))
  }

  
  # add CO2, TVOC, temperature and humidity 
  features <- c(features, 
                 list(co2 = co2, tvoc = tvoc, temp = temp, hum = hum))
  
  col_names <- c("lon", "lat", weather_vars, "fire", 
                 names(rasters), "co2", "tvoc", "temp", "hum")
  
  # add time 
  if (!is.null(temporal_features)) {
    features <- c(features, list(temporal_features))
    col_names <- c(col_names, colnames(temporal_features))
  }
  
  features <- do.call(cbind, features)
  
  
  # set the column names
  colnames(features) <- col_names
  features <- as.data.frame(features)
  
  return(features)
}

