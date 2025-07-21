#!/usr/bin/env Rscript
# Predict PM 2.5 on a grid using Random Forest model
# Choose carefully the number of points for the grid
# as it will affect the performance of the prediction but require storage (<200)

library(gstat)
library(raster)
library(sf)
library(DBI)
library(RMariaDB)
library(dplyr)
library(terra)
library(FNN)
library(geosphere)
library(tidyr)
library(randomForest)
library(lubridate)

# ----

# source the variables and tools scripts
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/predict.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# Use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)
tools_path <- file.path(project_root, "scripts_R", "tools.R")
source(tools_path)
prepare_x_path <- file.path(project_root, "scripts_R", "prepare_x.R")
source(prepare_x_path)

# ----

# function for prediction
rfst_predict <- function(model, 
                         pm25_rt, 
                         weather_rt, 
                         fires_rt,
                         rasters, 
                         pred_raster_path,
                         lon_min, 
                         lon_max, 
                         lat_min, 
                         lat_max, 
                         con, 
                         num_points = 100) {

  # create prediction grid on target area
  lon_seq <- seq(lon_min, lon_max, length.out = num_points)
  lat_seq <- seq(lat_min, lat_max, length.out = num_points)
  grid <- expand.grid(longitude = lon_seq, latitude = lat_seq)

  # get temporal features for now
  now <- Sys.time()
  now <- as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
  hour_val <- hour(now)
  dow_val <- wday(now)
  doy_val <- yday(now)
  temporal_features_rt <- data.frame(
    datetime = now,
    hour = hour_val,
    weekday = dow_val,
    sin_hour = sin(2 * pi * hour_val / 24),
    cos_hour = cos(2 * pi * hour_val / 24),
    sin_yearday = sin(2 * pi * doy_val / 365),
    cos_yearday = cos(2 * pi * doy_val / 365)
  )[rep(1, nrow(grid)), ]


  co2 <- pm25_rt$rco2_corrected
  tvoc <- pm25_rt$tvoc
  temp <- pm25_rt$atmp_corrected
  hum <- pm25_rt$rhum_corrected

  # get values on the grid
  co2_interpolated <- interpolate(grid, 
                                  pm25_rt[c("longitude", "latitude")], 
                                  co2, 
                                  c("IDW"))
  tvoc_interpolated <- interpolate(grid, 
                                   pm25_rt[c("longitude", "latitude")], 
                                   tvoc, 
                                   c("IDW"))
  temp_interpolated <- interpolate(grid, 
                                   pm25_rt[c("longitude", "latitude")], 
                                   temp, 
                                   c("IDW"))
  hum_interpolated <- interpolate(grid, 
                                  pm25_rt[c("longitude", "latitude")], 
                                  hum, 
                                  c("IDW"))

  
  # prediction set
  x_pred <- prepare_x(grid,
                      weather_rt, 
                      rasters, 
                      fires_rt,
                      co2_interpolated,
                      tvoc_interpolated, 
                      temp_interpolated, 
                      hum_interpolated,
                      temporal_features = temporal_features_rt)


  # prediction
  pred <- predict(model, x_pred)

  # save prediction
  method_name <- "RFST"
  save_data_sql(pred, grid, method_name, con)
  
  # save metrics
  mse <- tail(model$mse, 1)
  rsq <- tail(model$rsq, 1)

  cat("OOB MSE:", mse, "\n")
  cat("OOB R-squared:", rsq, "\n")

  save_metrics(mse, rsq, method_name, con)
  
  # save the raster of predictions
  save_raster(lon_min, lat_min, lon_max, lat_max, 
              pred, num_points, pred_raster_path)

  
  return(pred)
}

# call
model <- readRDS(file.path(project_root, "folder", "latest_model.rds"))
pred <- rfst_predict(model, 
                     pm25_rt, 
                     weather_rt, 
                     fires_rt,
                     list(ndvi = ndvi, 
                          evi = evi, 
                          pop = pop, 
                          lc = lc, 
                          dem = dem, 
                          roads = roads),
                     prediction_raster, 
                     lon_min, 
                     lon_max, 
                     lat_min, 
                     lat_max, 
                     con, 
                     num_points = nb_points)


dbDisconnect(con)