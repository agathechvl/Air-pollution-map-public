# Train Random Forest
# Takes variables, create datasets and train the model with it
# Parameters that can be changed:
# - training variables
# - number of trees


library(sf)
library(DBI)
library(RMariaDB)
library(dplyr)
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
  return(normalizePath("scripts_R/train.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)
tools_path <- file.path(project_root, "scripts_R", "tools.R")
source(tools_path)
prepare_x_path <- file.path(project_root, "scripts_R", "prepare_x.R")
source(prepare_x_path)


# ----

# function to train the model
rfst_train <- function(pm25, 
                       weather, 
                       fires_sql, 
                       rasters) {

  # convert timestamps
  
  #pm25$timestamp <- lubridate::ymd_hms(pm25$timestamp, tz = "UTC")
  pm25$timestamp <- as.POSIXct(pm25$timestamp, tz = "UTC")
  weather$time <- as.POSIXct(weather$time, tz = "UTC")
 
  # fix differences in sensors timestamps
  pm25 <- pm25 %>%
  arrange(timestamp) %>%
  mutate(group = cumsum(c(1, diff(timestamp) > 50))) %>%
  group_by(group) %>%
  mutate(timestamp = as.POSIXct(mean(as.numeric(timestamp)), origin = "1970-01-01", tz = "UTC")) %>%
  ungroup() %>%
  dplyr::select(-group)
  
  # remove duplicates
  weather <- weather[!duplicated(weather), ]
  weather$time <- weather$time - as.difftime(1, units = "hours")

  # duplicate weather rows for each PM 2.5 timestamp and add the new timestamp
  weather_split <- list()  
  
  for (i in seq_along(weather$time)) {
    hour_ts <- weather$time[i]
    if (is.na(hour_ts)) {
      next
    }
    start_time <- hour_ts
    end_time <- hour_ts + 3600 

  
    # get distinct PM 2.5 timestamps within the hour
    timestamps <- unique(pm25$timestamp[pm25$timestamp >= start_time 
                                        & pm25$timestamp < end_time])
    
    # duplicate weather rows and change timestamp
    if (length(timestamps) > 0) {
      weather_repeated <- weather[rep(i, length(timestamps)), ]
      weather_repeated$time <- timestamps

      weather_split[[length(weather_split) + 1]] <- weather_repeated
    }
  }

  # Ccmbine all rows into one data frame
  new_weather <- do.call(rbind, weather_split)
  

  ref_times <- sort(unique(new_weather$time))
  if (length(ref_times) == 0) {
    warning("No pm 25 timestamps, check SQL table weather")
  return(NULL)
  }
  
  x <- list()
  y <- c()

  # create the training set
  for (t in ref_times) {
    t_posix <- as.POSIXct(t, origin = "1970-01-01", tz = "UTC")

    # use pm 2.5 timestamps as temporal references
    pm25_t <- pm25 %>%
              filter(timestamp == t) %>%
              distinct()
    
    if (nrow(pm25_t)==0) {
      print("weather and pm25 timestamps do not fit. Check code")
      next
    }
    
    coords_pm25 <- unique(dplyr::select(pm25_t, longitude, latitude))
    pm25_values <- pm25_t$pm02_corrected
    
    co2 <- pm25_t$rco2_corrected
    tvoc <- pm25_t$tvoc
    temp <- pm25_t$atmp_corrected
    hum <- pm25_t$rhum_corrected
    
    fires_t <- fires_sql %>%
      filter(
        timestamp >= (t - seconds(1800)) & timestamp <= (t + seconds(1800))
      ) %>%
      distinct(longitude, latitude, frp)

    # add temporal features
    hour_val <- hour(t_posix)
    dow_val <- wday(t_posix)
    doy_val <- yday(t_posix)
    
    temporal_features <- data.frame(
      datetime = t_posix,
      sin_weekday = sin(2 * pi * dow_val / 6),
      cos_weekday = cos(2 * pi * dow_val / 6),
      sin_hour = sin(2 * pi * hour_val / 24), # periodicity of 24 hours
      cos_hour = cos(2 * pi * hour_val / 24), # periodicity of 24 hours
      sin_yearday = sin(2 * pi * doy_val / 365), # periodicity of 365 days
      cos_yearday = cos(2 * pi * doy_val / 365) # periodicity of 365 days
    )[rep(1, nrow(coords_pm25)), ]
    
    # training set
    x_t <- prepare_x(coords_pm25, 
                     new_weather, 
                     rasters, 
                     fires_t,
                     co2, 
                     tvoc, 
                     temp, 
                     hum, 
                     temporal_features = temporal_features)
    

    if (!is.null(x_t)) {
      x <- append(x, list(x_t))
      y <- c(y, pm25_values)
      } else {
        warning("prepare_x returned NULL for time ", t, ". Skipping this time point.")
      }
    }
   
  if (length(y) < 10) {
    warning("Not enough data for training")
    return(NULL)
  }

  # model training
  x_train <- do.call(rbind, x)
  model <- randomForest(x_train, y, ntree = 100, importance = TRUE)

  importance_vals <- importance(model, type = 1)
  print(importance_vals)
  print(model)
  varImpPlot(model, type = 1, main = "Permutation Importance (Mean Decrease Accuracy)")
  saveRDS(model, file = file.path(project_root, "folder", "latest_model.rds"))

  return(model)
}

# Call
model <- rfst_train(pm25, 
                    weather, 
                    fires,
                    list(ndvi = ndvi, 
                         evi = evi, 
                         pop = pop, 
                         lc = lc, 
                         dem = dem, 
                         roads = roads)
                   )