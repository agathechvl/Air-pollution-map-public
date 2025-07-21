# set of tools for data processing and interpolation

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

# source(variables.R) that can be called from any directory
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/tools.R"))
}

this_file <- get_script_path()
project_root <- dirname(dirname(this_file))

# use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)


# ----

# ordinary kriging interpolation
ok_interpolation <- function(coords_target, 
                             coords_source, 
                             values_source,
                             variogram_model = "Sph") {
  # if values are constant return values
  if (min(values_source) == max(values_source)) {
    return(rep(mean(values_source), nrow(coords_target)))
  }

  # prepare data format
  source_df <- data.frame(x = coords_source[, 1],
                          y = coords_source[, 2], values = values_source)
  coordinates(source_df) <- ~x + y
  proj4string(source_df) <- CRS("+proj=longlat +datum=WGS84")

  target_points <- SpatialPoints(coords_target,
                                proj4string = CRS("+proj=longlat +datum=WGS84"))
 
  # prepare variogram
  vgm_emp <- variogram(values ~ 1, data = source_df)
  
  vgm_fit <- fit.variogram(vgm_emp, model = vgm(psill = var(values_source),
                                                model = variogram_model,
                                                range = 1))
  
  # kriging
  kriging_result <- krige(values ~ 1, source_df, target_points, model = vgm_fit)
  

  return(kriging_result$var1.pred)
}

# ----

# nearest-neighbor interpolation 
kd_tree_interpolation <- function(coords_target, 
                                  coords_source, 
                                  values_source, 
                                  k = 4) {
  # if values are constant return values
  if (min(values_source) == max(values_source)) {
    return(rep(mean(values_source), nrow(coords_target)))
  }

  # get nearest neighbor index
  nn_result <- get.knnx(coords_source, coords_target, k = k)
  nearest_idx <- nn_result$nn.index[, 1]

  return(values_source[nearest_idx])
}

# ----

# inverse distance weighting interpolation
idw_interpolation <- function(coords_target, 
                              coords_source, 
                              values_source, 
                              k = 4, 
                              power = 2) {
  # if values are constant return values
  if (min(values_source) == max(values_source)) {
    return(rep(mean(values_source), nrow(coords_target)))
  }

  # calculate distances and weights
  nn <- get.knnx(coords_source, coords_target, k = k)
  neighbor_idx <- nn$nn.index
  weights <- 1 / nn$nn.dist
  weights <- weights ^ power
  weights[is.infinite(weights)] <- 0
  
  interpolated <- numeric(nrow(coords_target))

  for (i in seq_len(nrow(coords_target))) {
    interpolated[i] <- sum(weights[i, ] * values_source[neighbor_idx[i, ]]) / sum(weights[i, ])
  }
 
  return(interpolated)
}

# ----

# extract the raster value at specified coords, if no value -> IDW
extract_raster_value <- function(raster_path, coords, k = 200) {
  rast <- raster(raster_path)
  crs_raster <- crs(rast)
  
  coords_df <- as.data.frame(coords)
  coords_df <- coords_df[, 1:2]
  
  # reverse order for projection
  names(coords_df) <- c("lon", "lat")
  coords_df <- coords_df[, c("lon", "lat")]

  # reproject coordinates
  if (as.character(crs_raster) != "+proj=longlat +datum=WGS84 +no_defs") {
    coords_sp <- SpatialPoints(coords_df,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
    coords_proj <- spTransform(coords_sp, crs_raster)
    coords_df <- as.data.frame(coords_proj)
  }
  
  values <- raster::extract(rast, coords_df, cellnumbers = TRUE)
  raster_vals <- values[, 2]
  

  # find na cells in the raster
  na_indices <- which(is.na(raster_vals))

  if (length(na_indices) > 0) {
    # extract raster data
    raster_data <- as.vector(raster::values(rast))
    xy_coords <- raster::xyFromCell(rast, 1:ncell(rast))

    valid_idx <- which(!is.na(raster_data))
    train_coords <- xy_coords[valid_idx, ]
    train_vals <- raster_data[valid_idx]

    # idw for na values
    raster_vals[na_indices] <- idw_interpolation(
      coords_target = coords_df[na_indices, , drop = FALSE],
      coords_source = train_coords,
      values_source = train_vals,
      k = 4,
      power = 2
    )
  }

  return(raster_vals)
}

# ----

# save results to database
save_data_sql <- function(pred, 
                          grid, 
                          method, 
                          con, 
                          table="interpolation") {
  df <- as.data.frame(grid)
  df$PM_25 <- as.vector(pred)
  df$interpolation_method <- method
  df$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  dbWriteTable(con, 'interpolation', df, append = TRUE, row.names = FALSE)
  
}

# ----

# save metrics to database
save_metrics <- function(mse, 
                         rsq, 
                         method, 
                         con) {

  metrics <- data.frame(
    Mean_Squared_Error = mse,
    Interpolation_Method = method,
    Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    Coefficient_of_Determination = rsq
  )
  dbWriteTable(con, "metrics", metrics, append = TRUE, row.names = FALSE)
  
}

# ----

# save matrix as raster
save_raster <- function(lon_min, 
                        lat_min, 
                        lon_max, 
                        lat_max, 
                        pred_matrix, 
                        num_points, 
                        filename) {
  r <- rast(nrows=num_points, ncols=num_points, xmin=lon_min, xmax=lon_max, ymin=lat_min, ymax=lat_max)
  values(r) <- as.vector(t(pred_matrix))
  crs(r) <- "EPSG:4326"
  writeRaster(r, filename, overwrite=TRUE)
}

# ----

# give each sensor's feature a color according to its value
get_color_for_attribute <- function(attr, value, pal) {
  # na values
  if (is.na(value) || !is.numeric(value)) {
    return("gray")
  }
  
  if (attr == "atmp_corrected") {
    if (value < 15) return("blue")
    else if (value < 25) return("green")
    else return("red")
    
  } else if (attr == "rhum_corrected") {
    if (value < 30) return("yellow")
    else if (value < 60) return("green")
    else return("blue")
    
  } else if (attr == "rco2_corrected") {
    if (value < 700) return("green")
    else if (value < 1000) return("orange")
    else return("red")
    
  } else if (attr == "tvoc") {
    if (value < 150) return("green")
    else if (value < 250) return("orange")
    else return("red")
    
  } else if (attr %in% c("pm01_corrected", "pm02_corrected")) {
    return(pal(value))
    
  } else if (attr == "pm10_corrected") {
    if (value < 150) return("green")
    else if (value < 300) return("orange")
    else return("red")
  }
  
  return("gray")
}

# ----

# give a value to each coordinate, depicting its fire environment
distance_fire <- function(coords_target, fire_data) {
  list_distance <- c()
  nb_rows <- nrow(fire_data)
  

  if (nb_rows == 0) {
    list_distance <- rep(0, nrow(coords_target)) 
    return(list_distance)
  } else {
    # delete doubles
    fire_data <- fire_data %>%
      mutate(latlon_sum = latitude + longitude)

    fire_data_cleaned <- fire_data %>%
      arrange(latlon_sum) %>%
      mutate(group = cut(latlon_sum, breaks = seq(min(latlon_sum), 
                         max(latlon_sum) + 0.05, by = 0.05), 
                         include.lowest = TRUE)) %>%
      group_by(group) %>%
      summarise(
        longitude = mean(longitude),
        latitude = mean(latitude),
        frp = mean(frp),
        .groups = "drop"
      )
    
    # extract coords and frp
    coords_fire <- dplyr::select(fire_data_cleaned, longitude, latitude)
    frp <- fire_data_cleaned$frp

    coords_source_num <- apply(coords_fire[, c(1, 2)], 2, as.numeric)
    for (i in seq_len(nrow(coords_target))) {
      lon1 <- coords_target[i, 1]
      lat1 <- coords_target[i, 2]

      # calculate distances in km 
      distances <- distHaversine(matrix(c(lon1, lat1), ncol = 2),
                                 coords_source_num) / 1000
      
      distances[distances == 0] <- 1e-8  # avoid division by zero
      distances <- (1 / distances) * frp

      list_distance <- append(list_distance, 
                              mean(distances, na.rm = TRUE), after=0)
    }

  }

  return(list_distance)
}

# ----


# Creates a raster where each pixel contains the inverse distance to the nearest road 
# To use only once to create the raster 

distance_to_road <- function(roads_shp) {
  
  lon_seq <- seq(lon_min, lon_max, length.out = 500)
  lat_seq <- seq(lat_min, lat_max, length.out = 500)
  grid <- expand.grid(longitude = lon_seq, latitude = lat_seq)

  coords_sf <- st_as_sf(grid, coords = c("longitude", "latitude"), crs = 4326)
  roads_sf <- st_as_sf(roads_shp)

  distances_matrix <- st_distance(coords_sf, roads_sf)
  distances <- apply(distances_matrix, 1, min)
  distances <- distances * 10
  
  distances[distances == 0] <- 0.1

  # find a value for each pixel that defines its distance to the roads
  list_distances <- round(abs(log2(1+1/distances*1000)), digits = 2)

  mat <- matrix(list_distances, nrow = 500, ncol = 500, byrow = FALSE)

  mat_flipped <- mat[, seq(nrow(mat), 1)]

  list_distances <- as.vector(mat_flipped)
 
  save_raster(lon_min, lat_min, lon_max, lat_max, 
              list_distances, 500, 
              roads)
}

