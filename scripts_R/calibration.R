# Find calibration factors for AirGradient sensors


library(sf)
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(ggplot2)

# Get script's absolute path
get_script_path <- function() {
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(sf)) return(normalizePath(sf))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg)))
  return(normalizePath("scripts_R/calibration.R"))
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

# Humidity and Temperature
# Linear regression

reference = dbGetQuery(con, 
                       "SELECT * FROM weather 
                        WHERE time > '2025-04-30 12:00:00';")

sensor = dbGetQuery(con, 
                       "SELECT * FROM sensors 
                        WHERE timestamp > '2025-04-30 12:00:00';")
#dbDisconnect(con)
# Average sensor data by hour

# average last hour
sensor_hourly <- sensor %>%
  mutate(time = floor_date(timestamp, unit = "hour")) %>% 
  group_by(latitude, longitude, time) %>%
  summarise(
    atmp = mean(atmp_corrected, na.rm = TRUE),
    rhum = mean(rhum_corrected, na.rm = TRUE),
    .groups = 'drop'
  )


# Ensure timestamps align for comparison with reference
ref_time <- reference %>%
  filter(time %in% sensor_hourly$time) %>%
  pull(time)

ref_time <- unique(ref_time)

sensor_hourly <- sensor_hourly %>%
    filter(time %in% ref_time) 

sensor_hourly <- as.data.frame(sensor_hourly)
sensor_hourly_interp <- sensor_hourly

# Interpolate reference data to match sensor locations
for (t in ref_time) {
  reference_t <- reference %>%
    filter(time == t)
  reference_t <- reference_t[!duplicated(reference_t), ]
   
  sensor_t <- sensor_hourly %>%
    filter(time == t)
  sensor_t <- sensor_t[!duplicated(sensor_t), ]
  sensor_t <- as.data.frame(sensor_t)
  


  # Interpolate temperature and humidity for the current timestamp
  interpolated_atmp <- interpolate(
    sensor_t[c("longitude", "latitude")],
    reference_t[c("lon", "lat")],
    reference_t$tc,
    "IDW"
  )
  
  interpolated_rhum <- interpolate(
    sensor_t[c("longitude", "latitude")],
    reference_t[c("lon", "lat")],
    reference_t$rh,
    "IDW"
  )
  
  sensor_hourly_interp[sensor_hourly_interp$time == t, "atmp_ref"] <- interpolated_atmp
  sensor_hourly_interp[sensor_hourly_interp$time == t, "rhum_ref"] <- interpolated_rhum
}

# Linear regression: reference = a * sensor + b ---
fit_atmp <- lm(atmp_ref ~ atmp, data = sensor_hourly_interp)
a_atmp <- coef(fit_atmp)[2]
b_atmp <- coef(fit_atmp)[1]
cat("Calibration equation: y = ", round(a_atmp, 4), " * x + ", round(b_atmp, 4), "\n")

fit_rhum <- lm(rhum_ref ~ rhum, data = sensor_hourly_interp)
a_rhum <- coef(fit_rhum)[2]
b_rhum <- coef(fit_rhum)[1]
cat("Calibration equation: y = ", round(a_rhum, 4), " * x + ", round(b_rhum, 4), "\n")

# Apply calibration to sensor data
sensor_hourly_interp <- sensor_hourly_interp %>%
 mutate(calibrated_atmp = a_atmp * atmp + b_atmp,
        calibrated_rhum = a_rhum * rhum + b_rhum)

# Plot
p_atmp <- ggplot(sensor_hourly_interp, aes(x = time)) +
  geom_line(aes(y = atmp_ref), color = "black", size = 1) +
  geom_line(aes(y = atmp), color = "red", linetype = "solid", alpha = 0.6) +
  geom_line(aes(y = calibrated_atmp), color = "blue", linetype = "solid", alpha = 0.8) +
  labs(title = "Temperature: Sensor (raw and calibrated) vs Reference",
       x = "Time", y = "Temperature (°C)",
       caption = "Black = Reference, Red = Sensor (raw), Blue = Sensor (calibrated)") +
  theme_minimal()



pm25_all <- dbGetQuery(con, "SELECT *
                             FROM sensors
                             WHERE timestamp > '2025-06-10 12:00:00' ;
                             ")
dbDisconnect(con)

pm25_all <- pm25_all %>%
  mutate(hour = hour(timestamp),
         timestamp = as.POSIXct(timestamp, tz = "UTC"),
         timestamp_cos = cos(2 * pi * hour/24)*12,
         timestamp_sin = sin(2 * pi * hour/24)*12
   )


# Link temperature and PM 2.5
p <- ggplot(pm25_all, aes(x = timestamp_cos)) +
  geom_point(aes(y = atmp_corrected, color = "red", alpha = 0.6)) +
  labs(title = "Temperature: Sensor (raw and calibrated) vs PM 2.5",
       x = "Time", y = "Temperature (°C)/PM 2.5",
       caption = "Black = ATMP, Red = PM 2.5 (raw)") +
  theme_minimal()

print(p)


# PM 2.5
# Do the same -> find different levels like EPA calibration (pm < 30, 30 < pm < 50, etc.)
# Example data (replace with your real data)
# sensor <- c(...)       # vector of your sensor readings
# reference <- c(...)    # vector of reference sensor readings

sensors <- read_csv(file.path(project_root, "data", "sensors.csv"))
reference <- read_csv(file.path(project_root, "data", "reference.csv"))

# Combine into a data frame
df <- data.frame(sensor, reference)

# Define your intervals
ranges <- list(
  "0-30"    = c(0, 30),
  "30-50"   = c(30, 50),
  "50-210"  = c(50, 210),
  "210-260" = c(210, 260),
  "260+"    = c(260, Inf)
)

# Function to fit linear model in each range
fit_model_in_range <- function(df, min_val, max_val) {
  subset <- df %>% filter(reference >= min_val, reference < max_val)
  
  if (nrow(subset) >= 5) {  # only fit if enough data
    model <- lm(reference ~ sensor, data = subset)
    slope <- coef(model)[["sensor"]]
    intercept <- coef(model)[["(Intercept)"]]
    return(data.frame(
      range = paste0(min_val, "-", ifelse(is.infinite(max_val), "Inf", max_val)),
      slope = slope,
      intercept = intercept,
      n = nrow(subset)
    ))
  } else {
    return(data.frame(
      range = paste0(min_val, "-", ifelse(is.infinite(max_val), "Inf", max_val)),
      slope = NA,
      intercept = NA,
      n = nrow(subset)
    ))
  }
}

# Apply over all ranges
results <- do.call(rbind, lapply(ranges, function(r) {
  fit_model_in_range(df, r[1], r[2])
}))

# Show the calibration factors
print(results)