# Find calibration factors for AirGradient sensors
# There was a time lag between the sensor and the reference data -> corrected here

library(sf)
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# get script's absolute path
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

# use absolute path to call auxiliary scripts 
variables_path <- file.path(project_root, "scripts_R", "variables.R")
source(variables_path)
tools_path <- file.path(project_root, "scripts_R", "tools.R")
source(tools_path)
prepare_x_path <- file.path(project_root, "scripts_R", "prepare_x.R")
source(prepare_x_path)




# load sensor and reference data
sensors <- read_csv(file.path(project_root, "folder", "filepath"))
reference <- read_csv(file.path(project_root, "folder", "filepath"))

# rename columns
sensors <- sensors %>%
  rename(
    LocationId = "Location ID",
    timestamp = "UTC Date/Time",
    pm25 = "PM2.5 (μg/m³) raw",
    atmp = "Temperature (°C) raw",
    rhum = "Humidity (%) raw"
  ) %>%
  mutate(
    timestamp = ymd_hms(timestamp),
    time = floor_date(timestamp, unit = "minute")
  ) %>%
  dplyr::select(LocationId, time, pm25, atmp, rhum)

# prepare reference
reference <- reference %>%
  rename(
    time = "Date&Time", # fill with headers
    pm25_ref = "PM 2.5 (μg/m³)", # fill with headers
    atmp_ref = "Temperature", # fill with headers
    rhum_ref = "Humidity (%)" # fill with headers
  ) %>%
  mutate(
    time = as.POSIXct(time, format = "%d-%m-%Y %H:%M", tz = "UTC"),
    time = floor_date(time, unit = "minute")
  ) %>%
  dplyr::select(time, pm25_ref, atmp_ref, rhum_ref)

# join sensor and reference data to keep same time measurments
sensor_data <- sensors %>%
  inner_join(reference, by = "time") %>%
  drop_na(pm25, pm25_ref, atmp, atmp_ref, rhum, rhum_ref)

# calibrate
calibration_results <- sensor_data %>%
  group_by(LocationId) %>%
  group_map(~{
    df <- .x
    # prepare data
    if (nrow(df) < 10) return(NULL)
    df_lag <- df %>% drop_na(pm25, pm25_ref)

    # /!\ time lag between sensor and reference
    # cross correlation to detect lag (should be 92 minutes)
    ccf_result <- ccf(df_lag$pm25, df_lag$pm25_ref, lag.max = 200, plot = FALSE)
    best_lag <- ccf_result$lag[which.max(ccf_result$acf)]
    message("Lag for sensor ", unique(df$LocationId), ": ", best_lag, " minutes")

    # shift sensor values by detected lag
    df_lag <- df_lag %>%
      arrange(time) %>%
      mutate(pm25_shifted = dplyr::lag(pm25, n = best_lag),
             atmp_shifted = dplyr::lag(atmp, n = best_lag),
             rhum_shifted = dplyr::lag(rhum, n = best_lag))

    df_lag <- df_lag %>% drop_na(pm25_shifted, 
                                 pm25_ref, 
                                 atmp_shifted, 
                                 atmp_ref, 
                                 rhum_shifted, 
                                 rhum_ref)

    # linear regressions
    fit_pm25 <- lm(pm25_ref ~ pm25_shifted, data = df_lag)
    fit_atmp <- lm(atmp_ref ~ atmp_shifted, data = df_lag)
    fit_rhum <- lm(rhum_ref ~ rhum_shifted, data = df_lag)

    df_lag <- df_lag %>%
      mutate(
        calibrated_pm25 = predict(fit_pm25, df_lag),
        calibrated_atmp = predict(fit_atmp, df_lag),
        calibrated_rhum = predict(fit_rhum, df_lag)
      )

    # plot comparisons
    df_long_pm <- df_lag %>%
      dplyr::select(time, pm25_ref, pm25_shifted, calibrated_pm25) %>%
      pivot_longer(cols = c(pm25_ref, pm25_shifted, calibrated_pm25),
                   names_to = "variable", values_to = "value")

    plot_pm <- ggplot(df_long_pm, aes(x = time, y = value, color = variable)) +
      geom_line(alpha = 0.8) +
      scale_color_manual(
        values = c(
          pm25_ref = "black",
          pm25_shifted = "red",
          calibrated_pm25 = "blue"
        ),
        labels = c(
          pm25_ref = "Reference",
          pm25_shifted = "Raw Sensor (lag-corrected)",
          calibrated_pm25 = "Calibrated Sensor"
        ),
        name = "Legend"
      ) +
      labs(title = paste("PM 2.5 - Sensor", unique(df$LocationId)),
           y = "µg/m³", x = "Time") +
      theme_bw()

    # temperature and humidity plot
    df_long_temp <- df_lag %>%
      dplyr::select(time, atmp_ref, atmp_shifted, calibrated_atmp) %>%
      pivot_longer(cols = c(atmp_ref, atmp_shifted, calibrated_atmp),
                   names_to = "variable", values_to = "value")

    plot_temp <- ggplot(df_long_temp, aes(x = time, y = value, color = variable)) +
      geom_line(alpha = 0.8) +
      scale_color_manual(
        values = c(
          atmp_ref = "black",
          atmp_shifted = "red",
          calibrated_atmp = "blue"
        ),
        labels = c(
          atmp_ref = "Reference",
          atmp_shifted = "Raw Sensor (lag-corrected)",
          calibrated_atmp = "Calibrated Sensor"
        ),
        name = "Legend"
      ) +
      labs(title = paste("Temperature - Sensor", unique(df$LocationId)),
           y = "°C", x = "Time") +
      theme_bw()

    df_long_rhum <- df_lag %>%
      dplyr::select(time, rhum_ref, rhum_shifted, calibrated_rhum) %>%
      pivot_longer(cols = c(rhum_ref, rhum_shifted, calibrated_rhum),
                   names_to = "variable", values_to = "value")

    plot_rhum <- ggplot(df_long_rhum, aes(x = time, y = value, color = variable)) +
      geom_line(alpha = 0.8) +
      scale_color_manual(
        values = c(
          rhum_ref = "black",
          rhum_shifted = "red",
          calibrated_rhum = "blue"
        ),
        labels = c(
          rhum_ref = "Reference",
          rhum_shifted = "Raw Sensor (lag-corrected)",
          calibrated_rhum = "Calibrated Sensor"
        ),
        name = "Legend"
      ) +
      labs(title = paste("Humidity - Sensor", unique(df$LocationId)),
           y = "%", x = "Time") +
      theme_bw()

    # print coefficients and lag
    data.frame(
      LocationId = unique(df$LocationId),
      lag_minutes = best_lag,
      a_pm25 = coef(fit_pm25)[2], b_pm25 = coef(fit_pm25)[1],
      a_atmp = coef(fit_atmp)[2], b_atmp = coef(fit_atmp)[1],
      a_rhum = coef(fit_rhum)[2], b_rhum = coef(fit_rhum)[1]
    )
  }, .keep = TRUE) %>%
  bind_rows()

# save calibration results
write_csv(calibration_results, file.path(project_root, "data/calibration", "calibration_results.csv"))  

# plot all curves
df_all <- sensor_data %>%
  left_join(
    calibration_results %>% dplyr::select(LocationId, a_pm25, b_pm25),
    by = "LocationId"
  ) %>%
  mutate(
    calibrated_pm25 = a_pm25 * pm25 + b_pm25
  )

df_long <- df_all %>%
  dplyr::select(time, LocationId, pm25_ref, calibrated_pm25) %>%
  pivot_longer(cols = c(pm25_ref, calibrated_pm25),
               names_to = "variable", values_to = "value")

plot_all <- ggplot(df_long, aes(x = time, y = value, color = variable, group = interaction(LocationId, variable))) +
  geom_line(aes(linetype = variable)) +
  scale_color_manual(
    values = c(pm25_ref = "black", 
               calibrated_pm25 = "blue"),
    labels = c(pm25_ref = "Reference", 
               calibrated_pm25 = "Calibrated Sensors"),
    name = "Legend"
  ) +
  labs(title = "All Sensors: Calibrated PM 2.5 vs Reference",
       y = "µg/m³", x = "Time") +
  theme_bw()
