This project aims to develop a high-resolution map of PM 2.5 in Nan Province,
available in real-time with cron jobs
Sensors used are AirGradient Open Air 
Provided scripts generate a raster with PM 2.5 spatial predictions, stored in a 
SQL database
From this raster, the user can create an html map using leaflet (code not 
provided here)


Installation:

1. Install R packages

install.packages(c("s2", "sf", "terra", "gstat", "tidyr", "leaflet", 
                   "shiny", "DBI", "ggplot2", "dplyr",  "raster", "FNN", 
                   "geosphere", "randomForest", “lubridate", "glue", 
                   "htmltools", "htmlwidgets", "readr", "httr", "jsonlite", 
                   "xml2", “here”
                   ))

2. Git clone the project

3. Fill in variables.R with your data and tokens 
   Tokens can be created on the API websites (provided below)

3. Install SQL database
   install sql server: bash > sudo apt install sql-server
   create database: bash > sudo mysql
   define user: mysql > CREATE USER 'username'@'host' IDENTIFIED BY 'password';
   create tables:   mysql > CREATE DATABASE sensors;
                    mysql > CREATE TABLE fire (
	                            latitude FLOAT,
	                            longitude FLOAT,
	                            bright_ti4 FLOAT,
	                            scan FLOAT,
	                            track FLOAT,
	                            acq_date DATE,
	                            acq_time INT,
	                            satellite VARCHAR(255),
	                            instrument VARCHAR(255),
	                            confidence VARCHAR(255),
	                            version VARCHAR(255),
	                            bright_ti5 FLOAT,
	                            frp FLOAT,
	                            daynight VARCHAR(255),
	                            timestamp TIMESTAMP
                             );                                     

                            CREATE TABLE sensors (
                            	locationId INT,
                            	locationName VARCHAR(255),
                            	pm01 FLOAT,
                            	pm02 FLOAT,
                            	pm10 FLOAT,
                            	pm01_corrected FLOAT,
                            	pm02_corrected FLOAT,
                            	pm10_corrected FLOAT,
                            	pm003count FLOAT,
                            	atmp FLOAT,
                            	rhum FLOAT,
                            	rco2 FLOAT,
                            	atmp_corrected FLOAT,
                            	rhum_corrected FLOAT,
                            	rco2_corrected FLOAT,
                            	tvoc FLOAT,
                            	wifi VARCHAR(255),
                            	timestamp DATETIME,
                            	serialno VARCHAR(255),
                            	model VARCHAR(255),
                            	firmwareVersion VARCHAR(255),
                            	tvocIndex INT,
                            	noxIndex INT,
                            	latitude FLOAT,
                            	longitude FLOAT
                            );
                            
                            CREATE TABLE historic_sensors (
                            	locationId INT,
                            	locationName VARCHAR(255),
                            	pm01 FLOAT,
                            	pm02 FLOAT,
                            	pm10 FLOAT,
                            	pm01_corrected FLOAT,
                            	pm02_corrected FLOAT,
                            	pm10_corrected FLOAT,
                            	pm003count FLOAT,
                            	atmp FLOAT,
                            	rhum FLOAT,
                            	rco2 FLOAT,
                            	atmp_corrected FLOAT,
                            	rhum_corrected FLOAT,
                            	rco2_corrected FLOAT,
                            	tvoc FLOAT,
                            	wifi VARCHAR(255),
                            	timestamp DATETIME,
                            	serialno VARCHAR(255),
                            	model VARCHAR(255),
                            	firmwareVersion VARCHAR(255),
                            	tvocIndex INT,
                            	noxIndex INT,
                            	latitude FLOAT,
                            	longitude FLOAT
                            );
                            
                            CREATE TABLE weather (
                            	time DATETIME,
                            	lat FLOAT,
                            	lon FLOAT,
                            	tc FLOAT,
                            	rh FLOAT,
                            	rain FLOAT,
                            	slp FLOAT,
                            	ws10m FLOAT,
                            	wd10m FLOAT
                            );
                            
                            CREATE TABLE interpolation_mean (
                            	latitude FLOAT,
                            	longitude FLOAT,
                            	PM_25 FLOAT,
                            	Timestamp DATETIME,
                            	interpolation_method VARCHAR(255)
                            );
                            
                            CREATE TABLE interpolation(
                            	latitude FLOAT,
                            	longitude FLOAT,
                            	PM_25 FLOAT,
                            	Timestamp DATETIME,
                            	interpolation_method VARCHAR(255)
                            );
                            
                            CREATE TABLE interpolation_historic (
                            	latitude FLOAT,
                            	longitude FLOAT,
                            	PM_25 FLOAT,
                            	Timestamp DATETIME,
                            	interpolation_method VARCHAR(255)
                            );
                            
                            CREATE TABLE metrics (
                            	Mean_Squared_Error FLOAT,
                            	Interpolation_Method VARCHAR(255),
                            	Timestamp DATETIME,
                            	Coefficient_of_Determination FLOAT
                            );


   create a file .Renviron with the connection to the database: 
   bash > nano ~/.Renviron
   ini > DB_USER=you_user_mysql
         DB_PASS=your_password
         DB_HOST=localhost
         DB_NAME=sensors
   

4. Run files regularly to update the map via cron jobs
   bash > crontab -e
   # Update sensor data every 5 minutes
   */5 * * * * /usr/bin/Rscript /path/to/update_sensor.R 

   # Update fire data at 6:00 and 18:00
   0 6,18 * * * /usr/bin/Rscript /path/to/update_fires.R 
   
   # Update weather every hour
   0 * * * * /usr/bin/Rscript /path/to/update_weather.R 
   
   # Train model everyday at 6 AM (if few data, otherwise train only once
                                   and no need of cron jobs)
   0 6 * * * /usr/bin/Rscript /path/to/train.R
   
   # Update map every 5 min
   */5 * * * * /usr/bin/Rscript /path/to/predict.R
   
   # manage storage
   */5 * * * * /usr/bin/Rscript /path/to/data_storage.R 
   */5 * * * * /usr/bin/Rscript /path/to/interp_storage.R 




Description of the files:

1. update_fires.R

Purpose: Calls NASA FIRMS API, duplicates rows 24 times and add a timestamp 
         with each hour of the day. Update the SQL table 'fire' with the fire hotspots.

Parameters for the call:
- map_key: access token
- source: satellite (VIIRS_NOAA21_NRT)
- coordinates: area on which data are requested (longitude min, latitude min, longitude max, latitude max)
- day_range: get most recent data, from TODAY to TODAY - (DAY_RANGE-1)

Data provided by the API

| latitude   | float        | latitude of the fire (WGS84 coordinates)
| longitude  | float        | longitude of the fire (WGS84 coordinates)
| bright_ti4 | float        | brightness temperature measured in the 4 micrometers (mid-infrared) channel
| scan       | float        | spatial resolution in the x-direction (degrees), related to the size of the fire pixel
| track      | float        | spatial resolution in the y-direction (degrees), related to the size of the fire pixel
| acq_date   | date         | date of acquisition (YYYY-MM-DD)
| acq_time   | int          | time of acquisition (HHMM format UTC)
| satellite  | varchar(255) | satellite name
| instrument | varchar(255) | instrument used (MODIS)
| confidence | varchar(255) | confidence level in the fire detection
| version    | varchar(255) | version of the data processing algorithm used
| bright_ti5 | float        | brightness temperature measured in the 11 micrometers (thermal infrared) channel
| frp        | float        | fire radiative power of the fire (MW)
| daynight   | varchar(255) | D for day detection and N for Night

Row added:
| timestamp  | timestamp  | (YYYY-MM-DD HH:MM:SS)

Source: NASA VIIRS Land Science Team. (2024). 
        <i>VIIRS/JPSS2 Active Fires 6-Min L2 Swath 375m NRT</i> 
        [Data set]. NASA LANCE MODIS at the MODAPS. 
        https://doi.org/10.5067/VIIRS/VJ214IMG_NRT.002 
        Date Accessed: 2025-06-20

2. update_sensor.R

Purpose: Calls AirGradient API and add latitude and longitude of the sensor.
         Update the SQL table 'sensors' with the new data.

Data provided by the API
| locationId      | int          | Id associated to the sensor
| locationName    | varchar(255) | Name of location
| pm01            | float        | Raw PM 1.0 (ug/m3)
| pm02            | float        | Raw PM 2.5 (ug/m3)
| pm10            | float        | Raw PM 10 (ug/m3)
| pm01_corrected  | float        | PM 1.0 calibrated with EPA formula (ug/m3)
| pm02_corrected  | float        | PM 2.5 calibrated with EPA formula (ug/m3)
| pm10_corrected  | float        | PM 10 calibrated with EPA formula (ug/m3)
| pm003count      | float        | Particle count 0.3um per dL 
| atmp            | float        | Temperature in Degrees Celsius
| rhum            | float        | Relative Humidity
| rco2            | float        | CO2 (ppm)
| atmp_corrected  | float        | Temperature in Degrees Celsius with correction applied
| rhum_corrected  | float        | Relative Humidity with correction applied
| rco2_corrected  | float        | CO2 with correction applied
| tvoc            | float        | VOC raw value
| wifi            | varchar(255) | WiFi signal strength
| timestamp       | datetime     | Time of reception (YYYY-MM-DD HH:MM:SS)
| serialno        | varchar(255) | Serial Number of the monitor
| model           | varchar(255) | Current model name
| firmwareVersion | varchar(255) | Current firmware version
| tvocIndex       | int          | Senisiron VOC Index
| noxIndex        | int          | Senisirion NOx Index
| latitude        | float        | Latitude of the sensor (WGS84 coordinates)
| longitude       | float        | Longitude of the sensor (WGS84 coordinates)

Documentation: https://github.com/airgradienthq/arduino/blob/master/docs/local-server.md.


3. update_weather.R

Purpose: calls TMD API and update the SQL table 'weather' with the new data

| time  | datetime | Time of the forecast (YYYY-MM-DD HH:MM:SS)
| lat   | float    | Latitude of the forecast (WGS84 coordinates)
| lon   | float    | Longitude of the forecast (WGS84 coordinates)
| tc    | float    | Temperature (degrees Celsius)
| rh    | float    | Relative Humidity
| rain  | float    | Total rainfall for 24 hours (mm)
| slp   | float    | Sea Level Pressure (HPA)
| ws10m | float    | Wind speed at a height of 10 meters (m/s)
| wd10m | float    | Wind direction at a height of 10 meters (degree)

Documentation: https://data.tmd.go.th/nwpapi/doc/


4. variables.R

Purpose: contain all variables 

Rasters:

- ndvi, evi can be found here:
  Source: Didan, K. (2015). MOD13Q1 MODIS/Terra Vegetation Indices 
        16-Day L3 Global 250m SIN Grid V006 [Data set]. 
        NASA Land Processes Distributed Active Archive Center. 
        https://doi.org/10.5067/MODIS/MOD13Q1.006

- land cover, elevation, roads, population, national park, villages
  were given by the HealthDEEP IRL

Also includes:
- Queries to SQL table
- Name of the raster with predictions
- HTML path
- Variables for API requests
- Sensor coordinates
- Storage frequency and time for sensor and prediction data 


5. train.R 

Purpose: train a Random Forest algorithm with a set of predictors.
         The set includs:
         | Variable            | Source
         | Latitude            | Sensor coordinate
         | Longitude           | Sensor coordinate       
         | Temperature         | Weather forecast TMD       
         | Humidity            | Weather forecast TMD
         | Slp                 | Weather forecast TMD
         | Rain                | Weather forecast TMD        
         | Wind speed          | Weather forecast TMD        
         | Wind direction      | Weather forecast TMD        
         | Fire Hotspots       | NASA FIRMS        
         | NDVI                | NASA        
         | EVI                 | NASA        
         | Population          | HealthDEEP        
         | Land cover          | ESA        
         | Elevation           | HealthDEEP        
         | Roads               | HealthDEEP       
         | CO2                 | Sensor        
         | TVOC                | Sensor        
         | Temperature 2       | Sensor        
         | Humidity 2          | Sensor        
         | PM 2.5 interpolated | Sensor  (optionnal, can introduce a biais)       
         | Datetime            | Sensor        
         | Hour                | Sensor        
         | Weekday             | Sensor        
         | Sin_hour            | Sensor        
         | Cos_hour            | Sensor        
         | Sin_yearday         | Sensor        
         | Cos_yearday         | Sensor
         
         The number of trees is 100, which can be changed.

6. predict.R 

Purpose: predict PM 2.5 at current time on the studied area.
         predicting set:

         |Variable                  |Source
         |Latitude                  |Grid
         |Longitude                 |Grid
         |Temperature               |Weather forecast TMD
         |Humidity                  |Weather forecast TMD
         |Slp                       |Weather forecast TMD
         |Rain                      |Weather forecast TMD
         |Wind speed                |Weather forecast TMD
         |Wind direction            |Weather forecast TMD
         |Fire Hotspots             |NASA FIRMS
         |NDVI                      |NASA
         |EVI                       |NASA
         |Population                |HealthDEEP
         |Land cover                |ESA
         |Elevation                 |HealthDEEP
         |Roads                     |HealthDEEP
         |CO2                       |Sensor
         |TVOC                      |Sensor
         |Temperature 2             |Sensor
         |Humidity 2                |Sensor
         |PM 2.5 interpolated value |Sensor
         |Datetime                  |Sensor
         |Hour                      |Sensor
         |Weekday                   |Sensor
         |Sin_hour                  |Sensor
         |Cos_hour                  |Sensor
         |Sin_yearday               |Sensor
         |Cos_yearday               |Sensor


7. prepare_x.R

Purpose: build a set of predictors
         | Variable    | Intergration in the set
         | Latitude    | Value
         | Longitude   | Value
         | Temperature | IDW interpolation
         | Humidity    | IDW interpolation
         | Slp         | IDW interpolation
         | Rain        | IDW interpolation
         | Wind        | IDW interpolation
         | Wind        | IDW interpolation
         | Fire        | sum(1/distance*frp)
         | NDVI        | Raster: value extraction
         | EVI         | Raster: value extraction
         | Population  | Raster: value extraction
         | Land        | Raster: value extraction
         | Elevation   | Raster: value extraction
         | Roads       | Raster creation using inverse distance, then value extraction
         | CO2         | Sensor value
         | TVOC        | Sensor value
         | Temperature | Sensor value
         | Humidity    | Sensor value
         | PM          | Average sensor on last 24h + OK, Lin
         | Datetime    | Sensor value
         | Hour        | Sensor value
         | Weekday     | 0 - 6 for Monday - Sunday
         | Sin_hour    | sin(2*pi*hour/24)
         | Cos_hour    | cos(2*pi*hour/24)
         | Sin_yearday | sin(2*pi*day/365)
         | Cos_yearday | cos(2*pi*day/365)


8. tools.R
Purpose: contains auxiliary functions used in other scripts

9. mean.R 
Purpose: get a map of the average X last predictions

10.  calibration.R
Purpose: get the scaling and offset factors to calibrate the sensors

11. data_storage.R
Purpose: average old sensor's data and store them

12. interp_storage.R
Purpose: average old predictions and store them


