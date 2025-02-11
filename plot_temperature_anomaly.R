# Downloads and installs RNetCDF if it isn't installed already
if (!requireNamespace("RNetCDF", quietly = TRUE)){
  
  install.packages("RNetCDF")
}

# Downloads and installs maps if it isn't installed already
if (!requireNamespace("maps", quietly = TRUE)){
  
  install.packages("maps")
}

# Downloads and installs ggplot2 if it isn't installed already
if (!requireNamespace("ggplot2", quietly = TRUE)){
  
  install.packages("ggplot2")
}

# Imports the RNetCDF library
library(RNetCDF)
library(ggplot2)


# Defines our function to plot temperature anomaly
# user needs to input the following:
# 1) Date
# 2) URL for the THREDDS data
plot_temp_anomaly <- function (date, thredds_url) {
  
  url <- url
  date <- as.Date(date)
  # Downloading and extracting our netCDF data into a variable called data
  data <- open.nc(thredds_url)

  
  # Extract our lat/lon coordinates
  latitude <- var.get.nc(data,'lat')
  longitude <- var.get.nc(data,'lon')
  
  # Date we want to analyze
  days_since_1800 <- difftime(date, as.Date('1800-01-01'), units='days')
  
  time <- var.get.nc(data,'time')
  time_index <- which(time == days_since_1800)
  
  # Get our anomaly
  # NA in R is the same as : in Python when working with netCDF
  anom <- var.get.nc(data,'anom', start=c(NA, NA, 1, time_index), count=c(NA, NA,1,1))
  
  # Correcting the 360 degrees of data on a 180 degree map
  # If our longitude > 180, take the longitude - 360
  # Else just return the longitude
  longitude_shifted <- ifelse(longitude > 180, longitude - 360, longitude)
  
  
  # Extract data to dataframe
  df <- expand.grid(lon=longitude_shifted, lat=latitude)
  
  # Extract our anomaly data
  df$anom <- as.vector(anom)
  
  
  # Make our plot
  ggplot(df, aes(x=lon,y=lat, fill=anom)) +
    geom_tile() +
    coord_fixed(ratio = 1.5) +
    theme_minimal() +
    labs(fill = "Temperature Anomaly (Deg C)") +
    theme(legend.position = 'bottom') +
    ggtitle(paste("Temperature Anomaly", date)) +

    scale_fill_gradient2(low = 'blue', mid='white', high='red', midpoint = 0, limits = c(min(df$anom),max(df$anom))) +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group),
                 color = 'black', fill = NA)
  
  
}

# Calls our function and plots the temperature anomaly for a desired date
plot_temp_anomaly('2020-01-01', 'https://www.ncei.noaa.gov/thredds/dodsC/noaa-global-temp-v5/NOAAGlobalTemp_v5.0.0_gridded_s188001_e202212_c20230108T133308.nc')




