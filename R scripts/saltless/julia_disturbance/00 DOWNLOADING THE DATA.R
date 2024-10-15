# Script to automate the download of Copernicus files. You need to have Copernicus toolbox installed
# For help, past in the terminal: /usr/local/Caskroom/miniforge/base/envs/copernicusmarine/bin/copernicusmarine subset -h

Packages <- c("lubridate", "ncdf4", "future", "future.apply")                   # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages

# ================ Global Params ================
USERNAME = " " # input your Copernicus username
PASSWORD = " " # input your Copernicus password

path_copernicus_marine_toolbox = "/usr/local/Caskroom/miniforge/base/envs/copernicusmarine/bin/copernicusmarine" # If your are using Rstudio, add your path to the Copernicus Marine Toolbox

lon = list(-56, -36) #long and lat approximate for sbs
lat = list(-36, -21) 
depth = list(0.49, 5727.92) 

months <- seq(ymd("2022-01-31"), ymd("2022-01-31"), by = "month")

# ================ Currents ================
serviceId <- "GLOBAL_ANALYSISFORECAST_PHY_001_024"
productId <- "cmems_mod_glo_phy-cur_anfc_0.083deg_PT6H-i"

variable_uo <- c(" --variable uo")
variable_vo <- c(" --variable vo")

download_data <- function(month_start) {
  month_start <- as_date(month_start)
  output_directory <- file.path("~/Desktop/Brazilian_Shelf_disturbance/Data/Copernicus/Currents", sprintf("%02d-%d", month(month_start), year(month_start)))
  
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  next_month <- month_start %m+% months(1)
  
  timestep_start <- as.numeric(difftime(month_start, ymd("1950-01-01"), units = "hours"))
  timestep_end <- as.numeric(difftime(next_month, ymd("1950-01-01"), units = "hours")) - 1
  
  for (timestep in seq(timestep_start, timestep_end, by = 6)) {
    date <- ymd("1950-01-01") + dhours(timestep)
    
    if (date >= month_start && date <= next_month) {
      out_name <- paste("NWS_data_", "uovo_", format(date, "%Y%m%d"), "-", format(date, "%H"), "h_.nc", sep="")
      date_formatted <- paste0("\"", format(date, "%Y-%m-%d"), "T", sprintf('%02d:%02d:%02d', hour(date), 0, 0), "\"")
      
      command <- paste(path_copernicus_marine_toolbox, " subset -i", productId,
                       "-x", lon[1], "-X", lon[2],
                       "-y", lat[1], "-Y", lat[2],
                       "-t", date_formatted, "-T", date_formatted,
                       "-z", depth[1], "-Z", depth[2],
                       variable_uo, variable_vo, "-o", output_directory, "-f", out_name, 
                       "--force-download", "--username", USERNAME, "--password", PASSWORD, sep = " ")
      
      print(paste("======== Download starting on", date, "========"))
      print(command)
      system(command, intern = TRUE)
    }
  }
}

plan(multisession, workers = 6) #Optional
future_lapply(months, download_data)

# ================ Waves ================
serviceId = "GLOBAL_ANALYSISFORECAST_WAV_001_027"
productId = "cmems_mod_glo_wav_anfc_0.083deg_PT3H-i"

variable_vhm0 <- c(" --variable VHM0")
variable_vmdr <- c(" --variable VMDR")
variable_vtm01 <- c(" --variable VTM01_SW1")

for(month_start in months) {
  month_start <- as_date(month_start)
  output_directory <- file.path("~/Desktop/Brazilian_Shelf_disturbance/Data/", sprintf("%02d-%d", month(month_start), year(month_start)))
  
  if(!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  next_month <- month_start %m+% months(1) 
  
  timestep_start <- as.numeric(difftime(month_start, ymd("1950-01-01"), units = "hours"))
  timestep_end <- as.numeric(difftime(next_month, ymd("1950-01-01"), units = "hours")) - 1

  for (timestep in seq(timestep_start, timestep_end, by = 12)) {
    date <- ymd("1950-01-01") + dhours(timestep)
    date_end <- date + dhours(11)
  
    if (date >= month_start && date <= next_month) {
      out_name <- paste("mfwamglocep_", format(date, "%Y%m%d%H"), "_12H.nc", sep="")
      out_name <- gsub("03_12H.nc", "00_12H.nc", out_name)
      out_name <- gsub("15_12H.nc", "12_12H.nc", out_name)
    
      min_t <- paste0("\"",format(date, "%Y-%m-%dT%H:%M:%S"), "\"")
      max_t <- paste0("\"",format(date_end, "%Y-%m-%dT%H:%M:%S"), "\"")
    
      command <- paste(path_copernicus_marine_toolbox, " subset -i", productId,
                     "-x", lon[1], "-X", lon[2],
                     "-y", lat[1], "-Y", lat[2],
                     "-t", min_t, "-T", max_t,
                     "-z", depth[1], "-Z", depth[2],
                     variable_vhm0, variable_vmdr, variable_vtm01,"-o", output_directory, "-f", out_name, 
                     "--force-download", "--username", USERNAME, "--password", PASSWORD, sep = " ")
    
      print(paste("======== Download starting on", date, "========"))
      print(command)
      system(command, intern = TRUE)
    }  
  }
}

# ================ Tides ================
serviceId = "GLOBAL_ANALYSISFORECAST_PHY_001_024"
productId = "cmems_mod_glo_phy_anfc_merged-uv_PT1H-i"

variable_utide <- c(" --variable utide")
variable_vtide <- c(" --variable vtide")

date_min = ymd(20220101) 
date_max = ymd(20221231) 

out_name <- paste0("SMOC_", format(date_min, "%Y%m%d"), "_.nc")

for(month_start in months) {                 #if code stop working, consider adding as.date(month_start) to month_start, just like in the currents code
  output_directory <- paste("~/Desktop/Brazilian_Shelf_disturbance/Data/Copernicus/Tides",format(month_start, "%m-%Y"))
  
  if (!file.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  month_end <- as_date(month_start) %m+% months(1) - days(1)
  date_min <- as_date(month_start)
  
  while (date_min <= month_end) {
    next_date <- date_min + days(1)
    
    out_name <- paste0("SMOC_", format(date_min, "%Y%m%d"), "_.nc")
    
    command <- paste(path_copernicus_marine_toolbox, " subset -i", productId,
                     "-x", lon[1], "-X", lon[2],
                     "-y", lat[1], "-Y", lat[2],
                     "-t", format(date_min, "%Y-%m-%dT%H:%M:%S"), "-T", format(next_date - seconds(1), "%Y-%m-%dT%H:%M:%S"),
                     "-z", depth[1], "-Z", depth[2],
                     variable_utide, variable_vtide, "-o", output_directory, "-f", out_name, 
                     "--force-download", "--username", USERNAME, "--password", PASSWORD, sep = " ")
    
    print(paste("======== Download starting on", date_min, "========"))
    print(command)
    system(command, intern = TRUE)
    date_min <- next_date
  }  
}

# ================ Bathmetry ================
datasetId <- "cmems_mod_glo_phy_anfc_0.083deg_static"
output_directory <- setwd("~/Desktop/Brazilian_Shelf_disturbance/Data/Copernicus/")
out_name <- "GLO-MFC_001_024_mask_bathy_cropped.nc"

command <- paste(path_copernicus_marine_toolbox, " subset --dataset-id", datasetId,
                 "--dataset-part bathy",
                 "--minimum-longitude", lon[1], "--maximum-longitude", lon[2],
                 "--minimum-latitude", lat[1], "--maximum-latitude", lat[2],
                 "-o", output_directory, "-f", out_name, "--force-download", 
                 "--username", USERNAME, "--password", PASSWORD, sep = " ")

print(paste("======== Download starting on", Sys.time(), "========"))
print(command)
system(command, intern = TRUE)

# ================ Coordinates ================
datasetId <- "cmems_mod_glo_phy_anfc_0.083deg_static"
output_directory <- setwd("~/Desktop/Brazilian_Shelf_disturbance/Data/Copernicus/")
out_name <- "GLO-MFC_001_024_coordinates_cropped.nc"

command <- paste(path_copernicus_marine_toolbox, " subset --dataset-id", datasetId,
                 "--dataset-part coords",
                 "--minimum-longitude", lon[1], "--maximum-longitude", lon[2],
                 "--minimum-latitude", lat[1], "--maximum-latitude", lat[2],
                 "-o", output_directory, "-f", out_name,  "--force-download", 
                 "--username", USERNAME, "--password", PASSWORD, sep = " ")

print(paste("======== Download starting on", Sys.time(), "========"))
print(command)
system(command, intern = TRUE)
