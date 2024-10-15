#### Setup ####
rm(list=ls())

Packages <- c("MiMeMo.tools", "ncdf4", "furrr", "bedshear", "terra", "future", "future.apply")                     # List packages
lapply(Packages, library, character.only = TRUE) 

plan("multisession")

start <- readRDS("./Objects/CMEMS_start.rds")                                   # Object to restirct netcdf import with 
count <- readRDS("./Objects/CMEMS_count.rds")                                   # Object to restrict netcdf import with 
final_scheme <- readRDS("./Objects/CMEMS_scheme.rds")                           # Object to perform summary/detailed crop with 
coords <- readRDS("./Objects/CMEMS_coords.rds")                                 # Object to bind results to 
wave_start <- c(start[1:2], 1)                                                  # Update the start count vectors. The grid is shared but there is a time dimension
wave_count <- c(count[1:2], -1)
tide_start <- c(wave_start, 1)
tide_count <- c(wave_count, -1)

#### Iterate over Tides data ####
tic()
summary_t <- list.files("./Data/Copernicus/Tides", 
                        full.names = TRUE, recursive = TRUE) %>%                  # List data files
  future_map(~{
    
    date <- str_split(.x, "_")[[1]][[2]]
    
    times <- paste0(str_sub(date, 1, 8), "-", paste0(c(paste0(0, 0:9),10:23), "h"))   
    
    raw <- nc_open(.x)
    
    ut <- reshape2::melt(ncvar_get(raw, "utide", start = tide_start, count = tide_count), value.name = "ut") 
    vt <- reshape2::melt(ncvar_get(raw, "vtide", start = tide_start, count = tide_count), value.name = "vt")
    
    nc_close(raw)
    
    setDT(ut, key = c("Var1","Var2","Var3"))                                    # Convert to data.table without destroying memory
    setDT(vt, key = c("Var1","Var2","Var3"))
    
    colnames(ut)[1:3] <- c("x", "y", "timestep")                                # Rename columns to match currents
    
    ut <- ut[vt, vt := i.vt][                                                   # Add new columns without destroying waves in memory
      coords][                                                                  # Drop unwanted pixels
        ,timestep := times[timestep]]                                           # Update timestep values
    
  }) %>% 
  rbindlist()
toc()

setkey(summary_t, x, y, timestep, Bath, Shore, Habitat)

#J: Adding a column with the timestep equal to the cmems format to facilitate interpolation later
summary_t <- summary_t %>%
  mutate(timestep_cmems = as.numeric(difftime(as.Date(sub("-.*", "", timestep), "%Y%m%d"), as.Date("1950-01-01"), units = "hours")) + as.numeric(substr(timestep, 10, 11))) 

duplicates <- summary_t[, .N, by = .(x, y, timestep_cmems)][N > 1]              #J: checking for duplicates, it shouldn't have if everything went well at the end of code 2

#### Interpolate missing coastal cells for Tides ####
colSums(is.na(summary_t))                                                       #J: We also had NAs in tides so I applied your wave funcion

grab_nn_tides <- function(data_table) {
  
  values <- drop_na(data_table)  
  blanks <- filter(data_table, is.na(ut) | is.na(vt))                           # Get the coordinates of cells missing values
  
  v <- vect(values, geom=c("x", "y")) %>%                                       # Convert points with data to a spatvector
    voronoi()                                                                   # Build voronoi tessellation
  
  filled <- extract(v, blanks[,c("x", "y")]) %>%                                # Pull the values from the tessellation (nearest neighbour)
    group_by(id.y) %>% 
    slice_head() %>%                                                            # Keep one value per point
    ungroup() %>% 
    .[,c("ut", "vt")] %>%                                                       # Drop ID column
    cbind(blanks[,c("x", "y", "timestep", "Bath", "Shore", "Habitat", "sed_strath_diameter", "timestep_cmems")])    # J: Add all columns, I changed it because before I wasn't placing the timesteps correctly                                             
  
  complete <- rbind(values, filled, fill=TRUE)                                  # bind together
}                                            

tic()
summary_tides <- grab_nn_tides(summary_t)
toc()

ggplot(summary_tides %>% filter(timestep == "20220101-00h")) +
  geom_raster(aes(x=x, y=y, fill = ut))

colSums(is.na(summary_tides))
rm(summary_t)

#### Iterate over Currents data ####
tic()
summary_c <- list.files("./Data/Copernicus/Currents/", 
                        full.names = TRUE, recursive = TRUE) %>%                # List data files
  future_map(~{
    
    raw <- nc_open(.x)                                                          # Open file
    
    u <- ncvar_get(raw, "uo", start = c(start[1:3], 1), count = c(count[1:3], 1)) # Import variables (m/s)
    v <- ncvar_get(raw, "vo", start = c(start[1:3], 1), count = c(count[1:3], 1))    
    
    nc_close(raw)
    
    test <- cbind(coords,                                                       # pixel information
                  data.frame(timestep = str_split(.x, pattern = "_")[[1]][4]),  # Pull timestep from file name
                  u = array_w_mean(u, final_scheme),                            # calculate depth-averaged means
                  v = array_w_mean(v, final_scheme))
    
  }) %>% 
  rbindlist()                                                                   # quick bind and also conver to data.table
toc()

#J: WARNING: length of xy and uv doesnt match - don't know why, but it doesn't seem to be interfering

setkey(summary_c, x, y, timestep, Bath, Shore, Habitat)                         # Convert to data.table without destroying memory

#J: Adding a column with the timestep equal to the cmems format to facilitate interpolation later
summary_c <- summary_c %>%
  mutate(timestep_cmems = as.numeric(difftime(as.Date(sub("-.*", "", timestep), "%Y%m%d"), as.Date("1950-01-01"), units = "hours")) + as.numeric(substr(timestep, 10, 11)))

#### Interpolation currents to hourly timesteps ####
timesteps_hourly <- seq(min(summary_c$timestep_cmems), max(summary_tides$timestep_cmems), by = 1)
dates <- as.POSIXct("1950-01-01 00:00:00", tz = "UTC") + as.difftime(timesteps_hourly, units = "hours")
timesteps <- format(dates, "%Y%m%d-%Hh")

interpolate_c <- function(i, j) {
  subset_df <- summary_c[summary_c$x == i & summary_c$y == j, ]
  
  if (sum(!is.na(subset_df$u)) >= 2 && sum(!is.na(subset_df$v)) >= 2) {
    interpolated_uc <- approx(subset_df$timestep_cmems, subset_df$u, xout = timesteps_hourly)$y
    interpolated_vc <- approx(subset_df$timestep_cmems, subset_df$v, xout = timesteps_hourly)$y
    
    temp_df <- data.frame(
      x = rep(i, length(timesteps_hourly)),
      y = rep(j, length(timesteps_hourly)),
      Bath = subset_df$Bath[1], 
      Shore = subset_df$Shore[1],
      Habitat = subset_df$Habitat[1],
      sed_strath_diameter = subset_df$sed_strath_diameter[1],
      timestep = timesteps,
      timestep_cmems = timesteps_hourly,
      uc = interpolated_uc,
      vc = interpolated_vc
    )
    
    return(temp_df)
  }
  
  return(NULL)
}

plan(multisession, workers = 6)

tic()
results <- future_lapply(1:nrow(coords), function(idx) {
  i <- coords[idx, "x"]
  j <- coords[idx, "y"]
  interpolate_c(i, j)
})
hourly_c <- rbindlist(results, use.names = TRUE, fill = TRUE, idcol = FALSE)
toc()

rm(summary_c, results, interpolate_c, final_scheme)

#### Combine tides and depth-averaged currents following NOC ####

summary_currents <- merge(hourly_c, summary_tides, by = c("x", "y", "timestep", "timestep_cmems", "Bath", "Shore", "Habitat", "sed_strath_diameter"))
summary_currents$u <- summary_currents$uc + (summary_currents$ut / 1.07)
summary_currents$v <- summary_currents$vc + (summary_currents$vt / 1.07)
summary_currents[, c("uc", "vc", "ut", "vt") := NULL]

rm(summary_tides, hourly_c)
colSums(is.na(summary_currents))

#### Iterate over Wave data (height, direction, period) ####
tic()
summary_w <- list.files("./Data/Copernicus/Waves/", 
                        full.names = TRUE, recursive = TRUE) %>%                  # List data files
  future_map(~{
    
    date <- str_split(.x, "_")[[1]][[2]]
    
    if(str_sub(date, -2) == "00") times <- paste0(str_sub(date, 1, 8), "-", c("00h", "03h", "06h", "09h"))   
    if(str_sub(date, -2) == "12") times <- paste0(str_sub(date, 1, 8), "-", c("12h", "15h", "18h", "21h"))   
    
    raw <- nc_open(.x)
    
    waves <- reshape2::melt(ncvar_get(raw, "VHM0", start = wave_start, count = wave_count), value.name = "swh")  # Significant wave height
    mwd <- reshape2::melt(ncvar_get(raw, "VMDR", start = wave_start, count = wave_count), value.name = "mwd")   # mean wave direction
    wp <- reshape2::melt(ncvar_get(raw, "VTM01_SW1", start = wave_start, count = wave_count), value.name = "wp") # primary swell wave period
    
    nc_close(raw)
    
    setDT(waves, key = c("Var1","Var2","Var3"))                                     # Convert to data.table without destroying memory
    setDT(mwd, key = c("Var1","Var2","Var3"))
    setDT(wp, key = c("Var1","Var2","Var3"))
    
    colnames(waves)[1:3] <- c("x", "y", "timestep")                                 # Rename columns to match currents
    
    waves <- waves[mwd, mwd := i.mwd][                                              # Add new columns without destroying waves in memory
      wp, wp := i.wp][
        coords][                                                                    # Drop unwanted pixels
          ,timestep := times[timestep]]                                             # Update timestep values
  }) %>% 
  rbindlist()
toc()

setkey(summary_w, x, y, timestep, Bath, Shore, Habitat)

#J: Adding a column with the timestep equal to the cmems format to facilitate interpolation later
summary_w <- summary_w %>%
  mutate(timestep_cmems = as.numeric(difftime(as.Date(sub("-.*", "", timestep), "%Y%m%d"), as.Date("1950-01-01"), units = "hours")) + as.numeric(substr(timestep, 10, 11)))

### J: Interpolation Waves to hourly timesteps ####
#options(future.globals.maxSize = 1000 * 1024^2) 

interpolated_w <- function(i, j) {
  subset_df <- summary_w[summary_w$x == i & summary_w$y == j, ]
  
  if (sum(!is.na(subset_df$swh)) >= 2 && sum(!is.na(subset_df$mwd)) >= 2 && sum(!is.na(subset_df$wp)) >= 2) {
    interpolated_swh <- approx(subset_df$timestep_cmems, subset_df$swh, xout = timesteps_hourly)$y
    interpolated_mwd <- approx(subset_df$timestep_cmems, subset_df$mwd, xout = timesteps_hourly)$y
    interpolated_wp <- approx(subset_df$timestep_cmems, subset_df$wp, xout = timesteps_hourly)$y
  } else {
    interpolated_swh <- rep(NA, length(timesteps_hourly))
    interpolated_mwd <- rep(NA, length(timesteps_hourly))
    interpolated_wp <- rep(NA, length(timesteps_hourly))
  }
  
  temp_df <- data.frame(
    x = rep(i, length(timesteps_hourly)),
    y = rep(j, length(timesteps_hourly)),
    Bath = subset_df$Bath[1], 
    Shore = subset_df$Shore[1],
    Habitat = subset_df$Habitat[1],
    sed_strath_diameter = subset_df$sed_strath_diameter[1],
    timestep = timesteps,
    timestep_cmems = timesteps_hourly,
    swh = interpolated_swh,
    mwd = interpolated_mwd,
    wp = interpolated_wp
  )
    
  return(temp_df)
}

plan(multisession, workers = 6)

tic()
results <- future_lapply(1:nrow(coords), function(idx) {
  i <- coords[idx, "x"]
  j <- coords[idx, "y"]
  interpolated_w(i, j)
})
toc()

hourly_w <- rbindlist(results, use.names = TRUE, fill = TRUE, idcol = FALSE)

ggplot(hourly_w %>% filter(timestep == "20220101-00h")) +
  geom_raster(aes(x=x, y=y, fill = swh))

rm(results, coords, summary_w, count, dates, start, tide_count, tide_start, timesteps, timesteps_hourly, wave_count, wave_start, interpolated_w)

#### Interpolate missing coastal cells for waves ####
##!!## Even though the grid is shared, we have coastal cells with currents, but na for waves.

grab_nn_waves <- function(data_table) {                                         # Function to overwrite missing wave data with nearest neighbour
  
  values <- drop_na(data_table)  
  blanks <- filter(data_table, is.na(swh))                                      # Get the coordinates of cells missing values
  
  v <- vect(values, geom=c("x", "y")) %>%                                       # Convert points with data to a spatvector
    voronoi()                                                                   # Build voronoi tessellation
  
  filled <- extract(v, blanks[,c("x", "y")]) %>%                                # Pull the values from the tessellation (nearest neighbour)
    group_by(id.y) %>% 
    slice_head() %>%                                                            # Keep one value per point
    ungroup() %>% 
    .[,c("swh", "mwd", "wp")] %>%                                               # Drop ID column
    cbind(blanks[,c("x", "y", "Bath", "Shore", "Habitat", "sed_strath_diameter", "timestep", "timestep_cmems")])    # J: Add all columns, I changed it because before I wasn't placing the timesteps correctly
  
  complete <- rbind(values, filled, fill=TRUE)                                  # Bind together
}                                           

tic()
summary_waves <- grab_nn_waves(hourly_w)
toc()

colSums(is.na(summary_waves))

ggplot(summary_waves %>% filter(timestep == "20220101-00h")) +
  geom_raster(aes(x=x, y=y, fill = swh))

rm(hourly_w)

#### Combine everything ####

data <- full_join(summary_currents, summary_waves) %>%
  bind_cols(vectors_2_direction(.$u, .$v)) %>%
  dplyr::select(-u, - v)  

rm(summary_currents, summary_waves)

colSums(is.na(data))
duplicates <- data[, .N, by = .(x, y, timestep_cmems)][N > 1]

saveRDS(data, "./Objects/Data_2022.rds")

