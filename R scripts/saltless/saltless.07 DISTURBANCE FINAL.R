
# Extract natural disturbance by habitat type

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "ncdf4", "furrr", "bedshear")                                                   # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages

plan("multisession")

#### Identify crop for import of data, latitude and longitude in domain extent ####

domain <- readRDS("./Objects/Habitats.rds") %>% 
  select(-area, -Elevation) %>% 
  st_transform(4326) %>% 
  st_make_valid()

window <- st_bbox(domain)

raw <- nc_open("../Shared data/CMEMS/Currents/GLO-MFC_001_024_mask_bathy.nc")

grid <- expand.grid(y = which(between(raw$dim$latitude$vals, window$ymin, window$ymax)),
           x = which(between(raw$dim$longitude$vals, window$xmin, window$xmax)))

start <- scheme_to_start(grid)[1:3]                                             # There are only 3 dimensions in this product, so drop 4th
count <- scheme_to_count(grid)[1:3]                                             # There are only 3 dimensions in this product, so drop 4th

#### Identify max level to import (depth) ####

depths <- reshape2::melt(ncvar_get(raw, "deptho_lev", start = start[1:2], count = count[1:2])) %>% # max level
  left_join(reshape2::melt(ncvar_get(raw, "deptho", start = start[1:2], count = count[1:2]), value.name = "Bath")) %>%  # Bathymetry
  mutate(Latitude = raw$dim$latitude$vals[Var2+start[2]],
         Longitude = raw$dim$longitude$vals[Var1+start[1]])

nc_close(raw)

start[3] <-1 ; count[3] <- max(depths$value, na.rm = T)                         # Update 3rd dimension to import to max depth

ggplot(depths) +
  geom_raster(aes(x = Var1, y = Var2, fill = value))

ggplot(depths) +
  geom_raster(aes(x = Longitude, y = Latitude, fill = value))

## Identify model domain

depths <- st_as_sf(depths, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
  st_join(domain)                                                               # Identify pixels in our domain

ggplot() +
  geom_sf(data = depths, aes(colour = Habitat))

depths <- st_drop_geometry(depths)                                              # Drop slow sf formatting

#### Create 3D grid for subset, Add cell thicknesses for weighted depth average ####

raw <- nc_open("../Shared data/CMEMS/Currents/GLO-MFC_001_024_coordinates.nc")

thickness <- reshape2::melt(ncvar_get(raw, "e3t", start = start[1:3], count = count[1:3]))

nc_close(raw)

names(thickness) <- c("x", "y", "z", "weight")
names(depths) <- c("x", "y", "max", "Bath", "Latitude", "Longitude", "Habitat", "Shore")

## Generate slabr scheme, so drop levels not needed per pixel, and things in the rectangle we don't need

scheme <- left_join(thickness, depths) %>%                                      # Many to many relationship comes from pixels overlapping inshore and offshore zone. No problem double counting
  filter(z <= max, weight != 0, !is.na(Shore), Habitat!= "Overhang") %>% # Drop cells below sea floor, too thin, or outside model domain 
  group_by(x, y) %>%                            # Group by each horizontal pixel
  mutate(group = cur_group_id()) %>% 
  ungroup %>% 
  arrange(group)

ggplot(scheme %>% filter(z == 1)) +
  geom_raster(aes(x = x, y = y, fill = max))

count[3] <- max(scheme$z, na.rm = T)                                            # Update 3rd dimension to import domain max depths

final_scheme <- select(scheme, x, y, layer = z, group, weight)

coords <- distinct(select(scheme, -z, -weight)) %>% 
  select(x,y, Bath, Shore, Habitat)

setDT(coords, key = c("x", "y"))

#### iterate over data ####

tic()
summary <- list.files("../Shared data/CMEMS/Currents/", 
#summary <- list.files("C:/Users/alb19154/Downloads/", 
                                            full.names = TRUE, pattern = "uovo") %>%                  # List data files
  future_map(~{

    raw <- nc_open(.x)                                                          # Open file
  
    u <- ncvar_get(raw, "uo", start = c(start[1:3], 1), count = c(count[1:3], -1))          # Import variables (m/s)
    v <- ncvar_get(raw, "vo", start = c(start[1:3], 1), count = c(count[1:3], -1))
  
    nc_close(raw)
  
    test <- cbind(coords,                                                       # pixel information
                  data.frame(timestep = str_split(.x, pattern = "_")[[1]][4]),  # Pull timestamp from file name
                             u = array_w_mean(u, final_scheme),                 # calculate depth-averaged means
                             v = array_w_mean(v, final_scheme))
    
  }) %>% 
  rbindlist()                                                                   # quick bind and also conver to data.table
toc()

setkey(summary, x, y, timestep, Bath, Shore, Habitat)                           # Convert to data.table without destroying memory

#### Also import wave data (height, direction, period, same time step as currents) ####

## Update the start count vectors. The grid is shared but there is a time dimension

wave_start <- c(start[1:2], 1)
wave_count <- c(count[1:2], -1)

tic()
summary_w <- list.files("../Shared data/CMEMS/Waves/", 
# summary_w <- list.files("C:/Users/alb19154/Downloads/",                        # Test June data 
                                                full.names = TRUE, pattern = "mfwam") %>%                  # List data files
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
               coords][                                                         # Drop unwanted pixels
               ,timestep := times[timestep]]                                    # Update timestep values
}) %>% 
  rbindlist()
toc()

setkey(summary_w, x, y, timestep, Bath, Shore, Habitat)

ggplot(summary_w) +
  geom_raster(aes(x = x, y = y, fill = swh)) 








#### tides ####

tide_start <- c(wave_start, 1)
tide_count <- c(wave_count, -1)

tic()
summary_t <- list.files("../Shared data/CMEMS/tides/", 
                        full.names = TRUE, pattern = "SMOC") %>%                  # List data files
  future_map(~{

date <- str_split(.x, "_")[[1]][[2]]

times <- paste0(str_sub(date, 1, 8), "-", paste0(c(paste0(0, 0:9),10:23), "h"))   

raw <- nc_open(.x)

tides <- reshape2::melt(ncvar_get(raw, "utotal", start = tide_start, count = tide_count), value.name = "u")  # Significant wave height
v <- reshape2::melt(ncvar_get(raw, "vtotal", start = tide_start, count = tide_count), value.name = "v")   # mean wave direction

nc_close(raw)

setDT(tides, key = c("Var1","Var2","Var3"))                                     # Convert to data.table without destroying memory
setDT(v, key = c("Var1","Var2","Var3"))

colnames(tides)[1:3] <- c("x", "y", "timestep")                                 # Rename columns to match currents

tides <- tides[v, v := i.v][  