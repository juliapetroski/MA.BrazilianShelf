
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

## Generate slabr scheme, s0 drop levels not needed per pixel, and things in the rectangle we don't need

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
#summary <- list.files("../Shared data/CMEMS/Currents/", 
summary <- list.files("C:/Users/alb19154/Downloads/", 
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

#### Also import wave data (height, direction, period, same time step as currents)####

## Update the start count vectors. The grid is shared but there is a time dimension

wave_start <- c(start[1:2], 1)
wave_count <- c(count[1:2], -1)

tic()
#summary_w <- list.files("../Shared data/CMEMS/Waves/", 
 summary_w <- list.files("C:/Users/alb19154/Downloads/",                        # Test June data 
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

#### Bind data and final variables ####

all <- summary_w[summary] %>%                                                   # Fast join of wave data onto currents
  cbind(vectors_2_direction(.$u, .$v)) %>%                                      # Convert U V to speed and direction
  dplyr::select(-u, - v) %>% 
  mutate(D50 = case_when(Habitat == "Gravel" ~ 6.452,                           # Attach grain sizes from Julia in mm
                         Habitat == "Sand" & Shore == "Inshore" ~ 0.10964,
                         Habitat == "Sand" & Shore == "Offshore" ~ 0.11423,
                         Habitat == "Silt" & Shore == "Inshore"~ 0.01288,
                         Habitat == "Silt" & Shore == "Offshore"~ 0.01606))

ggplot(all %>% filter(timestep == "20230101-00h")) +
  geom_raster(aes(x=x, y=y, fill = uvSpeed))

#### Bedshear ####

tic()
stress <- shear_stress(all$Bath, all$D50, all$uvSpeed, all$uvDirection, all$swh, all$wp, all$mwd)
toc()

stress_95 <- cbind(all, stress = stress$shear_mean) %>% 
  group_by(x, y) %>% 
  summarise(stress95 = quantile(stress, 0.95))

ggplot(stress_95 %>%  filter(stress95 < 1)) +
  geom_raster(aes(x=x, y=y, fill = stress95))

disturbed <- cbind(all, stress, movement = stress$shields_number > stress$shields_critical) %>% 
  group_by(x, y) %>% 
  summarise(movement = mean(movement))

ggplot(disturbed) +
  geom_raster(aes(x=x, y=y, fill = movement))

##!! No movement, unrealistic grain sizes?
## Julia had some data

library(terra)
look <- rast("./Data/Sediment/sed_strath_diameter.tif")
plot(look)

diameter <- data.frame(distinct(depths[,1:2]), extract(look, as.matrix(distinct(depths[,6:5]))))

ggplot(data = diameter) +
  geom_raster(aes(x, y, fill = sed_strath_diameter))

all2 <- dplyr::select(all, -D50) %>% 
  left_join(diameter)

tic()
stress <- shear_stress(all2$Bath, all2$sed_strath_diameter, all2$uvSpeed, all2$uvDirection, all2$swh, all2$wp, all2$mwd)
toc()

stress_95 <- cbind(all2, stress = stress$shear_mean) %>% 
  group_by(x, y) %>% 
  summarise(stress95 = quantile(stress, 0.95))

#ggplot(stress_95 %>%  filter(stress95 < 1)) +
ggplot(stress_95) +
  geom_raster(aes(x=x, y=y, fill = stress95))

disturbed <- cbind(all2, movement = stress$shields_number > stress$shields_critical) %>% 
  group_by(x, y) %>% 
  summarise(movement = mean(movement))

ggplot(disturbed) +
  geom_raster(aes(x=x, y=y, fill = movement))

##!!! need to interpolate inshore wave action.


grab_nn <- function(dataframe) {
  
values <- drop_na(dataframe)  
blanks <- filter(dataframe, is.na(swh))                                         # Get the coordinates of cells missing values

v <- vect(values, geom=c("x", "y")) %>%                                         # Convert points with data to a spatvector
  voronoi()                                                                     # Build voronoi tessellation

filled <- extract(v, blanks[,1:2]) %>%                                          # Pull the values from the tessellation (nearest neighbour)
  group_by(id.y) %>% 
  slice_head() %>%                                                              # Keep one value per point
  ungroup() %>% 
  .[,2:ncol(.)] %>%                                                             # #drop ID column
  cbind(blanks[,1:2])                                                           # add XY
  
complete <- rbind(values, filled)                                               # bind together

}                                            # Function to overwrite missing wave data with nearest neighbour

test <- grab_nn(filter(all, timestep == timestep[1]))

#### try again ####

all3 <- split(all2, by = "timestep") %>%                                        # All zeros using both Julias summary and the tiff map
  future_map(grab_nn) %>% 
  rbindlist()

tic()
stress <- shear_stress(all3$Bath, all3$sed_strath_diameter, all3$uvSpeed, all3$uvDirection, all3$swh, all3$wp, all3$mwd)
toc()

stress_95 <- cbind(all3, stress = stress$shear_mean) %>% 
  group_by(x, y) %>% 
  summarise(stress95 = quantile(stress, 0.95))

#ggplot(stress_95 %>%  filter(stress95 < 1)) +
ggplot(stress_95) +
  geom_raster(aes(x=x, y=y, fill = stress95))

disturbed <- cbind(all3, movement = stress$shields_number > stress$shields_critical) %>% 
  group_by(x, y) %>% 
  summarise(movement = mean(movement))

ggplot(disturbed) +
  geom_raster(aes(x=x, y=y, fill = movement))









#### Calculate mean disturbance per habitat area, weighting by cell coverage and sediment fraction proportion ####

scale <- exact_extract(stack(weights), habitats, fun = 'mean') %>%         # Mean habitat disturbance weighted by sediment fraction per pixel
  setNames(c("Rock", "Gravel", "Sand", "Silt")) %>% 
  cbind(st_drop_geometry(habitats)) %>% 
  pivot_longer(-c(Habitat, Shore), names_to = "Fraction", values_to = "Proportion")

mobile <- map2_df(data, weights[2:4], ~{
  
values <- exact_extract(.x, habitats, fun = 'weighted_mean', weights = .y) %>% # Mean habitat disturbance weighted by sediment fraction per pixel
  t() %>%                                                                  # Swap habitats to columns and days to rows
  as.data.frame() %>%                                         
  setNames(paste0(habitats$Habitat, " ", habitats$Shore)) %>%              # Get habitat types as column names
  rownames_to_column(var = "Day") %>%                                      # Get day as a column
  mutate(Day = days[as.numeric(str_remove(Day, "weighted_mean.X"))],       # Clean the column
         Fraction = names(.y)) %>% 
  pivot_longer(-c(Day, Fraction), names_to = "Habitat", values_to = "Disturbance") %>%  # Collect habitat types into a column
  separate(Habitat, into = c("Habitat", "Shore"))                          # Regain the habitat meta-data
  }) %>% separate(Fraction, into = c(NA,NA,NA, "Fraction")) %>% 
  mutate(Fraction = str_to_title(Fraction))

rock_summary <- filter(mobile, Fraction == "Gravel") %>% 
  mutate(Fraction = "Rock",
         Disturbance = 0)

summary <- bind_rows(mobile, rock_summary) %>%                             # Add in Rock disturbance (0)
  left_join(scale) %>%                                                     # Attach weights of sediment classes within habitats
  group_by(Habitat, Shore, Day) %>% 
  summarise(Disturbance = weighted.mean(Disturbance, Proportion)) %>%      # Get the habitat-wide average by averaging sediment specific rates by sediment proportions per day
  ungroup() %>% 
  mutate(Month = lubridate::month(as.Date(Day, format = "%Y%m%d"))) %>%    # Convert days to months
  group_by(Month, Habitat, Shore) %>% 
  summarise(Disturbance = mean(Disturbance))                               # Average over months

saveRDS(summary, "./Objects/Habitat disturbance.rds")

ggplot(summary) +
  geom_line(aes(x = as.numeric(as.factor(Month)), y = Disturbance*100, colour = Shore, linetype = Habitat)) +
  theme_minimal() +
  scale_linetype_manual(values = c("solid", "twodash", "dotted")) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  labs(y = "Time disturbed (%)", x = "Month") +
NULL

ggsave("./Figures/saltless/Habitat disturbance.png", width = 16, height = 8, units = "cm")
