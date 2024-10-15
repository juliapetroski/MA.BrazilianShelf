# Luckily all of the CMEMs products we are using share a grid, we want to limit to cells in the model domain as early as we can

#remotes::install_github("Jack-H-Laverick/MiMeMo.tools")
#forcremotes::install_github("Jack-H-Laverick/nemomedusR")
#install.packages("RcppArmadillo")

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "tidyverse", "ncdf4", "terra", "gstat")           # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages

#### Identify crop for import of data, latitude and longitude in domain extent ####

domain <- readRDS("./Objects/Habitats.rds") %>% 
  select(-area, -Elevation) %>%  
  st_transform(4326) %>%
  st_make_valid() 

window <- st_bbox(domain) 

raw <- nc_open("./Data/Copernicus/GLO-MFC_001_024_mask_bathy_cropped.nc")       #J:I used the file downloaded in script 01

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

raw <- nc_open("./Data/Copernicus/GLO-MFC_001_024_coordinates_cropped.nc")

thickness <- reshape2::melt(ncvar_get(raw, "e3t", start = start[1:3], count = count[1:3]))

nc_close(raw)

names(thickness) <- c("x", "y", "z", "weight")
names(depths) <- c("x", "y", "max", "Bath", "Latitude", "Longitude", "Habitat", "Shore")

## Generate slabr scheme, so drop levels not needed per pixel, and things in the rectangle we don't need

scheme <- left_join(thickness, depths) %>%                                      # Many to many relationship comes from pixels overlapping inshore and offshore zone. No problem double counting
  filter(z <= max, weight != 0, !is.na(Shore), Habitat!= "Overhang") %>%        # Drop cells below sea floor, too thin, or outside model domain 
  group_by(x, y) %>%                                                            # Group by each horizontal pixel
  mutate(group = cur_group_id()) %>% 
  ungroup %>% 
  arrange(group)

ggplot(scheme) + # %>% filter(z == 1)) +
  geom_raster(aes(x = x, y = y, fill = max))

count[3] <- max(scheme$z, na.rm = T)                                            # Update 3rd dimension to import domain max depths

final_scheme <- select(scheme, x, y, layer = z, group, weight)

coords <- distinct(select(scheme, -z, -weight)) %>% 
  select(x,y, Bath, Shore, Habitat)

setDT(coords, key = c("x", "y"))

#### Add Grain size data to the grid ####

look <- rast("./Data/Sediment/sed_strath_diameter.tif")
plot(look)

diameter <- data.frame(distinct(depths[,1:2]), extract(look, as.matrix(distinct(depths[,6:5])))) %>% 
  mutate(sed_strath_diameter = 2^(sed_strath_diameter*-1))                      # Converting phi to mm

ggplot(data = diameter) +
  geom_raster(aes(x, y, fill = log10(sed_strath_diameter)))

coords <- left_join(coords, diameter)

# J: Function to fill NA with interpolation based on nearest neighbors and same habitat
interpolate_fill <- function(coords) {                                          #J: We had four coordinates without sed diameter, so I did this interpolation
  for (i in 1:nrow(coords)) {
    if (is.na(coords[i, "sed_strath_diameter"])) {
      x <- coords[i, "x"]
      y <- coords[i, "y"]
      h <- coords[i, "Habitat"]
      
      nearest <- coords[which.min(rowSums((coords[, c("x", "y")] - c(x, y))^2)), ]
      
      if (nearest$Habitat == h) {
        coords[i, "sed_strath_diameter"] <- nearest$sed_strath_diameter
      }
    }
  }
  coords[1411, "sed_strath_diameter"] <- 0.005658615                            #J:for this specific coordinate the interpolation didn't work, I don't know why, so I used the same value as the closest coordinate of the same habitat
  return(coords)
}

coords <- interpolate_fill(coords)

coords <- coords[-c(315, 402), ]                                                #J: We also had two coords with more than one Shore type, so I removed this duplicated lines

#### Save out what we need ####

saveRDS(start, "./Objects/CMEMS_start.rds")                                     # Object to restirct netcdf import with 
saveRDS(count, "./Objects/CMEMS_count.rds")                                     # Object to restrict netcdf import with 
saveRDS(final_scheme, "./Objects/CMEMS_scheme.rds")                             # Object to perform summary/detailed crop with 
saveRDS(coords, "./Objects/CMEMS_coords.rds")                                   # Object to bind results to 

