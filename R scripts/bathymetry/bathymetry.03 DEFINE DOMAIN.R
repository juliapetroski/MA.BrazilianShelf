
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")        # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

GEBCO <- raster("../Shared data/GEBCO_2020.nc")
GFW <- raster("../Shared data/distance-from-shore.tif")

crop <- as(extent(-60, -35, -36, -20), "SpatialPolygons")
crs(crop) <- crs(GEBCO)

GEBCO <- crop(GEBCO, crop)
GFW <- crop(GFW, crop)

#### Polygons based on depth ####

Depths <- GEBCO
Depths[GEBCO >= 0 | GEBCO < - 1000] <- NA

Depths[Depths < -50] <- -1000
Depths[Depths > -50] <- -50

Depths <- st_as_stars(Depths) %>% 
  st_as_sf(merge = TRUE) %>% 
  st_make_valid() %>% 
  group_by(Elevation.relative.to.sea.level) %>% 
  summarise(Depth = abs(mean(Elevation.relative.to.sea.level))) %>% 
  st_make_valid()

ggplot(Depths) +
  geom_sf(aes(fill = Depth), alpha = 0.2) + 
  theme_minimal() 

#### Polygons based on distance ####

Distance <- GFW
Distance[GFW == 0 | GFW > 20] <- NA  # Distance appears to be in KM not m as stated on the website.

Distance[is.finite(Distance)] <- 20  # Distance appears to be in KM not m as stated on the website.

Distance <- st_as_stars(Distance) %>% 
  st_as_sf(merge = TRUE) %>% 
  st_make_valid() %>% 
  group_by(distance.from.shore) %>% 
  summarise(Distance = (mean(distance.from.shore))) %>% 
  st_make_valid()

ggplot() +
  geom_sf(data = Distance, fill = "red") + 
  geom_sf(data = Depths, aes(fill = Depth), alpha = 0.2) +
  theme_minimal() 

#### Expand inshore and cut offshore ####

meld <- st_union(Distance, filter(Depths, Depth == 50)) %>% 
  st_make_valid()

sf_use_s2(F)

offshore <- filter(Depths, Depth == 1000) %>% 
  st_cast("POLYGON") %>% 
  mutate(area = as.numeric(st_area(.))) %>%
  slice_max(order_by = area) 

shrunk <- bind_rows(meld, offshore) %>%
  st_make_valid() %>% 
  st_difference()

ggplot(shrunk) +
  geom_sf(aes(fill = Depth), alpha = 0.5)

#### Cut to region mask ####

clipped <- st_intersection(shrunk, st_transform(Region_mask, st_crs(shrunk)))
  
ggplot(clipped) +
  geom_sf(aes(fill = Depth), alpha = 0.5)


#### Format to domains object ####


GEBCO[GEBCO < - 500] <- -500                     # Use the overhang depth to correctly calculate elevation 

Domains <- transmute(clipped, 
                     Shore = ifelse(Depth == 50, "Inshore", "Offshore"),
                     area = as.numeric(st_area(shrunk)),
                     Elevation = exactextractr::exact_extract(GEBCO, shrunk, "mean")) %>% 
  st_transform(crs = crs)

saveRDS(Domains, "./Objects/Domains.rds")

plot(GEBCO)

