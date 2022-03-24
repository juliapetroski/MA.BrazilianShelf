
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

mask <- readRDS("./Objects/Domains.rds") %>%  filter(Shore == "Offshore")

#### Polygons based on depth ####

Depths <- GEBCO
Depths[GEBCO >= -500 | GEBCO < -1000] <- NA

Depths[Depths < -500] <- -1000

Depths <- st_as_stars(Depths) %>% 
  st_as_sf(merge = TRUE) %>% 
  st_make_valid() %>% 
  group_by(Elevation.relative.to.sea.level) %>% 
  summarise(Depth = abs(mean(Elevation.relative.to.sea.level))) %>% 
  st_make_valid()

ggplot(Depths) +
  geom_sf(aes(fill = Depth), alpha = 0.2) + 
  theme_minimal() 

#### Cut to domain ####

clipped <- st_intersection(st_transform(Depths, crs = st_crs(mask)), mask)

ggplot(clipped) +
  geom_sf(aes(fill = Depth), alpha = 0.5)

#### Format to domains object ####

overhang <- transmute(clipped, 
                      Shore = "Offshore",
                      area = as.numeric(st_area(clipped)),
                      Elevation = exactextractr::exact_extract(GEBCO, clipped, "mean")) %>% 
   st_transform(crs = crs)

saveRDS(overhang, "./Objects/Overhang.rds")

