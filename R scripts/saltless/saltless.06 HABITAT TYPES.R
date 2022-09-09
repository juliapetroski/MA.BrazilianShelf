
# Convert NGU classess to 8 StrathE2E habitat types

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "stars", "raster")                 # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

overhang <- readRDS("./Objects/Overhang.rds") %>%                # Import overhang to scale proportions
  st_transform(crs = 4326) %>% 
  mutate(Habitat = "Overhang")
  
#### Import case study data ####

all <- stack(raster("./Data/Sediment/Mud_strath.tif"), 
             raster("./Data/Sediment/Sand_strath.tif"), 
             raster("./Data/Sediment/Gravel_strath.tif")) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%                     # Convert tifs to dataframes
  mutate(total_strath = Mud_strath + Sand_strath + Gravel_strath,# Infer rock from left overs
         Rock_strath = 100 - total_strath) %>%                   
  pivot_longer(ends_with("strath"), names_to = "Fraction", values_to = "Cover") 

ggplot(all) +
  geom_raster(aes(x=x, y=y, fill = Cover)) +
  facet_wrap(vars(Fraction), nrow = 1)

all <- filter(all, Fraction != "total_strath") %>%               # Limit to just sediments
  group_by(x, y) %>% 
  slice_max(Cover) %>%                                           # Take the fraction with the largest share
  ungroup() %>% 
  mutate(Habitat = as.factor(Fraction)) %>%                      # Use that fraction as the habitat label
  st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 4326)

ggplot(all) +
  geom_raster(aes(x=x, y=y, fill = Habitat))

numeric_habitats <- mutate(all, Habitat = as.numeric(Habitat))   # Convert factor to numeric as st_rasterize expects numbers

sf_use_s2(FALSE)

polygons <- st_rasterize(numeric_habitats["Habitat"],            # Rasterize habiat labels   
                         nx = length(unique(all$x)),             # At the resolution of the original data
                         ny = length(unique(all$y))) %>% 
  st_as_sf(aspoints = FALSE, merge = TRUE) %>%                   # Merge pixels into contiguous polygons
  mutate(Habitat = factor(Habitat, labels = levels(all$Habitat))) %>% # Reinstate labels for factor
  group_by(Habitat) %>%  
  summarise(Habitat = Habitat[1])                                # Combine polygons into a single row per habitat

sf_use_s2(TRUE)

plot(polygons)

polygons <- st_intersection(st_make_valid(st_transform(polygons, crs = crs)), # Split sediment polygons along model zones
                            st_transform(domains, crs = crs)) %>% 
  dplyr::select(-c(Elevation, area)) %>%                         # Drop excess data
  st_transform(crs = 4326)                                       # Switch back to mercator

plot(polygons)

sf_use_s2(F)

#### Accommodate overhang ####

ggplot() +                                                       # Check for overhang overlap with sediment data
  geom_sf(data = polygons, aes(fill = Habitat)) +
  geom_sf(data = overhang, aes(fill = Habitat), colour = "red", alpha = 0.5) +
  theme_minimal()

polygons <- st_difference(polygons, overhang) %>%                # Cut overhang out of sediment
  st_cast("POLYGON") %>%                                         # Expose each shape to allow removing whispy bits
  mutate(clean = as.numeric(st_area(.))) %>%                     # Get the size of each shape, assuming whisps are small
  filter(clean > 150) %>%                                        # Adjust to get rid of ghosts around the edge of the overhang
  group_by(Habitat, Shore, area) %>%                             # Cheat way to return to "MULTIPOLYGON", and drop redundant columns
  summarise(Elevation = mean(Elevation)) %>%                     # Clean
  bind_rows(overhang) %>%                                        # Add overhang polygon
  mutate(Habitat = str_remove(Habitat, "_strath"))               # Clean Habitat labels
#sf_use_s2(T)

plot(polygons)

saveRDS(polygons, "./Objects/Habitats.rds")

#### By median grain size ####
# 
# habitats <- raster("./Data/Sediment/sed_grain size.tif") %>% 
#   as.data.frame(xy = TRUE, na.rm = TRUE) %>%
#     mutate(Habitat = as.factor(case_when(sed_grain_size > 2 ~ "Gravel",
#                                        between(sed_grain_size, 0.0625, 2) ~ "Sand",
#                                        between(sed_grain_size, 0.00098, 0.0625) ~ "Silt",
#                                        T ~ "Rock"))) %>% 
#   st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 4326)
# 
# 
# ggplot(habitats) +
#   geom_raster(aes(x=x, y=y, fill = sed_grain_size))
# 
# numeric_habitats <- mutate(habitats, Habitat = as.numeric(Habitat)) # Convert factor to numeric as st_rasterize expects numbers
# 
# sf_use_s2(FALSE)
# 
# polygons <- st_rasterize(numeric_habitats["Habitat"],            # Rasterize habiat labels   
#                          nx = length(unique(habitats$x)),        # At the resolution of the original data
#                          ny = length(unique(habitats$y))) %>% 
#   st_as_sf(aspoints = FALSE, merge = TRUE) %>%                   # Merge pixels into contiguous polygons
#   mutate(Habitat = factor(Habitat, labels = levels(habitats$Habitat))) %>% # Reinstate labels for factor
#   group_by(Habitat) %>%  
#   summarise(Habitat = Habitat[1])                                # Combine polygons into a single row per habitat
# 
# sf_use_s2(TRUE)
# 
# plot(polygons)
# 
# polygons <- st_intersection(st_make_valid(st_transform(polygons, crs = crs)), # Split sediment polygons along model zones
#                             st_transform(domains, crs = crs)) %>% 
#   select(-c(Elevation, area)) %>%                                # Drop excess data
#   st_transform(crs = 4326)                                       # Switch back to mercator
# 
# saveRDS(polygons, "./Objects/Habitats.rds")

#### Calculate proportion of model zones in each habitat ####

proportions <- polygons %>% 
  mutate(Cover = as.numeric(st_area(.))) %>%                     # Measure the area of each habitat type
  st_drop_geometry() %>%                                         # Drop SF formatting
  mutate(Cover = Cover/sum(Cover)) %>%                           # Calculate the proportion of the model zone in each sediment polygon 
  rename(Bottom = Habitat)

saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
  geom_col(aes(x = Shore, y = Cover*100, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")
