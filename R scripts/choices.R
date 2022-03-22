
Tidy_packages <- c("tidyverse", "data.table", "pbapply", "furrr", "viridis", "patchwork") # List handy data packages
Geo_packages <- c("sf", "rnaturalearth",  "raster", "stars")                # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multisession)                                                          # Choose the method to parallelise by

domain200 <- readRDS("./Objects/Domains200.rds")
domain500 <- readRDS("./Objects/Domains.rds")
domain900 <- readRDS("./Objects/Domains700.rds")


world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection


GFW <- readRDS("./Notes/Cache/domain_GFW.rds") %>% 
  st_transform(crs = crs)

ggplot() +
  geom_sf(data = world, size = 0.1, fill = "grey30", colour = "white", size = 0.1) +
  geom_sf(data = domain500, fill = "grey", colour = "black", size = 0.1) +
  geom_stars(data = GFW, colour = NA) +
  geom_sf(data = domain500, fill = NA, colour = "white", size = 0.2) +
  viridis::scale_fill_viridis(na.value = NA, trans = "log10", option = "inferno") +
  zoom +
  theme_minimal() +
  labs(caption = "Total fishing effort, 500m max depth") +
  NULL

ggsave("500m domain.png", bg = "white")

ggplot() +
  geom_sf(data = world, size = 0.1, fill = "grey30", colour = "white", size = 0.1) +
  geom_sf(data = domain200, fill = "grey", colour = "black", size = 0.1) +
  geom_stars(data = GFW, colour = NA) +
  geom_sf(data = domain200, fill = NA, colour = "white", size = 0.2) +
  viridis::scale_fill_viridis(na.value = NA, trans = "log10", option = "inferno") +
  zoom +
  theme_minimal() +
  labs(caption = "Total fishing effort, 200m max depth") +
  NULL

ggsave("200m domain.png", bg = "white")

ggplot() +
  geom_sf(data = world, size = 0.1, fill = "grey30", colour = "white", size = 0.1) +
  geom_sf(data = domain900, fill = "grey", colour = "black", size = 0.1) +
  geom_stars(data = GFW, colour = NA) +
  geom_sf(data = domain900, fill = NA, colour = "white", size = 0.2) +
  viridis::scale_fill_viridis(na.value = NA, trans = "log10", option = "inferno") +
  zoom +
  theme_minimal() +
  labs(caption = "Total fishing effort, 900m max depth") +
  NULL

ggsave("900m domain.png", bg = "white")
